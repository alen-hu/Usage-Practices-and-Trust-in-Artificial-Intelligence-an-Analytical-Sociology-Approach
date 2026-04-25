# ==============================================================================
#     2.0 ACTIVATING PACKAGES AND LOADING DATA
# ==============================================================================
library(tidyverse)
library(igraph)
library(purrr)
agents <- readRDS("Podaci/agents.rds")
survey_data <- readRDS("Podaci/survey_data.rds")
networks_with_attributes <- readRDS("Podaci/networks_with_attributes.rds")

# ==============================================================================
#     2.1 MODEL PARAMETERS FOR CALIBRATION AND EXPERIMENTATION
# ==============================================================================
model_params <- list(
  # How much does social pressure influence the final decision on adoption?
  social_weight = 2.5,
  # Inflection point in the logistic function – i.e., the combined utility at which there is a 50% chance that the agent will adopt AI.
  adoption_threshold = 10.5,
  # Determines the steepness of the logistic curve used to convert utility into probability. The higher the value - the faster the transition from "does not use" to "uses".
  logistic_steepness = 0.5,
  # Maximum possible social threshold – i.e., the maximum percentage of friends that must be using AI for the agent to feel the pressure.
  max_threshold = 0.8,
  # Minimum possible social threshold. For some students, even 5% of friends using AI is enough to trigger social pressure.
  min_threshold = 0.05,
  # The range by which questions from Q6 (about social influences) affect the social threshold.
  influence_range = 0.75,
  # Empirical percentage of students from the survey who already use AI.
  baseline_adoption = 0.66
)

# ==============================================================================
#     2.2 SOCIAL THRESHOLD MECHANISM
# ==============================================================================

# Defining parameters for the social threshold
compute_social_threshold <- function(
  survey_data, q_items, params = model_params) {
  # Social influence composite (Likert 1–5)
  social_score <- rowMeans(survey_data[, q_items, drop = FALSE], na.rm = TRUE)
  # Normalization to 0–1
  social_norm <- (social_score - 1) / 4
  # Social threshold (higher influence - lower threshold)
  social_threshold <- model_params$max_threshold - model_params$influence_range * social_norm
  # Range limitation
  social_threshold <- pmax(
    model_params$min_threshold,
    pmin(model_params$max_threshold, social_threshold)
  )
  list(
    social_threshold = social_threshold,
    social_influence_composite = social_score
  )
}

# Extraction of key Q6 variables that directly measure the perception of social pressure
threshold_questions <- c("Q6.1", "Q6.2", "Q6.6")

# Social threshold parameters
threshold_results <- compute_social_threshold(
  survey_data = survey_data,
  q_items = threshold_questions,
  params = model_params
)

# Adding results to agent data
agents$social_threshold <- threshold_results$social_threshold
agents$social_influence_composite <- threshold_results$social_influence_composite

# Descriptive analysis of the social threshold
prijevod <- list(social_threshold = "Društveni prag", social_influence_composite = "Percepcija društvenog utjecaja (Q6)")
za_analizu <- names(prijevod)
for (varijabla in za_analizu) {
  if (varijabla %in% names(agents)) {
    srednja_vrijednost <- mean(agents[[varijabla]], na.rm = TRUE)
    standardna_devijacija <- sd(agents[[varijabla]], na.rm = TRUE)
    broj_valjanih <- sum(!is.na(agents[[varijabla]]))
    cat(sprintf("%-45s: M = %.2f, SD = %.2f, N = %d\n", prijevod[[varijabla]], srednja_vrijednost, standardna_devijacija, broj_valjanih))
  }
}

# Visualization
agents |>
  select(social_threshold, social_influence_composite) |>
  pivot_longer(cols = everything(), names_to = "varijabla", values_to = "vrijednost") |>
  mutate(varijabla = recode(varijabla, social_threshold = "Društveni prag", social_influence_composite = "Percepcija društvenog utjecaja")) |>
  ggplot(aes(x = vrijednost)) +
  geom_histogram(bins = 12, fill = "cadetblue3", alpha = 0.7, color = "white") +
  geom_vline(data = \(d) d |> group_by(varijabla) |> summarise(mean_score = mean(vrijednost, na.rm = TRUE), .groups = "drop"), aes(xintercept = mean_score), color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ varijabla, scales = "free") +
  theme_minimal() +
  labs(title = "Distribucije društvenog praga i društvenog utjecaja", x = "Vrijednosti", y = "Broj studenata") +
  theme(legend.position = "none")


# ==============================================================================
#     2.3 FUNCTIONS FOR CALCULATING SOCIAL PRESSURE
# ==============================================================================
calculate_peer_usage <- function(agent_id, network, current_usage_vector) {
  neighbor_indices <- as.numeric(neighbors(network, agent_id))
  if (length(neighbor_indices) == 0) return(0)
  neighbor_usage <- current_usage_vector[neighbor_indices]
  return(mean(neighbor_usage >= 3, na.rm = TRUE))
}

calculate_social_pressure <- function(peer_usage_rate, individual_threshold, social_factor) {
  if (peer_usage_rate > individual_threshold) {
    pressure <- social_factor * (peer_usage_rate - individual_threshold) / (1 - individual_threshold)
    return(pmax(0, pmin(1, pressure)))
  } else {
    return(0)
  }
}

# ==============================================================================
#     2.4 COMBINED ADOPTION PROBABILITY FUNCTION
# ==============================================================================
calculate_adoption_probability <- function(agent_id, network, agents_df, params = model_params) {
  desires <- agents_df$desires[agent_id]
  beliefs <- agents_df$beliefs[agent_id]
  opportunities <- agents_df$opportunities[agent_id]
  peer_usage <- calculate_peer_usage(agent_id, network, agents_df$current_usage)
  social_pressure <- calculate_social_pressure(peer_usage, agents_df$social_threshold[agent_id], agents_df$social[agent_id])
  trust <- agents_df$trust[agent_id]
  if (is.na(trust) || trust <= 2) {
    return(list(
      agent_id = agent_id,
      desires = desires,
      beliefs = beliefs,
      opportunities = opportunities,
      peer_usage_rate = peer_usage,
      social_pressure = social_pressure,
      linear_score = NA,
      adoption_probability = 0
    ))
  }
  
  linear_score <- desires + beliefs + opportunities + (params$social_weight * social_pressure)
  adoption_prob <- 1 / (1 + exp(-params$logistic_steepness * (linear_score - params$adoption_threshold)))
  
  return(list(agent_id = agent_id, desires = desires, beliefs = beliefs, opportunities = opportunities, peer_usage_rate = peer_usage, social_pressure = social_pressure, linear_score = linear_score, adoption_probability = adoption_prob))
}

# ==============================================================================
#     2.5 TESTING ON NETWORK CONNECTIVITY
# ==============================================================================

decision_results <- map_dfr(
  .x = seq_len(nrow(agents)),
  .f = \(i) {
    res <- calculate_adoption_probability(
      agent_id = i,
      network  = networks_with_attributes,
      agents_df = agents,
      params   = model_params
    )
    as_tibble(res)
  }
) |>
  mutate(
    current_usage = agents$current_usage,
    uses_ai = agents$current_usage >= 3,
    study_field = agents$study_field,
    social_threshold = agents$social_threshold,
    social_influence_composite = agents$social_influence_composite
  )
head(decision_results)

# Correlations
cor_vars <- c("desires", "beliefs", "opportunities", "peer_usage_rate", "social_pressure", "adoption_probability", "current_usage", "social_threshold", "social_influence_composite")
round(cor(decision_results[, cor_vars], use = "pairwise.complete.obs"), 3)

# Logistic regression
logit_model_empirical <- glm(
  uses_ai ~ desires + beliefs + opportunities + peer_usage_rate + social_pressure + social_threshold,
  data = decision_results,
  family = binomial
)
summary(logit_model_empirical)

# Accuracy + confusion matrix
predicted_prob_emp <- predict(logit_model_empirical, type = "response")
predicted_binary_emp <- predicted_prob_emp > 0.5

accuracy_empirical <- round(mean(predicted_binary_emp == decision_results$uses_ai, na.rm = TRUE), 3)
accuracy_empirical

table_results_emp <- table(Predicted = predicted_binary_emp, Actual = decision_results$uses_ai)
table_results_emp

# ==============================================================================
#     2.6 VALIDATION OF EMPIRICAL THRESHOLDS
# ==============================================================================

# Min, Q1, Median, Mean, Q3, Max of social threshold distribution
summary(agents$social_threshold)

# Threshold distribution by fields of study
agents |>
  group_by(study_field) |>
  summarise(
    mean_threshold = mean(social_threshold, na.rm = TRUE),
    sd_threshold = sd(social_threshold, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(mean_threshold))

# Correlation analysis with existing variables
threshold_cors <- cor(agents[, c("social_threshold", "social_influence_composite", "social", "desires", "beliefs", "opportunities", "current_usage")], use = "pairwise.complete.obs")
round(threshold_cors[1, ], 3)

# Visualization: Threshold vs current usage relationship
ggplot(agents, aes(x = social_threshold, y = current_usage)) +
  geom_jitter(alpha = 0.6, width = 0.02, height = 0.1, color = "cadetblue3") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(title = "Odnos društvenog praga i trenutačne učestalosti korištenja", x = "Društveni prag", y = "Trenutačna učestalost korištenja") +
  scale_y_continuous(breaks = 1:5, labels = c("Nijednom", "Jednom mjesečno ili rjeđe", "Nekoliko puta mjecečno", "Nekoliko puta tjedno", "Svakodnevno"))

# Visualization
decision_long <- decision_results |>
  select(desires, beliefs, opportunities, current_usage) |>
  pivot_longer(
    cols = c(desires, beliefs, opportunities),
    names_to = "component",
    values_to = "score"
  )
decision_long$component <- factor(decision_long$component, levels = c("desires", "beliefs", "opportunities"))
ggplot(decision_long, aes(x = score, y = current_usage)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5, color = "cadetblue3") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~component, scales = "free_x", labeller = as_labeller(c(desires = "Želje", beliefs = "Vjerovanja", opportunities = "Prilike"))) +
  theme_minimal() +
  labs(title = "Povezanost DBO komponenti s učestalošću korištenja AI alata", x = "Vrijednost komponente (1–5)", y = "Učestalost korištenja (1–5)") +
  theme(legend.position = "none")
 
# Select and rename variables for review
decision_results |>
  select(desires, beliefs, opportunities, peer_usage_rate, social_pressure, adoption_probability) |>
  rename(`Želja` = desires, `Vjerovanje` = beliefs, `Prilika` = opportunities, `Društvena upotreba` = peer_usage_rate, `Društveni pritisak` = social_pressure, `Vjerojatnost usvajanja` = adoption_probability) |>
  pivot_longer(cols = everything(), names_to = "Komponenta", values_to = "Vrijednost") |>
  ggplot(aes(x = Vrijednost, fill = Komponenta)) +
  geom_histogram(bins = 20, alpha = 0.7, color = "white") +
  facet_wrap(~Komponenta, scales = "free") +
  theme_minimal() +
  labs(title = "Distribucije komponenti odluke", x = "Vrijednost", y = "Broj studenata") +
  theme(legend.position = "none")

# Visualization of the logistic function
tibble(
  linear_score = seq(0, 15, by = 0.1),
  adoption_prob = 1 / (1 + exp(-model_params$logistic_steepness * (linear_score - model_params$adoption_threshold)))
) |>
  ggplot(aes(x = linear_score, y = adoption_prob)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = model_params$adoption_threshold, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Logistička funkcija vjerojatnosti usvajanja", subtitle = sprintf("Prag = %.1f, Strmina = %.2f", model_params$adoption_threshold, model_params$logistic_steepness), x = "Linear Score (D + B + O + social)", y = "Vjerojatnost usvajanja")

# ==============================================================================
#     2.7 SAVING THE AGENTS DATA FRAME AND SURVEY DATA
# ==============================================================================
saveRDS(agents, file = "Podaci/agents.rds")
saveRDS(decision_results, "Podaci/decision_results.rds")
saveRDS(model_params, "Podaci/model_params.rds")
