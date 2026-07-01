# ==============================================================================
#     1.0 PACKAGE ACTIVATION
# ==============================================================================
library(tidyverse)
library(readxl)
library(purrr)
library(igraph)
set.seed(123)

# ==============================================================================
#     1.1 LOADING DATA AND RENAME VARIABLES
# ==============================================================================
survey_data <- read_excel(
  path = "Podaci/Prakse korištenja i povjerenje u umjetnu inteligenciju.xlsx",
  skip = 1,
  na = c("", "NA", "N/A")
) |>
  setNames(c("vrijeme", paste0("Q1.", 1:6), paste0("Q2.", 1:14), paste0("Q3.", 1:8), paste0("Q4.", 1:8), paste0("Q5.", 1:8), paste0("Q6.", 1:8), paste0("Q7.", 1:8))) |>
  mutate(student_id = row_number())

# ==============================================================================
#     1.2 CREATING AN AGENT DATA FRAMEWORK
# ==============================================================================

# Conversion of questions into numerical values
# ==============================================================================
# Numerical values for questions Q2.6 - Q2.14
frequency_mapping <- c("Nikada" = 1, "Rijetko" = 2, "Ponekad" = 3, "Često" = 4, "Uvijek" = 5)
# Conversion of data into numerical values
practices_data <- survey_data[, paste0("Q2.", 5:14), drop = FALSE]
# Confirmation that Q2.5 remains a numerical value
practices_data[["Q2.5"]] <- as.numeric(practices_data[["Q2.5"]])
# Mapping of questions Q2.6–Q2.14
for (col in paste0("Q2.", 6:14)) {
  practices_data[[col]] <- as.numeric(frequency_mapping[practices_data[[col]]])
}

# Function for calculating LLM usage practices
# ==============================================================================
calculate_practice_score_vec <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Application of the function for calculating LLM usage practices
weights <- c("Q2.5" = 0.15, "Q2.6" = 0.10, "Q2.7" = 0.15, "Q2.8" = 0.15, "Q2.9" = 0.15, "Q2.10" = 0.10, "Q2.11" = 0.10, "Q2.12" = 0.15, "Q2.13" = 0.05, "Q2.14" = 0.10)
survey_data$practice_score <- practices_data |>
  mutate(practice_score = pmap_dbl(across(everything()), \(...) {
    x <- c(...)
    names(x) <- names(practices_data)
    w <- weights[names(x)]
    if (all(is.na(w))) return(NA_real_)
    calculate_practice_score_vec(x, w)
  })) |>
  pull(practice_score)

# Function for creating the agents data frame
# ==============================================================================
likert_cols <- c(paste0("Q3.", 1:8), paste0("Q4.", 1:8), paste0("Q5.", 1:8), paste0("Q6.", 1:8), paste0("Q7.", 1:8))
survey_data <- survey_data |> mutate(across(all_of(likert_cols), ~ as.numeric(.x)))

build_agents_df <- function(
  survey_data,
  practice_scores,
  usage_mapping = c("Nijednom" = 1, "Jednom mjesečno ili rjeđe" = 2, "Nekoliko puta mjesečno" = 3, "Nekoliko puta tjedno" = 4, "Svakodnevno" = 5),
  reverse_items = c("Q4.3", "Q4.7"),
  round_digits = 2
) {
  # Extraction of data for each dimension
  desires_data <- survey_data[, paste0("Q3.", 1:8), drop = FALSE]
  beliefs_data <- survey_data[, paste0("Q4.", 1:8), drop = FALSE]
  opportunities_data <- survey_data[, paste0("Q5.", 1:8), drop = FALSE]
  social_data <- survey_data[, paste0("Q6.", 1:8), drop = FALSE]
  trust_data <- survey_data[, paste0("Q7.", 1:8), drop = FALSE]
  # Reverse-coding for specific items
  for (item in reverse_items) {
    beliefs_data[[item]] <- 6 - beliefs_data[[item]]
  }
  # Conversion of responses about current level of AI usage into numerical values
  current_usage <- as.numeric(usage_mapping[survey_data$Q2.1])
  # Creation of the agents data frame
  agents <- data.frame(
    agent_id = survey_data$student_id,
    study_field = survey_data$Q1.4,
    faculty = survey_data$Q1.5,
    current_usage = current_usage,
    practice = round(practice_scores, round_digits),
    desires = round(rowMeans(desires_data, na.rm = TRUE), round_digits),
    beliefs = round(rowMeans(beliefs_data, na.rm = TRUE), round_digits),
    opportunities = round(rowMeans(opportunities_data, na.rm = TRUE), round_digits),
    social = round(rowMeans(social_data, na.rm = TRUE), round_digits),
    trust = round(rowMeans(trust_data, na.rm = TRUE), round_digits),
    stringsAsFactors = FALSE
  )
  # Setting usage practice to NA for agents who have not used AI
  agents$practice[agents$current_usage == 1] <- NA_real_
  # Returning the final result
  return(agents)
}

# Formation of the agents data frame
# ==============================================================================
agents <- build_agents_df(
  survey_data = survey_data,
  practice_scores = survey_data$practice_score
)

# ==============================================================================
#     1.3. DESCRIPTIVE STATISTICS AND VISUALIZATION
# ==============================================================================

# Analysis of current AI users
# ==============================================================================
# Defining labels for AI usage levels
usage_labels <- c("1" = "Nijednom", "2" = "Jednom mjesečno", "3" = "Nekoliko puta mjesečno", "4" = "Nekoliko puta tjedno", "5" = "Svakodnevno")
# Creation of a frequency table
usage_table <- table(agents$current_usage, useNA = "ifany")
# Loop through usage levels
for(i in 1:length(usage_table)) {
  # Segments for printing results
  level <- names(usage_table)[i]
  count <- usage_table[i]
  percent <- round(100 * count / nrow(agents), 1)
  # Converting the level into a readable label
  label <- ifelse(level %in% names(usage_labels), usage_labels[level], level)
  # Printing results in a structured format
  cat(sprintf("  %s: %d (%.1f %%)\n", label, count, percent))
}

# Descriptive check of agent components
# ==============================================================================
# Selection of components for analysis
for (comp in c("practice", "desires", "beliefs", "opportunities", "social", "trust")) {
  # Checking whether the component exists in the agents data frame
  if (comp %in% names(agents)) {
    # Descriptive statistics measures
    mean_val <- mean(agents[[comp]], na.rm = TRUE)
    sd_val <- sd(agents[[comp]], na.rm = TRUE)
    n_valid <- sum(!is.na(agents[[comp]]))
    min_val <- min(agents[[comp]], na.rm = TRUE)
    max_val <- max(agents[[comp]], na.rm = TRUE)
    # Printing results in a readable format
    cat(sprintf("%-15s: M = %.2f, SD = %.2f, N = %d, Min = %.2f, Max = %.2f\n", comp, mean_val, sd_val, n_valid, min_val, max_val))
  }
}

# Visualization
# ==============================================================================
component_levels <- c("desires", "beliefs", "opportunities", "practice", "social", "trust")
component_labels <- c(desires = "Želje", beliefs = "Vjerovanja", opportunities = "Prilike", practice = "Kvaliteta praksi korištenja", social = "Društveni utjecaji", trust = "Povjerenje")
mean_data <- agents |>
  select(practice, desires, beliefs, opportunities, social, trust) |>
  pivot_longer(cols = everything(), names_to = "component", values_to = "score") |>
  filter(!is.na(score)) |>
  mutate(component = factor(component, levels = component_levels, labels = component_labels)) |>
  group_by(component) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")
agents |>
  select(practice, desires, beliefs, opportunities, social, trust) |>
  pivot_longer(cols = everything(), names_to = "component", values_to = "score") |>
  filter(!is.na(score)) |>
  mutate(component = factor(component, levels = component_levels, labels = component_labels)) |>
  ggplot(aes(x = score, fill = component)) +
  geom_histogram(bins = 20, fill = "cadetblue3", alpha = 0.7) +
  geom_vline(data = mean_data, aes(xintercept = mean_score), color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~component, scales = "free", nrow = 2) +  # GORNJI i DONJI red (3 + 3)
  theme_minimal() +
  labs(title = "Distribucije agregiranih rezultata agenata", x = "Vrijednost (1-5)", y = "Broj studenata") +
  theme(legend.position = "none")

# ==============================================================================
#     1.4 CREATION OF THE NETWORK STRUCTURE
# ==============================================================================
# Ensuring that agent_id is a character variable
agents$agent_id <- as.character(agents$agent_id)

# Number of agents
n <- nrow(agents)

# Network structure based on field of study and faculty
# ==============================================================================
adj_matrix <- matrix(0, n, n)

# Network parameters
base_prob <- 0.02
max_field_boost <- 0.08
max_faculty_boost <- 0.045
noise_min <- 0.7
noise_max <- 1.3
p_max <- 0.95

# Filling the adjacency matrix
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    prob <- base_prob
    # 1. Bonus for the same field of study
    if (!is.na(agents$study_field[i]) && !is.na(agents$study_field[j]) &&
        agents$study_field[i] == agents$study_field[j]) {
      prob <- prob + max_field_boost
    }
    # 2. Bonus for the same faculty
    if (!is.na(agents$faculty[i]) && !is.na(agents$faculty[j]) &&
        agents$faculty[i] == agents$faculty[j]) {
      prob <- prob + max_faculty_boost
    }
    # 3. Random variation and limitation
    prob <- prob * runif(1, noise_min, noise_max)
    prob <- max(0, min(p_max, prob))
    # Generating a connection
    if (runif(1) < prob) {
      adj_matrix[i, j] <- adj_matrix[j, i] <- 1
    }
  }
}

# Creation of the graph object
networks_with_attributes <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
V(networks_with_attributes)$name <- agents$agent_id

# ==============================================================================
#     1.5 ANALYSIS OF THE NETWORK STRUCTURE
# ==============================================================================

# Calculation of modularity
membership_faculty <- as.numeric(as.factor(agents$faculty))
membership_field <- as.numeric(as.factor(agents$study_field))

mod_faculty <- if(length(unique(membership_faculty)) > 1) {
  modularity(networks_with_attributes, membership_faculty)
} else NA

mod_field <- if(length(unique(membership_field)) > 1) {
  modularity(networks_with_attributes, membership_field)
} else NA

# Data frame with network metrics
network_metrics <- data.frame(
  metrika = c("Broj čvorova", "Broj veza", "Gustoća mreže", "Koeficijent klasteriranja", "Prosječan stupanj", "Promjer mreže", "Prosječna duljina puta", "Broj komponenti", "Udio najveće komponente (%)", "Izolirani agenti", "Modularnost (fakultet)", "Modularnost (područje)"),
  vrijednost = c(
    vcount(networks_with_attributes),
    ecount(networks_with_attributes),
    round(edge_density(networks_with_attributes), 4),
    round(transitivity(networks_with_attributes), 4),
    round(mean(degree(networks_with_attributes)), 2),
    diameter(networks_with_attributes, directed = FALSE, unconnected = TRUE),
    round(mean_distance(networks_with_attributes, directed = FALSE, unconnected = TRUE), 2),
    components(networks_with_attributes)$no,
    round(100 * max(components(networks_with_attributes)$csize) / vcount(networks_with_attributes), 2),
    sum(degree(networks_with_attributes) == 0),
    round(mod_faculty, 4),
    round(mod_field, 4)
  )
)
network_metrics

# Degree distribution
degree_dist <- data.frame(
  agent_id = agents$agent_id,
  degree = degree(networks_with_attributes)
)

# Visualization of the degree distribution
ggplot(degree_dist, aes(x = degree)) +
  geom_histogram(binwidth = 2, fill = "cadetblue3", color = "white", alpha = 0.7) +
  geom_vline(xintercept = mean(degree_dist$degree), color = "red", linetype = "dashed", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Distribucija stupnjeva u mreži", x = "Broj veza (stupanj)", y = "Broj studenata")

# ==============================================================================
#     1.6 ADDING AGENT ATTRIBUTES TO THE NETWORK OBJECT
# ==============================================================================

agent_vars <- c("agent_id", "faculty", "study_field", "desires", "beliefs", "opportunities", "social", "trust", "practice", "current_usage")

add_agent_attributes <- function(net, agents, id_col = "agent_id", vars = agent_vars) {
  agents[[id_col]] <- as.character(agents[[id_col]])
  V(net)$name <- as.character(V(net)$name)
  idx <- match(V(net)$name, agents[[id_col]])
  if (anyNA(idx)) {
    missing_ids <- V(net)$name[is.na(idx)]
    stop("Neki agent_id iz mreže ne postoje u `agents`: ", paste(head(missing_ids, 10), collapse = ", "))
  }
  for (v in vars) {
    net <- igraph::set_vertex_attr(net, name = v, value = agents[[v]][idx])
  }
  net
}
networks_with_attributes <- add_agent_attributes(networks_with_attributes, agents)

# ==============================================================================
#     1.4 SAVING THE AGENTS DATA FRAME AND SURVEY DATA
# ==============================================================================
saveRDS(agents, file = "Podaci/agents.rds")
saveRDS(survey_data, file = "Podaci/survey_data.rds")
saveRDS(networks_with_attributes, file = "Podaci/networks_with_attributes.rds")
