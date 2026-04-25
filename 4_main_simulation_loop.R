# ==============================================================================
#     4.0 IMPORTING REQUIRED PACKAGES AND DATA
# ==============================================================================
library(tidyverse)
library(igraph)
source("0_funkcije_za_ABM_simulaciju.R", encoding = "UTF-8")
agents <- readRDS("Podaci/agents.rds")
networks_with_attributes <- readRDS("Podaci/networks_with_attributes.rds")
decision_results <- readRDS("Podaci/decision_results.rds")
model_params <- readRDS("Podaci/model_params.rds")
trust_params <- readRDS("Podaci/trust_params.rds")

# ==============================================================================
#     4.1 SIMULATION PARAMETERS
# ==============================================================================

simulation_params <- list(
  n_timesteps = 100,
  random_seed = 42,
  data_collection_frequency = 1,
  adoption_memory_decay = 0.50,
  experience_variance = 0.1
)

# ==============================================================================
#     4.2 SIMULATION INITIALIZATION
# ==============================================================================
# Setting the initial state of the simulation using survey data

# Function for initialization based on survey data
initialize_adopters_empirical <- function(agents_df, seed = NULL) {
  
  # 0. Aligning types for merging
  agents_df$agent_id <- as.integer(agents_df$agent_id)
  
  # 1. Merging only uses_ai by agent_id
  adopter_status <- decision_results |>
    select(agent_id, uses_ai) |>
    rename(is_adopter = uses_ai)
  
  agents_df <- left_join(agents_df, adopter_status, by = "agent_id")
  
  # 2. Setting the adoption timestep
  agents_df$adoption_timestep <- ifelse(agents_df$is_adopter, 0, NA)
  
  # 3. Initializing accumulated experience
  agents_df$accumulated_experience <- 0
  
  # 4. Setting initial experience for existing users (using practice)
  existing_adopters <- which(agents_df$is_adopter)
  if(length(existing_adopters) > 0) {
    agents_df$accumulated_experience[existing_adopters] <- agents_df$practice[existing_adopters]
  }
  return(agents_df)
}

# Initializing the simulation with empirical data before the simulation itself
set.seed(simulation_params$random_seed)
simulation_agents <- initialize_adopters_empirical(agents)
# Printing results
validation_summary <- tibble(
  metric = c("Broj adoptera", "Postotak adoptera", "Prosječan DESIRES (adopters)", "Prosječan DESIRES (non-adopters)", "Razlika DESIRES", "Prosječan BELIEFS (adopters)", "Prosječan BELIEFS (non-adopters)", "Razlika BELIEFS", "Prosječan OPPORTUNITIES (adopters)", "Prosječan OPPORTUNITIES (non-adopters)", "Razlika OPPORTUNITIES"),
  value = c(
    sum(simulation_agents$is_adopter),
    round(100 * mean(simulation_agents$is_adopter), 1),
    round(mean(simulation_agents$desires[simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$desires[!simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$desires[simulation_agents$is_adopter], na.rm = TRUE) - mean(simulation_agents$desires[!simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$beliefs[simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$beliefs[!simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$beliefs[simulation_agents$is_adopter], na.rm = TRUE) - mean(simulation_agents$beliefs[!simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$opportunities[simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$opportunities[!simulation_agents$is_adopter], na.rm = TRUE), 2),
    round(mean(simulation_agents$opportunities[simulation_agents$is_adopter], na.rm = TRUE) - mean(simulation_agents$opportunities[!simulation_agents$is_adopter], na.rm = TRUE), 2)
  )
)
validation_summary

# ==============================================================================
#     4.3 DATA STORAGE STRUCTURES
# ==============================================================================
# Creating empty data frames for storing data during the simulation

# Individual overview - How do individual students change over time?
agent_history <- data.frame(
  timestep = integer(),
  agent_id = integer(),
  is_adopter = logical(),
  trust_level = numeric(),
  beliefs_level = numeric(),
  peer_usage_rate = numeric(),
  adoption_probability = numeric(),
  stringsAsFactors = FALSE
)

# Population overview - How does the entire population change over time?
timestep_stats <- data.frame(
  timestep = integer(),
  total_adopters = integer(),
  adoption_rate = numeric(),
  mean_trust = numeric(),
  mean_beliefs = numeric(),
  mean_peer_usage = numeric(),
  new_adopters = integer(),
  mean_adoption_prob = numeric(),
  stringsAsFactors = FALSE
)

# Network effects - How does the network change over time?
network_stats <- data.frame(
  timestep = integer(),
  adopter_clustering = numeric(),
  assortativity_adoption = numeric(),
  mean_adopter_degree = numeric(),
  stringsAsFactors = FALSE
)

# ==============================================================================
#     4.4 CORE SIMULATION FUNCTIONS
# ==============================================================================
# Function for calculating peer usage rate for each agent: what proportion of their friends use AI
update_peer_usage_rates <- function(agents_df, network) {
  peer_rates <- numeric(nrow(agents_df))
  for(i in 1:nrow(agents_df)) {
    neighbors <- neighbors(network, i)
    if(length(neighbors) > 0) {
      peer_rates[i] <- mean(agents_df$is_adopter[neighbors])
    } else {
      peer_rates[i] <- 0
    }
  }
  return(peer_rates)
}

# Function for group decision making
batch_adoption_decisions <- function(agents_df, network, model_params) {
  n <- nrow(agents_df)
  decisions <- logical(n)
  probabilities <- numeric(n)
  # Updating peer usage rates
  peer_usage_rates <- update_peer_usage_rates(agents_df, network)
  for(i in 1:n) {
    if(!agents_df$is_adopter[i]) {
      # Calculating adoption probability
      decision_result <- calculate_adoption_probability(i, network, agents_df, model_params)
      probabilities[i] <- decision_result$adoption_probability
      # Probabilistic decision
      decisions[i] <- runif(1) < probabilities[i]
    }
  }
  return(list(decisions = decisions, probabilities = probabilities, peer_usage_rates = peer_usage_rates))
}

# Function for experience and trust updates for adopters - Trust dynamics
update_adopter_experiences <- function(agents_df, trust_params, simulation_params, network) {
  for (i in 1:nrow(agents_df)) {
    if (isTRUE(agents_df$is_adopter[i])) {
      
      # 1. Simulation of quality with variability
      experience_noise <- rnorm(1, 0, simulation_params$experience_variance)
      usage_quality <- agents_df$practice[i]
      if (is.null(usage_quality) || is.na(usage_quality) || !is.finite(usage_quality)) next
      
      quality_with_noise <- usage_quality + experience_noise
      quality_with_noise <- pmax(1, pmin(5, quality_with_noise))
      
      # 2. Experience calculation
      experience <- calculate_experience_outcome(
        quality_score = quality_with_noise,
        current_usage = agents_df$current_usage[i],
        desires = agents_df$desires[i],
        beliefs = agents_df$beliefs[i],
        params = trust_params
      )
      if (is.null(experience$combined_factor) || is.na(experience$combined_factor)) next
      
      # 3. Trust update
      trust_update <- update_trust_from_experience(
        current_trust = agents_df$trust[i],
        experience = experience,
        params = trust_params
      )
      agents_df$trust[i] <- trust_update$new_trust
      
      # 4. Belief update
      belief_update <- update_beliefs_from_trust(
        current_beliefs = agents_df$beliefs[i],
        trust_change = trust_update$trust_change,
        params = trust_params
      )
      agents_df$beliefs[i] <- belief_update$new_beliefs
      
      # 5. Accumulated experience update
      if (
        !is.null(agents_df$accumulated_experience[i]) &&
        !is.na(agents_df$accumulated_experience[i]) &&
        !is.null(quality_with_noise) &&
        !is.na(quality_with_noise)
      ) {
        agents_df$accumulated_experience[i] <- agents_df$accumulated_experience[i] *
          simulation_params$adoption_memory_decay + quality_with_noise
      }
    }
  }
  
  # 6. Network learning for non-adopters (imitation)
  for (i in 1:nrow(agents_df)) {
    if (!isTRUE(agents_df$is_adopter[i])) {
      peer_usage <- calculate_peer_usage(i, network, agents_df$current_usage)
      if (!is.na(peer_usage) && peer_usage > 0.5) {
        agents_df$trust[i] <- pmin(trust_params$max_trust, agents_df$trust[i] + 0.01)
        agents_df$beliefs[i] <- pmin(5.0, agents_df$beliefs[i] + 0.005)
      }
    }
  }
  return(agents_df)
}

# Function for data collection in a single timestep
collect_timestep_data <- function(t, agents_df, decisions, network) {
  # Agent-level data
  if(t %% simulation_params$data_collection_frequency == 0) {
    sample_agents <- seq(1, nrow(agents_df), by = 10)
    agent_data <- data.frame(
      timestep = t,
      agent_id = agents_df$agent_id[sample_agents],
      is_adopter = agents_df$is_adopter[sample_agents],
      trust_level = agents_df$trust[sample_agents],
      beliefs_level = agents_df$beliefs[sample_agents],
      peer_usage_rate = decisions$peer_usage_rates[sample_agents],
      adoption_probability = decisions$probabilities[sample_agents],
      stringsAsFactors = FALSE
    )
    agent_history <<- rbind(agent_history, agent_data)
  }
  # Aggregate statistics
  timestep_data <- data.frame(
    timestep = t,
    total_adopters = sum(agents_df$is_adopter),
    adoption_rate = mean(agents_df$is_adopter),
    mean_trust = mean(agents_df$trust, na.rm = TRUE),
    mean_beliefs = mean(agents_df$beliefs, na.rm = TRUE),
    mean_peer_usage = mean(decisions$peer_usage_rates, na.rm = TRUE),
    new_adopters = sum(decisions$decisions),
    mean_adoption_prob = mean(decisions$probabilities[!agents_df$is_adopter], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  timestep_stats <<- rbind(timestep_stats, timestep_data)
  # Network metrics
  if(t %% 10 == 0) {
    adopter_ids <- which(agents_df$is_adopter)
    if(length(adopter_ids) > 1) {
      adopter_subgraph <- induced_subgraph(network, adopter_ids)
      network_data <- data.frame(
        timestep = t,
        adopter_clustering = transitivity(adopter_subgraph, type = "global"),
        assortativity_adoption = assortativity(network, as.numeric(agents_df$is_adopter)),
        mean_adopter_degree = mean(degree(network)[adopter_ids]),
        stringsAsFactors = FALSE
      )
      network_stats <<- rbind(network_stats, network_data)
    }
  }
}

# ==============================================================================
#     4.5 MAIN SIMULATION LOOP
# ==============================================================================
# Creating the main function that runs the ABM simulation through 100 time steps

# Main simulation function
run_abm_simulation <- function(initial_agents, network, params = simulation_params) {
  # Initialization
  agents_state <- initial_agents
  # Progress tracking
  progress_points <- seq(10, params$n_timesteps, by = 10)
  # Main simulation loop
  for(t in 1:params$n_timesteps) {
    # 1. Decision making for non-adopters
    decisions <- batch_adoption_decisions(agents_state, network, model_params)
    # 2. Applying adoption decisions
    new_adopters <- which(decisions$decisions & !agents_state$is_adopter)
    if(length(new_adopters) > 0) {
      agents_state$is_adopter[new_adopters] <- TRUE
      agents_state$adoption_timestep[new_adopters] <- t
      agents_state$accumulated_experience[new_adopters] <- agents_state$practice[new_adopters]
    }
    # 3. Updating experiences and trust for existing adopters
    agents_state <- update_adopter_experiences(agents_state, trust_params, simulation_params, network)
    # 4. Data collection
    collect_timestep_data(t, agents_state, decisions, network)
    # 5. Progress report
    if(t %in% progress_points) {
      adoption_rate <- round(100 * mean(agents_state$is_adopter), 1)
      adoption_rate
    }
    # 6. Early stopping if everyone becomes adopters
    if(mean(agents_state$is_adopter) >= 0.99) {
      break
    }
  }
  return(list(final_agents = agents_state, timestep_stats = timestep_stats, agent_history = agent_history, network_stats = network_stats, simulation_params = params))
}

# ==============================================================================
#     4.6 RUNNING THE BASIC SIMULATION
# ==============================================================================
basic_simulation <- run_abm_simulation(
  simulation_agents, 
  networks_with_attributes,
  simulation_params
)

# ==============================================================================
#     4.7 BASIC ANALYSES AND VALIDATION
# ==============================================================================

# Creating a tibble with explanations and values
stimulation_results <- tibble::tibble(
  Metoda = c("Finalna stopa usvajanja (%)", "Broj izvršenih vremenskih koraka", "Ukupan broj usvajača na kraju", "Početno povjerenje (anketa)", "Završno povjerenje (simulacija)", "Promjena povjerenja", "Ukupan broj agenata", "Broj početnih usvajača (empirijski)", "Timestep s najviše novih usvajača", "Maksimalni broj novih usvajača (1 korak)", "Početno prosječno povjerenje (timestep 1)", "Završno prosječno povjerenje (zadnji timestep)", "Ukupna promjena prosječnog povjerenja", "Prosječna stopa korištenja među kolegama", "Prosječna vjerojatnost usvajanja (non-adopters)"),
  Vrijednost = c(
    round(100 * mean(basic_simulation$final_agents$is_adopter), 1),
    max(basic_simulation$timestep_stats$timestep),
    sum(basic_simulation$final_agents$is_adopter),
    round(mean(agents$trust, na.rm = TRUE), 2),
    round(mean(basic_simulation$final_agents$trust, na.rm = TRUE), 2),
    round(mean(basic_simulation$final_agents$trust, na.rm = TRUE) - mean(agents$trust, na.rm = TRUE), 2),
    nrow(basic_simulation$final_agents),
    sum(decision_results$uses_ai),
    basic_simulation$timestep_stats$timestep[which.max(basic_simulation$timestep_stats$new_adopters)],
    max(basic_simulation$timestep_stats$new_adopters),
    basic_simulation$timestep_stats$mean_trust[1],
    basic_simulation$timestep_stats$mean_trust[nrow(basic_simulation$timestep_stats)],
    basic_simulation$timestep_stats$mean_trust[nrow(basic_simulation$timestep_stats)] - basic_simulation$timestep_stats$mean_trust[1],
    mean(basic_simulation$timestep_stats$mean_peer_usage, na.rm = TRUE),
    mean(basic_simulation$timestep_stats$mean_adoption_prob, na.rm = TRUE)
  )
)
stimulation_results

# Comparison of state before and after the simulation
simulation_comparison <- tibble(
  komponenta = c("Broj korisnika LLM-ova", "Broj nekorisnika LLM-ova", "Postotak korisnika (%)", "Povjerenje", "Vjerovanja"),
  prije_simulacije = c(
    sum(simulation_agents$is_adopter),
    sum(!simulation_agents$is_adopter),
    round(100 * mean(simulation_agents$is_adopter), 1),
    round(mean(simulation_agents$trust, na.rm = TRUE), 2),
    round(mean(simulation_agents$beliefs, na.rm = TRUE), 2)
  ),
  na_kraju_simulacije = c(
    sum(basic_simulation$final_agents$is_adopter),
    sum(!basic_simulation$final_agents$is_adopter),
    round(100 * mean(basic_simulation$final_agents$is_adopter), 1),
    round(mean(basic_simulation$final_agents$trust, na.rm = TRUE), 2),
    round(mean(basic_simulation$final_agents$beliefs, na.rm = TRUE), 2)
  )
) |>
  mutate(razlika = round(na_kraju_simulacije - prije_simulacije, 2))
simulation_comparison

# Adoption curve analysis
adoption_curve <- timestep_stats |>
  select(timestep, adoption_rate, total_adopters, new_adopters) |>
  mutate(
    cumulative_new = cumsum(new_adopters),
    adoption_velocity = c(0, diff(adoption_rate))
  )
adoption_curve

# Trust evolution analysis
trust_evolution <- timestep_stats |>
  select(timestep, mean_trust, mean_beliefs) |>
  filter(!is.na(mean_trust))
trust_evolution

# Peak adoption periods
peak_adoption <- adoption_curve |> arrange(timestep)
peak_adoption

# Data preparation
combined <- left_join(trust_evolution, adoption_curve, by = "timestep") %>%
  filter(!is.na(mean_trust), !is.na(total_adopters))
# Finding the maximum value for scaling
max_trust <- max(combined$mean_trust, na.rm = TRUE)
max_adopters <- max(combined$total_adopters, na.rm = TRUE)
scaling_factor <- max_adopters / max_trust
# Creating a graph with two axes
ggplot(combined, aes(x = timestep)) +
  geom_line(aes(y = total_adopters, color = "Broj korisnika"), linewidth = 1.2) +
  geom_line(aes(y = mean_trust * scaling_factor, color = "Povjerenje u AI"), linewidth = 1.2, linetype = "dashed") +
  scale_y_continuous(name = "Broj korisnika AI alata", sec.axis = sec_axis(~ . / scaling_factor, name = "Prosječno povjerenje u AI")) +
  scale_color_manual(values = c("Broj korisnika" = "steelblue", "Povjerenje u AI" = "darkorange")) +
  labs(title = "Promjena broja korisnika i povjerenja u AI tijekom simulacije", x = "Vremenski korak", color = "Mjera") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3. Joint evolution of trust and beliefs
trust_evolution %>%
  pivot_longer(cols = c(mean_trust, mean_beliefs), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = timestep, y = value, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("mean_trust" = "darkgreen", "mean_beliefs" = "darkorange"),
    labels = c("Povjerenje", "Vjerovanja")
  ) +
  theme_minimal() +
  labs(title = "Evolucija povjerenja i vjerovanja u AI", x = "Vremenski korak", y = "Vrijednost", color = "")

# ==============================================================================
#     4.8 SAVING SIMULATION RESULTS
# ==============================================================================
saveRDS(simulation_params, file = "Podaci/simulation_params.rds")
saveRDS(simulation_agents, file = "Podaci/simulation_agents.rds")
saveRDS(basic_simulation, file = "Podaci/basic_simulation.rds")
saveRDS(timestep_stats, file = "Podaci/timestep_stats.rds")
saveRDS(agent_history, file = "Podaci/agent_history.rds")
saveRDS(network_stats, file = "Podaci/network_stats.rds")
