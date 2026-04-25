# ==============================================================================
#     3.0 ACTIVATING PACKAGES AND LOADING DATA
# ==============================================================================
library(tidyverse)
library(igraph)
agents <- readRDS("Podaci/agents.rds")
survey_data <- readRDS("Podaci/survey_data.rds")
networks_with_attributes <- readRDS("Podaci/networks_with_attributes.rds")

# ==============================================================================
#     3.1 TRUST DYNAMICS PARAMETERS
# ==============================================================================
# Parameters that determine how the change in trust functions in the model.
trust_params <- list(
  quality_threshold_high = 3.5,
  quality_threshold_low = 2.5,
  min_trust = 1.0, 
  max_trust = 5.0,
  trust_to_belief_weight = 0.5,
  belief_momentum = 0.6
)

# ==============================================================================
#     3.2 EXPERIENCE CALCULATION FUNCTION (EXPECTATION-ADJUSTED)
# ==============================================================================
# Converting the quality of usage into the "experience" the student has with AI, including psychological factors – the student's expectations and beliefs

# Function for mapping quality - experience with expectation adjustment
calculate_experience_outcome <- function(
  quality_score,
  current_usage,
  desires = NULL,
  beliefs = NULL,
  params = trust_params,
  expectation_anchor = 3.0,
  expectation_weight = 0.6,
  max_adjustment = 0.5,
  clip_to_scale = TRUE
) {

  # Checking whether the input is a single valid number
  valid_scalar <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
  }

  # Standardized neutral output
  neutral <- function(expectation_adjustment = 0, base_factor = NA_real_) {
    list(
      type = "neutral",
      strength = 0,
      combined_factor = NA_real_,
      expectation_adjustment = expectation_adjustment,
      base_factor = base_factor
    )
  }

  # 1. Input safety check (scalarity + validity)
  if (!valid_scalar(quality_score) || !valid_scalar(current_usage)) {
    return(neutral())
  }
  # Scale check 1-5
  if (quality_score < 1 || quality_score > 5 || current_usage < 1 || current_usage > 5) {
    return(neutral())
  }

  # 2. Base factor (quality + intensity of use) on a scale of 1–5
  base_factor <- 0.7 * quality_score + 0.3 * current_usage

  # 3. Expectations (desires + beliefs) – correction of the base factor
  expectation_adjustment <- 0
  if (valid_scalar(desires) && valid_scalar(beliefs)) {
    # Check 1–5 also for desires/beliefs
    if (desires < 1 || desires > 5 || beliefs < 1 || beliefs > 5) {
      return(neutral(expectation_adjustment = 0, base_factor = base_factor))
    }

    expectation_level <- (desires + beliefs) / 2

    # Higher expectations - negative correction; lower expectations - positive correction
    expectation_adjustment <- expectation_weight * (expectation_anchor - expectation_level)

    # Correction limitation
    expectation_adjustment <- max(-max_adjustment, min(max_adjustment, expectation_adjustment))
  }

  # 4. Combination
  combined_factor <- base_factor + expectation_adjustment
  if (!valid_scalar(combined_factor)) {
    return(neutral(expectation_adjustment = expectation_adjustment, base_factor = base_factor))
  }
  # Keeps combined_factor strictly on a 1–5 scale
  if (clip_to_scale) {
    combined_factor <- max(1, min(5, combined_factor))
  }

  # 5. Categorization and strength of experience
  if (combined_factor >= params$quality_threshold_high) {
    experience_type <- "positive"
    experience_strength <- (combined_factor - params$quality_threshold_high) /
      (5 - params$quality_threshold_high)
  } else if (combined_factor <= params$quality_threshold_low) {
    experience_type <- "negative"
    experience_strength <- (params$quality_threshold_low - combined_factor) /
      (params$quality_threshold_low - 1)
  } else {
    experience_type <- "neutral"
    experience_strength <- 0
  }

  # 6. Strength limitation to [0,1]
  experience_strength <- max(0, min(1, experience_strength))

  # 7. Output
  list(type = experience_type, strength = experience_strength, combined_factor = combined_factor, expectation_adjustment = expectation_adjustment, base_factor = base_factor)
}

# ==============================================================================
#     3.3 TRUST CHANGE MECHANISM
# ==============================================================================
# Creating a function that changes the student's trust based on the experience they had with AI
update_trust_from_experience <- function(current_trust, experience, params = trust_params) {

  valid_scalar <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)

  # Fallback if experience is not in the expected format
  exp_type <- if (!is.null(experience) && !is.null(experience$type)) experience$type else "neutral"
  exp_strength <- if (!is.null(experience) && !is.null(experience$strength)) experience$strength else 0

  # If trust is not a valid scalar, do not touch it
  if (!valid_scalar(current_trust)) {
    return(list(old_trust = current_trust, trust_change = 0, new_trust = current_trust, experience_type = exp_type, experience_strength = exp_strength))
  }

  # Ensure strength is in [0,1]
  if (!valid_scalar(exp_strength)) exp_strength <- 0
  exp_strength <- max(0, min(1, exp_strength))

  range_trust <- params$max_trust - params$min_trust
  if (!valid_scalar(range_trust) || range_trust <= 0) {
    stop("Neispravno zadani min_trust i max_trust u params.")
  }

  trust_change <- 0
  if (exp_type == "positive") {
    trust_change <- exp_strength * (params$max_trust - current_trust) / range_trust
  } else if (exp_type == "negative") {
    trust_change <- -exp_strength * (current_trust - params$min_trust) / range_trust
  }

  new_trust <- current_trust + trust_change
  new_trust <- max(params$min_trust, min(params$max_trust, new_trust))

  list(old_trust = current_trust, trust_change = trust_change, new_trust = new_trust, experience_type = exp_type, experience_strength = exp_strength)
}

# ==============================================================================
#     3.4 BELIEF CHANGE MECHANISM (trust - beliefs feedback loop)
# ==============================================================================
update_beliefs_from_trust <- function(current_beliefs, trust_change, params = trust_params) {

  valid_scalar <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)

  # If inputs are not valid, return a neutral update
  if (!valid_scalar(current_beliefs) || !valid_scalar(trust_change)) {
    return(list(old_beliefs = current_beliefs, belief_adjustment = 0, new_beliefs = current_beliefs))
  }

  # Parameter safety
  if (!valid_scalar(params$trust_to_belief_weight)) stop("trust_to_belief_weight nije valjan broj.")
  if (!valid_scalar(params$belief_momentum) || params$belief_momentum < 0 || params$belief_momentum > 1) {
    stop("belief_momentum mora biti u rasponu [0,1].")
  }

  # Signal from trust
  belief_adjustment <- trust_change * params$trust_to_belief_weight

  # Belief inertia (the greater the momentum, the smaller the change)
  effective_adjustment <- belief_adjustment * (1 - params$belief_momentum)

  new_beliefs <- current_beliefs + effective_adjustment
  new_beliefs <- max(1, min(5, new_beliefs))

  list(old_beliefs = current_beliefs, belief_adjustment = belief_adjustment, new_beliefs = new_beliefs
  )
}

# ==============================================================================
#     3.5 SIMULATION OF TRUST DYNAMICS FOR ALL AGENTS
# ==============================================================================
# Creation of a data frame for trust dynamics results
trust_dynamics <- data.frame(
  agent_id = agents$agent_id,
  initial_trust = agents$trust,
  usage_quality = agents$practice,
  current_usage = agents$current_usage,
  initial_beliefs = agents$beliefs,
  initial_desires = agents$desires,
  stringsAsFactors = FALSE
)

n <- nrow(trust_dynamics)

# Initialization of columns
trust_dynamics$experience_type <- rep(NA_character_, n)
trust_dynamics$experience_strength <- rep(NA_real_, n)
trust_dynamics$experience_combined <- rep(NA_real_, n)
trust_dynamics$experience_adjustment <- rep(NA_real_, n)
trust_dynamics$base_factor <- rep(NA_real_, n)
trust_dynamics$trust_change <- rep(NA_real_, n)
trust_dynamics$updated_trust <- rep(NA_real_, n)
trust_dynamics$belief_adjustment <- rep(NA_real_, n)
trust_dynamics$updated_beliefs <- rep(NA_real_, n)

# Running trust dynamics by agents
for (i in seq_len(n)) {

  # Local aliases (more readable and faster)
  quality <- trust_dynamics$usage_quality[i]
  usage <- trust_dynamics$current_usage[i]
  desires <- trust_dynamics$initial_desires[i]
  beliefs <- trust_dynamics$initial_beliefs[i]
  trust <- trust_dynamics$initial_trust[i]

  # 1. Experience calculation
  experience <- calculate_experience_outcome(quality_score = quality, current_usage = usage, desires = desires, beliefs = beliefs)

  trust_dynamics$experience_type[i] <- experience$type
  trust_dynamics$experience_strength[i] <- experience$strength
  trust_dynamics$experience_combined[i] <- experience$combined_factor
  trust_dynamics$experience_adjustment[i] <- experience$expectation_adjustment
  trust_dynamics$base_factor[i] <- experience$base_factor

  # 2. Trust update
  trust_update <- update_trust_from_experience(current_trust = trust, experience = experience)

  trust_dynamics$trust_change[i] <- trust_update$trust_change
  trust_dynamics$updated_trust[i] <- trust_update$new_trust

  # 3. Belief update (feedback loop)
  belief_update <- update_beliefs_from_trust(current_beliefs = beliefs, trust_change = trust_update$trust_change)

  trust_dynamics$belief_adjustment[i] <- belief_update$belief_adjustment
  trust_dynamics$updated_beliefs[i] <- belief_update$new_beliefs
}

# ==============================================================================
#     3.6 VALIDATION AND ANALYSIS OF TRUST DYNAMICS
# ==============================================================================

# Basic statistics
stats_basic <- c(
  initial_min = min(trust_dynamics$initial_trust, na.rm = TRUE),
  initial_max = max(trust_dynamics$initial_trust, na.rm = TRUE),
  updated_min = min(trust_dynamics$updated_trust, na.rm = TRUE),
  updated_max = max(trust_dynamics$updated_trust, na.rm = TRUE),
  change_mean = mean(trust_dynamics$trust_change, na.rm = TRUE),
  change_sd   = sd(trust_dynamics$trust_change, na.rm = TRUE)
)
round(stats_basic, 3)

# Distribution of experience types
exp_table <- table(trust_dynamics$experience_type, useNA = "ifany")
exp_percent <- round(100 * prop.table(exp_table), 1)
exp_table
exp_percent

# Correlation matrix of key variables
vars <- c("usage_quality", "initial_trust", "trust_change", "updated_trust", "current_usage", "experience_strength", "initial_beliefs", "updated_beliefs", "initial_desires", "experience_adjustment", "base_factor")
trust_cors <- cor(trust_dynamics[, vars], use = "pairwise.complete.obs")
round(trust_cors, 3)

# Correlation between quality - updated trust
quality_trust_cor <- cor(trust_dynamics$usage_quality, trust_dynamics$updated_trust, use = "pairwise.complete.obs")
round(quality_trust_cor, 3)

# Regression: updated_trust ~ quality + usage + initial_trust
trust_model <- lm(updated_trust ~ usage_quality + current_usage + initial_trust, data = trust_dynamics)
summary(trust_model)

# Change mechanism: trust_change
trust_change_model <- lm(trust_change ~ usage_quality + current_usage + initial_trust, data = trust_dynamics)
summary(trust_change_model)

# Expectation effects
expectation_level <- (trust_dynamics$initial_desires + trust_dynamics$initial_beliefs) / 2

round(range(trust_dynamics$experience_adjustment, na.rm = TRUE), 3)
round(cor(expectation_level, trust_dynamics$experience_adjustment, use = "pairwise.complete.obs"), 3)

round(mean(trust_dynamics$experience_adjustment[expectation_level > 4], na.rm = TRUE), 3)
round(mean(trust_dynamics$experience_adjustment[expectation_level < 3], na.rm = TRUE), 3)

# ==============================================================================
#     3.7 TRUST DYNAMICS VISUALIZATIONS
# ==============================================================================

# Filtering NA before visualization
trust_dynamics_valid <- trust_dynamics |> filter(!is.na(usage_quality))

# Visualization: Correlation between usage quality and trust change
ggplot(trust_dynamics_valid, aes(x = usage_quality, y = trust_change)) +
  geom_point(aes(color = experience_type), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Korelacija između kvalitete korištenja i promjene povjerenja", x = "Kvaliteta korištenja", y = "Promjena povjerenja") +
  scale_color_manual(values = c("positive" = "green", "neutral" = "gray", "negative" = "red"))

# Visualization: Trust change depending on the type of experience
ggplot(trust_dynamics_valid, aes(x = experience_type, y = trust_change, fill = experience_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Promjena povjerenja ovisno o tipu iskustva", x = "Tip iskustva", y = "Promjena povjerenja") +
  scale_fill_manual(values = c("positive" = "green", "neutral" = "gray", "negative" = "red")) +
  theme(legend.position = "none")

# ==============================================================================
#     3.8 SAVING TRUST DYNAMICS PARAMETERS
# ==============================================================================
saveRDS(trust_params, file = "Podaci/trust_params.rds")
saveRDS(trust_dynamics, file = "Podaci/trust_dynamics.rds")
