# ==============================================================================
#     00. FUNCTIONS FOR ABM SIMULATION
# ==============================================================================

# Decision-making functions
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

calculate_adoption_probability <- function(agent_id, network, agents_df, params) {
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
  return(list(
    agent_id = agent_id,
    desires = desires,
    beliefs = beliefs,
    opportunities = opportunities,
    peer_usage_rate = peer_usage,
    social_pressure = social_pressure,
    linear_score = linear_score,
    adoption_probability = adoption_prob
  ))
}

# Functions for trust dynamics
calculate_experience_outcome <- function(quality_score, current_usage, desires = NULL, beliefs = NULL, params, expectation_anchor = 3.0, expectation_weight = 0.6, max_adjustment = 0.5, clip_to_scale = TRUE) {
  valid_scalar <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
  }
  neutral <- function(expectation_adjustment = 0, base_factor = NA_real_) {
    list(
      type = "neutral",
      strength = 0,
      combined_factor = NA_real_,
      expectation_adjustment = expectation_adjustment,
      base_factor = base_factor
    )
  }
  if (!valid_scalar(quality_score) || !valid_scalar(current_usage)) {
    return(neutral())
  }
  if (quality_score < 1 || quality_score > 5 || current_usage < 1 || current_usage > 5) {
    return(neutral())
  }
  base_factor <- 0.7 * quality_score + 0.3 * current_usage
  expectation_adjustment <- 0
  if (valid_scalar(desires) && valid_scalar(beliefs)) {
    if (desires < 1 || desires > 5 || beliefs < 1 || beliefs > 5) {
      return(neutral(expectation_adjustment = 0, base_factor = base_factor))
    }
    expectation_level <- (desires + beliefs) / 2
    expectation_adjustment <- expectation_weight * (expectation_anchor - expectation_level)
    expectation_adjustment <- max(-max_adjustment, min(max_adjustment, expectation_adjustment))
  }
  combined_factor <- base_factor + expectation_adjustment
  if (!valid_scalar(combined_factor)) {
    return(neutral(expectation_adjustment = expectation_adjustment, base_factor = base_factor))
  }
  if (clip_to_scale) {
    combined_factor <- max(1, min(5, combined_factor))
  }
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
  experience_strength <- max(0, min(1, experience_strength))
  list(
    type = experience_type,
    strength = experience_strength,
    combined_factor = combined_factor,
    expectation_adjustment = expectation_adjustment,
    base_factor = base_factor
  )
}

update_trust_from_experience <- function(current_trust, experience, params) {
  valid_scalar <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
  exp_type <- if (!is.null(experience) && !is.null(experience$type)) experience$type else "neutral"
  exp_strength <- if (!is.null(experience) && !is.null(experience$strength)) experience$strength else 0
  if (!valid_scalar(current_trust)) {
    return(list(
      old_trust = current_trust,
      trust_change = 0,
      new_trust = current_trust,
      experience_type = exp_type,
      experience_strength = exp_strength
    ))
  }
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
  list(
    old_trust = current_trust,
    trust_change = trust_change,
    new_trust = new_trust,
    experience_type = exp_type,
    experience_strength = exp_strength
  )
}

update_beliefs_from_trust <- function(current_beliefs, trust_change, params) {
  valid_scalar <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
  if (!valid_scalar(current_beliefs) || !valid_scalar(trust_change)) {
    return(list(
      old_beliefs = current_beliefs,
      belief_adjustment = 0,
      new_beliefs = current_beliefs
    ))
  }
  if (!valid_scalar(params$trust_to_belief_weight)) stop("trust_to_belief_weight nije valjan broj.")
  if (!valid_scalar(params$belief_momentum) || params$belief_momentum < 0 || params$belief_momentum > 1) {
    stop("belief_momentum mora biti u rasponu [0,1].")
  }
  belief_adjustment <- trust_change * params$trust_to_belief_weight
  effective_adjustment <- belief_adjustment * (1 - params$belief_momentum)
  new_beliefs <- current_beliefs + effective_adjustment
  new_beliefs <- max(1, min(5, new_beliefs))
  list(
    old_beliefs = current_beliefs,
    belief_adjustment = belief_adjustment,
    new_beliefs = new_beliefs
  )
}
