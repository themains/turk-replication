coefficient_table <- function(fit, study, outcome, group, covariance = NULL) {
  b <- stats::coef(fit)
  v <- if (is.null(covariance)) stats::vcov(fit) else covariance
  se <- sqrt(diag(v))
  df <- stats::df.residual(fit)
  statistic <- b / se
  critical <- stats::qt(.975, df)
  data.frame(study, outcome, group,
    term = names(b), estimate = unname(b),
    std_error = unname(se), statistic = unname(statistic),
    p_value = unname(2 * stats::pt(-abs(statistic), df)),
    conf_low = unname(b - critical * se), conf_high = unname(b + critical * se),
    n = stats::nobs(fit), r_squared = summary(fit)$r.squared, row.names = NULL
  )
}

hc1_covariance <- function(fit) {
  x <- stats::model.matrix(fit)
  bread <- solve(crossprod(x))
  meat <- crossprod(x * as.numeric(stats::residuals(fit)))
  bread %*% meat %*% bread * nrow(x) / fit$df.residual
}

ordered_predictors <- function() {
  c("james_black", "james_gay", "james_evang", "james_aa", "james_lib", "james_con")
}

fit_ordered <- function(data, interaction = NULL) {
  rhs <- paste(ordered_predictors(), collapse = " + ")
  if (!is.null(interaction)) rhs <- paste0("(", rhs, ") * ", interaction)
  fit <- MASS::polr(stats::as.formula(paste("cf_ordered ~", rhs)),
    data = data,
    Hess = TRUE, method = "logistic",
    control = list(reltol = 1e-11, maxit = 1000)
  )
  if (fit$convergence != 0L || any(!is.finite(c(stats::coef(fit), fit$zeta)))) {
    stop("Ordered logit did not converge to finite coefficients")
  }
  fit
}

ordered_coefficient_table <- function(fit, group) {
  b <- c(stats::coef(fit), fit$zeta)
  se <- sqrt(diag(stats::vcov(fit)))
  data.frame(
    study = "study1", group, term = names(b), estimate = unname(b),
    std_error = unname(se), statistic = unname(b / se),
    p_value = unname(2 * stats::pnorm(-abs(b / se))),
    conf_low = unname(b - stats::qnorm(.975) * se),
    conf_high = unname(b + stats::qnorm(.975) * se), n = fit$n,
    log_likelihood = as.numeric(stats::logLik(fit)),
    aic = stats::AIC(fit), convergence = fit$convergence, row.names = NULL
  )
}

# A binary factor's marginal effect is an average discrete change, not a derivative.
ordered_margin_value <- function(parameters, x0, x1, outcome) {
  p <- ncol(x0)
  probability <- function(x) {
    eta <- as.vector(x %*% parameters[seq_len(p)])
    lower <- stats::plogis(parameters[p + 1L] - eta)
    upper <- stats::plogis(parameters[p + 2L] - eta)
    switch(as.character(outcome),
      `-1` = lower,
      `0` = upper - lower,
      `1` = 1 - upper
    )
  }
  mean(probability(x1) - probability(x0))
}

ordered_margins <- function(fit, group, standard_errors = TRUE, valid_profiles = FALSE) {
  x <- stats::model.matrix(fit)[, -1, drop = FALSE]
  parameters <- c(stats::coef(fit), fit$zeta)
  covariance <- if (standard_errors) stats::vcov(fit) else NULL
  rows <- list()
  for (term in ordered_predictors()) {
    x0 <- x1 <- x
    x0[, term] <- 0
    x1[, term] <- 1
    if (valid_profiles) {
      siblings <- c(
        james_evang = "james_aa", james_aa = "james_evang",
        james_lib = "james_con", james_con = "james_lib"
      )
      if (term %in% names(siblings)) {
        x0[, siblings[[term]]] <- 0
        x1[, siblings[[term]]] <- 0
      }
    }
    for (outcome in c(-1, 0, 1)) {
      fn <- function(theta) ordered_margin_value(theta, x0, x1, outcome)
      estimate <- fn(parameters)
      se <- NA_real_
      if (standard_errors) {
        gradient <- numDeriv::grad(fn, parameters)
        se <- sqrt(drop(t(gradient) %*% covariance %*% gradient))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        study = "study1", group, term, outcome,
        estimate, std_error = se, p_value = 2 * stats::pnorm(-abs(estimate / se)),
        conf_low = estimate - stats::qnorm(.975) * se,
        conf_high = estimate + stats::qnorm(.975) * se, n = fit$n
      )
    }
  }
  do.call(rbind, rows)
}

experiment_fits <- function(d) {
  groups <- list(
    all = rep(TRUE, nrow(d)), unflagged = d$combined_troll_1 == 0,
    flagged = d$combined_troll_1 == 1, flagged_ip = d$funny == 1,
    rare_responses = d$likely_troll == 1, high_hits = d$hits_recode == 4
  )
  results <- list()
  for (group in names(groups)) {
    for (outcome in c("unemploy", "inflation")) {
      fit <- stats::lm(stats::reformulate("out_party_treat", outcome),
        data = d[which(groups[[group]]), ]
      )
      conventional <- coefficient_table(fit, "study2", outcome, group)
      conventional$inference <- "conventional"
      robust <- coefficient_table(fit, "study2", outcome, group, hc1_covariance(fit))
      robust$inference <- "HC1"
      results[[length(results) + 1L]] <- rbind(conventional, robust)
    }
  }
  do.call(rbind, results)
}

attenuation_inputs <- function(experiment, margins) {
  result <- list()
  for (study in c("study1", "study2")) {
    d <- if (study == "study1") {
      margins[margins$outcome != 0, ]
    } else {
      experiment[experiment$term == "out_party_treat" & experiment$inference == "conventional", ]
    }
    d$key <- if (study == "study1") paste(d$term, d$outcome, sep = ":") else d$outcome
    keys <- unique(d$key)
    result[[study]] <- do.call(rbind, lapply(keys, function(key) {
      rows <- d[d$key == key, ]
      group_names <- c("unflagged", "flagged", "all")
      rows <- rows[match(group_names, rows$group), ]
      sign <- if (study == "study1") sign(rows$estimate[1]) else 1
      data.frame(
        group = key, nonsusp_beta = sign * rows$estimate[1], nonsusp_se = rows$std_error[1],
        susp_beta = sign * rows$estimate[2], susp_se = rows$std_error[2],
        full_beta = sign * rows$estimate[3], full_se = rows$std_error[3]
      )
    }))
  }
  result
}

attenuation_ratios <- function(prepared) {
  d <- prepared$study2
  ratio2 <- vapply(c("unemploy", "inflation"), function(outcome) {
    formula <- stats::reformulate("out_party_treat", outcome)
    full <- stats::coef(stats::lm(formula, d))[["out_party_treat"]]
    clean <- stats::coef(stats::lm(formula, d[d$combined_troll_1 == 0, ]))[["out_party_treat"]]
    full / clean
  }, numeric(1))
  d <- prepared$study1
  full <- ordered_margins(fit_ordered(d), "all", FALSE)
  clean <- ordered_margins(fit_ordered(d[d$badactor == 0, ]), "unflagged", FALSE)
  keep <- full$outcome != 0
  ratio1 <- full$estimate[keep] / clean$estimate[keep]
  names(ratio1) <- paste(full$term[keep], full$outcome[keep], sep = ":")
  c(
    stats::setNames(ratio1, paste0("study1/", names(ratio1))),
    stats::setNames(ratio2, paste0("study2/", names(ratio2)))
  )
}

bootstrap_attenuation <- function(prepared, draws = 1999L, seed = 20261009L,
                                  cluster = FALSE, cores = 1L) {
  stopifnot(draws >= 2, cores >= 1)
  point <- attenuation_ratios(prepared)
  # Draw seeds before parallel execution, so results do not depend on core count.
  set.seed(seed)
  seeds <- sample.int(.Machine$integer.max, draws)
  one_draw <- function(b) {
    set.seed(seeds[b])
    sampled <- lapply(prepared[c("study1", "study2")], function(d) {
      if (cluster) {
        key <- as.character(d$ip_index)
        key[is.na(key)] <- paste0("missing_", which(is.na(key)))
        groups <- split(seq_len(nrow(d)), key)
        selected <- sample(seq_along(groups), length(groups), replace = TRUE)
        index <- unlist(groups[selected], use.names = FALSE)
      } else {
        index <- sample.int(nrow(d), nrow(d), replace = TRUE)
      }
      d[index, , drop = FALSE]
    })
    tryCatch(
      withCallingHandlers(
        attenuation_ratios(sampled),
        warning = function(w) stop(conditionMessage(w))
      ),
      error = function(e) stats::setNames(rep(NA_real_, length(point)), names(point))
    )
  }
  sampled <- if (cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(seq_len(draws), one_draw, mc.cores = cores, mc.set.seed = FALSE)
  } else {
    lapply(seq_len(draws), one_draw)
  }
  values <- do.call(rbind, sampled)
  intervals <- apply(values, 2, stats::quantile, probs = c(.025, .975), na.rm = TRUE)
  data.frame(
    study = sub("/.*", "", names(point)), outcome = sub("^[^/]+/", "", names(point)),
    retained_ratio = unname(point), attenuation = unname(1 - point),
    ratio_conf_low = intervals[1, ], ratio_conf_high = intervals[2, ],
    attenuation_conf_low = 1 - intervals[2, ], attenuation_conf_high = 1 - intervals[1, ],
    successful_draws = colSums(is.finite(values)), requested_draws = draws,
    failed_draws = draws - colSums(is.finite(values)),
    ratio_draw_min = apply(values, 2, min, na.rm = TRUE),
    ratio_draw_max = apply(values, 2, max, na.rm = TRUE),
    unstable_ratio = apply(values, 2, function(x) any(x < 0 | abs(x) > 5, na.rm = TRUE)), seed,
    sampling_unit = if (cluster) "IP cluster" else "respondent", row.names = NULL
  )
}

model_results <- function(prepared) {
  experiment <- experiment_fits(prepared$study2)
  interactions <- list()
  for (predictor in c("combined_troll_1", "fast")) {
    for (outcome in c("unemploy", "inflation")) {
      formula <- stats::as.formula(paste(outcome, "~ out_party_treat *", predictor))
      fit <- stats::lm(formula, prepared$study2)
      interactions[[length(interactions) + 1L]] <- coefficient_table(
        fit, "study2", outcome, predictor
      )
    }
  }
  d <- prepared$study1
  coefficients <- margins <- predictions <- list()
  groups <- list(
    all = rep(TRUE, nrow(d)), unflagged = d$badactor == 0,
    flagged = d$badactor == 1, flagged_ip = d$funny == 1, rare_responses = d$likely_troll == 1
  )
  for (group in names(groups)) {
    fit <- fit_ordered(d[which(groups[[group]]), ])
    coefficients[[length(coefficients) + 1L]] <- ordered_coefficient_table(fit, group)
    margins[[length(margins) + 1L]] <- ordered_margins(fit, group)
    predictions[[length(predictions) + 1L]] <- ordered_predictions(fit, group)
  }
  interactions1 <- list(
    all_flagged = list("badactor", rep(TRUE, nrow(d))),
    ip_only = list("funny", d$likely_troll == 0),
    rare_only = list("likely_troll", d$funny == 0), fast_unflagged = list("fast", d$badactor == 0)
  )
  for (group in names(interactions1)) {
    specification <- interactions1[[group]]
    fit <- fit_ordered(d[which(specification[[2]]), ], specification[[1]])
    coefficients[[length(coefficients) + 1L]] <- ordered_coefficient_table(fit, group)
  }
  simple <- lapply(c("dem_cf", "rep_cf"), function(outcome) {
    fit <- stats::lm(stats::reformulate(ordered_predictors(), outcome), d)
    coefficient_table(fit, "study1", outcome, "all")
  })
  margins <- do.call(rbind, margins)
  computed <- attenuation_inputs(experiment, margins)
  attenuation <- do.call(rbind, lapply(names(computed), function(study) {
    x <- computed[[study]]
    data.frame(study,
      outcome = x$group, full_effect = x$full_beta,
      unflagged_effect = x$nonsusp_beta, flagged_effect = x$susp_beta,
      retained_ratio = x$full_beta / x$nonsusp_beta,
      attenuation = 1 - x$full_beta / x$nonsusp_beta
    )
  }))
  list(
    experiment_effects = experiment, experiment_interactions = do.call(rbind, interactions),
    study1_linear_models = do.call(rbind, simple),
    ordered_coefficients = do.call(rbind, coefficients),
    ordered_margins = margins, ordered_predictions = do.call(rbind, predictions),
    attenuation = attenuation,
    attenuation_summary_inputs = do.call(rbind, lapply(names(computed), function(study) {
      data.frame(study, computed[[study]])
    }))
  )
}

ordered_predictions <- function(fit, group) {
  x <- stats::model.matrix(fit)[, -1, drop = FALSE]
  parameters <- c(stats::coef(fit), fit$zeta)
  covariance <- stats::vcov(fit)
  p <- ncol(x)
  rows <- list()
  for (term in ordered_predictors()) {
    for (value in 0:1) {
      scenario <- x
      scenario[, term] <- value
      for (outcome in c(-1, 0, 1)) {
        fn <- function(theta) {
          eta <- drop(scenario %*% theta[seq_len(p)])
          lower <- stats::plogis(theta[p + 1L] - eta)
          upper <- stats::plogis(theta[p + 2L] - eta)
          mean(switch(as.character(outcome),
            `-1` = lower,
            `0` = upper - lower,
            `1` = 1 - upper
          ))
        }
        estimate <- fn(parameters)
        gradient <- numDeriv::grad(fn, parameters)
        se <- sqrt(drop(t(gradient) %*% covariance %*% gradient))
        rows[[length(rows) + 1L]] <- data.frame(
          study = "study1", group, term, value, outcome, estimate, std_error = se,
          conf_low = estimate - stats::qnorm(.975) * se,
          conf_high = estimate + stats::qnorm(.975) * se, n = fit$n
        )
      }
    }
  }
  do.call(rbind, rows)
}
