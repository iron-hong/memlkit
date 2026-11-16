.memlkit_prepare_data <- function(formula, data, group, random_slope = NULL) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("Use a two-sided formula such as y ~ x1 + x2.", call. = FALSE)
  }
  response <- all.vars(formula[[2L]])
  if (length(response) != 1L) stop("The formula must have one response variable.", call. = FALSE)
  group <- as.character(group)
  if (!length(group) || any(!group %in% names(data))) stop("'group' must name grouping columns in data.", call. = FALSE)
  if (anyNA(data[, group, drop = FALSE])) stop("Grouping columns cannot contain missing values.", call. = FALSE)

  model_data <- data
  fixed_names <- all.vars(formula[[3L]])
  for (v in intersect(fixed_names, names(model_data))) {
    if (is.character(model_data[[v]])) model_data[[v]] <- factor(model_data[[v]])
  }
  terms_full <- stats::terms(formula, data = model_data)
  model_frame <- stats::model.frame(terms_full, data = model_data, na.action = stats::na.fail)
  y <- stats::model.response(model_frame)
  terms_fixed <- stats::delete.response(terms_full)
  x_all <- stats::model.matrix(terms_fixed, model_frame)
  x <- x_all[, colnames(x_all) != "(Intercept)", drop = FALSE]
  if (!ncol(x)) stop("The formula must contain at least one fixed-effect predictor.", call. = FALSE)
  xlevels <- lapply(model_frame[intersect(fixed_names, names(model_frame))], function(z) if (is.factor(z)) levels(z) else NULL)
  xlevels <- Filter(Negate(is.null), xlevels)
  slopes <- NULL
  if (length(random_slope)) {
    if (any(!random_slope %in% names(data))) stop("Random-slope columns were not found in data.", call. = FALSE)
    if (any(!vapply(data[random_slope], is.numeric, logical(1)))) stop("Random-slope variables must be numeric.", call. = FALSE)
    if (anyNA(data[, random_slope, drop = FALSE])) stop("Random-slope columns cannot contain missing values.", call. = FALSE)
    slopes <- as.matrix(data[, random_slope, drop = FALSE])
  }
  list(
    y = y, x = x, group = group, group_names = group,
    random_slope = random_slope, slopes = slopes,
    terms = terms_fixed, contrasts = attr(x_all, "contrasts"),
    xlevels = xlevels, x_columns = colnames(x), response = response,
    predictors = fixed_names, training_data = data
  )
}

.memlkit_new_matrix <- function(object, newdata) {
  if (!is.data.frame(newdata)) stop("'newdata' must be a data frame.", call. = FALSE)
  nd <- newdata
  for (v in names(object$xlevels)) {
    if (v %in% names(nd) && is.character(nd[[v]])) nd[[v]] <- factor(nd[[v]], levels = object$xlevels[[v]])
  }
  x_all <- stats::model.matrix(object$terms, nd, contrasts.arg = object$contrasts, xlev = object$xlevels)
  x <- x_all[, colnames(x_all) != "(Intercept)", drop = FALSE]
  missing <- setdiff(object$x_columns, colnames(x))
  if (length(missing)) stop("Prediction data are missing model columns: ", paste(missing, collapse = ", "), call. = FALSE)
  x[, object$x_columns, drop = FALSE]
}

.memlkit_make_group_data <- function(data, group, random_slope = NULL,
                                correlated_random_effects = FALSE) {
  out <- data
  group_columns <- paste0(".memlkit_group_", seq_along(group))
  for (i in seq_along(group)) out[[group_columns[i]]] <- factor(as.character(data[[group[i]]]))
  slope_columns <- character()
  if (length(random_slope)) {
    slope_columns <- paste0(".memlkit_slope_", seq_along(random_slope))
    for (i in seq_along(random_slope)) out[[slope_columns[i]]] <- data[[random_slope[i]]]
  }
  terms <- if (length(slope_columns)) {
    operator <- if (correlated_random_effects) "|" else "||"
    c(paste0("(1 + ", paste(slope_columns, collapse = " + "), " ", operator, " ", group_columns[1L], ")"),
      if (length(group_columns) > 1L) paste0("(1 | ", group_columns[-1L], ")"))
  } else {
    paste0("(1 | ", group_columns, ")")
  }
  list(data = out, group_columns = group_columns, slope_columns = slope_columns,
       random_formula = paste(terms, collapse = " + "))
}

.memlkit_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Install the '", pkg, "' package to use this model.", call. = FALSE)
  }
}

#' Fit a mixed-effects random forest or GBM to panel data
#'
#' @param formula Model formula, such as `y ~ x1 + x2`.
#' @param data Long-format panel data, one row per subject-time observation.
#' @param id Subject identifier column name.
#' @param time Measurement-time column name.
#' @param algorithm `"random_forest"` or `"gbm"`.
#' @param task `"regression"` or `"classification"`. MEGB supports regression only.
#' @param random_slope Numeric subject-specific random-slope variables. Defaults to `time`.
#' @param include_time_slope If `FALSE`, use random intercepts only. If `TRUE`, use `random_slope` (time by default).
#' @param nrounds Number of trees for the selected engine.
#' @param engine Optional legacy engine override: `"MEGB"`, `"MERF"`, `"REEMforest"`, or `"MERFranger"`.
#' @param ... Additional arguments passed to the selected engine.
#' @return A fitted model that can be used with `predict_panel_model()` and the interpretation helpers.
#' @export
fit_panel_model <- function(formula, data, id, time,
                            algorithm = c("random_forest", "gbm"),
                            task = c("regression", "classification"),
                            random_slope = time, nrounds = 500L,
                            engine = NULL, include_time_slope = TRUE, ...) {
  algorithm <- match.arg(algorithm)
  task <- match.arg(task)
  if (!is.logical(include_time_slope) || length(include_time_slope) != 1L || is.na(include_time_slope)) {
    stop("'include_time_slope' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!include_time_slope) {
    random_slope <- character()
  } else if (is.null(random_slope)) {
    random_slope <- time
  }
  if (!is.data.frame(data) || length(id) != 1L || !id %in% names(data)) stop("'id' must name a column in data.", call. = FALSE)
  if (length(time) != 1L || !time %in% names(data)) stop("'time' must name a column in data.", call. = FALSE)
  if (is.null(engine)) engine <- if (algorithm == "gbm") "MEGB" else "MERF"
  if (!engine %in% c("MEGB", "MERF", "REEMforest", "MERFranger")) stop("Unknown panel engine.", call. = FALSE)
  if ((algorithm == "gbm") != (engine == "MEGB")) stop("Use MEGB with algorithm = 'gbm'; use MERF, REEMforest, or MERFranger with algorithm = 'random_forest'.", call. = FALSE)
  if (task == "classification" && engine == "MEGB") stop("MEGB is a regression engine. Choose algorithm = 'random_forest' for panel classification.", call. = FALSE)
  if (task == "classification") {
    origin <- min(data[[time]], na.rm = TRUE)
    slope_data <- data
    slope_names <- random_slope
    if (length(slope_names) && time %in% slope_names) {
      slope_data$.memlkit_centered_time <- data[[time]] - origin
      slope_names[slope_names == time] <- ".memlkit_centered_time"
    }
    prepared <- .memlkit_prepare_data(formula, slope_data, id, slope_names)
    classes <- if (is.factor(prepared$y)) levels(droplevels(prepared$y)) else if (is.numeric(prepared$y)) as.character(sort(unique(prepared$y))) else sort(unique(as.character(prepared$y)))
    if (length(classes) < 2L || length(classes) > 20L) stop("Classification requires 2 to 20 outcome classes.", call. = FALSE)
    prepared$classes <- classes
    prepared$y <- factor(as.character(prepared$y), levels = classes)
    result <- .memlkit_fit_multilevel_classifier(prepared, "random_forest", slope_data, nrounds, list(), ...)
    result$training_data <- data
    result$panel_mode <- TRUE
    result$time <- time
    result$time_origin <- origin
    result$algorithm <- "random_forest"
    result$include_time_slope <- include_time_slope
    return(result)
  }
  engine_args <- if (engine == "MERFranger") list(num.trees = as.integer(nrounds)) else list(ntree = as.integer(nrounds))
  fit <- do.call(fit_panel_forest, c(list(
    formula = formula, data = data, id = id, time = time,
    model = engine, random_slope = random_slope
  ), engine_args, list(...)))
  fit$algorithm <- algorithm
  fit$task <- task
  fit$training_data <- data
  fit$group_names <- id
  fit$include_time_slope <- include_time_slope
  fit
}

#' Fit mixed-effects random forest or LightGBM to cross-sectional multilevel data
#'
#' @param formula Model formula, such as `y ~ x1 + x2`.
#' @param data Cross-sectional data, one row per unit.
#' @param group One or more grouping-column names defining multilevel clusters.
#' @param algorithm `"random_forest"` or `"gbm"`.
#' @param task `"regression"` or `"classification"`.
#' @param random_slope Optional names of numeric predictor columns with random slopes at the first grouping level.
#' @param correlated_random_effects If `TRUE`, estimate correlations among the first-level random intercept and slopes; otherwise set their correlations to zero.
#' @param nrounds Number of trees or boosting rounds.
#' @param params Additional LightGBM parameters.
#' @param ... Additional arguments passed to the selected engine.
#' @return A fitted model for use with `predict_panel_model()` and interpretation helpers.
#' @export
fit_multilevel_model <- function(formula, data, group,
                                 algorithm = c("random_forest", "gbm"),
                                 task = c("regression", "classification"),
                                 random_slope = NULL, nrounds = 500L,
                                 params = list(), max_iter = 10L,
                                 tol = 1e-4, correlated_random_effects = FALSE,
                                 ...) {
  algorithm <- match.arg(algorithm)
  task <- match.arg(task)
  if (!is.logical(correlated_random_effects) ||
      length(correlated_random_effects) != 1L || is.na(correlated_random_effects)) {
    stop("'correlated_random_effects' must be TRUE or FALSE.", call. = FALSE)
  }
  prepared <- .memlkit_prepare_data(formula, data, group, random_slope)
  if (task == "regression" && !is.numeric(prepared$y)) stop("Regression requires a numeric response.", call. = FALSE)
  if (task == "classification") {
    classes <- if (is.factor(prepared$y)) levels(droplevels(prepared$y)) else if (is.numeric(prepared$y)) as.character(sort(unique(prepared$y))) else sort(unique(as.character(prepared$y)))
    if (length(classes) < 2L || length(classes) > 20L) stop("Classification requires 2 to 20 outcome classes.", call. = FALSE)
    prepared$classes <- classes
    prepared$y <- factor(as.character(prepared$y), levels = classes)
    return(.memlkit_fit_multilevel_classifier(
      prepared, algorithm, data, nrounds, params,
      correlated_random_effects = correlated_random_effects, ...
    ))
  }
  if (algorithm == "random_forest") {
    return(.memlkit_fit_cross_merfranger(
      prepared, data, nrounds,
      correlated_random_effects = correlated_random_effects, ...
    ))
  }
  .memlkit_fit_cross_lightgbm(
    prepared, data, nrounds, params, max_iter, tol,
    correlated_random_effects = correlated_random_effects, ...
  )
}

.memlkit_fit_cross_merfranger <- function(prepared, data, nrounds,
                                     correlated_random_effects = FALSE, ...) {
  .memlkit_require("SAEforest")
  .memlkit_require("lme4")
  group_setup <- .memlkit_make_group_data(data, prepared$group_names,
                                     prepared$random_slope, correlated_random_effects)
  predictors <- data[, prepared$predictors, drop = FALSE]
  args <- utils::modifyList(list(
    Y = as.numeric(prepared$y), X = predictors,
    random = group_setup$random_formula, data = group_setup$data,
    importance = "permutation", num.trees = as.integer(nrounds)
  ), list(...))
  engine <- do.call(SAEforest::MERFranger, args)
  structure(c(prepared, list(
    model = "MERFranger", algorithm = "random_forest", task = "regression",
    engine = engine, data = group_setup$data, training_data = data,
    group_columns = group_setup$group_columns,
    slope_columns = group_setup$slope_columns,
    random_formula = group_setup$random_formula,
    correlated_random_effects = correlated_random_effects
  )), class = c("memlkit_multilevel_rf", "memlkit_model"))
}

.memlkit_lgb_fit <- function(x, y, nrounds, params = list(), ...) {
  .memlkit_require("lightgbm")
  defaults <- list(objective = "regression", metric = "l2", verbosity = -1L)
  params <- utils::modifyList(defaults, params)
  dataset <- lightgbm::lgb.Dataset(data = x, label = as.numeric(y))
  lightgbm::lgb.train(params = params, data = dataset, nrounds = as.integer(nrounds), ...)
}

.memlkit_fit_cross_lightgbm <- function(prepared, data, nrounds, params,
                                   max_iter = 10L, tol = 1e-4,
                                   correlated_random_effects = FALSE, ...) {
  .memlkit_require("lightgbm")
  .memlkit_require("lme4")
  if (!is.numeric(prepared$y)) stop("The multilevel LightGBM regression backend requires a numeric outcome.", call. = FALSE)
  group_setup <- .memlkit_make_group_data(data, prepared$group_names,
                                     prepared$random_slope, correlated_random_effects)
  d <- group_setup$data
  d$.memlkit_resid <- as.numeric(prepared$y)
  random_formula <- stats::as.formula(paste(".memlkit_resid ~ 0 +", group_setup$random_formula))
  re_fit <- NULL
  re_hat <- rep(0, length(prepared$y))
  booster <- NULL
  convergence <- numeric()
  for (iteration in seq_len(as.integer(max_iter))) {
    booster <- .memlkit_lgb_fit(prepared$x, prepared$y - re_hat, nrounds, params, ...)
    fixed_hat <- as.numeric(stats::predict(booster, prepared$x))
    d$.memlkit_resid <- as.numeric(prepared$y) - fixed_hat
    re_fit <- lme4::lmer(random_formula, data = d, REML = TRUE)
    new_re_hat <- as.numeric(stats::predict(re_fit, newdata = d, allow.new.levels = TRUE))
    change <- sqrt(mean((new_re_hat - re_hat)^2))
    convergence <- c(convergence, change)
    re_hat <- new_re_hat
    if (change < tol) break
  }
  booster <- .memlkit_lgb_fit(prepared$x, prepared$y - re_hat, nrounds, params, ...)
  structure(c(prepared, list(
    model = "MERF_LightGBM", algorithm = "gbm", task = "regression",
    engine = booster, random_model = re_fit, training_data = data,
    data = group_setup$data, group_columns = group_setup$group_columns,
    slope_columns = group_setup$slope_columns,
    random_formula = group_setup$random_formula,
    correlated_random_effects = correlated_random_effects,
    nrounds = as.integer(nrounds), params = params,
    convergence = convergence, iterations = length(convergence)
  )), class = c("memlkit_multilevel_lgb", "memlkit_model"))
}

.memlkit_fit_multilevel_classifier <- function(prepared, algorithm, data, nrounds, params,
                                          correlated_random_effects = FALSE, ...) {
  .memlkit_require("lme4")
  if (algorithm == "random_forest") .memlkit_require("ranger") else .memlkit_require("lightgbm")
  group_setup <- .memlkit_make_group_data(data, prepared$group_names,
                                     prepared$random_slope, correlated_random_effects)
  model_data <- group_setup$data
  models <- lapply(prepared$classes, function(class_value) {
    y_bin <- as.integer(prepared$y == class_value)
    if (algorithm == "random_forest") {
      forest <- ranger::ranger(
        x = prepared$x, y = factor(y_bin, levels = c(0, 1)),
        probability = TRUE, num.trees = as.integer(nrounds),
        importance = "permutation", ...
      )
      p <- forest$predictions[, "1"]
      if (is.null(p) || length(p) != length(y_bin)) p <- predict(forest, data = prepared$x)$predictions[, "1"]
    } else {
      lgb_params <- utils::modifyList(list(objective = "binary", metric = "binary_logloss", verbosity = -1L), params)
      booster <- lightgbm::lgb.train(
        params = lgb_params,
        data = lightgbm::lgb.Dataset(prepared$x, label = y_bin),
        nrounds = as.integer(nrounds), ...
      )
      p <- as.numeric(stats::predict(booster, prepared$x))
      forest <- booster
    }
    p[!is.finite(p)] <- mean(y_bin)
    model_data$.memlkit_class_y <- factor(y_bin, levels = c(0, 1), labels = c("other", "target"))
    model_data$.memlkit_eta <- stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    glmer_formula <- stats::as.formula(paste(".memlkit_class_y ~ 0 + offset(.memlkit_eta) +", group_setup$random_formula))
    glmer_fit <- lme4::glmer(
      glmer_formula, data = model_data, family = stats::binomial(),
      control = lme4::glmerControl(optimizer = "bobyqa")
    )
    list(forest = forest, glmer = glmer_fit)
  })
  names(models) <- prepared$classes
  structure(c(prepared, list(
    model = paste0(if (algorithm == "gbm") "LightGBM" else "ranger", "_GLMM_offset"),
    algorithm = algorithm, task = "classification", models = models,
    training_data = data, data = model_data,
    group_columns = group_setup$group_columns,
    slope_columns = group_setup$slope_columns,
    random_formula = group_setup$random_formula,
    correlated_random_effects = correlated_random_effects,
    backend = "two_stage_offset_glmm"
  )), class = c("memlkit_multilevel_class", "memlkit_model"))
}

.memlkit_predict_multilevel <- function(object, newdata, type = "response") {
  if (!is.data.frame(newdata)) stop("'newdata' must be a data frame.", call. = FALSE)
  required_groups <- setdiff(object$group_names, names(newdata))
  if (length(required_groups)) stop("Prediction data are missing grouping columns: ", paste(required_groups, collapse = ", "), call. = FALSE)
  x <- .memlkit_new_matrix(object, newdata)
  nd <- newdata
  if (isTRUE(object$panel_mode) && length(object$random_slope)) {
    for (i in seq_along(object$random_slope)) {
      v <- object$random_slope[i]
      if (!v %in% names(nd) && identical(v, ".memlkit_centered_time")) nd[[v]] <- nd[[object$time]] - object$time_origin
    }
  }
  for (i in seq_along(object$group_names)) nd[[paste0(".memlkit_group_", i)]] <- factor(as.character(newdata[[object$group_names[i]]]))
  if (length(object$random_slope)) {
    for (i in seq_along(object$random_slope)) {
      v <- object$random_slope[i]
      if (!v %in% names(nd)) stop("Prediction data are missing random-slope column: ", v, call. = FALSE)
      nd[[paste0(".memlkit_slope_", i)]] <- nd[[v]]
    }
  }
  if (identical(object$task, "regression")) {
    if (type != "response") stop("Regression models support type = 'response' only.", call. = FALSE)
    if (identical(object$algorithm, "random_forest")) {
      fixed_data <- newdata[, object$predictors, drop = FALSE]
      fixed <- stats::predict(object$engine$Forest, data = fixed_data)$predictions
      random <- stats::predict(object$engine$EffectModel, newdata = nd, allow.new.levels = TRUE)
      return(as.numeric(fixed + random))
    }
    fixed <- as.numeric(stats::predict(object$engine, x))
    random <- as.numeric(stats::predict(object$random_model, newdata = nd, allow.new.levels = TRUE))
    return(fixed + random)
  }
  probabilities <- lapply(object$models, function(m) {
    if (object$algorithm == "random_forest") p <- predict(m$forest, data = x)$predictions[, "1"] else p <- as.numeric(stats::predict(m$forest, x))
    nd$.memlkit_class_y <- factor(rep("other", nrow(nd)), levels = c("other", "target"))
    nd$.memlkit_eta <- stats::qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    as.numeric(stats::predict(m$glmer, newdata = nd, type = "response", allow.new.levels = TRUE))
  })
  prob <- do.call(cbind, probabilities)
  prob[!is.finite(prob)] <- 0
  prob[prob < 0] <- 0
  rs <- rowSums(prob)
  prob[rs <= 0, ] <- 1 / ncol(prob)
  prob <- prob / rowSums(prob)
  colnames(prob) <- object$classes
  if (type == "prob") return(prob)
  if (type == "class") return(factor(object$classes[max.col(prob)], levels = object$classes))
  if (ncol(prob) == 2L) return(prob[, 2L])
  prob
}

#' Predict from a panel or multilevel model
#'
#' @param object A fitted model from `fit_panel_model()` or `fit_multilevel_model()`.
#' @param newdata Data frame with predictors and grouping columns.
#' @param type `"response"`, `"class"`, or `"prob"`.
#' @param ... Additional arguments passed to `pdp::partial()` for panel models.
#' @return Predictions, class labels, or class probabilities.
#' @export
predict_panel_model <- function(object, newdata, type = c("response", "class", "prob"), ...) {
  type <- match.arg(type)
  if (inherits(object, "memlkit_multilevel_lgb") || inherits(object, "memlkit_multilevel_rf") || inherits(object, "memlkit_multilevel_class")) {
    return(.memlkit_predict_multilevel(object, newdata, type))
  }
  if (!inherits(object, "memlkit_model")) stop("'object' must be a memlkit model.", call. = FALSE)
  if (identical(object$task, "regression") && type != "response") stop("Regression models support type = 'response' only.", call. = FALSE)
  if (object$model == "MERFranger") {
    nd <- newdata
    nd$.memlkit_panel_id <- factor(as.character(newdata[[object$id]]))
    if (!is.null(object$time) && is.numeric(newdata[[object$time]])) {
      nd$.time_centered <- newdata[[object$time]] - object$time_origin
    }
    for (slope in object$random_slope) {
      slope_column <- if (identical(slope, object$time)) ".time_centered" else slope
      if (!slope_column %in% names(nd)) {
        stop("Prediction data are missing random-slope column: ", slope_column, call. = FALSE)
      }
    }
    fixed_data <- newdata[, object$predictors, drop = FALSE]
    fixed <- stats::predict(object$engine$Forest, data = fixed_data)$predictions
    random <- stats::predict(object$engine$EffectModel, newdata = nd, allow.new.levels = TRUE)
    return(as.numeric(fixed + random))
  }
  if (object$model %in% c("MERF", "REEMforest")) {
    x <- as.matrix(newdata[, object$predictors, drop = FALSE])
    z <- data.frame(Intercept = rep(1, nrow(newdata)))
    for (v in object$random_slope) {
      value <- newdata[[v]]
      if (identical(v, object$time)) value <- value - object$time_origin
      z[[v]] <- value
    }
    return(as.numeric(stats::predict(object$engine, X = x, Z = as.matrix(z), id = newdata[[object$id]], time = newdata[[object$time]])))
  }
  if (object$model == "MEGB") {
    z <- data.frame(Intercept = rep(1, nrow(newdata)))
    for (v in object$random_slope) {
      value <- newdata[[v]]
      if (identical(v, object$time)) value <- value - object$time_origin
      z[[v]] <- value
    }
    return(as.numeric(stats::predict(
      object$engine,
      X = as.matrix(newdata[, object$predictors, drop = FALSE]),
      Z = as.matrix(z), id = newdata[[object$id]], time = newdata[[object$time]],
      ntree = object$engine$forest$n.trees
    )))
  }
  stop("Unsupported panel model.", call. = FALSE)
}

#' Extract random effects
#'
#' @param object A fitted memlkit model.
#' @return Estimated random effects from the selected model engine.
#' @export
extract_group_effects <- function(object) random_effects(object)

#' Extract subject or group random effects
#'
#' @param object A fitted memlkit model.
#' @return Random-effect estimates.
#' @export
random_effects <- function(object) {
  if (inherits(object, "memlkit_multilevel_lgb")) return(lme4::ranef(object$random_model))
  if (inherits(object, "memlkit_multilevel_class")) return(lapply(object$models, function(m) lme4::ranef(m$glmer)))
  if (inherits(object, "memlkit_multilevel_rf")) return(lme4::ranef(object$engine$EffectModel))
  if (!inherits(object, "memlkit_model")) stop("'object' must be a fitted memlkit model.", call. = FALSE)
  fit <- object$engine
  if (object$model %in% c("MEGB", "MERF", "REEMforest")) {
    out <- as.data.frame(fit$random_effects)
    names(out) <- paste0("RandomEffect", seq_len(ncol(out)))
    out[[object$id]] <- fit$id_btilde
    return(out[, c(object$id, setdiff(names(out), object$id)), drop = FALSE])
  }
  lme4::ranef(fit$EffectModel)
}

#' Calculate and rank fixed-effect variable importance
#'
#' @param object A fitted memlkit model.
#' @return A data frame of predictors and importance scores.
#' @export
variable_importance <- function(object) {
  if (inherits(object, "memlkit_multilevel_class")) {
    vals <- lapply(object$models, function(m) {
      if (object$algorithm == "random_forest") ranger::importance(m$forest) else lightgbm::lgb.importance(m$forest)
    })
    if (object$algorithm == "random_forest") {
      vars <- names(vals[[1L]])
      scores <- vapply(vals, function(z) as.numeric(z[vars]), numeric(length(vars)))
    } else {
      vars <- vals[[1L]]$Feature
      scores <- vapply(vals, function(z) z$Gain[match(vars, z$Feature)], numeric(length(vars)))
    }
    scores[!is.finite(scores)] <- 0
    score <- rowMeans(as.matrix(scores))
    return(data.frame(variable = vars, importance = score)[order(score, decreasing = TRUE), ])
  }
  if (inherits(object, "memlkit_multilevel_lgb")) {
    z <- lightgbm::lgb.importance(object$engine)
    return(data.frame(variable = z$Feature, importance = z$Gain)[order(z$Gain, decreasing = TRUE), ])
  }
  if (inherits(object, "memlkit_multilevel_rf")) {
    z <- object$engine$Forest$variable.importance
    return(data.frame(variable = names(z), importance = as.numeric(z))[order(z, decreasing = TRUE), ])
  }
  if (object$model == "MEGB") {
    z <- summary(object$engine$forest, n.trees = object$engine$forest$n.trees)
    return(data.frame(variable = z$var, importance = z$rel.inf))
  }
  if (object$model %in% c("MERF", "REEMforest")) {
    z <- randomForest::importance(object$engine$forest)
    return(data.frame(variable = rownames(z), importance = z[, 1L], row.names = NULL)[order(z[, 1L], decreasing = TRUE), ])
  }
  z <- object$engine$Forest$variable.importance
  data.frame(variable = names(z), importance = as.numeric(z))[order(z, decreasing = TRUE), ]
}

.memlkit_plot_pdp <- function(object, variable, class = NULL,
                         show_raw = FALSE, raw_alpha = 0.35,
                         raw_color = "#2166AC", show_smooth = TRUE,
                         smooth_span = 0.8, smooth_color = "#B2182B",
                         smooth_linewidth = 1.1, show_rug = TRUE,
                         grid_points = 40L) {
  .memlkit_require("ggplot2")
  .memlkit_validate_plot_options(show_raw, raw_alpha, show_smooth, smooth_span,
                            smooth_linewidth, show_rug)
  if (!variable %in% all.vars(object$terms)) stop("'variable' must appear in the fitted formula.", call. = FALSE)
  source <- object$training_data[[variable]]
  numeric_x <- is.numeric(source)
  grid <- if (numeric_x) {
    unique(as.numeric(stats::quantile(source, seq(0.02, 0.98, length.out = grid_points), na.rm = TRUE)))
  } else {
    levels(as.factor(source))
  }
  if (object$task == "classification") {
    if (is.null(class)) class <- tail(object$classes, 1L)
    if (!class %in% object$classes) stop("'class' must be a modeled outcome class.", call. = FALSE)
  }
  curves <- lapply(grid, function(value) {
    nd <- object$training_data
    nd[[variable]] <- if (numeric_x) rep(value, nrow(nd)) else factor(rep(value, nrow(nd)), levels = levels(as.factor(source)))
    pred <- predict_panel_model(object, nd, type = if (object$task == "classification") "prob" else "response")
    if (object$task == "classification") pred <- pred[, class]
    data.frame(x = rep(value, length(pred)), y = as.numeric(pred), row = seq_along(pred))
  })
  curve_data <- do.call(rbind, curves)
  pd <- stats::aggregate(y ~ x, curve_data, mean)
  p <- ggplot2::ggplot()
  if (numeric_x) {
    if (show_raw) {
      p <- p + ggplot2::geom_line(data = pd, ggplot2::aes(x = x, y = y),
                                  color = raw_color, alpha = raw_alpha, linewidth = 0.8) +
        ggplot2::geom_point(data = pd, ggplot2::aes(x = x, y = y),
                            color = raw_color, alpha = raw_alpha, size = 1.5)
    }
    if (show_smooth && length(unique(pd$x)) >= 3L) {
      p <- p + ggplot2::geom_smooth(
        data = pd, ggplot2::aes(x = x, y = y), method = "loess",
        formula = y ~ x, span = smooth_span, se = TRUE, level = 0.95,
        color = smooth_color, fill = smooth_color, alpha = 0.18,
        linewidth = smooth_linewidth
      )
    }
    if (show_rug) {
      p <- p + ggplot2::geom_rug(
        data = object$training_data,
        mapping = do.call(ggplot2::aes, list(x = as.name(variable))),
        sides = "b", alpha = 0.15, inherit.aes = FALSE
      )
    }
  } else if (show_raw) {
    p <- p + ggplot2::geom_col(data = pd, ggplot2::aes(x = x, y = y),
                               fill = raw_color, alpha = raw_alpha)
  } else {
    p <- p + ggplot2::geom_point(data = pd, ggplot2::aes(x = x, y = y),
                                 color = smooth_color, size = 2)
  }
  p + ggplot2::theme_bw() + ggplot2::labs(
    title = paste("Partial dependence for", variable), x = variable,
    y = if (object$task == "classification") paste("Predicted probability:", class) else "Predicted response"
  )
}

.memlkit_validate_plot_options <- function(show_raw, raw_alpha, show_smooth,
                                      smooth_span, smooth_linewidth, show_rug) {
  toggles <- list(show_raw = show_raw, show_smooth = show_smooth,
                  show_rug = show_rug)
  valid_toggle <- vapply(toggles, function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1))
  if (!all(valid_toggle)) stop("Plot display options must be single TRUE/FALSE values.", call. = FALSE)
  if (!is.numeric(raw_alpha) || length(raw_alpha) != 1L ||
      !is.finite(raw_alpha) || raw_alpha < 0 || raw_alpha > 1) {
    stop("'raw_alpha' must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(smooth_span) || length(smooth_span) != 1L ||
      !is.finite(smooth_span) || smooth_span <= 0) {
    stop("'smooth_span' must be one finite positive number.", call. = FALSE)
  }
  if (!is.numeric(smooth_linewidth) || length(smooth_linewidth) != 1L ||
      !is.finite(smooth_linewidth) || smooth_linewidth <= 0) {
    stop("'smooth_linewidth' must be one finite positive number.", call. = FALSE)
  }
  invisible(TRUE)
}

.memlkit_plot_partial_data <- function(partial_data, variable, train = NULL,
                                  show_raw = FALSE, raw_alpha = 0.35,
                                  raw_color = "#2166AC", show_smooth = TRUE,
                                  smooth_span = 0.8, smooth_color = "#B2182B",
                                  smooth_linewidth = 1.1, show_rug = TRUE) {
  partial_data <- as.data.frame(partial_data)
  .memlkit_validate_plot_options(show_raw, raw_alpha, show_smooth, smooth_span,
                            smooth_linewidth, show_rug)
  if (!all(c(variable, "yhat") %in% names(partial_data))) {
    stop("The partial-dependence output is missing the predictor or 'yhat' column.", call. = FALSE)
  }
  numeric_x <- is.numeric(partial_data[[variable]])
  pd <- stats::aggregate(partial_data[["yhat"]],
                         by = list(value = partial_data[[variable]]), FUN = mean)
  names(pd) <- c(variable, "yhat")
  p <- ggplot2::ggplot()
  mean_mapping <- do.call(ggplot2::aes, list(x = as.name(variable), y = as.name("yhat")))
  if (numeric_x) {
    if (show_raw) {
      p <- p + ggplot2::geom_line(data = pd, mapping = mean_mapping,
                                  color = raw_color, alpha = raw_alpha, linewidth = 0.8) +
        ggplot2::geom_point(data = pd, mapping = mean_mapping,
                            color = raw_color, alpha = raw_alpha, size = 1.5)
    }
    if (show_smooth && length(unique(pd[[variable]])) >= 3L) {
      p <- p + ggplot2::geom_smooth(
        data = pd, mapping = mean_mapping, method = "loess", formula = y ~ x,
        span = smooth_span, se = TRUE, level = 0.95,
        color = smooth_color, fill = smooth_color, alpha = 0.18,
        linewidth = smooth_linewidth
      )
    }
    if (show_rug && !is.null(train) && variable %in% names(train) && is.numeric(train[[variable]])) {
      p <- p + ggplot2::geom_rug(
        data = train, mapping = do.call(ggplot2::aes, list(x = as.name(variable))),
        sides = "b", alpha = 0.15, inherit.aes = FALSE
      )
    }
  } else if (show_raw) {
    p <- p + ggplot2::geom_col(data = pd, mapping = mean_mapping,
                               fill = raw_color, alpha = raw_alpha)
  } else {
    p <- p + ggplot2::geom_point(data = pd, mapping = mean_mapping,
                                 color = smooth_color, size = 2)
  }
  p + ggplot2::theme_bw() + ggplot2::labs(
    title = paste("Partial dependence for", variable),
    x = variable, y = "Predicted response"
  )
}

#' Plot partial dependence with LOESS smoothing
#'
#' @param object A fitted memlkit model.
#' @param variable Predictor to display.
#' @param class Outcome class for classification models.
#' @param rug Legacy alias for `show_rug`.
#' @param smooth Legacy alias for `show_smooth`.
#' @param show_original Legacy alias for `show_raw`.
#' @param show_raw Show the unsmoothed PDP line or categorical bars.
#' @param raw_alpha Transparency of the raw PDP line or bars, from 0 to 1.
#' @param raw_color Color of the raw PDP line or bars.
#' @param show_smooth Add a LOESS-smoothed curve and 95% confidence interval
#'   for numeric predictors.
#' @param smooth_span LOESS span; larger values produce smoother curves.
#' @param smooth_color Color of the smoothed curve.
#' @param smooth_linewidth Width of the smoothed curve.
#' @param show_rug Add a rug showing the training-data distribution for numeric predictors.
#' @param ... Additional arguments passed to `pdp::partial()` for panel models.
#' @return A ggplot object.
#' @export
plot_pdp <- function(object, variable, class = NULL,
                     rug = NULL, smooth = NULL, show_original = NULL,
                     show_raw = FALSE, raw_alpha = 0.35,
                     raw_color = "#2166AC", show_smooth = TRUE,
                     smooth_span = 0.8, smooth_color = "#B2182B",
                     smooth_linewidth = 1.1, show_rug = TRUE, ...) {
  dots <- list(...)
  if ("ice" %in% names(dots)) {
    stop("The 'ice' option has been removed; plot_pdp() now displays PDP curves only.", call. = FALSE)
  }
  if (!is.null(rug)) show_rug <- rug
  if (!is.null(smooth)) show_smooth <- smooth
  if (!is.null(show_original)) show_raw <- show_original
  .memlkit_validate_plot_options(show_raw, raw_alpha, show_smooth, smooth_span,
                            smooth_linewidth, show_rug)
  if (inherits(object, "memlkit_multilevel_lgb") || inherits(object, "memlkit_multilevel_rf") || inherits(object, "memlkit_multilevel_class")) {
    return(.memlkit_plot_pdp(
      object, variable, class = class, show_raw = show_raw,
      raw_alpha = raw_alpha, raw_color = raw_color,
      show_smooth = show_smooth, smooth_span = smooth_span,
      smooth_color = smooth_color, smooth_linewidth = smooth_linewidth,
      show_rug = show_rug
    ))
  }
  if (!inherits(object, "memlkit_model")) stop("'object' must be a fitted memlkit model.", call. = FALSE)
  if (length(variable) != 1L || !variable %in% object$predictors) stop("'variable' must be a predictor in the fitted model.", call. = FALSE)
  .memlkit_require("pdp")
  .memlkit_require("ggplot2")
  fit <- object$engine
  forest <- if (object$model == "MERFranger") fit$Forest else fit$forest
  args <- c(list(object = forest, pred.var = variable, train = object$x, ice = FALSE), dots)
  if (object$model == "MEGB") args$n.trees <- fit$forest$n.trees
  if (object$model == "MERFranger") args$pred.fun <- function(object, newdata) predict(object, data = newdata)$predictions
  pd <- do.call(pdp::partial, args)
  .memlkit_plot_partial_data(
    pd, variable = variable, train = object$x,
    show_raw = show_raw, raw_alpha = raw_alpha, raw_color = raw_color,
    show_smooth = show_smooth, smooth_span = smooth_span,
    smooth_color = smooth_color, smooth_linewidth = smooth_linewidth,
    show_rug = show_rug
  )
}

#' Plot iterative convergence for a panel model
#'
#' @param object A fitted model from `fit_panel_model()`.
#' @param measure `"OOB"` or `"LL"`.
#' @return A base R plot.
#' @export
plot_convergence <- function(object, measure = c("OOB", "LL")) {
  measure <- match.arg(measure)
  if (inherits(object, "memlkit_multilevel_lgb")) values <- object$convergence else values <- object$engine[[measure]]
  if (is.null(values)) stop("The selected convergence measure is unavailable.", call. = FALSE)
  plot(values, type = "o", xlab = "Iteration", ylab = measure, main = paste(object$model, measure, "by iteration"))
  invisible(values)
}
