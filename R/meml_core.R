.memlkit_panel_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for this model. Install it with install.packages('", pkg, "').", call. = FALSE)
  }
}

.memlkit_panel_formula_vars <- function(formula, data) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("'formula' must be a two-sided formula such as y ~ x1 + x2.", call. = FALSE)
  }
  response <- all.vars(formula[[2L]])
  predictors <- all.vars(formula[[3L]])
  if (length(response) != 1L) stop("The formula must specify exactly one response variable.", call. = FALSE)
  missing_vars <- setdiff(c(response, predictors), names(data))
  if (length(missing_vars)) stop("Variables not found in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  if (!length(predictors)) stop("At least one fixed-effect predictor is required.", call. = FALSE)
  list(response = response, predictors = predictors)
}

.memlkit_panel_z_matrix <- function(data, time, slopes) {
  z <- data.frame(Intercept = rep(1, nrow(data)))
  if (length(slopes)) {
    for (v in slopes) {
      if (!v %in% names(data) || !is.numeric(data[[v]])) {
        stop("Random-slope variables must be numeric columns in data: ", v, call. = FALSE)
      }
      value <- data[[v]]
      if (identical(v, time)) value <- value - min(value, na.rm = TRUE)
      z[[v]] <- value
    }
  }
  as.matrix(z)
}

#' Fit a mixed-effects forest model to panel or clustered data
#'
#' @param formula A model formula, for example `y ~ x1 + x2`.
#' @param data A long-format data frame with one row per observation.
#' @param id Name of the subject or cluster identifier column.
#' @param time Optional name of the measurement-time column. Required by MEGB,
#'   MERF, and REEMforest.
#' @param model Engine: `"MEGB"`, `"MERF"`, `"REEMforest"`, or `"MERFranger"`.
#' @param random_slope Optional character vector of numeric variables used as
#'   random slopes. For longitudinal engines, the intercept is always included.
#'   For MERFranger, these terms are used to build the lme4 random-effects formula.
#' @param random Optional lme4-style random-effects formula for MERFranger,
#'   such as `"(1 | .memlkit_panel_id)"`. Overrides `random_slope` for that engine.
#' @param importance Variable-importance method for MERFranger.
#' @param ... Additional arguments passed to the selected model engine.
#'
#' @return An object of class `memlkit_model` containing the fitted engine,
#'   predictors, response, and model metadata.
#' @export
fit_panel_forest <- function(formula, data, id, time = NULL,
                             model = c("MEGB", "MERF", "REEMforest", "MERFranger"),
                             random_slope = NULL, random = NULL,
                             importance = "permutation", ...) {
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (length(id) != 1L || !id %in% names(data)) stop("'id' must name a column in data.", call. = FALSE)
  meta <- .memlkit_panel_formula_vars(formula, data)
  y <- data[[meta$response]]
  if (!is.numeric(y)) stop("The response variable must be numeric.", call. = FALSE)
  x <- data[, meta$predictors, drop = FALSE]
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All fixed-effect predictors must be numeric for these engines.", call. = FALSE)
  }
  if (anyNA(y) || anyNA(x) || anyNA(data[[id]])) stop("Remove or impute missing response, predictor, and ID values before fitting.", call. = FALSE)
  if (!is.null(time) && (length(time) != 1L || !time %in% names(data))) stop("'time' must name a column in data.", call. = FALSE)
  if (model != "MERFranger" && is.null(time)) stop("'time' is required for MEGB, MERF, and REEMforest.", call. = FALSE)
  if (is.null(random_slope) && model != "MERFranger" && !is.null(time)) random_slope <- time
  if (!is.null(random_slope) && any(!random_slope %in% names(data))) stop("Every random-slope variable must be a column in data.", call. = FALSE)

  dots <- list(...)
  merge_args <- function(defaults) utils::modifyList(defaults, dots)
  id_value <- data[[id]]
  fit_data <- data
  fit_data$.memlkit_panel_id <- factor(id_value)
  if (!is.null(time) && is.numeric(data[[time]])) {
    fit_data$.time_centered <- data[[time]] - min(data[[time]], na.rm = TRUE)
  }
  z <- if (model == "MERFranger") NULL else .memlkit_panel_z_matrix(data, time, random_slope)

  if (model == "MEGB") {
    .memlkit_panel_require("MEGB")
    engine <- do.call(MEGB::MEGB, merge_args(list(
      X = as.matrix(x), Y = matrix(y, ncol = 1L), Z = z,
      id = id_value, time = data[[time]], ntree = 500,
      cv.folds = 0, verbose = TRUE
    )))
  } else if (model %in% c("MERF", "REEMforest")) {
    .memlkit_panel_require("LongituRF")
    fn <- if (model == "MERF") LongituRF::MERF else LongituRF::REEMforest
    defaults <- list(
      X = as.matrix(x), Y = y, id = id_value, Z = z,
      ntree = 500, mtry = max(1L, ceiling(ncol(x) / 3)),
      time = data[[time]], sto = "BM"
    )
    engine <- do.call(fn, merge_args(defaults))
  } else {
    .memlkit_panel_require("SAEforest")
    if (is.null(random)) {
      if (length(random_slope)) {
        random_terms <- if (!is.null(time)) {
          ifelse(random_slope == time, ".time_centered", random_slope)
        } else {
          random_slope
        }
        random <- paste0("(1 + ", paste(random_terms, collapse = " + "), " || .memlkit_panel_id)")
      } else {
        random <- "(1 | .memlkit_panel_id)"
      }
    }
    defaults <- list(
      Y = y, X = x, random = random, data = fit_data,
      importance = importance, num.trees = 500
    )
    engine <- do.call(SAEforest::MERFranger, merge_args(defaults))
  }

  structure(list(
    model = model, engine = engine, formula = formula,
    response = meta$response, predictors = meta$predictors,
    data = fit_data, x = x, y = y, id = id,
    time = time, random_slope = random_slope,
    time_origin = if (!is.null(time) && is.numeric(data[[time]])) min(data[[time]], na.rm = TRUE) else NULL
  ), class = "memlkit_model")
}

#' @export
print.memlkit_model <- function(x, ...) {
  cat("memlkit model\n")
  cat("  Engine: ", x$model, "\n", sep = "")
  cat("  Formula: ", deparse(x$formula), "\n", sep = "")
  cat("  Observations: ", length(x$y), "\n", sep = "")
  invisible(x)
}

#' Extract subject-level random effects
#'
#' @param object A fitted `memlkit_model` object.
#' @return A data frame or list of random-effect estimates.
#' @export
random_effects <- function(object) {
  if (inherits(object, "memlkit_multilevel_lgb")) return(lme4::ranef(object$random_model))
  if (inherits(object, "memlkit_multilevel_class")) {
    .memlkit_panel_require("lme4")
    return(lapply(object$models, function(m) lme4::ranef(m$glmer)))
  }
  if (inherits(object, "memlkit_multilevel_rf")) return(lme4::ranef(object$engine$EffectModel))
  if (!inherits(object, "memlkit_model")) stop("'object' must be a memlkit model.", call. = FALSE)
  fit <- object$engine
  if (object$model %in% c("MEGB", "MERF", "REEMforest")) {
    re <- fit$random_effects
    ids <- fit$id_btilde
    if (is.null(re)) return(NULL)
    out <- as.data.frame(re)
    names(out) <- paste0("RandomEffect", seq_len(ncol(out)))
    out[[object$id]] <- ids
    return(out[, c(object$id, setdiff(names(out), object$id)), drop = FALSE])
  }
  .memlkit_panel_require("lme4")
  lme4::ranef(fit$EffectModel)
}

#' Extract and rank fixed-effect variable importance
#'
#' @param object A fitted `memlkit_model` object.
#' @return A data frame with predictor and importance columns.
#' @export
variable_importance <- function(object) {
  if (inherits(object, "memlkit_multilevel_class")) {
    vals <- lapply(object$models, function(m) {
      if (object$algorithm == "random_forest") ranger::importance(m$forest) else lightgbm::lgb.importance(m$forest)
    })
    if (object$algorithm == "random_forest") {
      vars <- names(vals[[1L]])
      score <- rowMeans(vapply(vals, function(z) as.numeric(z[vars]), numeric(length(vars))))
    } else {
      vars <- vals[[1L]]$Feature
      score <- rowMeans(vapply(vals, function(z) z$Gain[match(vars, z$Feature)], numeric(length(vars))))
    }
    return(data.frame(variable = vars, importance = score)[order(score, decreasing = TRUE), , drop = FALSE])
  }
  if (inherits(object, "memlkit_multilevel_lgb")) {
    z <- lightgbm::lgb.importance(object$engine)
    return(data.frame(variable = z$Feature, importance = z$Gain)[order(z$Gain, decreasing = TRUE), , drop = FALSE])
  }
  if (inherits(object, "memlkit_multilevel_rf")) {
    z <- object$engine$Forest$variable.importance
    return(data.frame(variable = names(z), importance = as.numeric(z))[order(z, decreasing = TRUE), , drop = FALSE])
  }
  if (!inherits(object, "memlkit_model")) stop("'object' must be a memlkit model.", call. = FALSE)
  fit <- object$engine
  if (object$model == "MEGB") {
    z <- summary(fit$forest, n.trees = fit$forest$n.trees)
    out <- data.frame(variable = z$var, importance = z$rel.inf)
  } else if (object$model %in% c("MERF", "REEMforest")) {
    .memlkit_panel_require("randomForest")
    z <- randomForest::importance(fit$forest)
    out <- data.frame(variable = rownames(z), as.data.frame(z), row.names = NULL)
    names(out)[-1L] <- paste0("importance_", names(out)[-1L])
    out <- out[order(out[[2L]], decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  } else {
    z <- fit$Forest$variable.importance
    out <- data.frame(variable = names(z), importance = as.numeric(z))
  }
  out[order(out$importance, decreasing = TRUE), , drop = FALSE]
}

#' Plot partial dependence with LOESS smoothing
#'
#' @param object A fitted `memlkit_model` object.
#' @param variable Predictor name to display.
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
#' @param ... Additional arguments passed to `pdp::partial`.
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
  if (!inherits(object, "memlkit_model")) stop("'object' must be a memlkit model.", call. = FALSE)
  if (length(variable) != 1L || !variable %in% object$predictors) stop("'variable' must be a fixed-effect predictor in the fitted model.", call. = FALSE)
  .memlkit_panel_require("pdp")
  .memlkit_panel_require("ggplot2")
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

#' Plot model convergence diagnostics
#'
#' @param object A fitted `memlkit_model` object.
#' @param measure Diagnostic to plot: `"OOB"` or `"LL"`.
#' @return A base R plot.
#' @export
plot_convergence <- function(object, measure = c("OOB", "LL")) {
  if (!inherits(object, "memlkit_model")) stop("'object' must be a memlkit model.", call. = FALSE)
  measure <- match.arg(measure)
  fit <- object$engine
  values <- if (measure == "OOB") fit$OOB else fit$LL
  if (measure == "LL" && is.null(values)) values <- fit$Vraisemblance
  if (measure == "LL" && is.null(values)) values <- fit$LogLik
  if (is.null(values)) stop("The selected convergence measure is not available for this engine.", call. = FALSE)
  plot(values, type = "o", xlab = "Iteration", ylab = measure,
       main = paste(object$model, measure, "by iteration"))
  invisible(values)
}
