#' Compare fitted memlkit models on validation data
#'
#' @param models A fitted memlkit model or a named list of fitted memlkit models.
#' @param newdata Validation data containing the observed outcome and predictors.
#' @param response Optional outcome-column name. By default, the response stored
#'   in each fitted model is used.
#' @param task Optional task override: `"regression"` or `"classification"`.
#' @param positive_class Positive class for binary classification. Defaults to
#'   the last modeled class. Multiclass metrics are macro-averaged.
#' @return A `memlkit_model_comparison` list with a `metrics` data frame and, for
#'   classification, a named list of confusion matrices.
#' @export
compare_model_fit <- function(models, newdata, response = NULL,
                              task = NULL, positive_class = NULL) {
  if (inherits(models, "memlkit_model")) models <- list(models)
  if (!is.list(models) || !length(models) ||
      !all(vapply(models, inherits, logical(1), "memlkit_model"))) {
    stop("'models' must be a memlkit model or a non-empty list of memlkit models.", call. = FALSE)
  }
  if (!is.data.frame(newdata)) stop("'newdata' must be a data frame.", call. = FALSE)

  model_names <- names(models)
  if (is.null(model_names)) model_names <- rep("", length(models))
  missing_names <- is.na(model_names) | !nzchar(model_names)
  model_names[missing_names] <- paste0("Model ", which(missing_names))
  model_names <- make.unique(model_names)
  names(models) <- model_names

  stored_responses <- vapply(models, function(x) {
    if (is.null(x$response)) "" else as.character(x$response)[1L]
  }, character(1))
  if (is.null(response)) {
    unique_responses <- unique(stored_responses[nzchar(stored_responses)])
    if (length(unique_responses) != 1L) {
      stop("Supply 'response' when the fitted models do not share one outcome column.", call. = FALSE)
    }
    response <- unique_responses
  }
  if (length(response) != 1L || !is.character(response) || !response %in% names(newdata)) {
    stop("'response' must name an outcome column in 'newdata'.", call. = FALSE)
  }
  if (any(nzchar(stored_responses) & stored_responses != response)) {
    stop("The selected response does not match the response stored in every fitted model.", call. = FALSE)
  }

  inferred_task <- vapply(models, function(x) {
    if (!is.null(x$task)) return(as.character(x$task)[1L])
    if (inherits(x, "memlkit_model")) return("regression")
    ""
  }, character(1))
  inferred_task <- unique(inferred_task[nzchar(inferred_task)])
  if (is.null(task)) {
    task <- if (length(inferred_task)) inferred_task[1L] else if (is.numeric(newdata[[response]])) "regression" else "classification"
  }
  task <- match.arg(task, c("regression", "classification"))
  if (length(inferred_task) > 1L || (length(inferred_task) == 1L && inferred_task != task)) {
    stop("All models must use the selected task.", call. = FALSE)
  }
  if (task == "regression" && !is.numeric(newdata[[response]])) {
    stop("Regression evaluation requires a numeric observed outcome.", call. = FALSE)
  }

  keep <- !is.na(newdata[[response]])
  if (!any(keep)) stop("No non-missing observed outcomes are available.", call. = FALSE)
  observed <- newdata[[response]][keep]
  validation_data <- newdata[keep, , drop = FALSE]

  if (task == "regression") {
    rows <- lapply(models, function(model) {
      predicted <- predict_panel_model(model, validation_data, type = "response")
      predicted <- as.numeric(predicted)
      if (length(predicted) != length(observed)) stop("A model returned the wrong number of predictions.", call. = FALSE)
      valid <- is.finite(observed) & is.finite(predicted)
      if (!all(valid)) {
        stop("Observed outcomes or predictions contain non-finite values.", call. = FALSE)
      }
      error <- predicted - observed
      total_ss <- sum((observed - mean(observed))^2)
      data.frame(
        MAE = mean(abs(error)),
        RMSE = sqrt(mean(error^2)),
        R2 = if (total_ss > 0) 1 - sum(error^2) / total_ss else NA_real_,
        MeanError = mean(error)
      )
    })
    metrics <- do.call(rbind, rows)
    rownames(metrics) <- model_names
    metrics <- data.frame(model = rownames(metrics), metrics, row.names = NULL)
    return(structure(list(task = task, response = response, metrics = metrics,
                          confusion_matrices = NULL),
                     class = "memlkit_model_comparison"))
  }

  classes <- unique(unlist(lapply(models, function(x) x$classes), use.names = FALSE))
  if (!length(classes)) {
    classes <- if (is.factor(observed)) levels(droplevels(observed)) else sort(unique(as.character(observed)))
  }
  classes <- as.character(classes)
  if (length(classes) < 2L) stop("Classification evaluation requires at least two modeled classes.", call. = FALSE)
  if (is.null(positive_class)) positive_class <- tail(classes, 1L)
  if (length(positive_class) != 1L || !as.character(positive_class) %in% classes) {
    stop("'positive_class' must match one of the modeled classes.", call. = FALSE)
  }

  confusion_matrices <- lapply(seq_along(models), function(i) {
    predicted <- predict_panel_model(models[[i]], validation_data, type = "class")
    if (length(predicted) != length(observed)) stop("A model returned the wrong number of predictions.", call. = FALSE)
    actual_labels <- as.character(observed)
    predicted_labels <- as.character(predicted)
    valid <- !is.na(actual_labels) & !is.na(predicted_labels)
    if (!all(valid)) stop("Observed outcomes or predicted classes contain missing values.", call. = FALSE)
    table(
      Observed = factor(actual_labels, levels = classes),
      Predicted = factor(predicted_labels, levels = classes)
    )
  })
  names(confusion_matrices) <- model_names
  metric_rows <- lapply(seq_along(confusion_matrices), function(i) {
    cm <- confusion_matrices[[i]]
    true_positive <- diag(cm)
    precision <- ifelse(colSums(cm) > 0, true_positive / pmax(colSums(cm), 1), NA_real_)
    recall <- ifelse(rowSums(cm) > 0, true_positive / pmax(rowSums(cm), 1), NA_real_)
    f1 <- ifelse(precision + recall > 0,
                 2 * precision * recall / pmax(precision + recall, .Machine$double.eps),
                 NA_real_)
    if (length(classes) == 2L) {
      j <- match(as.character(positive_class), classes)
      precision_value <- precision[j]
      recall_value <- recall[j]
      f1_value <- f1[j]
    } else {
      precision_value <- if (all(is.na(precision))) NA_real_ else mean(precision, na.rm = TRUE)
      recall_value <- if (all(is.na(recall))) NA_real_ else mean(recall, na.rm = TRUE)
      f1_value <- if (all(is.na(f1))) NA_real_ else mean(f1, na.rm = TRUE)
    }
    data.frame(
      model = model_names[i],
      Accuracy = sum(true_positive) / sum(cm),
      Precision = precision_value,
      Recall = recall_value,
      F1 = f1_value
    )
  })
  metrics <- do.call(rbind, metric_rows)
  rownames(metrics) <- NULL
  structure(list(task = task, response = response, metrics = metrics,
                 confusion_matrices = confusion_matrices,
                 positive_class = if (length(classes) == 2L) as.character(positive_class) else NULL,
                 classes = classes),
            class = "memlkit_model_comparison")
}

#' @export
print.memlkit_model_comparison <- function(x, ...) {
  cat("memlkit model comparison (", x$task, ")\n", sep = "")
  cat("Outcome: ", x$response, "\n", sep = "")
  print(x$metrics, row.names = FALSE)
  if (identical(x$task, "classification")) {
    cat("Confusion matrices are available in $confusion_matrices.\n")
  }
  invisible(x)
}
