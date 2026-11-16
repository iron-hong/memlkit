.memlkit_validate_count <- function(x, name, minimum = 2L) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x != floor(x) || x < minimum) {
    stop("'", name, "' must be a single integer greater than or equal to ", minimum, ".", call. = FALSE)
  }
  as.integer(x)
}

.memlkit_validate_sd <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
    stop("'", name, "' must be a single non-negative number.", call. = FALSE)
  }
  as.numeric(x)
}

.memlkit_set_seed <- function(seed) {
  if (is.null(seed)) return(invisible(NULL))
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) || seed != floor(seed)) {
    stop("'seed' must be NULL or a single finite integer.", call. = FALSE)
  }
  set.seed(as.integer(seed))
  invisible(NULL)
}

.memlkit_validate_train_prop <- function(train_prop) {
  if (length(train_prop) != 1L || !is.numeric(train_prop) ||
      !is.finite(train_prop) || train_prop <= 0 || train_prop >= 1) {
    stop("'train_prop' must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  as.numeric(train_prop)
}

.memlkit_make_split <- function(data, train_index, train_prop, strategy) {
  train_index <- sort(unique(as.integer(train_index)))
  test_index <- setdiff(seq_len(nrow(data)), train_index)
  if (!length(train_index) || !length(test_index)) {
    stop("The split must contain at least one row in both training and test data.", call. = FALSE)
  }
  structure(list(
    train = data[train_index, , drop = FALSE],
    test = data[test_index, , drop = FALSE],
    train_indices = train_index,
    test_indices = test_index,
    requested_train_prop = train_prop,
    actual_train_prop = length(train_index) / nrow(data),
    strategy = strategy
  ), class = "memlkit_data_split")
}

#' Split panel data into training and test sets
#'
#' @param data Long-format panel data.
#' @param train_prop Proportion assigned to training data, strictly between 0 and 1.
#' @param seed Optional integer seed for random split strategies.
#' @param id Subject identifier column.
#' @param time Measurement-time column.
#' @param strategy Split strategy: `"time"` holds out later waves, `"subject"`
#'   holds out subjects, and `"random"` samples individual rows.
#' @return A `memlkit_data_split` list containing `train`, `test`, row indices,
#'   requested and actual training proportions, and the selected strategy.
#' @details The time strategy chooses a wave boundary closest to the requested
#'   training proportion. Its realized proportion may differ from the requested
#'   value because whole time waves are kept together.
#' @export
split_panel_data <- function(data, train_prop = 0.8, seed = NULL,
                             id = "id", time = "time",
                             strategy = c("time", "subject", "random")) {
  strategy <- match.arg(strategy)
  train_prop <- .memlkit_validate_train_prop(train_prop)
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("'data' must be a data frame with at least two rows.", call. = FALSE)
  }
  if (strategy != "random") {
    if (length(id) != 1L || !id %in% names(data)) stop("'id' must name a column in data.", call. = FALSE)
    if (anyNA(data[[id]])) stop("The ID column cannot contain missing values.", call. = FALSE)
  }
  .memlkit_set_seed(seed)

  if (strategy == "time") {
    if (length(time) != 1L || !time %in% names(data)) stop("'time' must name a column in data.", call. = FALSE)
    if (anyNA(data[[time]])) stop("The time column cannot contain missing values.", call. = FALSE)
    time_values <- sort(unique(data[[time]]))
    if (length(time_values) < 2L) stop("The time strategy requires at least two distinct waves.", call. = FALSE)
    wave_rank <- match(data[[time]], time_values)
    wave_counts <- tabulate(wave_rank, nbins = length(time_values))
    target <- nrow(data) * train_prop
    cut <- which.min(abs(cumsum(wave_counts)[-length(wave_counts)] - target))
    train_index <- which(wave_rank <= cut)
  } else if (strategy == "subject") {
    subjects <- unique(data[[id]])
    if (length(subjects) < 2L) stop("The subject strategy requires at least two subjects.", call. = FALSE)
    n_train <- min(length(subjects) - 1L, max(1L, round(length(subjects) * train_prop)))
    train_subjects <- sample(subjects, size = n_train, replace = FALSE)
    train_index <- which(data[[id]] %in% train_subjects)
  } else {
    n_train <- min(nrow(data) - 1L, max(1L, round(nrow(data) * train_prop)))
    train_index <- sample.int(nrow(data), size = n_train, replace = FALSE)
  }
  .memlkit_make_split(data, train_index, train_prop, strategy)
}

#' Split multilevel data into training and test sets
#'
#' @param data Cross-sectional multilevel data.
#' @param train_prop Proportion assigned to training data, strictly between 0 and 1.
#' @param seed Optional integer seed for random split strategies.
#' @param group Grouping-column names. For `"within_group"`, rows are sampled
#'   within combinations of these groups. For `"group"`, entire combinations
#'   are held out.
#' @param strategy Split strategy: `"within_group"`, `"group"`, or `"random"`.
#' @return A `memlkit_data_split` list containing `train`, `test`, row indices,
#'   requested and actual training proportions, and the selected strategy.
#' @details `"within_group"` is useful when both training and test data should
#'   represent each cluster. `"group"` evaluates held-out group combinations.
#' @export
split_multilevel_data <- function(data, train_prop = 0.8, seed = NULL,
                                  group = c("region", "district_id"),
                                  strategy = c("within_group", "group", "random")) {
  strategy <- match.arg(strategy)
  train_prop <- .memlkit_validate_train_prop(train_prop)
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("'data' must be a data frame with at least two rows.", call. = FALSE)
  }
  if (strategy != "random") {
    if (!length(group) || any(!group %in% names(data))) {
      stop("'group' must name one or more grouping columns in data.", call. = FALSE)
    }
    if (anyNA(data[, group, drop = FALSE])) stop("Grouping columns cannot contain missing values.", call. = FALSE)
  }
  .memlkit_set_seed(seed)

  if (strategy == "random") {
    n_train <- min(nrow(data) - 1L, max(1L, round(nrow(data) * train_prop)))
    train_index <- sample.int(nrow(data), size = n_train, replace = FALSE)
  } else {
    group_id <- do.call(
      interaction,
      c(lapply(data[, group, drop = FALSE], as.factor),
        list(drop = TRUE, lex.order = TRUE))
    )
    group_rows <- split(seq_len(nrow(data)), group_id)
    if (strategy == "group") {
      if (length(group_rows) < 2L) stop("The group strategy requires at least two distinct groups.", call. = FALSE)
      n_train <- min(length(group_rows) - 1L, max(1L, round(length(group_rows) * train_prop)))
      train_groups <- sample(names(group_rows), size = n_train, replace = FALSE)
      train_index <- unlist(group_rows[train_groups], use.names = FALSE)
    } else {
      too_small <- names(group_rows)[lengths(group_rows) < 2L]
      if (length(too_small)) {
        stop("The within_group strategy requires at least two rows in every group.", call. = FALSE)
      }
      train_index <- unlist(lapply(group_rows, function(index) {
        n_train <- min(length(index) - 1L, max(1L, round(length(index) * train_prop)))
        sample(index, size = n_train, replace = FALSE)
      }), use.names = FALSE)
    }
  }
  .memlkit_make_split(data, train_index, train_prop, strategy)
}

#' Simulate panel data for memlkit examples
#'
#' @param n_subjects Number of subjects. Must be at least 2.
#' @param n_times Number of repeated measurements per subject. Must be at least 2.
#' @param sd_intercept Standard deviation of subject random intercepts.
#' @param sd_slope Standard deviation of subject random time slopes.
#' @param sd_error Standard deviation of the continuous outcome error.
#' @param seed Optional integer random seed. The default gives reproducible data.
#' @param train_prop Optional training-data proportion. If supplied, return a
#'   train/test split instead of the complete data frame.
#' @param split_strategy Panel split strategy passed to `split_panel_data()`.
#' @param split_seed Optional seed for splitting. Defaults to the generator's
#'   continuing random-number stream.
#' @return A long-format data frame with `id`, `time`, `x1`, `x2`, continuous
#'   outcome `y`, and binary factor outcome `status`; if `train_prop` is
#'   supplied, a `memlkit_data_split` list is returned.
#' @details The continuous outcome includes nonlinear fixed effects, a subject-specific random intercept, and a subject-specific random time slope. The binary outcome is generated from a logistic model with subject-level heterogeneity.
#' @export
simulate_panel_data <- function(n_subjects = 40L, n_times = 6L,
                                sd_intercept = 0.8, sd_slope = 0.25,
                                sd_error = 0.7, seed = 20261117L,
                                train_prop = NULL,
                                split_strategy = c("time", "subject", "random"),
                                split_seed = NULL) {
  n_subjects <- .memlkit_validate_count(n_subjects, "n_subjects")
  n_times <- .memlkit_validate_count(n_times, "n_times")
  sd_intercept <- .memlkit_validate_sd(sd_intercept, "sd_intercept")
  sd_slope <- .memlkit_validate_sd(sd_slope, "sd_slope")
  sd_error <- .memlkit_validate_sd(sd_error, "sd_error")
  .memlkit_set_seed(seed)

  dat <- expand.grid(
    id = seq_len(n_subjects),
    time = seq_len(n_times),
    KEEP.OUT.ATTRS = FALSE
  )
  dat <- dat[order(dat$id, dat$time), , drop = FALSE]
  rownames(dat) <- NULL
  dat$x1 <- stats::rnorm(nrow(dat))
  dat$x2 <- stats::rnorm(nrow(dat))

  random_intercept <- stats::rnorm(n_subjects, mean = 0, sd = sd_intercept)
  random_slope <- stats::rnorm(n_subjects, mean = 0, sd = sd_slope)
  time_centered <- dat$time - mean(seq_len(n_times))
  id_index <- dat$id

  dat$y <- 1 + 0.45 * dat$time + 1.1 * dat$x1 - 0.7 * dat$x2 +
    0.25 * dat$x1^2 + random_intercept[id_index] +
    random_slope[id_index] * time_centered + stats::rnorm(nrow(dat), sd = sd_error)

  class_probability <- stats::plogis(
    -0.25 + 0.8 * dat$x1 - 0.55 * dat$x2 +
      0.35 * random_intercept[id_index] +
      0.15 * random_slope[id_index] * time_centered
  )
  dat$status <- factor(
    ifelse(stats::rbinom(nrow(dat), size = 1L, prob = class_probability) == 1L, "event", "none"),
    levels = c("none", "event")
  )
  if (is.null(train_prop)) return(dat)
  split_panel_data(
    dat, train_prop = train_prop, seed = split_seed,
    id = "id", time = "time", strategy = split_strategy
  )
}

#' Simulate nested multilevel data for memlkit examples
#'
#' @param n_regions Number of top-level regions. Must be at least 2.
#' @param n_districts Number of districts per region. Must be at least 2.
#' @param n_units_per_district Number of observations per district. Must be at least 2.
#' @param sd_region Standard deviation of region random intercepts.
#' @param sd_district Standard deviation of nested district random intercepts.
#' @param random_slope Optional numeric vector of region-specific slopes for `x1`. Its length must equal `n_regions`; each value is assigned to one region. The default `NULL` omits region-specific slopes.
#' @param sd_error Standard deviation of the continuous outcome error.
#' @param seed Optional integer random seed. The default gives reproducible data.
#' @param train_prop Optional training-data proportion. If supplied, return a
#'   train/test split instead of the complete data frame.
#' @param split_strategy Multilevel split strategy passed to
#'   `split_multilevel_data()`.
#' @param split_group Grouping columns used when splitting the simulated data.
#' @param split_seed Optional seed for splitting. Defaults to the generator's
#'   continuing random-number stream.
#' @return A cross-sectional data frame with `region`, `district`, unique nested
#'   `district_id`, predictors `x1` and `x2`, continuous outcome `y`, and binary
#'   factor outcome `status`; if `train_prop` is supplied, a `memlkit_data_split`
#'   list is returned.
#' @details The continuous and binary outcomes both include region- and district-level heterogeneity. When supplied, `random_slope` adds a region-specific coefficient for `x1` to both outcomes. `district_id` uniquely identifies each district within its region and can be supplied with `region` to the `group` argument of `fit_multilevel_model()`.
#' @export
simulate_multilevel_data <- function(n_regions = 8L, n_districts = 4L,
                                     n_units_per_district = 10L,
                                     sd_region = 0.65, sd_district = 0.45,
                                     sd_error = 0.8, seed = 20261118L,
                                     random_slope = NULL, train_prop = NULL,
                                     split_strategy = c("within_group", "group", "random"),
                                     split_group = c("region", "district_id"),
                                     split_seed = NULL) {
  n_regions <- .memlkit_validate_count(n_regions, "n_regions")
  n_districts <- .memlkit_validate_count(n_districts, "n_districts")
  n_units_per_district <- .memlkit_validate_count(n_units_per_district, "n_units_per_district")
  sd_region <- .memlkit_validate_sd(sd_region, "sd_region")
  sd_district <- .memlkit_validate_sd(sd_district, "sd_district")
  sd_error <- .memlkit_validate_sd(sd_error, "sd_error")
  if (!is.null(random_slope) &&
      (!is.numeric(random_slope) || length(random_slope) != n_regions ||
       any(!is.finite(random_slope)))) {
    stop("'random_slope' must be NULL or a finite numeric vector with one value per region.", call. = FALSE)
  }
  if (is.null(random_slope)) random_slope <- rep(0, n_regions)
  .memlkit_set_seed(seed)

  dat <- data.frame(
    region = factor(rep(seq_len(n_regions), each = n_districts * n_units_per_district)),
    district = rep(rep(seq_len(n_districts), each = n_units_per_district), times = n_regions)
  )
  dat$district_id <- interaction(dat$region, dat$district, drop = TRUE, lex.order = TRUE)
  dat$x1 <- stats::rnorm(nrow(dat))
  dat$x2 <- stats::rnorm(nrow(dat))

  region_effect <- stats::rnorm(n_regions, mean = 0, sd = sd_region)
  district_effect <- stats::rnorm(n_regions * n_districts, mean = 0, sd = sd_district)
  region_index <- as.integer(dat$region)
  district_index <- as.integer(dat$district_id)

  dat$y <- 1 + 1.2 * dat$x1 - 0.8 * dat$x2 + 0.2 * dat$x1^2 +
    region_effect[region_index] + random_slope[region_index] * dat$x1 +
    district_effect[district_index] +
    stats::rnorm(nrow(dat), sd = sd_error)

  class_probability <- stats::plogis(
    -0.2 + 0.9 * dat$x1 - 0.65 * dat$x2 +
      random_slope[region_index] * dat$x1 +
      0.45 * region_effect[region_index] + 0.35 * district_effect[district_index]
  )
  dat$status <- factor(
    ifelse(stats::rbinom(nrow(dat), size = 1L, prob = class_probability) == 1L, "event", "none"),
    levels = c("none", "event")
  )
  if (is.null(train_prop)) return(dat)
  split_multilevel_data(
    dat, train_prop = train_prop, seed = split_seed,
    group = split_group, strategy = split_strategy
  )
}

#' @rdname simulate_multilevel_data
#' @export
simulate_nested_data <- function(n_regions = 8L, n_districts = 4L,
                                 n_units_per_district = 10L,
                                 sd_region = 0.65, sd_district = 0.45,
                                 sd_error = 0.8, seed = 20261118L,
                                 random_slope = NULL, train_prop = NULL,
                                 split_strategy = c("within_group", "group", "random"),
                                 split_group = c("region", "district_id"),
                                 split_seed = NULL) {
  simulate_multilevel_data(
    n_regions = n_regions,
    n_districts = n_districts,
    n_units_per_district = n_units_per_district,
    sd_region = sd_region,
    sd_district = sd_district,
    random_slope = random_slope,
    sd_error = sd_error,
    seed = seed,
    train_prop = train_prop,
    split_strategy = split_strategy,
    split_group = split_group,
    split_seed = split_seed
  )
}
