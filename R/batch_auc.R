#' Cross-validated random forest batch-prediction AUC
#'
#' Computes out-of-sample random forest prediction of batch labels separately
#' within each metric/modality. For multi-class batch labels, the function
#' reports the macro-average one-vs-rest AUC across batch classes.
#'
#' @param data_list A list of data frames or matrices, one per metric/modality.
#'   Rows are observations and columns are features.
#' @param bat_list A list of batch-label vectors aligned with `data_list`, or a
#'   single batch-label vector that will be reused for all metrics.
#' @param k Number of cross-validation folds.
#' @param ntree Number of trees used by [randomForest::randomForest()].
#' @param seed Random seed used for fold creation and random forest fitting.
#' @param metric_prefix Prefix used to label metrics in the output.
#'
#' @return A data frame with one row per metric and fold. Columns include
#'   `measurement`, `fold`, and `auc`.
#' @export
#'
#' @examples
#' \dontrun{
#' auc_df <- batch_auc_cal_cv(data_list, bat_list, k = 5, ntree = 100)
#' }
batch_auc_cal_cv <- function(data_list, bat_list, k = 5, ntree = 100,
                             seed = 1, metric_prefix = "M") {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for batch_auc_cal_cv().", call. = FALSE)
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required for batch_auc_cal_cv().", call. = FALSE)
  }
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required for batch_auc_cal_cv().", call. = FALSE)
  }

  if (is.null(data_list) || length(data_list) == 0) {
    stop("data_list must be a non-empty list.", call. = FALSE)
  }

  if (!is.list(data_list)) data_list <- list(data_list)
  m <- length(data_list)

  if (!is.list(bat_list)) {
    bat_list <- replicate(m, bat_list, simplify = FALSE)
  }
  if (length(bat_list) == 1L && m > 1L) {
    bat_list <- replicate(m, bat_list[[1L]], simplify = FALSE)
  }
  if (length(bat_list) != m) {
    stop("bat_list must have length 1 or the same length as data_list.", call. = FALSE)
  }

  out <- lapply(seq_len(m), function(i) {
    X <- as.data.frame(data_list[[i]], check.names = FALSE)
    batch <- factor(bat_list[[i]])

    if (nrow(X) != length(batch)) {
      stop("Rows of data_list[[", i, "]] do not match length of bat_list[[", i, "]].", call. = FALSE)
    }
    if (nlevels(batch) < 2L) {
      return(data.frame(
        measurement = paste0(metric_prefix, i),
        fold = seq_len(k),
        auc = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    # Keep numeric/integer/logical columns only, then handle missing values.
    keep_cols <- vapply(X, function(z) is.numeric(z) || is.integer(z) || is.logical(z), logical(1))
    X <- X[, keep_cols, drop = FALSE]
    if (ncol(X) == 0L) {
      stop("No numeric feature columns available for data_list[[", i, "]].", call. = FALSE)
    }

    X <- as.data.frame(lapply(X, function(z) as.numeric(z)), check.names = FALSE)

    # Median imputation for occasional missing values.
    for (cc in names(X)) {
      z <- X[[cc]]
      if (anyNA(z)) {
        med <- stats::median(z, na.rm = TRUE)
        if (!is.finite(med)) med <- 0
        z[is.na(z)] <- med
        X[[cc]] <- z
      }
    }

    # Remove zero-variance or non-finite columns.
    finite_var <- vapply(X, function(z) {
      v <- stats::var(z)
      all(is.finite(z)) && is.finite(v) && v > 0
    }, logical(1))
    X <- X[, finite_var, drop = FALSE]
    if (ncol(X) == 0L) {
      stop("No non-constant numeric feature columns available for data_list[[", i, "]].", call. = FALSE)
    }

    # Avoid requesting more folds than observations in the smallest class.
    class_counts <- table(batch)
    k_use <- min(as.integer(k), length(batch), max(2L, min(class_counts)))

    set.seed(seed + i)
    folds <- caret::createFolds(batch, k = k_use, returnTrain = TRUE)

    aucs <- vapply(seq_along(folds), function(fold_id) {
      train_idx <- folds[[fold_id]]
      test_idx <- setdiff(seq_along(batch), train_idx)

      if (length(test_idx) < 2L || length(unique(batch[test_idx])) < 2L) {
        return(NA_real_)
      }

      train_X <- X[train_idx, , drop = FALSE]
      test_X <- X[test_idx, , drop = FALSE]
      train_y <- droplevels(batch[train_idx])
      test_y <- batch[test_idx]

      if (nlevels(train_y) < 2L) return(NA_real_)

      set.seed(seed + i * 1000L + fold_id)
      rf_batch <- randomForest::randomForest(
        x = train_X,
        y = train_y,
        ntree = ntree,
        importance = FALSE
      )

      batch_probs <- stats::predict(rf_batch, test_X, type = "prob")
      batch_probs <- as.matrix(batch_probs)

      batch_aucs <- vapply(colnames(batch_probs), function(this_class) {
        y_bin <- test_y == this_class
        if (length(unique(y_bin)) < 2L) return(NA_real_)
        as.numeric(pROC::roc(response = y_bin,
                             predictor = batch_probs[, this_class],
                             quiet = TRUE)$auc)
      }, numeric(1))

      mean(batch_aucs, na.rm = TRUE)
    }, numeric(1))

    data.frame(
      measurement = paste0(metric_prefix, i),
      fold = seq_along(aucs),
      auc = as.numeric(aucs),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}
