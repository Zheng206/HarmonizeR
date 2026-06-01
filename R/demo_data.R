#' Simulate cross-sectional demo data for HarmonizeR
#'
#' Creates a list of modality-specific feature matrices, shared batch labels,
#' and covariates. This mirrors the demo dataset used by the Shiny app and is
#' useful for examples, tests, and package development.
#'
#' @param n Number of observations.
#' @param G Number of features per modality.
#' @param m Number of imaging metrics/modalities.
#' @param n_batches Number of batches/sites/scanners.
#' @param seed Random seed.
#'
#' @return A list with elements `data`, `bat`, `covar`, `feat_names`,
#'   `batch_levels`, `m`, `n`, `G`, and longitudinal metadata fields.
#' @export
simulate_demo_data <- function(n = 90, G = 20, m = 3, n_batches = 3, seed = 42) {
  set.seed(seed)
  batch_labels <- LETTERS[seq_len(n_batches)]
  bat_vec <- factor(rep(batch_labels, length.out = n))
  age <- stats::rnorm(n, 55, 10)
  sex <- factor(sample(c("M", "F"), n, replace = TRUE))
  covar_df <- data.frame(age = age, sex = sex)
  feat_names <- paste0("feat_", seq_len(G))

  shifts <- lapply(seq_len(m), function(i) c(0, stats::runif(n_batches - 1, -3, 3)))
  scales <- lapply(seq_len(m), function(i) c(1, stats::runif(n_batches - 1, 0.6, 1.8)))

  data_list <- lapply(seq_len(m), function(i) {
    base <- matrix(
      stats::rnorm(n * G, mean = 50 + age * 0.3 + as.integer(sex) * 2, sd = 5),
      n,
      G
    )
    bshift <- shifts[[i]][as.integer(bat_vec)]
    bscale <- scales[[i]][as.integer(bat_vec)]
    y <- sweep(sweep(base, 1, bshift, "+"), 1, bscale, "*")
    colnames(y) <- feat_names
    rownames(y) <- paste0("S", seq_len(n))
    as.data.frame(y)
  })

  list(
    data = data_list,
    bat = replicate(m, bat_vec, simplify = FALSE),
    covar = replicate(m, covar_df, simplify = FALSE),
    feat_names = feat_names,
    batch_levels = levels(bat_vec),
    m = m,
    n = n,
    G = G,
    is_longitudinal = FALSE,
    subid_vec = NULL,
    visit_vec = NULL,
    n_subjects = NULL
  )
}

#' Simulate longitudinal demo data for HarmonizeR
#'
#' Creates a longitudinal multi-modality dataset with subject IDs, visit numbers,
#' subject-level random intercepts, and time-varying symptom scores.
#'
#' @param n_subjects Number of subjects.
#' @param G Number of features per modality.
#' @param m Number of imaging metrics/modalities.
#' @param n_batches Number of batches/sites/scanners.
#' @param max_visits Maximum number of visits per subject.
#' @param seed Random seed.
#'
#' @return A list with data, batch labels, covariates, feature names, and
#'   longitudinal metadata.
#' @export
simulate_demo_longitudinal_data <- function(
    n_subjects = 40,
    G = 20,
    m = 3,
    n_batches = 3,
    max_visits = 4,
    seed = 42) {
  set.seed(seed)
  batch_labels <- LETTERS[seq_len(n_batches)]
  subj_ids <- paste0("SUB", sprintf("%03d", seq_len(n_subjects)))
  subj_batch <- factor(sample(batch_labels, n_subjects, replace = TRUE))
  subj_age <- stats::rnorm(n_subjects, 55, 10)
  subj_sex <- factor(sample(c("M", "F"), n_subjects, replace = TRUE))

  n_visits_per_subj <- if (max_visits == 1L) {
    rep(1L, n_subjects)
  } else {
    vapply(seq_len(n_subjects), function(i) {
      b <- as.integer(subj_batch[i])
      probs <- if (n_batches == 1L) {
        rep(1 / max_visits, max_visits)
      } else if (b == 1L) {
        w <- seq_len(max_visits)
        w / sum(w)
      } else if (b == n_batches) {
        w <- rev(seq_len(max_visits))
        w / sum(w)
      } else {
        rep(1 / max_visits, max_visits)
      }
      sample(seq_len(max_visits), 1L, prob = probs)
    }, integer(1))
  }

  row_subj <- rep(subj_ids, times = n_visits_per_subj)
  row_visit <- unlist(lapply(n_visits_per_subj, seq_len), use.names = FALSE)
  n_obs <- length(row_subj)
  bat_vec <- subj_batch[match(row_subj, subj_ids)]
  baseline_age <- subj_age[match(row_subj, subj_ids)]
  sex_vec <- subj_sex[match(row_subj, subj_ids)]
  visit_effect <- (row_visit - 1) * (-1.5) + stats::rnorm(n_obs, 0, 1.5)
  symptom_score <- 50 + visit_effect + as.integer(bat_vec) * 0.8 + stats::rnorm(n_obs, 0, 2)

  covar_df <- data.frame(
    subid = row_subj,
    visit = row_visit,
    baseline_age = baseline_age,
    sex = sex_vec,
    symptom_score = round(symptom_score, 2)
  )

  feat_names <- paste0("feat_", seq_len(G))
  shifts <- lapply(seq_len(m), function(i) c(0, stats::runif(n_batches - 1, -3, 3)))
  scales <- lapply(seq_len(m), function(i) c(1, stats::runif(n_batches - 1, 0.6, 1.8)))
  subj_re <- stats::rnorm(n_subjects, 0, 3)
  row_re <- subj_re[match(row_subj, subj_ids)]
  symptom_score_c <- symptom_score - mean(symptom_score)

  data_list <- lapply(seq_len(m), function(i) {
    base <- matrix(
      stats::rnorm(
        n_obs * G,
        mean = 50 + baseline_age * 0.3 + as.integer(sex_vec) * 2.0 +
          row_re + symptom_score_c * 0.4,
        sd = 5
      ),
      n_obs,
      G
    )
    bshift <- shifts[[i]][as.integer(bat_vec)]
    bscale <- scales[[i]][as.integer(bat_vec)]
    y <- sweep(sweep(base, 1, bshift, "+"), 1, bscale, "*")
    colnames(y) <- feat_names
    rownames(y) <- paste0(row_subj, "_V", row_visit)
    as.data.frame(y)
  })

  list(
    data = data_list,
    bat = replicate(m, bat_vec, simplify = FALSE),
    covar = replicate(m, covar_df, simplify = FALSE),
    feat_names = feat_names,
    batch_levels = levels(bat_vec),
    m = m,
    n = n_obs,
    G = G,
    is_longitudinal = TRUE,
    subid_vec = row_subj,
    visit_vec = row_visit,
    n_subjects = n_subjects,
    visits_per_subj = n_visits_per_subj
  )
}
