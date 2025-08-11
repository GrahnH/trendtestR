#' Infer variable type from numeric vector / Typ-Erkennung numerischer Vektoren
#'
#' This function analyzes a numeric vector and infers the underlying variable type:
#' "binary", "proportion", "count", "discrete", or "continuous".
#' Useful for selecting statistical tests or visualization strategies.
#'
#' Diese Funktion analysiert einen numerischen Vektor und erkennt den zugrunde liegenden Typ:
#' "binary", "proportion", "count", "discrete" oder "continuous".
#' Nützlich zur Auswahl geeigneter statistischer Tests oder Visualisierungen.
#'
#' @encoding UTF-8
#'
#' @param values A numeric vector<br>Ein numerischer Vektor
#' @param verbose Logical, whether to show warnings (default TRUE)<br>Gibt an, ob Warnmeldungen angezeigt werden (Standard: TRUE)
#' @param thresholds Optional list of numeric thresholds for type detection (for internal use only)<br>Optionale Liste von Schwellwerten zur Typ-Erkennung (nur intern verwendet)
#' @return A character string indicating the inferred type: "binary", "proportion", "count", "discrete", or "continuous".<br>
#' Ein Zeichenstring mit dem erkannten Typ: "binary", "proportion", "count", "discrete" oder "continuous".
#'
#' @seealso [standardize_case_columns()]
#' @examples
#' infer_value_type(c(1, 0, 1, 1))
#' infer_value_type(c(0.2, 0.5, 0.8))
#' infer_value_type(c(3, 4, 6, 1000000))
#'
#' @export

infer_value_type <- function(values, verbose = TRUE, thresholds = NULL) {
  `%||%` <- function(x, y) if (!is.null(x)) x else y

  default_thresholds <- list(
    integer_tol        = 1e-10,
    integer_ratio_min  = 0.95,
    min_values         = 3,
    large_value_limit  = 1e8,
    max_val_cumu       = 10,
    unique_ratio_count = 0.9,
    zero_ratio_count   = 0.01,
    cv_count           = 0.1,
    max_negative_prop  = 0.01,
    zi_ratio           = 0.4,
    skew_tol           = 0.2
  )

  th <- utils::modifyList(default_thresholds, thresholds %||% list())

  non_na <- values[!is.na(values)]
  n <- length(non_na)

  if (n < th$min_values) {
    if (verbose) warning("Zu wenige gueltige Werte (<", th$min_values, "); Standardtyp 'continuous' wird verwendet.")
    return(list(type = "continuous", features = NA))
  }

  if (any(abs(non_na) > th$large_value_limit)) {
    if (verbose) warning("Extremwerte erkannt; Typ-Erkennung koennte unzuverlaessig sein.")
  }

  if (all(non_na %in% c(0, 1))) {
    return(list(type = "binary", features = NA))
  }

  if (all(non_na >= 0 & non_na <= 1)) {
    return(list(type = "proportion", features = NA))
  }

  is_integer_like <- mean(abs(non_na - round(non_na)) < th$integer_tol)
  unique_ratio    <- length(unique(non_na)) / n
  max_val         <- max(non_na)
  zero_ratio      <- mean(non_na == 0)
  mean_val        <- mean(non_na)
  var_val         <- stats::var(non_na)
  cv              <- if (mean_val == 0) NA else sqrt(var_val) / mean_val
  neg_prop        <- mean(non_na < 0)
  is_monotonic    <- all(diff(non_na) >= -th$integer_tol)

  # Count candidate
  if (is_integer_like > th$integer_ratio_min && neg_prop <= th$max_negative_prop) {
    if (is_monotonic && max_val > th$max_val_cumu && unique_ratio > 0.5) {
      if (verbose) message("Hinweis: Variable sieht aus wie eine kumulative Zaehlvariable.")
      zi_flag <- if (zero_ratio > th$zi_ratio) "zi" else NA
      return(list(type = "count", features = zi_flag))
    }

    if (!is.na(cv) && zero_ratio > th$zero_ratio_count &&
        unique_ratio < th$unique_ratio_count &&
        cv > th$cv_count) {
      zi_flag <- if (zero_ratio > th$zi_ratio) "zi" else NA
      return(list(type = "count", features = zi_flag))
    } else {
      return(list(type = "discrete", features = NA))
    }
  }

  if (is_integer_like > 0.1 && verbose) {
    warning("Mischtyp erkannt (Anteil ganze Zahlen: ",
            round(is_integer_like * 100, 1), "%); Datenqualitaet bitte pruefen.")
  }

  # Continuous + skewness
  skew <- if (n >= 3) e1071::skewness(non_na) else NA
  skew_flag <- if (is.na(skew)) NA else {
    if (skew > th$skew_tol) "right-skewed"
    else if (skew < -th$skew_tol) "left-skewed"
    else "symmetric"
  }

  return(list(type = "continuous", features = skew_flag))
}
