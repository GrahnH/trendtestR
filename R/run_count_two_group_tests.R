#' Statistical Test for Count Data (Two Groups) / Statistischer Test fuer Zaehldaten (Zwei Gruppen)
#'
#' This function performs a simple comparison of count data between two groups.
#' It uses Poisson or Negative Binomial regression based on overdispersion,
#' focusing on providing a p-value and direction of difference without complex model interpretation.
#' Now includes basic descriptive statistics and confidence intervals.
#'
#' Diese Funktion fuehrt einen einfachen Vergleich von Zaehldaten zwischen zwei Gruppen durch.
#' Sie verwendet Poisson- oder Negative Binomial-Regression (abhaengig von Ueberdispersion),
#' wobei der Schwerpunkt auf der Bereitstellung eines p-Wertes und der Richtung des Unterschieds liegt,
#' ohne komplexe Modellinterpretation. Nun auch mit grundlegenden deskriptiven Statistiken und Konfidenzintervallen.
#'
#' @encoding UTF-8
#'
#' @param df A data frame containing the data, already prepared (e.g., by prepare_group_data).
#' @param value_col Name of the column containing count values. Default is ".value".
#' @param group_col Name of the grouping variable. Default is "jahr".
#' @param alpha Significance level for hypothesis testing. Default is 0.05.
#' @param phi Common heuristic for overdispersion. Default is 1.5.
#' @param effect_size Logical. Whether to calculate and return a simple effect size (e.g., Incidence Rate Ratio).
#' @param report_assumptions Logical. Whether to report basic assumption diagnostics (e.g., overdispersion status).
#'
#' @return A list containing test results (p-value, direction, chosen method),
#'         basic statistics, and confidence intervals.
#' @importFrom stats glm residuals sd coef confint na.omit poisson  glm.control
#' @importFrom MASS glm.nb
#' @importFrom dplyr filter group_by summarise n
#' @importFrom rlang .data
#' @export
run_count_two_group_tests <- function(df, value_col = ".value", group_col = "jahr",
                                      alpha = 0.05, phi = 1.5, effect_size = FALSE,
                                      report_assumptions = TRUE) {

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Bitte installieren Sie das Paket 'MASS' fuer Negative Binomial Regression (install.packages(\"MASS\")).")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Bitte installieren Sie das Paket 'dplyr' (install.packages(\"dplyr\")).")
  }

  if (any(df[[value_col]] < 0, na.rm = TRUE)) {
    warning("Negative Werte in Zaehldaten erkannt und entfernt.")
    df <- dplyr::filter(df, .data[[value_col]] >= 0 | is.na(.data[[value_col]]))
  }

  if (!is.factor(df[[group_col]])) {
    df[[group_col]] <- as.factor(df[[group_col]])
    message(paste0("Hinweis: Spalte '", group_col, "' wurde in einen Faktor umgewandelt."))
  }

  all_groups_stats <- df %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      sample_size = dplyr::n(),
      mean_val = mean(.data[[value_col]], na.rm = TRUE),
      sd_val = stats::sd(.data[[value_col]], na.rm = TRUE),
      .groups = 'drop'
    )

  non_empty_groups_df <- dplyr::filter(all_groups_stats, .data$sample_size > 0)
  non_empty_group_names <- non_empty_groups_df[[group_col]]


  if (length(non_empty_group_names) < 2) {
    stop("Fehler: Nicht genuegend Gruppen mit Daten fuer einen Vergleich (benoetigt mindestens 2 nach Filterung). Test wird abgebrochen.")
  }
  min_sample_per_group <- 2
  if (any(non_empty_groups_df$sample_size < min_sample_per_group)) {
    stop(paste0("Fehler: Mindestens eine Gruppe hat weniger als ", min_sample_per_group,
                " Beobachtungen. Ein Vergleich ist nicht moeglich."))
  }

  df <- dplyr::filter(df, .data[[group_col]] %in% non_empty_group_names)
  df[[group_col]] <- droplevels(df[[group_col]])

  vals <- split(df[[value_col]], df[[group_col]])
  group_levels <- levels(df[[group_col]])

  results <- list(
    type = NA,
    group_col = group_col,
    group_names = group_levels,
    sample_sizes = NA,
    p_value = NA,
    direction = NA,
    basic_stats = list(),
    effect_size = NA,
    confidence_intervals = list(),
    assumption_status = list()
  )

  results$sample_sizes <- setNames(non_empty_groups_df$sample_size, non_empty_groups_df[[group_col]])
  results$basic_stats$group_means <- setNames(non_empty_groups_df$mean_val, non_empty_groups_df[[group_col]])
  results$basic_stats$group_sds <- setNames(non_empty_groups_df$sd_val, non_empty_groups_df[[group_col]])

  check_overdispersion <- function(model) {
    if (inherits(model, "glm")) {
      phi_res <- sum(residuals(model, type = "pearson")^2) / model$df.residual
      return(list(
        overdispersion_phi = phi_res,
        is_overdispersed = phi_res > phi
      ))
    }
    return(list(overdispersion_phi = NA, is_overdispersed = FALSE))
  }

  poisson_model <- tryCatch({
    formula_str <- paste0(value_col, " ~ ", group_col)
    stats::glm(as.formula(formula_str), family = poisson(link = "log"), data = df)
  }, error = function(e) {
    warning("Fehler beim Anpassen des Poisson-Modells: ", e$message)
    NULL
  })

  model_to_use <- NULL
  chosen_model_type <- NA
  overdispersion_diagnostics <- list()

  if (!is.null(poisson_model)) {
    overdispersion_diagnostics <- check_overdispersion(poisson_model) # Get diagnostics

    if (overdispersion_diagnostics$is_overdispersed) {
      message("Hinweis: Ueberdispersion erkannt, versuche Negative Binomial Regression.")
      nb_model <- tryCatch({
        formula_str <- paste0(value_col, " ~ ", group_col)
        MASS::glm.nb(as.formula(formula_str), data = df,
                     control = glm.control(maxit = 100, trace = FALSE))
      }, error = function(e) {
        warning("Fehler beim Anpassen des Negative Binomial-Modells: ", e$message)
        NULL
      })
      model_to_use <- if (!is.null(nb_model)) nb_model else poisson_model # Fallback to Poisson if NB fails
      chosen_model_type <- if (!is.null(nb_model)) "Negative Binomial Regression" else "Poisson Regression (with Overdispersion warning)"
    } else {
      model_to_use <- poisson_model
      chosen_model_type <- "Poisson Regression"
    }
  }

  if (is.null(model_to_use)) {
    results$status <- "Fehler: Kein geeignetes Modell fuer Zaehldaten gefunden."
    return(results)
  }

  if (report_assumptions) {
    results$assumption_status <- overdispersion_diagnostics
  }

  if (length(group_levels) != 2) {
    results$p_value <- NA
    results$direction <- "Die Anzahl der Gruppen mit Daten ist nicht genau zwei nach Filterung."
    warning("Nicht genau zwei Gruppen mit Daten nach Filterung. P-Wert und Richtung nicht berechnet.")
    return(results) # Exit early if not exactly two groups for two-group test
  }

  if (nrow(coef(summary(model_to_use))) >= 2) { # Ensure there's at least a second coefficient (group effect)
    results$p_value <- coef(summary(model_to_use))[2, "Pr(>|z|)"]
  } else {
    warning("Gruppenkoeffizient nicht im Modell gefunden oder nicht an erwarteter Position 2, P-Wert ist NA.")
    results$p_value <- NA
  }

  results$type <- chosen_model_type

  mean_group1 <- results$basic_stats$group_means[group_levels[1]]
  mean_group2 <- results$basic_stats$group_means[group_levels[2]]

  if (is.na(mean_group1) || is.na(mean_group2)) {
    results$direction <- "Die mittleren Zaehlraten konnten aufgrund fehlender Werte nicht verglichen werden."
  } else if (mean_group1 > mean_group2) {
    results$direction <- paste0("Die mittlere Zaehlrate in Gruppe '", group_levels[1], "' ist hoeher als in Gruppe '", group_levels[2], "'.")
  } else if (mean_group2 > mean_group1) {
    results$direction <- paste0("Die mittlere Zaehlrate in Gruppe '", group_levels[2], "' ist hoeher als in Gruppe '", group_levels[1], "'.")
  } else {
    results$direction <- "Die mittleren Zaehlraten sind aehnlich."
  }

  if (!is.null(model_to_use)) {
    tryCatch({
      conf_int_coef <- confint(model_to_use)
      if (nrow(conf_int_coef) >= 2) {
        results$confidence_intervals$group_coefficient <- conf_int_coef[2, ]
      } else {
        warning("Konfidenzintervall fuer Gruppenkoeffizient nicht verfuegbar oder nicht an erwarteter Position 2.")
      }
    }, error = function(e) {
      warning("Fehler beim Berechnen der Konfidenzintervalle fuer Modellkoeffizienten: ", e$message)
    })
  }

  if (effect_size && (chosen_model_type == "Poisson Regression" || chosen_model_type == "Negative Binomial Regression" || grepl("Poisson Regression \\(with Overdispersion warning\\)", chosen_model_type))) {
    if (nrow(coef(summary(model_to_use))) >= 2) {
      irr <- exp(coef(model_to_use)[2]) # e^beta for the group dummy variable
      results$effect_size <- list(value = irr)

      if (!is.null(results$confidence_intervals$group_coefficient)) {
        results$confidence_intervals$IRR <- exp(results$confidence_intervals$group_coefficient)
      }
    } else {
      results$effect_size <- list(type = "Incidence Rate Ratio (IRR)", value = NA, note = "IRR konnte nicht berechnet werden, da Gruppenkoeffizient fehlt.")
    }
  }

  return(results)
}
