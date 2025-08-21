#' Statistical Test for Count Data (Multi-Groups) / Statistischer Test fuer Zaehldaten (Mehrere Gruppen)
#'
#' This function performs a simple comparison of count data across three or more groups.
#' It uses Poisson or Negative Binomial regression, considering overdispersion,
#' followed by an ANOVA-like test for the overall group effect and optional post-hoc tests.
#' Focus is on overall p-value and identifying differing groups, without complex model interpretation.
#'
#' Diese Funktion fuehrt einen einfachen Vergleich von Zaehldaten bei drei oder mehr Gruppen durch.
#' Sie verwendet Poisson- oder Negative Binomial-Regression (abhaengig von Ueberdispersion),
#' gefolgt von einem ANOVA-aehnlichen Test fuer den Gesamtgruppeneffekt und optionalen Post-Hoc-Tests.
#' Der Schwerpunkt liegt auf dem Gesamt-p-Wert und der Identifizierung unterschiedlicher Gruppen,
#' ohne komplexe Modellinterpretation.
#'
#' @encoding UTF-8
#'
#' @param df A data frame containing the data, already prepared (e.g., by prepare_group_data).
#' @param value_col Name of the column containing count values. Default is ".value".
#' @param group_col Name of the grouping variable. Default is "jahr".
#' @param alpha Significance level for hypothesis testing. Default is 0.05.
#' @param phi Common heuristic for overdispersion. Default is 1.5.
#' @param effect_size Logical. Whether to calculate and return a simple effect size (e.g., Pseudo R-squared).
#' @param report_assumptions Logical. Whether to report basic assumption diagnostics (e.g., overdispersion status).
#'
#' @return A list containing test results (p-value, significant groups, chosen method).
#' @examples
#' set.seed(123)
#' data <- data.frame(
#' .value = c(rpois(50, 3), rpois(50, 5), rpois(50, 4)),
#' jahr = factor(rep(c("2020", "2021", "2022"), each = 50))
#' )
#'
#' result <- run_count_multi_group_tests(
#' df = data,
#' value_col = ".value",
#' group_col = "jahr",
#' alpha = 0.05,
#' phi = 1.5,
#' effect_size = TRUE,
#' report_assumptions = TRUE
#' )
#'
#' print(result$p_value)
#' print(result$significant_pairwise_differences)
#' print(result$effect_size)
#'
#' @importFrom stats glm poisson residuals na.omit setNames
#' @importFrom MASS glm.nb
#' @importFrom car Anova
#' @importFrom multcomp glht mcp
#' @importFrom dplyr filter
#' @importFrom emmeans emmeans contrast
#' @export
run_count_multi_group_tests <- function(df, value_col = ".value", group_col = "jahr",
                                        alpha = 0.05, phi = 1.5, effect_size = FALSE,
                                        report_assumptions = FALSE) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Bitte installieren Sie das Paket 'MASS' fuer Negative Binomial Regression (install.packages(\"MASS\")).")
  }
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Bitte installieren Sie das Paket 'car' fuer Anova auf GLM-Objekten (install.packages(\"car\")).")
  }
  if (!requireNamespace("multcomp", quietly = TRUE)) {
    stop("Bitte installieren Sie das Paket 'multcomp' fuer Post-Hoc-Tests auf GLM-Objekten (install.packages(\"multcomp\")).")
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
    dplyr::filter(!is.na(.data[[value_col]])) %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      sample_size = sum(!is.na(.data[[value_col]])),
      mean_val = mean(.data[[value_col]], na.rm = TRUE),
      sd_val = stats::sd(.data[[value_col]], na.rm = TRUE),
      .groups = 'drop'
    )

  non_empty_groups_df <- dplyr::filter(all_groups_stats, .data$sample_size > 0)
  non_empty_group_names <- non_empty_groups_df[[group_col]]

  if (length(non_empty_group_names) < 3) {
    stop("Fehler: Nicht genuegend Gruppen mit Daten fuer einen Vergleich (benoetigt mindestens 3 nach Filterung). Test wird abgebrochen.")
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
  sample_sizes <- setNames(non_empty_groups_df$sample_size, non_empty_groups_df[[group_col]])

  results <- list(
    type = NA,
    group_col = group_col,
    group_names = group_levels,
    sample_sizes = sample_sizes,
    p_value = NA,
    significant_pairwise_differences = NULL,
    effect_size = NA,
    assumption_status = list()
  )

  check_overdispersion <- function(model) {
    if (inherits(model, "glm")) {
      phi_res <- sum(residuals(model, type = "pearson")^2) / model$df.residual
      is_overdispersed <- phi_res > phi

      if (report_assumptions) {
        results$assumption_status$overdispersion_phi_res <<- phi_res
        results$assumption_status$is_overdispersed <<- is_overdispersed
      }

      return(is_overdispersed)
    }
    return(FALSE)
  }

  poisson_model <- tryCatch({
    formula_str <- paste0(value_col, " ~ ", group_col)
    stats::glm(as.formula(formula_str), family = stats::poisson(link = "log"), data = df)
  }, error = function(e) {
    warning("Fehler beim Anpassen des Poisson-Modells: ", e$message)
    NULL
  })

  model_to_use <- NULL
  chosen_model_type <- NA

  if (!is.null(poisson_model)) {
    if (check_overdispersion(poisson_model)) {
      message("Hinweis: Ueberdispersion erkannt, versuche Negative Binomial Regression.")
      nb_model <- tryCatch({
        formula_str <- paste0(value_col, " ~ ", group_col)
        MASS::glm.nb(as.formula(formula_str), data = df)
      }, error = function(e) {
        warning("Fehler beim Anpassen des Negative Binomial-Modells: ", e$message)
        NULL
      })

      if (!is.null(nb_model)) {
        model_to_use <- nb_model
        chosen_model_type <- "Negative Binomial Regression"
      } else {
        model_to_use <- poisson_model
        chosen_model_type <- "Poisson Regression (with Overdispersion warning)"
        warning("Negative Binomial model fitting failed, using Poisson with overdispersion")
      }
    } else {
      model_to_use <- poisson_model
      chosen_model_type <- "Poisson Regression"
    }
  }

  if (is.null(model_to_use)) {
    results$status <- "Fehler: Kein geeignetes Modell fuer Zaehldaten gefunden."
    return(results)
  }

  anova_result <- tryCatch({
    car::Anova(model_to_use, type = "II")
  }, error = function(e) {
    warning("Fehler bei Anova auf GLM-Objekt: ", e$message)
    NULL
  })

  if (!is.null(anova_result)) {
    message("ANOVA-Ergebnisse verfuegbar. Zeilennamen: ", paste(rownames(anova_result), collapse = ", "))
    message("Spaltennamen: ", paste(colnames(anova_result), collapse = ", "))
  }

  if (!is.null(anova_result)) {
    p_col <- grep("Pr\\(>", colnames(anova_result), value = TRUE)[1]
    group_row <- which(grepl(group_col, rownames(anova_result)))[1]

    if (!is.na(group_row) && !is.na(p_col)) {
      results$p_value <- anova_result[group_row, p_col]
    } else {
      warning("Konnte p-Wert nicht aus ANOVA-Ergebnissen extrahieren.")
      results$p_value <- NA
    }

    results$type <- chosen_model_type

    if (!is.na(results$p_value) && is.numeric(results$p_value) && results$p_value < alpha) {
      message("Gesamttest signifikant (p < ", alpha, "). Fuehre Post-Hoc-Tests durch.")


      posthoc_result <- tryCatch({
        emm <- emmeans::emmeans(model_to_use, specs = as.formula(paste("~", group_col)))
        emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
      }, error = function(e) {
        message("Fehler bei Post-Hoc-Test (emmeans::contrast): ", e$message)
        message("Versuche Fallback auf multcomp::glht direkt.")
        tryCatch({
          mcp_formula <- stats::setNames(list("Tukey"), group_col)
          names(mcp_formula) <- group_col
          mc <- multcomp::glht(model_to_use, linfct = multcomp::mcp(mcp_formula))
          summary(mc)
        }, error = function(e2) {
          message("Fehler bei Post-Hoc-Test (multcomp::glht Fallback): ", e2$message)
          NULL
        })
      })

      if (!is.null(posthoc_result)) {
        posthoc_summary <- summary(posthoc_result)

        if ("p.value" %in% names(posthoc_summary)) {
          all_comparisons <- data.frame(
            contrast = as.character(posthoc_summary$contrast),
            estimate = posthoc_summary$estimate,
            SE = posthoc_summary$SE,
            z_ratio = posthoc_summary$z.ratio,
            p_value = posthoc_summary$p.value,
            significant = posthoc_summary$p.value < alpha,
            stringsAsFactors = FALSE
          )

          sig_diffs <- posthoc_summary$p.value < alpha
          if (any(sig_diffs)) {
            results$significant_pairwise_differences <- all_comparisons[sig_diffs, ]
            if (any(!sig_diffs)) {
              results$all_pairwise_comparisons <- all_comparisons
            }
          } else {
            results$significant_pairwise_differences <- "Keine signifikanten paarweisen Unterschiede (nach Anpassung)."
            results$all_pairwise_comparisons <- all_comparisons
          }
        } else if (inherits(posthoc_result, "summary.glht")){

          sig_diffs <- posthoc_result$test$pvalues < alpha
          if (any(sig_diffs)) {
            sig_contrasts <- data.frame(
              contrast = names(posthoc_result$test$pvalues)[sig_diffs],
              p_value = posthoc_result$test$pvalues[sig_diffs],
              stringsAsFactors = FALSE
            )
            results$significant_pairwise_differences <- sig_contrasts
          } else {
            results$significant_pairwise_differences <- "Keine signifikanten paarweisen Unterschiede (nach Anpassung)."
          }
        } else {
          results$significant_pairwise_differences <- "Error: Could not extract p-values from post-hoc test."
        }
      }
    } else {
      if (is.na(results$p_value)) {
        results$significant_pairwise_differences <- "Fehler: p-Wert konnte nicht berechnet werden."
      } else {
        results$significant_pairwise_differences <- "Gesamttest nicht signifikant. Keine Post-Hoc-Tests erforderlich."
      }
    }

    if (effect_size) {
      if (!is.null(model_to_use$null.deviance) && !is.null(model_to_use$deviance)) {
        mcfadden_r2 <- 1 - (model_to_use$deviance / model_to_use$null.deviance)
        results$effect_size <- list(
          type = "Pseudo R-squared (McFadden)",
          value = mcfadden_r2
        )
      } else {
        results$effect_size <- list(
          type = "Pseudo R-squared (McFadden)",
          value = NA
        )
      }
    } else {
      results$effect_size <- NA
    }

  } else {
    results$status <- "Fehler: Gesamt-ANOVA-aehnlicher Test konnte nicht durchgefuehrt werden."
  }

  return(results)
}
