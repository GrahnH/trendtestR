#' Paired / Unpaired Two-Group Tests with Assumption Checks / Zwei-Gruppen-Test mit Vorannahmepruefung
#'
#' This function performs both parametric (t-test) and non-parametric (Wilcoxon test) comparisons between two groups.
#' For paired data, it calculates the difference and performs Shapiro-Wilk normality test on the difference. Based on this, it recommends either a paired t-test or a Wilcoxon signed-rank test.
#' Optionally, it calculates the effect size (Cohen's d) and returns assumption diagnostics.
#'
#' Diese Funktion fuehrt sowohl parametrische (t-Test) als auch nicht-parametrische (Wilcoxon-Test) Vergleiche zwischen zwei Gruppen durch.
#' Bei gepaarten Daten wird die Differenz gebildet und auf Normalverteilung geprueft(Shapiro-Test). Je nach Ergebnis wird ein gepaarter t-Test oder ein Wilcoxon-Vorzeichen-Rang-Test empfohlen.
#' Optional wird die Effektgroesse (Cohens d) berechnet und die Vorannahmen zurueckgegeben.
#'
#' @encoding UTF-8
#'
#' @param df A data frame with exactly two groups. / Ein Data Frame mit genau zwei Gruppen
#' @param value_col Name of the column containing values to compare. Default is `".value"`. / Name der Werte-Spalte, Standard: `".value"`
#' @param group_col Name of the grouping variable. Default is `"jahr"`. / Spaltenname der Gruppierungsvariable, Standard: `"jahr"`
#' @param alpha Significance level for hypothesis testing. Default is `0.05`. / Signifikanzniveau fuer Testentscheidungen, Standard: `0.05`
#' @param effect_size Logical. Whether to calculate Cohen's d. / Logisch, ob Cohen's d berechnet werden soll
#' @param report_assumptions Logical. Whether to include normality test results. / Logisch, ob Shapiro-Test zurueckgegeben wird
#' @param paired Logical. Whether the data are paired. / Logisch: gepaarte Daten?
#'
#' @return A list containing:
#' \describe{
#'   \item{type}{Test type performed ("Paired Test" or "Unpaired Test")}
#'   \item{sample_sizes}{Number of observations per group}
#'   \item{group_names}{Names of the two groups}
#'   \item{t_test}{Result of the t-test (paired or unpaired)}
#'   \item{wilcox_test}{Result of the Wilcoxon test}
#'   \item{effect_size}{Cohen's d (if enabled)}
#'   \item{assumptions}{Shapiro-Wilk normality test result(s)}
#'   \item{recommendation}{Recommended test based on normality}
#' }
#'
#' @seealso [run_group_tests()], [run_multi_group_tests()], [prepare_group_data()]
#'
#' @examples
#' df <- data.frame(
#'   jahr = rep(c("2020", "2021"), each = 10),
#'   .value = c(rnorm(10, 30), rnorm(10, 32))
#' )
#' run_paired_tests(df, paired = TRUE)
#'
#' @importFrom stats t.test wilcox.test shapiro.test qnorm
#' @export


run_paired_tests <- function(df, value_col = ".value", group_col = "jahr",
                             alpha = 0.05, effect_size = TRUE,
                             report_assumptions = TRUE, paired = TRUE) {
  prep <- prepare_group_data(df, value_col, group_col)
  vals <- prep$vals
  n1 <- length(vals[[1]])
  n2 <- length(vals[[2]])
  group_names <- names(vals)

  if (n1 < 3 || n2 < 3) {
    return(list(
      type = "Fehler - Zu wenig Daten",
      sample_sizes = c(n1, n2),
      group_names = group_names,
      error = "Mindestens eine Gruppe hat weniger als 3 Beobachtungen."
    ))
  }

  if (paired && n1 != n2) {
    return(list(
      type = "Fehler - Ungleiche Gruppengroessen",
      sample_sizes = c(n1, n2),
      group_names = group_names,
      error = "Unterschiedliche Gruppengroessen: gepaarter Test nicht moeglich."
    ))
  }

  shapiro1 <- NULL
  shapiro2 <- NULL
  diff_shapiro <- NULL
  normality <- NULL
  if (paired) {
    diff <- vals[[1]] - vals[[2]]
    if (!is.null(diff) && length(stats::na.omit(diff)) >= 3) {
      diff_shapiro <- tryCatch(stats::shapiro.test(diff), error = function(e) NULL)
      normality <- !is.null(diff_shapiro) && diff_shapiro$p.value > alpha
    }
  } else {
    if (length(stats::na.omit(vals[[1]])) >= 3) {
      shapiro1 <- tryCatch(stats::shapiro.test(vals[[1]]), error = function(e) NULL)
    }
    if (length(stats::na.omit(vals[[2]])) >= 3) {
      shapiro2 <- tryCatch(stats::shapiro.test(vals[[2]]), error = function(e) NULL)
    }
    normality <- !is.null(shapiro1) && !is.null(shapiro2) &&
      shapiro1$p.value > alpha && shapiro2$p.value > alpha
  }

  cohens_d <- NULL
  if (effect_size && paired && !is.null(diff) && !all(is.na(diff)) && sd(diff, na.rm = TRUE) != 0) {
    cohens_d <- mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE)
  }

  t_test <- tryCatch(
    stats::t.test(vals[[1]], vals[[2]], paired = paired),
    error = function(e) list(error = e$message)
  )

  wilcox_test <- tryCatch(
    stats::wilcox.test(vals[[1]], vals[[2]], paired = paired, exact = FALSE),
    error = function(e) list(error = e$message)
  )

  nonparam_r <- NULL
  nonparam_rank_biserial <- NULL
  if (effect_size && !is.null(wilcox_test) && !is.null(wilcox_test$statistic) && !is.null(wilcox_test$p.value)) {
    direction <- sign(mean(vals[[1]], na.rm = TRUE) - mean(vals[[2]], na.rm = TRUE))
    Z <- stats::qnorm(wilcox_test$p.value / 2, lower.tail = FALSE) * direction
    nonparam_r <- Z / sqrt(n1 + n2)
  }
  if (!is.null(wilcox_test$statistic)) {
    if(paired){
      V_pos <- wilcox_test$statistic
      n_pairs <- sum(!is.na(vals[[1]] - vals[[2]]))
      total_rank_sum <- n_pairs * (n_pairs + 1) / 2
      V_neg <- total_rank_sum - V_pos
      nonparam_rank_biserial <- (V_pos - V_neg) / (V_pos + V_neg)
      }
    else {
      W <- wilcox_test$statistic
      U <- W - n1 * (n1 + 1) / 2
      nonparam_rank_biserial <- unname(1 - (2 * U) / (n1 * n2))
   }
 }


  if (paired) {
    if (!is.null(diff_shapiro)) {
      recommendation <- if (normality) "Paired t-test empfohlen" else "Wilcoxon-Vorzeichen-Rang-Test empfohlen"
    } else {
      recommendation <- "Paarungsstatus beruecksichtigt, aber Normalitaetspruefung nicht moeglich oder unzureichend."
    }
  } else {
    if (!is.null(shapiro1) && !is.null(shapiro2)) {
      recommendation <- if (normality) "Ungepaarter t-Test empfohlen" else "Mann-Whitney U-Test empfohlen"
    } else {
      recommendation <- "Ungepaarte Tests durchgefuehrt, aber Normalitaetspruefung nicht moeglich."
    }
  }

  assumptions <- NULL
  if (report_assumptions) {
    assumptions <- if (paired) {
      list(
        normality_test = diff_shapiro,
        normality = normality
      )
    } else {
      list(
        normality_test = list(group1 = shapiro1, group2 = shapiro2),
        normality = normality
      )
    }
  }

  return(list(
    type = if (paired) "Paired Test" else "Unpaired Test",
    sample_sizes = c(n1, n2),
    group_col = group_col,
    group_names = group_names,
    t_test = t_test,
    wilcox_test = wilcox_test,
    effect_size = list(cohens_d = cohens_d, nonparam_r = nonparam_r, rank_biserial = nonparam_rank_biserial),
    assumptions = assumptions,
    recommendation = recommendation
  ))
}

