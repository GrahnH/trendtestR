#' Automated Selection of Statistical Group Tests / Automatisierte Auswahl statistischer Gruppentests
#'
#' This function automatically determines whether to perform a two-group test (paired or unpaired) or a multi-group test depending on the number of groups in the data.
#' For two groups, both paired t-test (if specified) and Wilcoxon test are run. For three or more groups, the function checks assumptions (normality and homogeneity of variances) and selects either ANOVA with Tukey post-hoc or Kruskal-Wallis with Dunn post-hoc.
#' All tests include assumption checking and optional effect size calculation.
#'
#' Diese Funktion erkennt anhand der Anzahl der Gruppen automatisch, ob ein Zwei-Gruppen-Test (gepaart oder ungepaart) oder ein Mehr-Gruppen-Test erforderlich ist.
#' Bei zwei Gruppen werden t-Test (gepaart oder ungepaart) und Wilcoxon-Test durchgefuehrt. Bei drei oder mehr Gruppen erfolgt eine Auswahl zwischen ANOVA mit Tukey oder Kruskal-Wallis mit Dunn, je nach Verteilungsannahmen.
#' Alle Tests beinhalten Vorannahmepruefungen und (optional) Effektgroessenschaetzungen.
#' @encoding UTF-8
#'
#' @param df A data frame with at least two groups. / Ein Data Frame mit mindestens zwei Gruppen
#' @param value_col Name of the column containing values to compare. Default is `".value"`. / Name der Werte-Spalte, Standard: `".value"`
#' @param group_col Name of the grouping variable. Default is `"jahr"`. / Spaltenname der Gruppierungsvariable, Standard: `"jahr"`
#' @param alpha Significance level for hypothesis testing. Default is `0.05`. / Signifikanzniveau fuer Testentscheidungen, Standard: `0.05`
#' @param effect_size Logical. Whether to calculate effect sizes. / Logisch, ob Effektgroessen berechnet werden sollen
#' @param report_assumptions Logical. Whether to include assumption check results. / Logisch, ob Vorannahmen ausgegeben werden sollen
#' @param paired Only relevant for two groups: `TRUE` for paired data. / Nur bei zwei Gruppen relevant: `TRUE` fuer gepaarte Daten
#'
#' @return A list containing:
#' \describe{
#'   \item{type}{Type of test performed (e.g., "Paired Test", "ANOVA")}
#'   \item{sample_sizes}{Number of observations per group}
#'   \item{group_names}{Group labels}
#'   \item{t_test / kruskal / anova}{Test result object(s)}
#'   \item{effect_size}{Effect size estimates (e.g., Cohen's d, eta-squared)}
#'   \item{assumptions}{Assumption check results}
#'   \item{recommendation}{Recommended test type based on assumptions}
#' }
#'
#' @seealso [run_paired_tests()], [run_multi_group_tests()], [run_count_two_group_tests()], [run_count_multi_group_tests()]
#'
#' @examples
#' df <- data.frame(
#'   jahr = rep(c("2020", "2021"), each = 10),
#'   .value = c(rnorm(10, 20, 3), rnorm(10, 22, 3))
#' )
#' result <- run_group_tests(df)
#'
#' @export


run_group_tests <- function(df, value_col = ".value", group_col = "jahr",
                            alpha = 0.05, effect_size = TRUE,
                            report_assumptions = TRUE, paired = FALSE) {
  prep <- prepare_group_data(df, value_col, group_col)
  attrv <- attr(df, "value_data_type")
  if (is.null(attr(df, "value_data_type"))) {
    message("Kein 'value_data_type'-Attribut gefunden. Fuehre standardize_case_columns() automatisch aus.")
    attrc <- infer_value_type(df[[value_col]])
    attrv <- attrc$type}

  if (attrv %in% c("binary", "proportion")) {
    message(
      "Hinweis: Variablentyp '", attrv,
      "' erkannt. In trendtestR 1.0.0 werden die Daten weiter mit Standardtyp 'continuous' bearbeiten")
  }

  if (prep$n_groups == 2) {
    if (attrv != "count"){
      return(run_paired_tests(prep$df, value_col, group_col,
                              alpha, effect_size, report_assumptions, paired = paired))
    } else if(attrv == "count"){
      return(run_count_two_group_tests(prep$df, value_col, group_col,
                                       alpha,effect_size, report_assumptions))
      }

  } else if (prep$n_groups >= 3) {
    if (attrv != "count"){
      return(run_multi_group_tests(prep$df, value_col, group_col,
                                 alpha, effect_size, report_assumptions))
      } else if(attrv == "count"){
      return(run_count_multi_group_tests(prep$df, value_col, group_col,
                                       alpha,effect_size, report_assumptions))
      }
  }else {
    return(list(
      type = "Nicht unterstuetzt",
      group_count = prep$n_groups,
      error = "Weniger als 2 Gruppen"
    ))
  }
}
