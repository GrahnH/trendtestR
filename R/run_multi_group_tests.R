#' Multi-Group Test with Assumption Checks / Mehr-Gruppen-Test mit Annahmepruefung
#'
#' This function performs multi-group statistical comparisons depending on distribution and variance assumptions.
#' If all groups pass the Shapiro-Wilk test for normality and Levene's test for homogeneity of variances, an ANOVA is performed with post-hoc Tukey test.
#' Otherwise, the Kruskal-Wallis test is used, followed by Dunn's test (Bonferroni-adjusted).
#' Effect size (eta2 or approximate) and assumption diagnostics are returned.
#'
#' Diese Funktion fuehrt Mehr-Gruppen-Vergleiche durch, abhaengig von Verteilungs- und Varianzannahmen.
#' Wenn alle Gruppen normalverteilt sind (Shapiro-Wilk) und die Varianz homogen ist (Levene-Test), wird eine ANOVA mit Tukey-Post-Hoc-Test durchgefuehrt.
#' Andernfalls wird ein Kruskal-Wallis-Test mit anschliessender Dunn-Analyse (Bonferroni-korrigiert) verwendet.
#' Effektgroessen (eta2 oder Annaeherung) und Annahmepruefungen werden zurueckgegeben.
#'
#' @encoding UTF-8
#'
#' @param df A data frame with three or more groups. / Ein Data Frame mit drei oder mehr Gruppen
#' @param value_col Name of the column containing values to compare. Default is ".value". / Name der Werte-Spalte, Standard: ".value"
#' @param group_col Name of the grouping variable. Default is "jahr". / Spaltenname der Gruppierungsvariable, Standard: "jahr"
#' @param alpha Significance level for hypothesis testing. Default is 0.05. / Signifikanzniveau fuer Testentscheidungen, Standard: 0.05
#' @param effect_size Logical. Whether to calculate eta2 or its approximation. / Logisch, ob eta2 berechnet werden soll
#' @param report_assumptions Logical. Whether to include assumption checks. / Logisch, ob Vorannahmen ausgegeben werden sollen
#'
#' @return A list containing:
#' \describe{
#'   \item{type}{Type of test performed ("ANOVA" or "Kruskal-Wallis")}
#'   \item{sample_sizes}{Sample size per group}
#'   \item{assumptions}{List of assumption test results: Shapiro-Wilk, Levene, Bartlett}
#'   \item{anova / kruskal}{Test result object}
#'   \item{eta_squared / eta_squared_approx}{Effect size}
#'   \item{interpretation}{Interpretation of eta2 magnitude}
#'   \item{posthoc / dunn}{Post-hoc test result (Tukey or Dunn)}
#'   \item{recommendation}{Recommended method based on assumption checks}
#' }
#'
#' @seealso [run_group_tests()], [run_paired_tests()]
#'
#' @examples
#' df <- data.frame(
#'   jahr = rep(c("2020", "2021", "2022"), each = 10),
#'   .value = c(rnorm(10, 20), rnorm(10, 23), rnorm(10, 22))
#' )
#' run_multi_group_tests(df)
#'
#' @importFrom stats aov bartlett.test kruskal.test shapiro.test TukeyHSD
#' @importFrom car leveneTest
#' @importFrom FSA dunnTest
#' @export


run_multi_group_tests <- function(df, value_col = ".value", group_col = "jahr",
                                  alpha = 0.05, effect_size = TRUE, report_assumptions = TRUE) {

  prep <- prepare_group_data(df, value_col, group_col)
  vals <- prep$vals
  sample_sizes <-prep$sample_sizes
  group_names <- prep$group_names
  n_groups <- prep$n_groups
  if (!is.factor(df[[group_col]])) {
    df[[group_col]] <- as.factor(df[[group_col]])}

  if (any(sample_sizes < 3)) {
    warning("Mindestens eine Gruppe hat weniger als 3 Beobachtungen: Tests moeglicherweise nicht zuverlaessig.")
  }

  shapiro_list <- lapply(vals, function(v) {
    if (length(v) >= 3 && length(v) <= 5000) tryCatch(stats::shapiro.test(v), error = function(e) NULL) else NULL
  })
  normality_ok <- all(sapply(shapiro_list, function(x) !is.null(x) && x$p.value > alpha), na.rm = TRUE)

  levene_p <- tryCatch(car::leveneTest(df[[value_col]] ~ df[[group_col]])$`Pr(>F)`[1], error = function(e) NA)
  bartlett_p <- tryCatch(stats::bartlett.test(df[[value_col]] ~ df[[group_col]])$p.value, error = function(e) NA)
  var_homogeneous <- !is.na(levene_p) && levene_p > alpha

  #eta2 interpretieren
  interpret_eta2 <- function(eta2) {
    if (is.null(eta2)) return(NA)
    if (eta2 < 0.01) "negligible"
    else if (eta2 < 0.06) "small"
    else if (eta2 < 0.14) "moderate"
    else "large"
  }

  if (normality_ok && var_homogeneous) {
    aov_model <- aov(df[[value_col]] ~ df[[group_col]])
    anova_table <- summary(aov_model)
    eta_sq <- if (effect_size) {
      ss_total <- sum(anova_table[[1]]$`Sum Sq`)
      ss_between <- anova_table[[1]]$`Sum Sq`[1]
      ss_between / ss_total
    } else NULL
    posthoc <- tryCatch(TukeyHSD(aov_model), error = function(e) NULL)

    return(list(
      type = "ANOVA",
      group_col = group_col,
      group_names = group_names,
      sample_sizes = sample_sizes,
      assumptions = list(shapiro = shapiro_list, levene_p = levene_p, bartlett_p = bartlett_p),
      anova = anova_table,
      eta_squared = eta_sq,
      interpretation = interpret_eta2(eta_sq),
      posthoc = posthoc,
      recommendation = "ANOVA + Tukey (Normalverteilung & Varianzhomogenitaet erfuellt)"
    ))

  } else {
    kw_result <- stats::kruskal.test(df[[value_col]] ~ df[[group_col]])
    dunn <- tryCatch({
      if (!requireNamespace("FSA", quietly = TRUE)) stop("FSA package fehlt")
      FSA::dunnTest(df[[value_col]] ~ df[[group_col]], method = "bonferroni")
    }, error = function(e) e$message)

    eta_approx <- if (effect_size) {
      n_total <- sum(sample_sizes)
      (kw_result$statistic - n_groups + 1) / (n_total - n_groups)
    } else NULL

    return(list(
      type = "Kruskal-Wallis",
      group_col = group_col,
      group_names = group_names,
      sample_sizes = sample_sizes,
      assumptions = list(shapiro = shapiro_list, levene_p = levene_p, bartlett_p = bartlett_p),
      kruskal = kw_result,
      eta_squared_approx = eta_approx,
      interpretation = interpret_eta2(eta_approx),
      dunn = dunn,
      recommendation = "Kruskal-Wallis + Dunn (Assumptions verletzt)"
    ))
  }
}

