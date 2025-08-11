#' Diagnose a fitted model using residual plots and statistical tests (ggplot2 only) /
#' Modell-Diagnose mittels Residuenplots und statistischen Tests (nur ggplot2)
#'
#' Diagnose model fit for lm, glm, gam (mgcv), and zeroinfl (pscl) models using residual plots and tests. /
#' Diagnose lineare Modelle (lm), generalisierte lineare Modelle (glm), GAMs von mgcv und Zero-Inflated-Modelle von pscl mit ggplot2.
#'
#' @encoding UTF-8
#'
#' @param model A fitted model object (lm, glm, gam, or zeroinfl). / Ein angepasstes Modellobjekt (lm, glm, gam oder zeroinfl).
#' @param value_col Name of the response variable (used in axis labels). / Name der Zielvariable (verwendet fuer Achsenbeschriftungen).
#' @param residual_type Type of residuals to use ("deviance", "pearson", "response", etc.). / Art der Residuen ("deviance", "pearson", "response" usw.).
#' @param group_col Optional. Grouping variable to color residual plots. / Optional. Gruppierungsvariable fuer Farbgebung in den Residuenplots.
#' @param verbose Logical; whether to print diagnostic messages. / Logisch; ob Diagnosenachrichten ausgegeben werden sollen.
#'
#' @return A list with ggplot2 plots and diagnostic test results. /
#' Eine Liste mit ggplot2-Plots und diagnostischen Testergebnissen:
#' \describe{
#'   \item{plots}{A named list with residual plots (`residuals_vs_fitted`, `qq`, `scale_location`). /
#'                Eine Liste mit Residuenplots (`residuals_vs_fitted`, `qq`, `scale_location`).}
#'   \item{diagnostics}{A named list of statistical test results (Shapiro, KS, Levene, GAM check). /
#'                      Eine Liste mit Ergebnissen statistischer Tests (Shapiro, KS, Levene, GAM check).}
#' }
#'
#' @seealso [explore_poisson_trend], [explore_continuous_trend], [explore_zinb_trend], [explore_trend_auto]
#'
#' @examples
#' # Example for a linear model
#' set.seed(123)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' model_lm <- lm(y ~ x, data = df)
#' diagnose_model_trend(model_lm)
#'
#' # Beispiel fuer ein GLM
#' df_glm <- data.frame(x = rnorm(100), y = rpois(100, lambda = 2))
#' model_glm <- glm(y ~ x, data = df_glm, family = poisson())
#' diagnose_model_trend(model_glm)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline stat_qq stat_qq_line labs theme_minimal
#' @importFrom stats residuals fitted complete.cases ks.test shapiro.test
#' @importFrom utils capture.output
#' @importFrom car leveneTest
#' @importFrom mgcv gam.check
#'
#' @export


diagnose_model_trend <- function(model,
                                 value_col = "value",
                                 residual_type = "deviance",
                                 group_col = NULL,
                                 verbose = TRUE) {

  is_zi <- inherits(model, "zeroinfl")
  is_gam <- inherits(model, "gam")

  if (is_zi && residual_type != "response") {
    if (verbose) message("Hinweis: Fuer 'zeroinfl'-Modelle wird residual_type = 'response' verwendet.")
    residual_type <- "response"
  }

  plots <- list()
  diagnostics <- list(residual_type = residual_type)

  if (is_gam && requireNamespace("mgcv", quietly = TRUE)) {
    if (verbose) message("GAM-Modell erkannt. Fuehre gam.check() aus.")
    diagnostics$gam_check <- utils::capture.output(mgcv::gam.check(model, rep = FALSE))
  }

  res <- tryCatch({
    stats::residuals(model, type = residual_type)
  }, error = function(e) {
    tryCatch({
      stats::residuals(model)
    }, error = function(e2) {
      if (verbose) warning("Auch Standard-Residuen konnten nicht berechnet werden: ", e2$message)
      return(NULL)
      })
  })
  if (is.null(res) || identical(res, FALSE) || !is.numeric(res) || length(res) < 2 || anyNA(res)) {
    if (verbose) warning("Residuen konnten nicht korrekt berechnet werden.")
    return(list(plots = NULL, diagnostics = NULL))
  }

  fitted_vals <- stats::fitted(model)
  complete_cases <- stats::complete.cases(res, fitted_vals)
  res <- res[complete_cases]
  fitted_vals <- fitted_vals[complete_cases]

  if (length(res) == 0) {
    warning("Keine vollstaendigen Faelle verfuegbar fuer Analyse.")
    return(list(plots = NULL, diagnostics = NULL))
  }

  res_df <- data.frame(
    fitted = fitted_vals,
    residuals = res
  )

  has_model_frame <- !is.null(model$model) && group_col %in% names(model$model)

  if (!is.null(group_col) && has_model_frame) {
    res_df$group <- model$model[[group_col]]
    if (!is.factor(res_df$group)) {
      res_df$group <- as.factor(res_df$group)
    }
  }

  aes_base <- ggplot2::aes(x = .data$fitted, y = .data$residuals)
  if (!is.null(group_col) && "group" %in% names(res_df)) {
    aes_base$colour <- quote(.data$group)
  }

  plots$residuals_vs_fitted <- ggplot2::ggplot(res_df, aes_base) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      title = "Residuen vs Angepasste Werte",
      x = paste0("Angepasste Werte von ", value_col),
      y = paste(residual_type, "-Residuen")
    ) +
    ggplot2::theme_minimal()

  plots$qq <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample = .data$res)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(
      title = "Normal Q-Q-Plot",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    ggplot2::theme_minimal()

  scale_df <- data.frame(
    fitted = fitted_vals,
    sqrt_abs_res = sqrt(abs(res))
  )
  if (!is.null(group_col) && "group" %in% names(res_df)) {
    scale_df$group <- res_df$group
    if (!is.factor(scale_df$group)) {
      scale_df$group <- as.factor(scale_df$group)
    }
  }

  aes_scale <- ggplot2::aes(x = .data$fitted, y = .data$sqrt_abs_res)
  if (!is.null(group_col) && "group" %in% names(scale_df)) {
    aes_scale$colour <- quote(.data$group)
  }

  plots$scale_location <- ggplot2::ggplot(scale_df, aes_scale) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggplot2::labs(
      title = "Scale-Location-Plot",
      x = paste0("Angepasste Werte von ", value_col),
      y = "Wurzel der Absolutwerte der Residuen"
    ) +
    ggplot2::theme_minimal()

  diagnostics$ks_test <- tryCatch({
    if (length(res) >= 3) stats::ks.test(res, "pnorm", mean(res, na.rm = TRUE), sd(res, na.rm = TRUE)) else NA
  }, error = function(e) {
    if (verbose) warning("KS-Test fehlgeschlagen: ", e$message)
    NA
  })

  diagnostics$shapiro_test <- tryCatch({
    if (length(res) >= 3 && length(res) <= 5000) stats::shapiro.test(res) else NA
  }, error = function(e) {
    if (verbose) warning("Shapiro-Wilk-Test fehlgeschlagen: ", e$message)
    NA
  })

  diagnostics$levene_test <- tryCatch({
    if (length(unique(fitted_vals)) < 2) NA
    num_breaks <- min(4, length(unique(fitted_vals)))
    fitted_groups <- cut(fitted_vals, breaks = num_breaks, include.lowest = TRUE)
    if (length(levels(fitted_groups)) > 1) {
      car::leveneTest(res, fitted_groups)
    } else {
      NA
    }
  }, error = function(e) {
    if (verbose) warning("Levene-Test fehlgeschlagen: ", e$message)
    NA
  })

  return(list(
    plots = plots,
    diagnostics = diagnostics
  ))
}
