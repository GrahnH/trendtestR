#' Assess Time Series Readiness for ARIMA Modeling / Pruefung der Eignung fuer ARIMA-Zeitreihenmodellierung
#'
#' This function performs diagnostics on a numeric time series (e.g., rate difference) to evaluate whether ARIMA modeling is appropriate.
#' It runs tests for autocorrelation (Ljung-Box), trend presence, and stationarity (ADF & KPSS), and gives modeling recommendations.
#' Optional visualizations include line plot, ACF/PACF, and STL decomposition.
#'
#' Diese Funktion prueft, ob eine Zeitreihe (z.B. Differenz von Raten) fuer ARIMA-Modelle geeignet ist.
#' Sie fuehrt Autokorrelationspruefung (Ljung-Box), Trendtest, sowie Stationaritaetstests (ADF & KPSS) durch und gibt Modellierungsempfehlungen.
#' Optional werden Zeitreihengrafiken wie Linienplot, ACF/PACF und STL-Dekomposition erstellt.
#'
#' @encoding UTF-8
#'
#' @param rate_diff_vec Numeric vector of rate differences. / Numerischer Vektor (z.B. Rate)
#' @param date_vec Optional. Corresponding date vector (used for plotting). / Optionaler Datumsvektor
#' @param frequency Time series frequency (e.g., 52 for weekly). Default is 52. / Frequenz der Zeitreihe
#' @param plot_acf Logical. Whether to compute and plot ACF/PACF. Default is TRUE. / ACF/PACF anzeigen?
#' @param do_stl Logical. Whether to perform and plot STL decomposition. Default is TRUE. / STL-Dekomposition durchfuehren?
#' @param max_lag_acf Max lag to use for ACF plots. Default is min(3 * frequency, floor(length(rate_diff_vec) / 4)). / Max. Verzoegerung fuer ACF
#'
#' @return A list containing:
#' \describe{
#'    \item{ts_data}{The cleaned numeric time series}
#'    \item{assessment}{Overall diagnostic and modeling recommendation}
#'    \item{adf}{ADF test result (stationarity)}
#'    \item{kpss}{KPSS test result (stationarity)}
#'    \item{plots}{Optional ggplot objects (e.g., time series plot)}
#'    \item{stationarity_assessment}{Summary of stationarity status and differencing recommendation}
#' }
#' @examples
#' vec <- c(NA, rnorm(60, 0.1, 1))
#' check_rate_diff_arima_ready(vec, frequency = 12)
#'
#' @importFrom tseries adf.test kpss.test
#' @importFrom forecast ndiffs nsdiffs Acf Pacf
#' @importFrom stats ts Box.test cor.test sd stl var
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal geom_hline geom_segment geom_point
#'
#' @export

check_rate_diff_arima_ready <- function(
    rate_diff_vec,
    date_vec = NULL,
    frequency = 52,
    plot_acf = TRUE,
    do_stl = TRUE,
    max_lag_acf = min(3 * frequency, floor(length(rate_diff_vec) / 4))
) {
  .iqr_outlier_check <- function(x) {
    qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qnt[2] - qnt[1]
    lower <- qnt[1] - 1.5 * iqr
    upper <- qnt[2] + 1.5 * iqr
    which(x < lower | x > upper)
  }

  .ts_plot <- function(data_vec, date_vec = NULL, var_name = "Variable", date_range = NULL) {
    if (!is.null(date_vec)) {
      df <- data.frame(date = date_vec, value = data_vec)
      xlab <- "Datum"
    } else {
      df <- data.frame(date = seq_along(data_vec), value = data_vec)
      xlab <- "Index"
    }
    xvar <- ggplot2::aes(x = .data[["date"]], y = .data[["value"]])
    subtitle_text <- if (!is.null(date_range)) {
      sprintf("Zeitraum: %s bis %s", format(date_range[1]), format(date_range[2]))
    } else {
      NULL
    }

    ggplot2::ggplot(df, xvar) +
      ggplot2::geom_line(color = "steelblue", alpha = 0.8) +
      ggplot2::geom_hline(yintercept = mean(data_vec), color = "red", linetype = "dashed", alpha = 0.7) +
      ggplot2::labs(
        title = sprintf("Zeitreihe der %s", var_name),
        subtitle = subtitle_text,
        x = xlab,
        y = "Wert"
      ) +
      ggplot2::theme_minimal()
  }


  .corr_plot <- function(lag_values, corr_values, title, color = "steelblue", ts_length, date_range = NULL) {
    ci <- qnorm(0.975) / sqrt(ts_length)
    df <- data.frame(lag = lag_values, correlation = corr_values)

    full_title <- switch(
      title,
      "ACF" = "Autokorrelationsfunktion (ACF)",
      "PACF" = "Partielle Autokorrelationsfunktion (PACF)",
      title
    )

    subtitle_text <- if (!is.null(date_range)) {
      sprintf("Zeitraum: %s bis %s", format(date_range[1]), format(date_range[2]))
    } else {
      sprintf("Konfidenzintervall: pm%.2f", ci)
    }

    ggplot2::ggplot(df, ggplot2::aes(x = .data[["lag"]], y = .data[["correlation"]])) +
      ggplot2::geom_hline(yintercept = c(0, ci, -ci),
                          color = c("black", "blue", "blue"),
                          linetype = c("solid", "dashed", "dashed")) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data[["lag"]], yend = 0), color = color) +
      ggplot2::geom_point(color = color, size = 1.5) +
      ggplot2::labs(
        title = full_title,
        subtitle = subtitle_text,
        x = "Lag",
        y = "Korrelationswert"
      ) +
      ggplot2::theme_minimal(base_size = 13)
  }

  .acf_plot <- function(ts_data, max_lag, date_range = NULL) {
    acf_result <- forecast::Acf(ts_data, lag.max = max_lag, plot = FALSE)
    .corr_plot(0:max_lag, as.numeric(acf_result$acf), "ACF", "steelblue", length(ts_data), date_range)
  }

  .pacf_plot <- function(ts_data, max_lag, date_range = NULL) {
    pacf_result <- forecast::Pacf(ts_data, lag.max = max_lag, plot = FALSE)
    .corr_plot(1:max_lag, as.numeric(pacf_result$acf), "PACF", "darkorange", length(ts_data), date_range)
  }

  if (!is.numeric(rate_diff_vec) || any(!is.finite(rate_diff_vec[!is.na(rate_diff_vec)]))) {
    stop("rate_diff_vec muss numerisch sein und darf keine Inf/-Inf-Werte enthalten.")
  }
  valid_idx <- which(!is.na(rate_diff_vec) & is.finite(rate_diff_vec))
  if (length(valid_idx) < 10) {
    stop("Nicht genuegend gueltige Datenpunkte (mindestens 10 erforderlich).")
  }

  rate_diff_vec_clean <- rate_diff_vec[valid_idx]
  date_vec_clean <- if (!is.null(date_vec)) as.Date(date_vec[valid_idx]) else NULL
  ts_data <- stats::ts(rate_diff_vec_clean, frequency = frequency)

  plots <- list(plot_timeseries = NULL, plot_acf = NULL, plot_pacf = NULL, plot_stl = NULL)

  var_name <- deparse(substitute(rate_diff_vec))
  date_range <- if (!is.null(date_vec_clean)) range(date_vec_clean, na.rm = TRUE) else NULL

  n <- length(ts_data)
  mean_val <- mean(ts_data, na.rm = TRUE)
  sd_val <- sd(ts_data, na.rm = TRUE)
  cv <- if (is.finite(mean_val) && abs(mean_val) > 1e-6) sd_val / abs(mean_val) else NA
  ljung_test <- stats::Box.test(ts_data, lag = min(20, max(1, floor(n / 4))), type = "Ljung-Box")
  has_trend <- if (n > 1) {
    cor_result <- tryCatch(stats::cor.test(1:n, ts_data), error = function(e) NULL)
    !is.null(cor_result) && cor_result$p.value < 0.05
  } else FALSE
  has_variation <- is.finite(cv) && cv > 0.01
  has_structure <- ljung_test$p.value < 0.1
  recommendation <- n >= 12 && has_variation && (has_structure || has_trend)
  recommendation_text <- if (recommendation) {
    "ARIMA-Modellierung wird empfohlen."
  } else {
    "ARIMA-Modellierung wird nicht empfohlen, da die Serie keine ausreichende Struktur fuer eine Modellierung aufweist."
  }

  assessment <- list(
    n = n,
    mean = mean_val,
    sd = sd_val,
    ljung_pvalue = ljung_test$p.value,
    has_trend = has_trend,
    has_structure = has_structure,
    recommendation = recommendation,
    recommendation_text = recommendation_text
  )

  cat("=== Zeitreihen-Analysebericht ===\n")
  cat("Laenge:", length(ts_data), " | Wertebereich:", round(range(ts_data), 5), "\n")
  if (!is.null(date_vec_clean)) cat("Datum:", format(min(date_vec_clean)), "bis", format(max(date_vec_clean)), "\n")
  cat("Frequenz:", frequency, "\n\n")

  if (!is.null(date_vec_clean)) {
    iqr_outlier_idx <- .iqr_outlier_check(rate_diff_vec_clean)
    iqr_outlier_dates <- if (!is.null(date_vec_clean)) date_vec_clean[iqr_outlier_idx] else iqr_outlier_idx
    n_outliers <- length(iqr_outlier_idx)

    if (n_outliers > 0) {
      cat("=== IQR-Ausreisserpruefung ===\n")
      cat("Anzahl potenzieller Ausreisser:", length(iqr_outlier_idx), "\n")
      cat("Z.B. an:", format(date_vec_clean[iqr_outlier_idx[1:min(3, length(iqr_outlier_idx))]]), "\n")

      plots$plot_timeseries <- .ts_plot(rate_diff_vec_clean, date_vec_clean, var_name = var_name, date_range = date_range) +
        ggplot2::geom_point(data = data.frame(date = date_vec_clean[iqr_outlier_idx],
                                              value = rate_diff_vec_clean[iqr_outlier_idx]),
                            ggplot2::aes(x = date, y = value),
                            color = "red", size = 2.2, shape = 1, stroke = 1.2)  +
        ggplot2::labs(title = paste0("Zeitreihe mit IQR-Ausreissern (", var_name, ")"))
    } else {
      plots$plot_timeseries <- .ts_plot(rate_diff_vec_clean, date_vec_clean, var_name = var_name, date_range = date_range)
    }
    print(plots$plot_timeseries)
  }

  cat("=== Bewertung der Modellierungsnotwendigkeit ===\n")
  cat("Laenge:", assessment$n, " | Ljung-Box p:", round(assessment$ljung_pvalue, 4), "\n")
  cat(assessment$recommendation_text, "\n\n")

  if (!assessment$recommendation) {
    cat("Analyse wurde fruehzeitig beendet. Basisinformationen sind im Rueckgabeobjekt enthalten.\n\n")
    empty_result <- list(
      ts_data = ts_data,
      assessment = assessment,
      adf = NULL,
      kpss = NULL,
      stl = NULL,
      plots = plots,
      stationarity_assessment = list(
        is_likely_stationary = NA,
        adf_p_value = NA,
        kpss_p_value = NA,
        ndiffs_suggested = NA,
        nsdiffs_suggested = NA
      )
    )
    return(invisible(empty_result))
  }

  cat("=== Stationaritaetstests ===\n")
  adf_result <- tseries::adf.test(ts_data)
  kpss_result <- tseries::kpss.test(ts_data, null = "Level")
  cat(sprintf("ADF: p = %.4f | KPSS: p = %.4f\n", adf_result$p.value, kpss_result$p.value))
  nd <- tryCatch(forecast::ndiffs(ts_data), error = function(e) NA)
  nsd <- tryCatch(forecast::nsdiffs(ts_data), error = function(e) NA)
  if (!is.na(nd)) {
    cat(sprintf("Empfohlene nicht-saisonale Differenzierung (d): %d\n", nd))
    if (nd == 0) {
      cat("    (Die Serie scheint ohne Differenzierung stationaer zu sein.)\n")
    } else if (nd == 1) {
      cat("Eine einfache Differenzierung (d = 1) wird empfohlen.\n")
    } else {
      cat(sprintf("Achtung: Mehrfache Differenzierung (d = %d) erforderlich: bitte Stabilitaet pruefen.\n", nd))
    }
  }
  if (!is.na(nsd)) {
    cat(sprintf("Empfohlene saisonale Differenzierung (D): %d (bei Frequenz = %d)\n", nsd, frequency))
    if (nsd >= 1) {
      cat("Saisonale Struktur erkannt: saisonale Differenzierung (z.B. diff(x, lag = frequency)) wird empfohlen.\n")
    }
  }
  cat("\n")

  is_likely_stationary <- adf_result$p.value < 0.05 && kpss_result$p.value > 0.05

  if (plot_acf) {
    max_lag <- min(max_lag_acf, floor(length(ts_data) / 4), 40)
    plots$plot_acf <- tryCatch(.acf_plot(ts_data, max_lag, date_range = date_range),
                               error = function(e) { cat("ACF-Plot fehlgeschlagen\n"); NULL })
    if (!is.null(plots$plot_acf)) print(plots$plot_acf)

    plots$plot_pacf <- tryCatch(.pacf_plot(ts_data, max_lag, date_range = date_range),
                                error = function(e) { cat("PACF-Plot fehlgeschlagen\n"); NULL })
    if (!is.null(plots$plot_pacf)) print(plots$plot_pacf)
  }

  stl_result <- NULL
  if (do_stl && frequency > 1 && length(ts_data) >= 2 * frequency) {
    cat("=== STL-Zerlegung ===\n")
    stl_result <- tryCatch({
      stl_obj <- stats::stl(ts_data, s.window = "periodic")
      plot(stl_obj)
      seasonal_strength <- 1 - var(stl_obj$time.series[, "remainder"], na.rm = TRUE) /
        var(stl_obj$time.series[, "remainder"] + stl_obj$time.series[, "seasonal"], na.rm = TRUE)
      cat("Saisonale Staerke:", round(max(0, seasonal_strength), 3), "\n")
      stl_obj
    }, error = function(e) {
      cat("STL-Zerlegung fehlgeschlagen:", e$message, "\n")
      NULL
    })
  }

  cat("=== ARIMA-Empfehlung ===\n")
  if (is_likely_stationary) {
    cat("Stationaer. ARIMA(p,0,q) moeglich.\n")
  } else {
    cat("Nicht stationaer. Differenzierung empfohlen.\n")
  }

  invisible(list(
    ts_data = ts_data,
    assessment = assessment,
    adf = adf_result,
    kpss = kpss_result,
    stl = stl_result,
    plots = plots,
    stationarity_assessment = list(
      is_likely_stationary = is_likely_stationary,
      adf_p_value = adf_result$p.value,
      kpss_p_value = kpss_result$p.value,
      ndiffs_suggested = nd,
      nsdiffs_suggested = nsd
    )
  ))
}
