#' Explore linear and GLM trends for continuous data with automatic model selection /
#' Analyse linearer und GLM-Trends fuer kontinuierliche Daten mit automatischer Modellauswahl
#'
#' Fits linear models or GLMs (Gaussian or Gamma) to continuous time series data, optionally using natural cubic splines. /
#' Passt lineare Modelle oder GLMs (Gaussian oder Gamma) an kontinuierliche Zeitreihendaten an, optional mit natuerlichen kubischen Splines.
#'
#' @encoding UTF-8
#'
#' @param data Dataframe with time series continuous data. / Dataframe mit Zeitreihen-kontinuierlichen Daten.
#' @param datum_col Name of the time column (usually Date). / Name der Zeitspalte (normalerweise Date).
#' @param value_col Name of the continuous value column (dependent variable). / Name der Spalte mit kontinuierlichen Werten (abhaengige Variable).
#' @param group_col Optional. Name of grouping column for interaction. / Optional. Name der Gruppierungsspalte fuer Interaktion.
#' @param df_spline Degrees of freedom for spline (default = 2). Set to 1 for linear trend. / Freiheitsgrade fuer den Spline (Standard = 2). Setze auf 1 fuer linearen Trend.
#' @param family Specifies the GLM family: "auto" (default), "gaussian", or "gamma". / Gibt die GLM-Familie an: "auto" (Standard), "gaussian" oder "gamma".
#' @param return_formula If TRUE, returns the model formula instead of fitting. / Wenn TRUE, wird nur die Modellformel zurueckgegeben.
#' @param verbose Logical. Whether to print model fitting messages. / Ob Anpassungsmeldungen ausgegeben werden sollen.
#'
#' @return A list containing fitted model, formula, summary, plot, and model diagnostics. /
#' Eine Liste mit Modell, Formel, Zusammenfassung, Plot und Diagnosen:
#' \describe{
#'   \item{model}{The fitted GLM object. / Das angepasste GLM-Objekt.}
#'   \item{summary}{Model summary. / Zusammenfassung des Modells.}
#'   \item{plot}{ggplot2 visualization of the trend. / ggplot2-Visualisierung des Trends.}
#'   \item{dispersion_parameter}{Estimated dispersion. / Geschaetzter Dispersionsparameter.}
#'   \item{model_family_used}{Family used for final model. / Verwendete Modellfamilie.}
#'   \item{model_selection_info}{Information about family selection (if auto). / Hinweise zur Modellauswahl (bei auto).}
#'   \item{aic_comparison}{Optional AIC comparison table (if auto and gamma used). / Optionale AIC-Vergleichstabelle (bei auto mit Gamma).}
#'   \item{messages}{Concatenated messages from model fitting. / Konsolidierte Anpassungsmeldungen.}
#' }
#'
#' @seealso [explore_trend_auto()]
#'
#' @examples
#' # Basic Gaussian GLM on continuous data
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = 5 + sin(1:100 / 10) + rnorm(100)
#' )
#' explore_continuous_trend(df, datum_col = "datum", value_col = "value", df_spline = 1)
#'
#' # Automatische Auswahl zwischen Gaussian und Gamma
#' df2 <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = exp(seq(-1, 1, length.out = 100)) + rnorm(100, sd = 0.2)
#' )
#' explore_continuous_trend(df2,
#'                          datum_col = "datum",
#'                          value_col = "value",
#'                          df_spline = 2, family = "auto")
#'
#' @importFrom stats as.formula na.omit AIC predict glm gaussian Gamma
#' @importFrom ggplot2 ggplot geom_point geom_line theme_minimal labs
#' @importFrom splines ns
#' @importFrom rlang sym !!! as_name
#' @export

explore_continuous_trend <- function(data, datum_col, value_col, group_col = NULL,
                                     df_spline = 2,
                                     family = c("auto", "gaussian", "gamma"),
                                     return_formula = FALSE,
                                     verbose = FALSE) {
  .log_msg <- function(...) if (verbose) message(...)

  .fit_glm_robustly <- function(formula, data, glm_family) {
    model_res <- list(model = NULL, status = "error", message = NULL)
    family_name <- deparse(glm_family[[1]])
    if (family_name == "structure") {
      family_name <- "Gamma"
    } else if (family_name == "gaussian") {
      family_name <- "Gaussian"
    } else {
      family_name <- as.character(glm_family[[1]])
    }
    tryCatch({
      model_res$model <- stats::glm(formula, data = data, family = glm_family)
      model_res$status <- "success"
      model_res$message <- paste0("GLM mit ", family_name, " Familie angepasst.")
    }, warning = function(w) {
      model_res$message <- paste0("Warnung bei der Anpassung des GLM (", family_name, "): ", w$message)
      model_res$status <- "warning"
    }, error = function(e) {
      model_res$message <- paste0("Fehler bei der Anpassung des GLM (", family_name, "): ", e$message)
      model_res$status <- "error"
      warning(paste0("Detaillierter GLM-Anpassungsfehler (", family_name, "): ", e$message), call. = FALSE)
    })
    return(model_res)
  }

  .make_plot <- function(data, model_data, model, datum_col, value_col, group_col, family_label, selection_info) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Das Paket 'ggplot2' ist nicht verfuegbar, Plot wird uebersprungen.")
      return(NULL)
    }

    fitted_data <- model_data
    fitted_data$fitted <- predict(model, newdata = fitted_data, type = "response")

    datum_col <- rlang::sym(datum_col)
    value_col <- rlang::sym(value_col)
    if (!is.null(group_col)) group_col_sym <- rlang::sym(group_col)
    p <- ggplot2::ggplot(fitted_data, ggplot2::aes(x = !!datum_col, y = !!value_col))
    if (!is.null(group_col)) {
      p <- p +
        ggplot2::geom_point(aes(color = !!group_col_sym), alpha = 0.4) +
        ggplot2::geom_line(aes(y = .data$fitted, color = !!group_col_sym))
    } else {
      p <- p +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_line(aes(y = .data$fitted), color = "steelblue")
    }
    p <- p +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste0(family_label, " GLM-Anpassung"),
        subtitle = selection_info,
        x = "Time",
        y = rlang::as_name(value_col)
      )
    return(p)
  }

  if (!is.data.frame(data)) stop("'data' muss ein data Frame sein.")
  if (!is.character(datum_col) || !(datum_col %in% names(data))) stop("'datum_col' muss ein gueltiger Spaltenname im data Frame sein.")
  if (!is.character(value_col) || !(value_col %in% names(data))) stop("'value_col' muss ein gueltiger Spaltenname im data Frame sein.")
  if (!inherits(data[[datum_col]], c("Date", "POSIXct"))) stop(paste0("'", datum_col, "' muss vom Typ 'Date' oder 'POSIXct' sein."))
  if (!is.numeric(data[[value_col]])) stop(paste0("'", value_col, "' muss numerisch sein."))
  if (any(data[[value_col]] < 0, na.rm = TRUE) && match.arg(family) == "gamma") {
    stop(paste0("'", value_col, "' darf fuer die Gamma-Familie keine negativen Werte enthalten."))
  }
  if (df_spline < 1) stop("'df_spline' muss mindestens 1 sein.")
  if (!requireNamespace("splines", quietly = TRUE) && df_spline > 1) {
    stop("Das Paket 'splines' wird fuer Spline-Anpassungen benoetigt (df_spline > 1).")
  }

  family <- match.arg(family)

  cols_to_select <- c(datum_col, value_col)
  if (!is.null(group_col)) {
    if (!(group_col %in% names(data))) stop(paste0("'", group_col, "' ist keine gueltige Spalte im data Frame."))
    cols_to_select <- c(cols_to_select, group_col)
  }

  model_data <- stats::na.omit(data[, cols_to_select])

  if (!is.null(group_col)) {
    if (length(unique(model_data[[group_col]])) < 2) {
      warning(paste0("'", group_col, "' enthaelt weniger als zwei Gruppen nach Datenbereinigung, Interaktion wird ignoriert."), call. = FALSE)
      group_col <- NULL
      cols_to_select <- c(datum_col, value_col)
      model_data <- stats::na.omit(data[, cols_to_select])
    } else {
      model_data[[group_col]] <- as.factor(model_data[[group_col]])
    }
  }

  time_numeric_col_name <- paste0(datum_col, "_numeric")
  model_data[[time_numeric_col_name]] <- as.numeric(model_data[[datum_col]])

  time_term <- if (df_spline > 1) {
    paste0("splines::ns(", time_numeric_col_name, ", df = ", df_spline, ")")
  } else {
    time_numeric_col_name
  }

  if (!is.null(group_col)) {
    fml_str <- paste0(value_col, " ~ ", time_term, " * ", group_col)
  } else {
    fml_str <- paste0(value_col, " ~ ", time_term)
  }
  fml <- stats::as.formula(fml_str)
  if (return_formula) return(fml)

  model <- NULL; family_used <- ""; selection_info <- ""; aic_comparison <- NULL; fit_messages <- character(0)


  if (family == "auto") {
    .log_msg("Versuche Anpassung mit Gaussian GLM (Standard LM) ...")
    gaussian_res <- .fit_glm_robustly(fml, model_data, stats::gaussian())
    fit_messages <- c(fit_messages, gaussian_res$message)

    if (gaussian_res$status != "error" && !is.null(gaussian_res$model)) {
      model <- gaussian_res$model
      gaussian_aic <- stats::AIC(model)
      family_used <- "Gaussian GLM (automatisch)"
      selection_info <- "Automatisch: Gaussian GLM gewaehlt."

      if (all(model_data[[value_col]] > 0, na.rm = TRUE)) {
        .log_msg("Daten sind alle positiv, versuche Gamma GLM zum AIC-Vergleich ...")
        gamma_res <- .fit_glm_robustly(fml, model_data, stats::Gamma(link = "log")) # Log link for Gamma is common
        fit_messages <- c(fit_messages, gamma_res$message)

        if (gamma_res$status != "error" && !is.null(gamma_res$model)) {
          gamma_aic <- stats::AIC(gamma_res$model)
          aic_comparison <- data.frame(Model = c("Gaussian GLM", "Gamma GLM"), AIC = c(gaussian_aic, gamma_aic), Delta_AIC = c(gaussian_aic, gamma_aic) - min(gaussian_aic, gamma_aic))

          if (gamma_aic < gaussian_aic) {
            model <- gamma_res$model
            family_used <- "Gamma GLM (automatisch)"
            selection_info <- paste0("Automatisch: Gamma GLM gewaehlt (niedrigerer AIC: ", round(gamma_aic, 2), " vs ", round(gaussian_aic, 2), ").")
            .log_msg("Gamma GLM gewaehlt (niedrigerer AIC).")
          } else {
            family_used <- "Gaussian GLM (automatisch)"
            selection_info <- paste0("Automatisch: Gaussian GLM beibehalten (Gamma-AIC war hoeher: ", round(gamma_aic, 2), " vs ", round(gaussian_aic, 2), ").")
            .log_msg("Gaussian GLM beibehalten (Gamma-AIC war hoeher).")
          }
        } else {
          .log_msg(paste0("Gamma GLM-Anpassung fuer AIC-Vergleich fehlgeschlagen: ", gamma_res$message, ", beibehalte Gaussian GLM."))
        }
      } else {
        .log_msg("Daten enthalten nicht-positive Werte, Gamma GLM nicht moeglich fuer AIC-Vergleich.")
      }
    } else {
      stop(paste0("Gaussian GLM-Anpassung fehlgeschlagen: ", gaussian_res$message, ". Keine Alternative moeglich."))
    }
  } else { # User explicitly chose a family
    glm_family_obj <- if (family == "gaussian") stats::gaussian() else stats::Gamma(link = "log")
    .log_msg(paste0("Passe ", ifelse(family == "gaussian", "Gaussian", "Gamma"), " GLM an ..."))

    if (family == "gamma" && any(model_data[[value_col]] <= 0, na.rm = TRUE)) {
      stop("Kann keine Gamma GLM anpassen: 'value_col' enthaelt Werte <= 0.")
    }

    res <- .fit_glm_robustly(fml, model_data, glm_family_obj)
    fit_messages <- c(fit_messages, res$message)
    if (res$status == "error") stop(paste0("Anpassung des ", family, "-GLMs fehlgeschlagen: ", res$message))

    model <- res$model
    family_used <- paste0(ifelse(family == "gaussian", "Gaussian", "Gamma"), " GLM (benutzerdefiniert)")
    selection_info <- paste0("Benutzerdefiniertes ", family_used, ".")
  }

  if (is.null(model)) stop("GLM-Anpassung fehlgeschlagen, kein gueltiges Modell verfuegbar.")

  dispersion_param <- NA
  if (family_used == "Gaussian GLM (automatisch)" || family_used == "Gaussian GLM (benutzerdefiniert)") {
    dispersion_param <- summary(model)$dispersion
  } else if (family_used == "Gamma GLM (automatisch)" || family_used == "Gamma GLM (benutzerdefiniert)") {
    dispersion_param <- summary(model)$dispersion
  }

  return(list(
    model = model,
    summary = summary(model),
    plot = .make_plot(data, model_data, model, datum_col, value_col, group_col, family_used, selection_info),
    dispersion_parameter = dispersion_param,
    model_family_used = family_used,
    model_selection_info = selection_info,
    aic_comparison = aic_comparison,
    messages = paste(unique(fit_messages), collapse = "; ")
  ))
}
