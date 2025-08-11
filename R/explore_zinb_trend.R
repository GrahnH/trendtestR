#' Explore zero-inflated models (ZIP/ZINB) for count data trends /
#' Analyse von Zero-Inflated-Modellen (ZIP/ZINB) fuer Zeitreihen mit Zaehldaten
#'
#' Fits zero-inflated Poisson (ZIP) or negative binomial (ZINB) models to time series count data using splines. /
#' Passt Zero-Inflated Poisson- oder Negativ-Binomial-Modelle mit Splines an Zeitreihen mit Zaehldaten an.
#'
#' Supports automatic model selection based on AIC, optional Vuong test, flexible optimizer control, and visualization. /
#' Unterstuetzt automatische Modellauswahl basierend auf AIC, optionalen Vuong-Test, flexible Optimierungssteuerung und Visualisierung.
#'
#' @encoding UTF-8
#'
#' @param data Dataframe with time series count data. / Dataframe mit Zeitreihenzaehldaten.
#' @param datum_col Name of the time column (usually Date). / Name der Zeitspalte (normalerweise Date).
#' @param value_col Name of the count column (dependent variable). / Name der Zaehlspalte (abhaengige Variable).
#' @param group_col Optional. Name of grouping column for interaction. / Optional. Name der Gruppierungsspalte fuer Interaktion.
#' @param k_spline Basis dimension for spline terms (default = 4). / Basisdimension fuer Spline-Terme (Standard = 4).
#' @param family One of `"zip"`, `"zinb"`, or `"auto"`. If `"auto"`, selects model based on AIC. /
#' Einer von `"zip"`, `"zinb"` oder `"auto"`. Bei `"auto"` erfolgt die Auswahl basierend auf AIC.
#' @param run_vuong Logical. If TRUE, run Vuong test for model comparison (default = FALSE). /
#' Wenn TRUE, wird Vuong-Test fuer Modellvergleich durchgefuehrt (Standard = FALSE).
#' @param return_formula If TRUE, return model formula instead of fitting. / Wenn TRUE, wird nur die Modellformel zurueckgegeben.
#' @param verbose Logical. Whether to print model fitting messages. / Ob Meldungen zur Modellanpassung gedruckt werden.
#' @param control Optional. List for `pscl::zeroinfl.control` (e.g., `list(maxit = 200)`). Default: `maxit = 100`. /
#' Steuerparameter fuer `pscl::zeroinfl.control` (z.B. `list(maxit = 200)`). Standard: `maxit = 100`.
#'
#' @return A list containing model object and diagnostics. /
#' Eine Liste mit Modellobjekt und Diagnoseergebnissen:
#' \describe{
#'   \item{model}{The fitted `zeroinfl` model. / Das angepasste `zeroinfl`-Modell.}
#'   \item{summary}{Model summary (if available). / Modellzusammenfassung (wenn verfuegbar).}
#'   \item{plot}{Trend plot with fitted values (ggplot2). / Trendplot mit geschaetzten Werten (ggplot2).}
#'   \item{model_family_used}{Used model type: "ZIP" or "ZINB". / Verwendeter Modelltyp: "ZIP" oder "ZINB".}
#'   \item{model_selection_info}{Information about model selection logic. / Hinweise zur Modellauswahl.}
#'   \item{aic_comparison}{Data frame with AIC values for both models. / Data Frame mit AIC-Werten fuer beide Modelle.}
#'   \item{vuong_test}{Vuong test result (if computed). / Vuong-Testergebnis (falls berechnet).}
#'   \item{messages}{Messages from the fitting process. / Meldungen aus dem Anpassungsprozess.}
#' }
#'
#' @seealso [pscl::zeroinfl()], [pscl::vuong()], [explore_poisson_trend()]
#'
#' @examples
#' # Simulierte Zero-Inflated Zaehldaten
#' set.seed(123)
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = rbinom(100, 1, 0.3) * rpois(100, lambda = 4)
#' )
#'
#' # Automatische Auswahl zwischen ZIP und ZINB
#' explore_zinb_trend(df, datum_col = "datum", value_col = "value", family = "auto")
#'
#' # Nur ZIP-Modell erzwingen
#' explore_zinb_trend(df, datum_col = "datum", value_col = "value", family = "zip", k_spline = 3)
#'
#' @importFrom pscl zeroinfl vuong zeroinfl.control
#' @importFrom splines ns
#' @importFrom stats as.formula AIC na.omit predict fitted
#' @importFrom ggplot2 ggplot geom_point geom_line labs theme_minimal
#' @importFrom rlang sym !!! as_name
#' @export
explore_zinb_trend <- function(data, datum_col, value_col, group_col = NULL,
                               k_spline = 4, family = c("auto", "zip", "zinb"),
                               run_vuong = FALSE, return_formula = FALSE,
                               verbose = FALSE, control = NULL) {

  initial_family <- match.arg(family)

  if (is.null(control)) {
    control <- pscl::zeroinfl.control(maxit = 200)
  } else if (is.numeric(control) && length(control) == 1) {
    control <- pscl::zeroinfl.control(maxit = control)
  } else if (is.list(control)) {
    control <- do.call(pscl::zeroinfl.control, control)
  } else {
    stop("Das Argument 'control' muss entweder NULL, eine einzelne Zahl (z.B. 300), oder eine Liste (z.B. list(maxit = 300)) sein.")
  }

  .log_msg <- function(...) if (verbose) message("[explore_zinb_trend] ", ...)

  .fit_zi_robustly <- function(formula, data, dist_type) {
    model_res <- list(model = NULL, status = "error", message = NULL)

    tryCatch({
      fit <- pscl::zeroinfl(
        formula = formula,
        data = data,
        dist = dist_type,
        zero = ~ 1,
        control = control
      )
      model_res$model <- fit
      model_res$status <- "success"
      model_res$message <- paste0("Zeroinfl-Modellanpassung (", toupper(dist_type), ") erfolgreich.")
    }, warning = function(w) {
      model_res$message <- paste0("Warnung bei der Anpassung (", toupper(dist_type), "): ", w$message)
      model_res$status <- "warning"
      if (exists("fit", inherits = FALSE)) model_res$model <- fit
    }, error = function(e) {
      model_res$message <- paste0("Fehler bei der Anpassung (", toupper(dist_type), "): ", e$message)
      model_res$status <- "error"
    })
    return(model_res)
  }

  .make_plot <- function(model_data, model, datum_col, value_col, group_col, family_label, selection_info) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Das Paket 'ggplot2' ist nicht verfuegbar, Plot wird uebersprungen.")
      return(NULL)
    }

    fitted_values <- tryCatch({
      fitted(model)
    }, error = function(e) {
      try(predict(model, type = "response"), silent = TRUE)
    })

    if (is.null(fitted_values) || length(fitted_values) != nrow(model_data)) {
      warning("kann Anpassungswerte nicht berechnen. Plot wird uebersprungen.")
      return(NULL)
    }

    fitted_data <- model_data
    fitted_data$fitted <- fitted_values
    datum_col <- rlang::sym(datum_col)
    value_col <- rlang::sym(value_col)
    if (!is.null(group_col)) group_col_sym <- rlang::sym(group_col)

    p <- ggplot2::ggplot(fitted_data, ggplot2::aes(x = !!datum_col, y = !!value_col))

    if (!is.null(group_col)) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(color = !!group_col_sym), alpha = 0.4) +
        ggplot2::geom_line(ggplot2::aes(y = .data$fitted, color = !!group_col_sym))
    } else {
      p <- p +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_line(ggplot2::aes(y = .data$fitted), color = "steelblue")
    }

    p <- p +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste0(family_label, " Modellanpassung"),
        subtitle = selection_info,
        x = "Time",
        y = rlang::as_name(value_col)
      )

    return(p)
  }

  if (!is.data.frame(data)) stop("'data' muss ein data.frame sein.")
  if (!datum_col %in% names(data)) stop("Spalte '", datum_col, "' existiert nicht.")
  if (!inherits(data[[datum_col]], c("Date", "POSIXct")))
    stop(paste0("'", datum_col, "' muss vom Typ 'Date' oder 'POSIXct' sein."))
  if (!value_col %in% names(data)) stop("Spalte '", value_col, "' existiert nicht.")
  if (!is.null(group_col) && !(group_col %in% names(data))) {
    stop("Spalte '", group_col, "' existiert nicht.")
  }
  if (any(data[[value_col]] < 0, na.rm = TRUE))
    stop(paste0("'", value_col, "' darf keine negativen Werte enthalten."))
  if (!requireNamespace("pscl", quietly = TRUE)) {
    stop("Paket 'pscl' erforderlich. Installieren mit: install.packages('pscl')")
  }

  cols <- c(datum_col, value_col)
  if (!is.null(group_col)) cols <- c(cols, group_col)
  model_data <- stats::na.omit(data[, cols, drop = FALSE])
  model_data[[datum_col]] <- as.Date(model_data[[datum_col]])
  if (any(is.na(model_data[[datum_col]]))) stop("Ungueltige Datumsformatierung in '", datum_col, "'.")

  time_numeric <- as.numeric(model_data[[datum_col]])
  model_data$time_std <- (time_numeric - min(time_numeric)) / diff(range(time_numeric))

  if (!is.null(group_col)) {
    model_data[[group_col]] <- as.factor(model_data[[group_col]])
    fml_str <- paste0(value_col, " ~ splines::ns(time_std, ", k_spline, ") * ", group_col)
  } else {
    fml_str <- paste0(value_col, " ~ splines::ns(time_std, ", k_spline, ")")
  }
  fml <- stats::as.formula(fml_str)

  if (return_formula) {
    .log_msg("Formel zurueckgegeben (return_formula = TRUE).")
    return(fml)
  }

  zip_res <- list(model = NULL, status = "skipped", message = "Nicht angepasst")
  zinb_res <- list(model = NULL, status = "skipped", message = "Nicht angepasst")
  model <- NULL

  if (initial_family %in% c("zip", "zinb")) {
    .log_msg(paste("Nur", toupper(initial_family), "Modell wird angepasst (family ist nicht 'auto')."))

    if (initial_family == "zip") {
      zip_res <- .fit_zi_robustly(fml, model_data, "poisson")
      model <- zip_res$model
    } else {
      zinb_res <- .fit_zi_robustly(fml, model_data, "negbin")
      model <- zinb_res$model
    }

  } else {
    .log_msg("Fitting ZIP-Modell (Zero-Inflated Poisson) ...")
    zip_res <- .fit_zi_robustly(fml, model_data, "poisson")
    .log_msg(zip_res$message)

    .log_msg("Fitting ZINB-Modell (Zero-Inflated Negative Binomial) ...")
    zinb_res <- .fit_zi_robustly(fml, model_data, "negbin")
    .log_msg(zinb_res$message)
  }

  family_used <- initial_family
  aic_model_data <- NULL
  if (initial_family == "auto") {
    aic_zip <- if (zip_res$status %in% c("success", "warning") && !is.null(zip_res$model)) {
      tryCatch(AIC(zip_res$model), error = function(e) Inf)
    } else Inf

    aic_zinb <- if (zinb_res$status %in% c("success", "warning") && !is.null(zinb_res$model)) {
      tryCatch(AIC(zinb_res$model), error = function(e) Inf)
    } else Inf

    aic_model_data <- data.frame(Model = c("ZIP", "ZINB"), AIC = c(aic_zip, aic_zinb))
    aic_model_data$Delta_AIC <- aic_model_data$AIC - min(aic_model_data$AIC, na.rm = TRUE)

    family_used <- if (aic_zinb < aic_zip) "zinb" else "zip"
    model <- if (family_used == "zinb") zinb_res$model else zip_res$model
  }

  selection_info <- if (initial_family == "auto") {
    paste("Automatisch gewaehlt:", toupper(family_used), "(niedrigster AIC)")
  } else {
    paste("Benutzerdefiniert:", toupper(initial_family))
  }
  family_label <- paste("Zero-Inflated", toupper(family_used))

  vuong_result <- NULL
  fit_messages <- c(
    zip_res$message[zip_res$message != "Nicht angepasst"],
    zinb_res$message[zinb_res$message != "Nicht angepasst"]
  )
  fit_messages <- fit_messages[!is.na(fit_messages) & nzchar(fit_messages)]

  if (run_vuong) {
    if (initial_family == "auto") {
      if (!is.null(zip_res$model) && !is.null(zinb_res$model) &&
          zip_res$status %in% c("success", "warning") &&
          zinb_res$status %in% c("success", "warning")) {
        .log_msg("Fuehre Vuong-Test (ZIP vs ZINB) durch...")
        vuong_result <- pscl::vuong(zip_res$model, zinb_res$model)
      } else {
        fit_messages <- c(fit_messages,
                          "Hinweis: Vuong-Test uebersprungen, mindestens ein Modell konnte nicht angepasst werden.")
      }
    } else {
      fit_messages <- c(fit_messages,
                        paste0("Hinweis: Vuong-Test nicht moeglich, nur ein Modell (", toupper(initial_family), ") wurde angepasst."))
    }
  }

  any_successful <- !is.null(model)
  if (!any_successful) {
    hint <- paste0(
      "Hinweis: kein Modell konnte konvergieren. Probieren Sie: ",
      "(1) k=", k_spline, " anpassen; ",
      "(2) control=list(maxit=200); ",
      "(3) family='zip' manuell verwenden."
    )
    fit_messages <- c(fit_messages, hint)
  }

  plot_obj <- tryCatch({
    .make_plot(model_data = model_data, model = model,
               datum_col = datum_col, value_col = value_col,
               group_col = group_col, family_label = family_label,
               selection_info = selection_info)
  }, error = function(e) {
    warning("Plotfehler: ", e$message)
    NULL
  })

  return(list(
    model = model,
    summary = if (!is.null(model)) tryCatch(summary(model), error = function(e) NULL) else NULL,
    plot = plot_obj,
    model_family_used = family_label,
    model_selection_info = selection_info,
    aic_comparison = aic_model_data,
    vuong_test = vuong_result,
    messages = fit_messages
  ))
}
