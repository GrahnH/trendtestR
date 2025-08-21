#' Main dispatcher for trend analysis based on data type /
#' Hauptverzweiger fuer Trendanalyse basierend auf Datentyp
#'
#' Automatically selects and calls the appropriate trend analysis function depending on whether the data is count-based or continuous. /
#' Waehlt automatisch die passende Trendanalyse-Funktion basierend auf Zaehldaten oder stetigen Daten.
#'
#' @encoding UTF-8
#'
#' @param df Data frame with time series data. / Data Frame mit Zeitreihendaten.
#' @param datum_col Name of the date/time column. / Name der Datums- oder Zeitspalte.
#' @param value_col Name of the dependent variable column. / Name der abhaengigen Variablen.
#' @param group_col Optional. Name of the grouping column. / Optional. Name der Gruppierungsvariable.
#' @param family Model family to use: "auto", "poisson", "negbin", "zip", "zinb", "gaussian", etc. Passed to sub-functions. /
#' Modellfamilie: "auto", "poisson", "negbin", "zip", "zinb", "gaussian" usw. Wird an Unterfunktionen weitergegeben.
#' @param kdf Basis dimension for spline terms (k for GAM or ZI models). / Basisdimension fuer Splines (k bei GAM oder ZI-Modellen).
#' @param return_formula If TRUE, return only the model formula without fitting. / Bei TRUE wird nur die Modellformel zurueckgegeben.
#' @param verbose If TRUE, print detailed messages. / Bei TRUE werden Diagnosemeldungen ausgegeben.
#' @param control Optional control parameters for model fitting (e.g., maxit). / Optionale Steuerparameter fuer die Modellanpassung.
#'
#' @return Result from the appropriate trend analysis function. /
#' Rueckgabe des Ergebnisses der ausgewaehlten Trendanalysefunktion (z.B. explore_poisson_trend()).
#'
#' @seealso [explore_poisson_trend()], [explore_zinb_trend()], [explore_continuous_trend()], [infer_value_type()], [prepare_group_data()]
#'
#' @examples
#' # Simulated count data (Poisson)
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = rpois(100, lambda = 5)
#' )
#' explore_trend_auto(df, datum_col = "datum", value_col = "value")
#'
#' # Beispiel mit kontinuierlichen Werten
#' df2 <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = sin(1:100 / 10) + rnorm(100)
#' )
#' explore_trend_auto(df2, datum_col = "datum", value_col = "value")
#'
#' @importFrom stats na.omit
#' @export

explore_trend_auto <- function(df, datum_col, value_col, group_col = NULL,
                          family = "auto", kdf = 3,
                          return_formula = FALSE, verbose = FALSE, control = NULL) {

  required_cols <- c(datum_col, value_col)
  if (!is.null(group_col)) required_cols <- c(required_cols, group_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {stop("Fehler: Die folgenden Spalten wurden nicht gefunden: '", paste(missing_cols, collapse = "', '"), "'.")
  }
  if (!is.null(kdf) && (!is.numeric(kdf) || length(kdf) != 1 || kdf < 1)) {
    stop("Fehler: 'kdf' muss eine positive Zahl sein (Standard: 3).")
  }


  if (!is.null(group_col)) {
    prep <- tryCatch(
      prepare_group_data(df, value_col, group_col),
      error = function(e) {
        warning("Fehler in 'prepare_group_data()': ", e$message, ". Fortsetze mit rohen Daten.")
        df[[value_col]] <- as.numeric(df[[value_col]])
        list(df = stats::na.omit(df[required_cols]), n_groups = NA)
      }
    )
  }

  attrv <- attr(df, "value_data_type")
  attrf <- attr(df, "value_data_feature")

  if (is.null(attrv)) {
    if (verbose) message("Hinweis: Kein 'value_data_type'-Attribut gefunden. Fuehre infer_value_type() aus.")
    infer_result <- infer_value_type(df[[value_col]])
    attrv <- infer_result$type
    attrf <- infer_result$features
    }

  if (attrv %in% c("binary", "proportion")) {
    message(
      "Hinweis: Variablentyp '", attrv,
      "' erkannt. In trendtestR 1.0.0 werden die Daten weiter mit Standardtyp 'continuous' bearbeiten"
      )
  }
  result <- NULL
  fit_function <- NULL
  data_input <- if (!is.null(group_col)) prep$df else df

  if (attrv == "count") {
    if (!is.na(attrf) && attrf == "zi") {
      fit_function <- "explore_zinb_trend"
      if (verbose) message("Waehle explore_zinb_trend() fuer zero-inflated count data.")
      result <- tryCatch({
        explore_zinb_trend(
          data_input,
          datum_col = datum_col,
          value_col = value_col,
          group_col = group_col,
          family = family,
          k_spline = kdf,
          return_formula = return_formula,
          verbose = verbose,
          control = control
        )
      }, error = function(e) {
        stop("Fehler in explore_zinb_trend(): ", e$message)
      })

    } else {
      fit_function <- "explore_poisson_trend"
      if (verbose) message("Waehle explore_poisson_trend() fuer count data (nicht zero-inflated).")
      result <- tryCatch({
        explore_poisson_trend(
          data_input,
          datum_col = datum_col,
          value_col = value_col,
          group_col = group_col,
          family = family,
          k_spline = kdf,
          phi = 1.5,
          return_formula = return_formula,
          verbose = verbose
        )
      }, error = function(e) {
        stop("Fehler in explore_poisson_trend(): ", e$message)
      })
    }

  } else if (attrv != "count") {
    fit_function <- "explore_continuous_trend"
    if (verbose) message("Waehle explore_continuous_trend() fuer stetige Daten.")
    result <- tryCatch({
      explore_continuous_trend(
        data_input,
        datum_col = datum_col,
        value_col = value_col,
        group_col = group_col,
        df_spline = kdf,
        family = family,
        return_formula = return_formula,
        verbose = verbose
      )
    }, error = function(e) {
      stop("Fehler in explore_continuous_trend(): ", e$message)
    })

  } else {
    stop("Unbekannter value_data_type: '", attrv, "'. Unterstuetzt: 'count', 'continuous'.")
  }

  if (is.null(result)) {
    stop("Fehler: Kein gueltiges Ergebnis von ", fit_function, " zurueckgegeben.")
  }

  if (is.list(result) && !return_formula) {
    result$dispatched_by <- "explore_trend"
    result$data_type <- attrv
    result$data_features <- attrf
    result$used_function <- fit_function
  }


  return(result)
}
