#' Standardisierung von Datum und Werten / Standardize date and value columns
#'
#' Diese Funktion konvertiert eine Datumsspalte in das `Date`-Format und stellt sicher, dass die
#' Wertespalte numerisch ist. Falls eine `"monat"`-Spalte vorhanden ist, wird sie als geordneter
#' Faktor umkodiert. Nuetzlich fuer die Vorverarbeitung von Zeitreihendaten (z.B. Fallzahlen).
#'
#' This function converts a date column to `Date` format and ensures the value column is numeric.
#' If a `"monat"` column exists, it will be converted to an ordered factor. Useful for preprocessing
#' time series data (e.g., daily cases).
#'
#' @encoding UTF-8
#'
#' @param df Ein Data Frame / A data.frame
#' @param datum_col Spaltenname des Datums, default value is NULL / Name of the date column, default is `NULL`
#' @param value_col Spaltenname der Werte, Standard: `"neue_faelle"` / Name of the value column, default is `"neue_faelle"`
#' @param verbose Ob Statusinformationen ausgegeben werden sollen / Whether to print standardization info
#'
#' @return Ein aufbereiteter Data Frame mit `.value`-Spalte, konvertiertem Datum und ggf. geordnetem `monat`-Faktor.<br>
#' A cleaned data.frame with a `.value` column, standardized `Date` column, and possibly ordered `monat` factor.<br>
#'
#' @seealso [infer_value_type()]
#' @examples
#' df <- data.frame(
#'   datum = c("2021-01-01", "2021-01-02"),
#'   neue_faelle = c("12", "15"),
#'   monat = c("Jan", "Jan")
#' )
#' df_clean <- standardize_case_columns(df, datum_col = "datum", value_col = "neue_faelle" )
#' head(df_clean)
#'
#' @importFrom lubridate month parse_date_time
#'
#' @export

standardize_case_columns <- function(df,
                                     datum_col = NULL,
                                     value_col,
                                     verbose = TRUE) {

  if (!value_col %in% names(df)) stop(paste0("Spalte '", value_col, "' fehlt im DataFrame."))

  if (!is.null(datum_col)){
    if (!datum_col %in% names(df)) stop(paste0("Spalte '", datum_col, "' fehlt im DataFrame."))
    if (!inherits(df[[datum_col]], "Date") && !inherits(df[[datum_col]], "POSIXct")) {
    if (verbose) {
      message(paste0("Versuche, Spalte '", datum_col, "' in ein Datumsformat zu konvertieren."))
    }
    converted_date <- suppressWarnings(
      lubridate::parse_date_time(df[[datum_col]], orders = c("dmy", "ymd", "mdy", "dym"))
    )

    if (all(is.na(converted_date)) && !all(is.na(df[[datum_col]]))) {
      stop(paste0("Fehler: Spalte '", datum_col, "' konnte nicht erfolgreich in ein Datumsformat konvertiert werden (keine der ueblichen Formate erkannt)."))
    }
    if (sum(is.na(converted_date)) > 0 && verbose) {
      warning(paste0("Warnung: ", sum(is.na(converted_date)), " Werte in der Spalte '", datum_col, "' konnten nicht in ein Datum konvertiert werden und wurden zu NA."))
    }

    df[[datum_col]] <- as.Date(converted_date)

  } else {
    if (verbose) {
      message(paste0("Spalte '", datum_col, "' ist bereits ein Datums- oder POSIXct-Objekt. Keine Konvertierung noetig."))
    }
    df[[datum_col]] <- as.Date(df[[datum_col]])
  }}

  if (is.character(df[[value_col]])) {
    df[[value_col]] <- as.numeric(gsub(",", ".", df[[value_col]]))
  }

  df$.value <- df[[value_col]]

  if (!is.null(datum_col)){
    if ("monat" %in% names(df)) {
    month_levels_abbr <- lubridate::month(
      as.Date(paste0("2000-", 1:12, "-01")),
      label = TRUE, abbr = TRUE
    )
    df$monat <- factor(df$monat, levels = month_levels_abbr, ordered = TRUE)
  }}

  if (verbose) {
    missing_n <- sum(is.na(df$.value))
    negative_n <- sum(df$.value < 0, na.rm = TRUE)

    message("Standardisierung abgeschlossen:")
    if (!is.null(datum_col)){
      message("Datumsbereich: ", format(min(df[[datum_col]], na.rm = TRUE)), ": ", format(max(df[[datum_col]], na.rm = TRUE)))
      }
    message("Input Value Spalte: ", value_col,
            " | NA-Werte: ", missing_n,
            " | Negative Werte: ", negative_n)
  }

  if (!is.null(attr(df, "value_data_type"))) {
    inferred_type <- attr(df, "value_data_type")
    inferred_feature <- attr(df, "value_data_feature")
    if (verbose) {
      message("Datentyp-Erkennung uebersprungen: vorhandener Attributwert '", inferred_type, "' (Merkmal: ", inferred_feature, ") wird beibehalten.")
    }
  } else {
    type_info <- tryCatch({
      infer_value_type(df$.value, verbose = verbose)
    }, error = function(e) {
      if (verbose) warning("Typ-Erkennung fehlgeschlagen, Standardtyp 'continuous' wird verwendet: ", e$message)
      list(type = "continuous", features = NA)
    })

    inferred_type <- type_info$type
    inferred_feature <- type_info$features

    attr(df, "value_data_type") <- inferred_type
    attr(df, "value_data_feature") <- inferred_feature

    if (verbose) {
      msg <- paste0("Input Value Spalte Datentyp: ", inferred_type)
      if (!is.na(inferred_feature)) {
        msg <- paste0(msg, " (Merkmal: ", inferred_feature, ")")
      }
      message(msg)
    }
  }
  return(df)
}
