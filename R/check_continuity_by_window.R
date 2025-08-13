#' Check Time Series Continuity within Defined Window / Pruefung der Zeitreihen-Kontinuitaet
#'
#' This function checks whether a date vector contains all expected time points within a specified window.
#' Users can define the time unit (day, week, or month), granularity step, and whether ISO week starts (Monday) should be used.
#' Returns a list indicating whether the data are continuous and reports any missing dates.
#'
#' Diese Funktion prueft, ob ein Datumsvektor alle erwarteten Zeitpunkte innerhalb eines definierten Fensters enthaelt.
#' Die Zeitgranularitaet (Tag/Woche/Monat), Schrittweite und ISO-Wochenstart (Montag) koennen angepasst werden.
#' Gibt zurueck, ob die Zeitreihe vollstaendig ist, und listet fehlende Zeitpunkte auf.
#' @encoding UTF-8
#' @param date_vec A vector of dates. / Ein Datumsvektor
#' @param years Numeric vector indicating year range (e.g., `c(2021, 2022)`). / Jahr(e)
#' @param months Numeric vector of months (1:12). / Monate (1:12)
#' @param window_unit Time unit for continuity check: `"day"`, `"week"`, or `"month"`. / Zeiteinheit fuer Pruefung
#' @param step Step size for the sequence. Default is `1`. / Schrittweite
#' @param use_isoweek Logical. If `TRUE`, weeks start on Monday. / ISO-Woche (Montag)?
#' @param start_date Optional. Override default start date (must be in `"YYYY-MM-DD"` format). / Optionales Startdatum
#' @param allow_leading_gap Logical. If `TRUE`, allows first date to be missing but considers rest as continuous. / Erlaubt Anfangsluecke?
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{continuous}{Logical. Whether the time series is complete}
#'   \item{gaps}{Data frame of missing expected dates}
#'   \item{datum}{Vector of available dates within the window}
#'   \item{range}{Start and end of expected time window}
#' }
#'
#' @examples
#' vec <- seq(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
#' check_continuity_by_window(vec, years = 2021, months = 1:3, window_unit = "day")
#'
#' @importFrom lubridate wday ceiling_date floor_date
#'
#' @export

check_continuity_by_window <- function(
    date_vec,
    years,
    months,
    window_unit = c("week", "day", "month"),
    step = 1,
    use_isoweek = FALSE,
    start_date = NULL,
    allow_leading_gap = FALSE
) {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Paket 'lubridate' ist erforderlich. Bitte zuerst installieren.")
  }
  if (length(years) == 0 || length(months) == 0) {
    stop("Jahre und Monate muessen angegeben werden.")
  }
    if (any(months < 1 | months > 12)) {
    stop("Monate muessen zwischen 1 und 12 liegen.")
  }
    if (step <= 0) {
    stop("Schrittweite muss groesser als 0 sein.")
    }
  window_unit <- match.arg(window_unit)
  date_vec    <- as.Date(date_vec)
  date_vec <- date_vec[!is.na(date_vec)]
   if (length(date_vec) == 0) {
    stop("Eingabevektor enthaelt keine gueltigen Daten.")
   }

  data_min <- min(date_vec, na.rm = TRUE)
  data_max <- max(date_vec, na.rm = TRUE)
  min_year <- min(years)
  max_year <- max(years)
  start_month <- head(months,1)
  max_month <- tail(months,1)

  ym_start_user <- as.Date(sprintf("%04d-%02d-01", min_year, start_month))
  ym_end_user <- lubridate::ceiling_date(
    as.Date(sprintf("%04d-%02d-01", max_year, max_month)),
    unit = "month"
  ) - 1

  if (use_isoweek && window_unit == "week") {
    ym_start_iso  <- lubridate::floor_date(ym_start_user, unit = "week", week_start = 1)
    ym_end_iso <- lubridate::floor_date(ym_end_user, unit = "week", week_start = 1)
  }
  if (use_isoweek && window_unit == "week") {
    ym_start <- if (is.null(start_date)) {
      ym_start_iso
    } else {
      as.Date(start_date)
    }
    ym_end <- ym_end_iso
  } else {
    ym_start <- if (!is.null(start_date)) {
      as.Date(start_date)
    } else {
      ym_start_user
    }
    ym_end <- ym_end_user
  }

  if (ym_start < data_min || ym_end > data_max) {
    stop(
      sprintf(
        "Angefordertes Zeitfenster [%s: %s] liegt ausserhalb der Datenabdeckung [%s: %s].",
        format(ym_start), format(ym_end),
        format(data_min), format(data_max)
      )
    )
  }
  expected_dates <- switch(
    window_unit,
    day   = seq(ym_start, ym_end, by = paste(step, "day")),
    week  = seq(ym_start, ym_end, by = paste(step * 7, "day")),
    month = seq(ym_start, ym_end, by = paste(step, "month"))
  )

  if (use_isoweek && window_unit == "week") {
     expected_dates <- expected_dates[expected_dates <= ym_end]
  } else {
    expected_dates <- expected_dates[
      expected_dates >= ym_start_user & expected_dates <= ym_end_user
    ]
  }

  dates <- sort(unique(date_vec[date_vec >= ym_start & date_vec <= ym_end]))

  if (length(dates) == 0) {
    warning("Keine Daten im angegebenen Zeitfenster vorhanden.")
    return(list(
      continuous = FALSE,
      gaps       = data.frame(missing = expected_dates),
      datum      = dates,
      range      = c(min(expected_dates), max(expected_dates))
    ))
  }
  max_data_date <- max(dates)
  out_of_range  <- expected_dates[ expected_dates > max_data_date ]
  if (length(out_of_range) > 0) {
    warning(
      "Erwartete Zeitpunkte liegen ausserhalb der Datenreichweite:",
      paste0(as.character(out_of_range), collapse = ", ")
    )
  }

  missing_dates <- setdiff(expected_dates, dates)

  if (length(missing_dates) == 0) {
    continuous_flag <- TRUE
  } else if (isTRUE(allow_leading_gap)) {
    present_idx <- which(expected_dates %in% dates)
    if (length(present_idx) == 0) {
      continuous_flag <- FALSE
    } else {
      start_idx <- min(present_idx)
      tail_expect <- expected_dates[start_idx:length(expected_dates)]
      missing_after_lead <- setdiff(tail_expect, dates)
      continuous_flag <- length(missing_after_lead) == 0
    }
  } else {
    continuous_flag <- FALSE
  }

  return(list(
    continuous = continuous_flag,
    gaps       = if (length(missing_dates)==0) NULL
    else data.frame(missing = as.Date(sort(missing_dates))),
    datum      = dates,
    range      = c(min(expected_dates), max(expected_dates))
  ))
}
