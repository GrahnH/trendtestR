#' Validate Time and Group Inputs for Case Comparison / Eingabepruefung fuer Zeit- und Gruppierungsvariablen
#'
#' This function checks the validity of time-based and grouping arguments passed to functions like compare_monthly_cases().
#' It validates month/year ranges, aggregation settings, and optionally the presence and structure of group_col,
#' returning standardized values and user-friendly messages for potential issues (e.g., non-factors or too many levels).
#'
#' Diese Funktion prueft Zeit- und Gruppierungsparameter, wie sie z.B. in compare_monthly_cases() verwendet werden.
#' Sie validiert Monats- und Jahresangaben, Aggregationseinstellungen und (optional) die Struktur von group_col,
#' und gibt standardisierte Werte sowie Hinweise bei potenziellen Problemen (z.B. fehlende Faktoren, zu viele Gruppen) zurueck.
#'
#' @encoding UTF-8
#'
#' @param months Integer vector of months (1:12). / Vektor der Monate (1:12).
#' @param years Integer vector of years (must be strictly increasing). / Vektor der Jahre (streng aufsteigend).
#' @param shift_month One of "none", "mth_to_next", "mth_to_prev"; defines cross-year logic. / Jahreslogik.
#' @param granularity "day" or "week". / Aggregationsebene.
#' @param agg_fun Aggregation function: "sum", "mean", or "median". / Aggregationsfunktion.
#' @param df Data frame used to validate group_col. / Datensatz zur Validierung von group_col.
#' @param group_col Optional grouping column(s) to validate. / Optionale Gruppierungsvariable(n).
#'

#' @details
#' Function Behavior and Messages:
#' - Issues stop() for invalid months/years or aggregation settings.
#' - Warns if group_col is missing in df.
#' - Gives messages if group variables are not factors, or if too many levels (>8) are detected.
#'
#' Funktionsverhalten und Hinweise:
#' - Bei ungueltigen Zeitangaben erfolgt ein Abbruch (stop()).
#' - Warnung bei nicht vorhandenen Gruppierungsvariablen.
#' - Hinweis, falls Gruppenvariablen keine Faktoren sind oder zu viele Auspraegungen (>8) besitzen.
#'
#' @return A list with standardized values for:
#' \describe{
#'   \item{months}{Validated months vector}
#'   \item{years}{Validated and sorted years}
#'   \item{granularity}{One of "day" or "week"}
#'   \item{agg_fun}{One of "sum", "mean", or "median"}
#'   \item{shift_month}{Cross-year setting}
#' }
#'
#' @seealso
#' [compare_monthly_cases()], [run_group_tests()]
#'
#' Diese Funktion wird typischerweise zusammen mit [compare_monthly_cases()], [run_group_tests()] verwendet.
#'
#' @examples
#' # Example 1: Valid input without group_col
#' # Beispiel 1: Gueltige Eingabe ohne Gruppenvariable
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-12-01"), to = as.Date("2025-02-28"), by = "day"),
#'   neue_faelle = sample(0:100, 456, replace = TRUE)
#' )
#' check_input_validity(
#'   months = c(12, 1, 2),
#'   years = c(2024, 2025),
#'   shift_month = "mth_to_next",
#'   granularity = "day",
#'   agg_fun = "sum",
#'   df = df
#' )
#'
#' # Example 2: group_col exists but is not a factor
#' # Beispiel 2: group_col ist kein Faktor: Hinweis wird ausgegeben
#' df$region <- sample(c("Nord", "Sued", "West"), size = nrow(df), replace = TRUE)
#' check_input_validity(
#'   months = c(12, 1, 2),
#'   years = c(2024, 2025),
#'   shift_month = "mth_to_next",
#'   granularity = "day",
#'   agg_fun = "mean",
#'   df = df,
#'   group_col = "region"
#' )
#'
#' # Example 3: Too many group levels triggers a message
#' # Beispiel 3: Zu viele Gruppenauspraegungen (>8): Warnung zur Plot-Lesbarkeit
#' df$gruppe <- factor(paste0("G", sample(1:12, size = nrow(df), replace = TRUE)))
#' check_input_validity(
#'   months = c(12, 1, 2),
#'   years = c(2024, 2025),
#'   shift_month = "mth_to_next",
#'   granularity = "week",
#'   agg_fun = "median",
#'   df = df,
#'   group_col = "gruppe"
#' )
#'
#' @export

check_input_validity <- function(months, years, shift_month, granularity, agg_fun, df, group_col = NULL) {
  if (length(months) == 0) stop("'months' darf nicht leer sein.")

  if (!is.numeric(months)) stop("'months' muss ein numerischer Vektor sein.")
  if (!all(months %in% 1:12)) stop("'months' darf nur Ganzzahlen zwischen 1 und 12 enthalten.")

  if (!is.numeric(years) || length(years) < 1) stop("'years' muss ein numerischer Vektor mit mindestens einem Jahr sein.")
  if (is.unsorted(years, strictly = TRUE)) stop("'years' muss streng aufsteigend sortiert sein.")
  if (shift_month != "none" && any(duplicated(months))) stop("Doppelte Monate sind bei aktivem 'shift_month' nicht erlaubt.")

  granularity  <- match.arg(granularity, choices = c("day", "week"))
  agg_fun      <- match.arg(agg_fun,     choices = c("sum", "mean", "median"))
  shift_month  <- match.arg(shift_month, choices = c("none", "mth_to_next", "mth_to_prev"))

  if (!is.null(group_col)) {
    if (!all(group_col %in% colnames(df))) {
      missing_cols <- group_col[!group_col %in% colnames(df)]
      warning(paste0("Folgende 'group_col' wurden nicht in den Spalten gefunden: ",
                     paste(missing_cols, collapse = ", ")))
      return(list(
        months = months,
        years = years,
        granularity = granularity,
        agg_fun = agg_fun,
        shift_month = shift_month
      ))
    }

    non_factor_cols <- group_col[!sapply(df[group_col], is.factor)]
    if (length(non_factor_cols) > 0) {
      message(paste0("Hinweis: Die folgenden 'group_col'-Variablen sind keine Faktoren. ",
                     "Sie werden in 'factor' umgewandelt, um konsistente Facettierung zu ermoeglichen: ",
                     paste(non_factor_cols, collapse = ", ")))
    }

    if (length(group_col) > 1) {
      message("Mehrere Gruppierungsvariablen angegeben: dies kann zu kleinen Stichprobengroessen je Gruppe fuehren, was statistische Tests erschwert.")
    }

    level_count <- sapply(df[group_col], function(col) length(unique(col)))
    too_many_levels <- names(level_count[level_count > 8])
    if (length(too_many_levels) > 0) {
      message(paste0("Achtung: Die folgenden Gruppenvariablen haben viele Auspraegungen (>8), ",
                     "was die Lesbarkeit der Plots beeintraechtigen kann: ",
                     paste(too_many_levels, collapse = ", ")))
    }
  }

  return(list(
    months = months,
    years = years,
    granularity = granularity,
    agg_fun = agg_fun,
    shift_month = shift_month
  ))
}
