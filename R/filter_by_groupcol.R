#' Filter and optionally reshape a data frame by group column / Nach Gruppenspalte filtern und optional umstrukturieren
#'
#' This function filters a data frame based on specified grouping levels and
#' optionally transforms it into a wide format for further analysis.
#' It supports retaining extra columns and provides robust error checking.
#'
#' Diese Funktion filtert einen Data Frame basierend auf bestimmten Gruppenwerten
#' und kann ihn optional in ein Wide-Format umwandeln. Es koennen weitere Spalten
#' beibehalten werden, und die Funktion enthaelt robuste Fehlerpruefungen.
#'
#' @encoding UTF-8
#'
#' @details
#' This function is particularly useful for preparing time series grouped by categories, such as cases per region or age group.
#'
#' Diese Funktion eignet sich besonders zur Vorbereitung gruppierter Zeitreihen, z.B. nach Region oder Altersgruppe.
#'
#'
#' @param df A data.frame or tibble containing the data.
#' @param group_col A string specifying the grouping column (e.g., "region", "age_group").
#' @param value_col A string for the value column (default: "neue_faelle").
#' @param datum_col A string for the date column (default: "datum").
#' @param keep_levels Optional vector of levels to retain in group_col. Default = NULL (keep all).
#' @param to_wide Logical, if TRUE returns a wide-format table (each level a column).
#' @param keep_other_cols Logical, if TRUE keeps all other original columns.
#'
#' @return A filtered and optionally reshaped tibble.
#'
#' @examples
#' # English / Deutsch
#' df <- data.frame(
#'   datum = as.Date("2024-01-01") + 0:9,
#'   gruppe = rep(c("A", "B"), each = 5),
#'   neue_faelle = c(10, 12, 13, 15, 11, 20, 21, 22, 19, 18),
#'   region = rep("Berlin", 10)
#' )
#'
#' filter_by_groupcol(
#'   df,
#'   group_col = "gruppe",
#'   value_col = "neue_faelle",
#'   datum_col = "datum",
#'   keep_levels = "A"
#' )
#'
#' filter_by_groupcol(
#'   df,
#'   group_col = "gruppe",
#'   value_col = "neue_faelle",
#'   datum_col = "datum",
#'   to_wide = TRUE
#' )
#'
#' @importFrom dplyr filter select %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#'
#' @export

filter_by_groupcol <- function(df,
                               group_col,
                               value_col,
                               datum_col,
                               keep_levels = NULL,
                               to_wide = FALSE,
                               keep_other_cols = FALSE) {

  # Input Validation: Check if required packages are loaded.
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Fehler: Das 'dplyr'-Paket ist erforderlich, aber nicht installiert oder geladen. Bitte fuehren Sie install.packages('dplyr') aus und versuchen Sie es erneut.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Fehler: Das 'tidyr'-Paket ist erforderlich, aber nicht installiert oder geladen. Bitte fuehren Sie install.packages('tidyr') aus und versuchen Sie es erneut.")
  }
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("Please install the 'tidyselect' package.")
  }

  # Input Validation: Check if df is a data frame.
  if (!is.data.frame(df)) {
    stop("Fehler: Das 'df'-Argument muss ein Datenrahmen sein.")
  }

  # Input Validation: Check if crucial columns exist in the data frame.
  required_cols <- c(group_col, value_col, datum_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste0("Fehler: Dem Datenrahmen fehlt eine oder mehrere der erforderlichen Spalten: ", paste(missing_cols, collapse = ", "), ". Bitte ueberpruefen Sie die Parameter 'group_col', 'value_col' und 'datum_col'."))
  }

  # Input Validation: Check the type of keep_levels (if not NULL).
  if (!is.null(keep_levels) && !is.atomic(keep_levels)) {
    stop("Fehler: Das 'keep_levels'-Argument muss NULL oder ein atomarer Vektor (z.B. Zeichen- oder Zahlenvektor) sein.")
  }

  # Input Validation: Check the type of to_wide and keep_other_cols.
  if (!is.logical(to_wide) || length(to_wide) != 1) {
    stop("Fehler: Das 'to_wide'-Argument muss ein einzelner logischer Wert (WAHR/FALSCH) sein.")
  }
  if (!is.logical(keep_other_cols) || length(keep_other_cols) != 1) {
    stop("Fehler: Das 'keep_other_cols'-Argument muss ein einzelner logischer Wert (WAHR/FALSCH) sein.")
  }

  df[[group_col]] <- as.factor(df[[group_col]])

  if (!is.null(keep_levels)) {
    invalid_levels <- setdiff(keep_levels, levels(df[[group_col]]))
    if (length(invalid_levels) > 0) {
      warning(paste0("Warnung: 'keep_levels' enthaelt Werte, die in der Spalte '", group_col, "' des Datenrahmens nicht vorhanden sind: ", paste(invalid_levels, collapse = ", "), ". Diese Werte werden ignoriert."))
    }
    df <- df %>% dplyr::filter(.data[[group_col]] %in% keep_levels)
  }

  selected_cols <- c(datum_col, value_col, group_col)
  if (keep_other_cols) {
    selected_cols <- union(selected_cols, names(df))
  }
  df <- df %>% dplyr::select(all_of(selected_cols))

  if (to_wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = tidyselect::all_of(group_col),    # Values from group_col become new column names.
        values_from = tidyselect::all_of(value_col),   # Values from value_col populate the new columns.
        names_prefix = paste0(value_col, "_") # Add a prefix to the new column names.
      )
  }

  return(df)
}
