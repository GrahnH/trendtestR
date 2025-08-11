#' Prepare Grouped Data for Statistical Testing
#'
#' Cleans and splits the input data by group, removing missing values and empty groups.
#'
#' @description
#' This function prepares a dataset for grouped statistical tests by:
#' - Filtering out `NA` values in the target variable;
#' - Dropping empty groups and reporting excluded levels;
#' - Splitting the values by group and computing sample sizes.
#'
#' Diese Funktion bereitet Daten fuer gruppierte Tests vor:
#' - Entfernt fehlende Werte (`NA`) in der Zielvariablen;
#' - Entfernt leere Gruppen und gibt eine Warnung aus;
#' - Teilt die Werte nach Gruppen und berechnet Stichprobengroessen.
#'
#' @encoding UTF-8
#'
#' @param df A data.frame or tibble containing the data.
#' @param value_col A string indicating the name of the column with values to test.
#' @param group_col A string indicating the name of the grouping column.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{df}{The filtered data frame with updated grouping column.}
#'   \item{vals}{A list of vectors, one per group, containing values.}
#'   \item{sample_sizes}{A named vector with sample sizes per group.}
#'   \item{n_groups}{The number of groups remaining after filtering.}
#'   \item{group_names}{The names of the groups.}
#' }
#'
#' @export


prepare_group_data <- function(df, value_col = ".value", group_col = "jahr") {
  if (!is.data.frame(df)) {
    stop("Fehler: Das Argument 'df' muss ein Data Frame sein.")
  }

  if (!value_col %in% names(df)) {
    stop(paste0("Fehler: Die Spalte '", value_col, "' wurde im Data Frame 'df' nicht gefunden."))
  }
  if (!is.numeric(df[[value_col]])) {
    stop(paste0("Fehler: Die Spalte '", value_col, "' muss numerisch sein."))
  }

  df <- df %>% dplyr::filter(!is.na(.data[[value_col]]))
  group_counts <- table(df[[group_col]])
  dropped_levels <- names(group_counts[group_counts == 0])
  if (!group_col %in% colnames(df)) {
    stop(paste0("Fehler: Die Spalte '", group_col, "' wurde im Data Frame 'df' nicht gefunden."))
  }

  df[[group_col]] <- droplevels(as.factor(df[[group_col]]))

  df[[group_col]] <- droplevels(df[[group_col]])

  if (length(dropped_levels) > 0) {
    message("Hinweis: Folgende Gruppen wurden ausgeschlossen (keine Daten): ",
            paste(dropped_levels, collapse = ", "))
  }

  vals <- split(df[[value_col]], df[[group_col]])
  sample_sizes <- sapply(vals, length)
  n_groups <- length(vals)
  group_names <- names(vals)

  return(list(
    df = df,
    vals = vals,
    sample_sizes = sample_sizes,
    n_groups = n_groups,
    group_names = group_names
  ))
}

