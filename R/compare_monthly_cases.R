#' Compare Monthly Case Trends across Years / Vergleich monatlicher Falltrends zwischen Jahren
#'
#' This function compares numeric variables (e.g., new case numbers) across specified months and years.
#' It supports aggregation by day or ISO week, optional cross-year logic (e.g., combining Dec:Jan),
#' automated visualization (trend line, dot plot, boxplot), and group-wise faceting.
#' Statistical tests (e.g., t-test, ANOVA) are automatically selected and executed.
#'
#' Diese Funktion vergleicht numerische Variablen (z.B. Fallzahlen) ueber ausgewaehlte Monate und Jahre hinweg.
#' Sie unterstuetzt Aggregation nach Tag oder ISO-Woche, optionale Jahreswechsel-Logik (z.B. Dezember:Januar),
#' automatische Visualisierung (Linien-, Punkt- und Boxplot) sowie Facetierung nach Gruppenvariablen.
#' Die passenden statistischen Tests (z.B. t-Test, ANOVA) werden automatisch durchgefuehrt.
#'
#' @encoding UTF-8
#'
#' @param df Data frame with at least a date and value column. / Data Frame mit Datum und Wert
#' @param datum_col Name of the date column. Default is `"datum"`. / Name der Datums-Spalte
#' @param value_col Name of the value column. Default is `"neue_faelle"`. / Name der Wertespalte
#' @param group_col Optional grouping variable(s) for faceting. / Optionale Gruppierung
#' @param years Vector of years to include. E.g., `c(2023, 2024)`. / Zu vergleichende Jahre
#' @param months Vector of months to include (1:12). / Zu vergleichende Monate
#' @param granularity Aggregation level: `"day"` or `"week"`. / Aggregationsebene
#' @param agg_fun Aggregation function: `"sum"`, `"mean"`, or `"median"`. / Aggregationsfunktion
#' @param shift_month Cross-year adjustment for Dec/Jan: `"none"`, `"mth_to_next"`, `"mth_to_prev"`. / Jahreswechsel-Logik
#' @param save_plot Logical. Whether to save the plots as PNG files. / Plots speichern?
#' @param save_path Path to folder where plots should be saved. / Speicherpfad
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{data}{Aggregated and annotated data frame}
#'   \item{trend_plot}{Line plot showing daily/weekly trends}
#'   \item{monthly_trend_plot}{Dot plot by year and month}
#'   \item{box_plot}{Boxplot comparing distributions across months and years}
#'   \item{tests}{Result of statistical test (from [run_group_tests()])}
#'   \item{table}{Frequency table of observations per year}
#' }
#'
#' @seealso [run_group_tests()], [check_continuity_by_window()], [standardize_case_columns()], [infer_value_type()]
#' @details
#' Function Behavior and Notes:
#' - The function compares a numeric variable (e.g., case counts) across selected months and years.
#' - Aggregation can be done at the `"day"` or `"week"` level (ISO week, Monday start).
#' - When `shift_month` is set to `"mth_to_next"` or `"mth_to_prev"`, months like December and January can be merged across year boundaries:
#'     - `"mth_to_next"`: assigns months to the *next* year group (e.g., Dec 2023 to 2024).
#'     - `"mth_to_prev"`: assigns monthd to the *previous* year group (e.g., Jan 2024 to 2023).
#' - All plots (`trend_plot`, `monthly_trend_plot`, `box_plot`) are automatically colored by year and faceted if `group_col` is provided.
#' - Statistical tests are performed automatically based on the number of groups (e.g., `t.test`, `Wilcoxon`, `ANOVA`, `Kruskal-Wallis`).
#'
#' ---
#'
#' Funktionsverhalten und Hinweise:
#' - Die Funktion vergleicht eine numerische Variable (z.B. Fallzahlen) ueber Monate und Jahre hinweg.
#' - Die Aggregation erfolgt auf `"day"`- oder `"week"`-Ebene (ISO-Woche, Montag-basiert).
#' - Mit `shift_month = "mth_to_next"` oder `"mth_to_prev"` koennen Monate ueber Jahresgrenzen hinweg zugeordnet werden:
#'     - `"mth_to_next"`: Monat zum Folgejahr (z.B. Dez. 2023 → 2024)
#'     - `"mth_to_prev"`: Monat zum Vorjahr (z.B. Jan. 2024 → 2023)
#' - Alle Plots sind nach Jahr eingefaerbt; bei Angabe von `group_col` erfolgt eine Facetierung.
#' - Die geeigneten statistischen Tests werden automatisch ausgewaehlt und durchgefuehrt.
#'
#' ---

#' @examples
#' set.seed(123)
#' test_df <- data.frame(
#'   datum = seq.Date(from = as.Date("2024-12-15"), to = as.Date("2025-01-20"), by = "day"),
#'   value = sample(0:50, size = 37, replace = TRUE)
#' )
#'
#' compare_monthly_cases(
#'   df = test_df,
#'   datum_col = "datum",
#'   value_col = "value",
#'   years = c(2024, 2025),
#'   months = c(12, 1),
#'   granularity = "day",
#'   agg_fun = "sum",
#'   shift_month = "mth_to_next",
#'   save_plot = FALSE
#' )
#'
#' @importFrom dplyr mutate filter summarise group_by arrange case_when pull select n_distinct
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_boxplot labs theme_minimal theme scale_x_date ggsave element_text facet_grid ggtitle
#' @importFrom lubridate year month isoweek isoyear
#' @importFrom e1071 skewness
#' @importFrom stats na.omit as.formula
#' @importFrom utils head tail
#' @importFrom rlang sym
#'
#' @export

compare_monthly_cases <- function(df,
                                  datum_col = "datum",
                                  value_col = "neue_faelle",
                                  group_col = NULL,
                                  years = c(2024, 2025),
                                  months = 1:5,
                                  granularity = "day",       # "day" oder "week"
                                  agg_fun = "sum",           # "sum", "mean", "median"
                                  shift_month = "none",
                                  save_plot = FALSE,
                                  save_path = ".") {
  #Eingabe der Parameter pruefen
  validated <- check_input_validity(months, years, shift_month, granularity, agg_fun)

  months       <- validated$months
  years        <- validated$years
  granularity  <- validated$granularity
  agg_fun      <- validated$agg_fun
  shift_month  <- validated$shift_month

  #Datum und Werte vorbereiten
  df <- standardize_case_columns(df, datum_col = datum_col, value_col = value_col)

  if (!is.null(group_col)) {
    for (col in group_col) {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  #die Voraussetzung der Crossyearlogic pruefen（nur wenn  shift != "none" und months enthaelt 1 und 12)
  cross_year <- if (shift_month != "none") {
    all(c(12, 1) %in% months) #&& !setequal(sort(months), 1:12)
  } else {
    FALSE
  }

  #Jahre erstmal filtern, um die betroffenen originalen Jahre einzugrenzen
  if (shift_month == "mth_to_next" && cross_year) {
    years_filter <- c(min(years) - 1, years)
  } else if (shift_month == "mth_to_prev" && cross_year) {
    years_filter <- c(max(years) + 1, years)
  } else {
    years_filter <- years
  }

  df <- df %>%
    filter(
      lubridate::year(.data[[datum_col]]) %in% years_filter,
      lubridate::month(.data[[datum_col]]) %in% months,
      !is.na(.data[[value_col]])
    )

  #Crossjahrlogig anwenden, die Monaten in die cross_jahr_group zuweisen
  df <- df %>%
    dplyr::mutate(
      monat_num = lubridate::month(.data[[datum_col]]),
      monat     = lubridate::month(.data[[datum_col]], label = TRUE, abbr = TRUE),

      jahr_group = dplyr::case_when(
        shift_month == "mth_to_next" ~ lubridate::year(.data[[datum_col]]) + (monat_num %in% head(months,1):12),
        shift_month == "mth_to_prev" ~ lubridate::year(.data[[datum_col]]) - (monat_num %in% 1:tail(months,1)),
        TRUE                         ~ lubridate::isoyear(.data[[datum_col]])
      ),

      jahr = as.factor(jahr_group)
    ) %>%
    filter(
      monat_num %in% months,
      jahr_group %in% years
    )

  #Aggregation nach Tag oder Woche
  agg_function <- match.fun(agg_fun)
  if (granularity == "week") {
    df <- df %>%
      dplyr::mutate(
        iso_week = lubridate::isoweek(.data[[datum_col]]),
        iso_year = lubridate::isoyear(.data[[datum_col]])
      )

    grouping_vars <- c("jahr", "iso_year", "iso_week", group_col)
    df <- df %>%
      dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
      dplyr::summarise(
        datum = min(.data[[datum_col]], na.rm = TRUE),
        .value = agg_function(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(monat = factor(lubridate::month(datum, label = TRUE, abbr = TRUE),
                                   levels = levels(df$monat)))

    names(df)[names(df) == "datum"]  <- datum_col
    names(df)[names(df) == ".value"] <- value_col

  } else if (granularity == "day") {
    grouping_vars <- c("jahr", datum_col, group_col)

    df <- df %>%
      dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
      dplyr::summarise(
        .value = agg_function(.data[[value_col]], na.rm = TRUE),
        monat = dplyr::first(monat),
        .groups = "drop"
      )

    names(df)[names(df) == ".value"] <- value_col
  }

  if (!".value" %in% names(df)) {
    df <- df %>% dplyr::mutate(.value = .data[[value_col]])
  }

  inferred_type <- infer_value_type(df$.value)
  attr(df, "value_data_type") <- inferred_type$type
  attr(df, "value_data_feature") <- inferred_type$features

  #plot
  month_levels_abbr <- levels(lubridate::month(
    as.Date(paste0("2000-", 1:12, "-01")),
    label = TRUE, abbr = TRUE
  ))

  monat_order <- month_levels_abbr[unique(months)]
  df$monat <- factor(df$monat, levels = monat_order)

  # Plot Trend ueber Datum
  x_min <- min(df[[datum_col]], na.rm = TRUE)
  x_max <- max(df[[datum_col]], na.rm = TRUE)

  p1 <- ggplot(df, aes(x = .data[[datum_col]], y = .value, color = jahr, group = jahr)) +
    geom_line(linewidth = 1) +
    scale_x_date(limits = c(x_min, x_max), date_labels = "%d.%b") +
    labs(
      title = paste0("Taegliche/Woechentliche ", agg_fun, " ", value_col,
                     " (from ", head(months, 1), " to ", tail(months, 1),
                     " in ", paste(years, collapse = " to "), ")"),
      x = "Datum", y = value_col
    ) +
    theme_minimal()

  # Plot Trend nach Jahr-Monat
  p2 <- ggplot(df, aes(x = monat, y = .value, color = jahr, group = jahr)) +
    geom_point(alpha = 0.6) +
    labs(
      title = paste0("Trend nach ", agg_fun, " ", value_col,
                     " (from ", head(months, 1), " to ", tail(months, 1),
                     " in ", paste(years, collapse = " vs. "), ")"),
      x = "Jahr-Monat", y = value_col
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Plot Boxplot Verteilung
  p3 <- ggplot(df, aes(x = monat, y = .value, fill = jahr)) +
    geom_boxplot() +
    labs(
      title = paste0("Verteilung der ", agg_fun, " ", value_col,
                     " (from ", head(months, 1), " to ", tail(months, 1),
                     " in ", paste(years, collapse = " vs. "), ")"),
      x = "Jahr-Monat", y = value_col
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(group_col)) {
    if (length(group_col) == 1) {
      facet_formula <- as.formula(paste(group_col, "~ ."))
    } else if (length(group_col) == 2) {
      facet_formula <- as.formula(paste(group_col[1], "~", group_col[2]))
    } else {
      stop("facet_grid() unterstuetzt derzeit nur bis zu 2 Gruppierungsvariablen.")
    }

    p1 <- p1 + facet_grid(facet_formula, scales = "free_y")
    p2 <- p2 + facet_grid(facet_formula, scales = "free_y")
    p3 <- p3 + facet_grid(facet_formula, scales = "free_y")
  }

  print(p1)
  print(p2)
  print(p3)

  if (any(df$.value <= 0, na.rm = TRUE)) {
    warning("Boxplot enthaelt nicht-positive Werte: log10-Achse ist ggf. ungeeignet.")
  } else {
    feature <- attr(df, "value_data_feature")
    if (!is.null(feature) && !is.na(feature) && feature == "right-skewed") {
      warning("Daten sind rechtsschief: log10-Skala koennte hilfreich sein.")
    }
  }

  #Plots speichern
  if (save_plot) {
    today <- Sys.Date()
    ggplot2::ggsave(file.path(save_path, paste0("comepareMtrend_",agg_fun, value_col,"_", today, ".png")), p1, width = 8, height = 5)
    ggplot2::ggsave(file.path(save_path, paste0("comepareMtrend_monthly_",agg_fun, value_col,"_", today, ".png")), p2, width = 8, height = 5)
    ggplot2::ggsave(file.path(save_path, paste0("comepareMbox_",agg_fun, value_col,"_", today, ".png")), p3, width = 8, height = 5)
  }

  #Statistische Tests jenach Groupanzahl automatisch durchfuehren
  table(df$jahr)
  test_results <- run_group_tests(df, value_col = ".value", group_col = "jahr")


  return(list(
    data = df,
    trend_plot = p1,
    monthly_trend_plot = p2,
    box_plot = p3,
    tests = test_results,
    table=table(df$jahr)
  ))
}
