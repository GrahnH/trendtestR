#' Visualize Weekly Aggregated Values / Woechentliche aggregierte Werte visualisieren
#'
#' This function aggregates time series data by calendar week and generates three plots trend line, histogram, and boxplot based on a specified retrospective period (either a number of weeks or a date range). It also shows a 95% confidence interval and can optionally save the plots.
#'
#' Diese Funktion aggregiert Zeitreihendaten nach Kalenderwochen und erstellt fuer den angegebenen Rueckblickzeitraum (als Anzahl der Wochen oder Zeitfenster) drei Diagramme: Trendverlauf, Histogramm und Boxplot. Zusaetzlich wird ein 95%-Konfidenzintervall angezeigt. Optional koennen die Plots gespeichert werden.
#'
#' @encoding UTF-8
#'
#' @param df A data.frame with date and value columns. / Ein Data Frame mit Datums- und Wertespalten
#' @param datum_col Name of the date column, default is `"datum"`. / Name der Datumsspalte, Standard: `"datum"`
#' @param value_col Name of the value column. / Name der Wertespalte /
#' @param weeks_back Number of recent weeks or a length-2 numeric vector. / Anzahl der zurueckliegenden Wochen oder ein Vektor mit zwei Werten
#' @param agg_fun Aggregation function (e.g., `"sum"`, `"mean"`). / Aggregationsfunktion, z.B. `"sum"`, `"mean"`
#' @param plottype Optional plot type: `1` for all, `2` for hist+box, `3` for trend+box. / Optionaler Plottyp: `1` fuer alle, `2` fuer hist+box, `3` fuer trend+box
#' @param save_plot Logical, whether to save the plots. / Logisch, ob die Plots gespeichert werden sollen
#' @param save_path Folder to save the plots. / Zielpfad zum Speichern der Plots
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Aggregated weekly data}
#'   \item{trend_plot}{Trend plot}
#'   \item{hist_plot}{Histogram}
#'   \item{box_plot}{Boxplot}
#' }
#'
#' @examples
#' df <- data.frame(
#'   datum = as.Date("2022-01-01") + 0:100,
#'   neue_faelle = rpois(101, lambda = 20)
#' )
#' result <- plot_weekly_cases(df, value_col = "neue_faelle", weeks_back = 20)
#' @importFrom lubridate isoyear isoweek
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_histogram geom_density geom_boxplot labs theme theme_minimal element_text ggsave after_stat
#' @importFrom dplyr %>%
#' @importFrom stats aggregate as.formula
#'
#' @export


plot_weekly_cases <- function(df,
                              datum_col = "datum",
                              value_col = NULL,
                              weeks_back = 51,
                              agg_fun = "sum",
                              plottype = NULL,
                              save_plot = FALSE,
                              save_path = "."
) {
  df <- standardize_case_columns(df, datum_col = datum_col, value_col = value_col, verbose = FALSE)

  #Datum in Jahr und Woche spalten, besonders fuer die EpiDatenanalyse
  df$jahr <- isoyear(df[[datum_col]])
  df$woche <- isoweek(df[[datum_col]])
  jahr <- "jahr"
  woche <- "woche"

  #Aggregation
  value_quoted <- paste0("`", gsub("`", "", value_col), "`")
  formula_text <- paste0(value_quoted, " ~ ", jahr, " + ", woche)
  agg_function <- match.fun(agg_fun)
  weekly_cases <- aggregate(as.formula(formula_text), data = df, agg_function, na.rm = TRUE)

  #weeklycases definieren
  weekly_cases <- weekly_cases[order(weekly_cases[[jahr]], weekly_cases[[woche]]), ]
  n <- nrow(weekly_cases)


  if (length(weeks_back) == 1) {
    from_index <- max(1, n - weeks_back)
    to_index <- n
    if (from_index >= to_index) {
      stop("Ungueltiger weeks_back-Bereich: Start ist groesser als Ende.")
    }
  } else if (length(weeks_back) == 2) {
    from_index <- n - weeks_back[2]
    to_index <- n - weeks_back[1]
    if (from_index < 1 || to_index < 1 || from_index >= to_index) {
      stop("Ungueltiger weeks_back-Bereich: Zeitraum liegt ausserhalb der verfuegbaren Daten.")
    }
  } else {
    stop("weeks_back muss entweder ein einzelner Wert oder ein Vektor der Laenge 2 sein.")
  }


  weekly_cases <- weekly_cases[from_index:to_index, ]
  weekly_cases$jahr_woche <- paste0(
    weekly_cases[[jahr]], "-KW", sprintf("%02d", weekly_cases[[woche]])
  )

  #Plottitel eingeben
  if (length(weeks_back) == 1) {
    week_range_text <- paste0(weeks_back + 1, " Wochen")
  } else {
    week_range_text <- paste0("vor letzter ", weeks_back[2], " bis letzter ", weeks_back[1], " Wochen")
  }

  #Plot Trendlinie
  p1 <- ggplot(weekly_cases, aes(x = jahr_woche, y = .data[[value_col]])) +
    geom_col(fill = "steelblue") +
    geom_line(aes(group = 1), color = "black") +
    labs(title = paste0("Woechentliche ", agg_fun, "-Werte (", week_range_text, ")"),
         x = "Kalenderwoche", y = value_col) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  #Plot Histogramm
  p2 <- ggplot(weekly_cases, aes(x = .data[[value_col]])) +
    geom_histogram(aes(y = ggplot2::after_stat(density)), bins = 10, fill = "orange", color = "white") +
    geom_density(color = "darkred", linewidth = 1.0) +
    labs(
      title = paste0("Histogramm der woechentlichen ", agg_fun, "-Werte (", week_range_text, ")"),
      x = value_col,
      y = "Dichte"
    ) +
    theme_minimal()

  #Plot Box
  p3 <- ggplot(weekly_cases, aes(y = .data[[value_col]])) +
    geom_boxplot(fill = "skyblue", outlier.colour = "red") +
    labs(title = paste0("Boxplot der woechentlichen ", agg_fun, "-Werte (", week_range_text, ")"),
         y = value_col)

  #PlotsaveOptionen
  if (save_plot && !dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
  if (is.null(plottype) || plottype == 1 || plottype == 3) {
    print(p1)
    if (save_plot) ggsave(file.path(save_path, paste0("plot1_trend_", agg_fun, "_", value_col, ".png")), p1, width = 10, height = 5)
  }
  if (is.null(plottype) || plottype == 1 || plottype == 2) {
    print(p2)
    if (save_plot) ggsave(file.path(save_path, paste0("plot2_hist_", agg_fun, "_", value_col, ".png")), p2, width = 8, height = 5)
  }
  if (is.null(plottype) || plottype == 2 || plottype == 3) {
    print(p3)
    if (save_plot) ggsave(file.path(save_path, paste0("plot3_boxplot_", agg_fun, "_", value_col, ".png")), p3, width = 6, height = 5)
  }

  if (save_plot) {
    message("Plots gespeichert unter: ", normalizePath(save_path))
  }

  #95%CI ausdruecken
  x <- weekly_cases[[value_col]]
  n_valid <- sum(!is.na(x))
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  se <- sd_x / sqrt(n_valid)
  ci95 <- 1.96 * se

  cat("\n",
      "Anzahl gueltiger Wochen:", n_valid, "\n",
      "Mittelwert:", round(mean_x, 2), "\n",
      "Standardabweichung:", round(sd_x, 2), "\n",
      "95%-Konfidenzintervall: [", round(mean_x - ci95, 2), ", ", round(mean_x + ci95, 2), "]\n"
  )

  return(list(
    data = weekly_cases,
    trend_plot = p1,
    hist_plot = p2,
    box_plot = p3
  ))
}
