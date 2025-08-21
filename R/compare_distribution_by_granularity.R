#' Compare Normality across Granularity Levels / Vergleich der Normalverteilung je Granularitaet
#'
#' This function compares Shapiro-Wilk normality results between two granularity levels (e.g., daily vs. weekly data).
#' It extracts diagnostics from test result objects (from [run_paired_tests()], [run_group_tests()] or [run_multi_group_tests()]) and displays them side-by-side.
#' Optionally, QQ plots are generated to visualize distributional properties.
#'
#' Diese Funktion vergleicht die Shapiro-Wilk-Ergebnisse der Normalverteilung zwischen zwei Granularitaetsebenen (z.B. Tages- vs. Wochen-Daten).
#' Diagnosen aus Testergebnissen (z.B. von [run_paired_tests()], [run_group_tests()] oder [run_multi_group_tests()]) werden extrahiert und nebeneinander dargestellt.
#' Optional werden QQ-Plots erzeugt, um Verteilungen zu visualisieren.
#'
#' @encoding UTF-8
#' @param res_day Result object from daily-level analysis. / Ergebnisobjekt der Tagesebene
#' @param res_week Result object from weekly-level analysis. / Ergebnisobjekt der Wochenebene
#' @param plot Logical. Whether to display QQ plots. / QQ-Plots anzeigen?
#' @param save_plot Logical. Whether to save the plot as PNG. / Soll der Plot gespeichert werden?
#' @param save_path Folder to save plot. Default is ".". / Speicherpfad fuer den Plot
#'
#' @return A data frame with normality diagnostics for each granularity level.
#' \describe{
#'   \item{factor}{Group name(s)}
#'   \item{granularity}{Data granularity ("day" or "week")}
#'   \item{shapiro_W}{Shapiro-Wilk W statistic}
#'   \item{shapiro_p}{p-value of Shapiro-Wilk test}
#'   \item{normal}{Whether data are considered normal ("ja"/"nein")}
#'   \item{levene_p}{Levene's test p-value (if available)}
#'   \item{bartlett_p}{Bartlett's test p-value (if available)}
#' }
#'
#' @seealso [run_paired_tests()], [run_group_tests()], [run_multi_group_tests()], [compare_monthly_cases]
#'
#' @examples
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 400),
#'   neue_faelle = rpois(400, lambda = 20)
#' )
#' res_day <- compare_monthly_cases(
#'   df = df,
#'   datum_col = "datum",
#'   value_col = "neue_faelle",
#'   years = c(2024,2025),
#'   months = 1:2,
#'   granularity = "day",
#'   shift_month = "none",
#'   agg_fun = "sum",
#'   save_plot = FALSE
#' )
#' res_week <- compare_monthly_cases(
#'   df = df,
#'   datum_col = "datum",
#'   value_col = "neue_faelle",
#'   years = c(2024,2025),
#'   months = 1:2,
#'   granularity = "week",
#'   shift_month = "none",
#'   agg_fun = "sum",
#'   save_plot = FALSE
#' )
#' compare_distribution_by_granularity(res_day, res_week)
#'
#' @importFrom ggpubr ggqqplot
#' @importFrom ggplot2 ggsave
#' @importFrom dplyr bind_rows mutate select
#' @export

compare_distribution_by_granularity <- function(res_day, res_week, plot = TRUE, save_plot = FALSE, save_path = ".") {


  get_df <- function(res, granularity_label) {
    test_type <- res$tests$type
    if (is.null(res$tests$group_col)) {
      stop("Fehlende Information: group_col wurde in tests nicht gespeichert.")
    }

    group_col <- res$tests$group_col
    attrv <- attr(res$data, "value_data_type")


    if (attrv == "count") {
      factor_vec <- if (!is.null(res$tests$group_names)) res$tests$group_names else "alle"
      data.frame(
        factor = factor_vec,
        granularity = granularity_label,
        shapiro_W = NA_real_,
        shapiro_p = NA_real_,
        normal = "nicht anwendbar",
        levene_p = NA_real_,
        bartlett_p = NA_real_,
        data_type = "count",
        stringsAsFactors = FALSE
      )
    } else if (test_type == "Paired Test") {
      shapiro <- res$tests$assumptions$normality_test
      shapiro_W <- if (!is.null(shapiro) && inherits(shapiro, "htest")) unname(shapiro$statistic) else NA_real_
      shapiro_p <- if (!is.null(shapiro) && inherits(shapiro, "htest")) shapiro$p.value else NA_real_
      normal <- if (!is.na(shapiro_p)) ifelse(shapiro_p > 0.05, "ja", "nein") else NA

      data.frame(
        factor = paste(res$tests$group_names, collapse = " vs "),
        granularity = granularity_label,
        shapiro_W = round(shapiro_W, 3),
        shapiro_p = round(shapiro_p, 4),
        normal = normal,
        levene_p = NA_real_,
        bartlett_p = NA_real_,
        data_type = "numeric",
        stringsAsFactors = FALSE
      )

    } else if (test_type == "Unpaired Test") {
      shapiro_list <- res$tests$assumptions$normality_test
      group_names <- res$tests$group_names
      shapiro_W <- sapply(shapiro_list, function(x) if (inherits(x, "htest")) x$statistic else NA_real_)
      shapiro_p <- sapply(shapiro_list, function(x) if (inherits(x, "htest")) x$p.value else NA_real_)
      normal <- ifelse(!is.na(shapiro_p), ifelse(shapiro_p > 0.05, "ja", "nein"), NA)

      data.frame(
        factor = group_names,
        granularity = granularity_label,
        shapiro_W = round(shapiro_W, 3),
        shapiro_p = round(shapiro_p, 4),
        normal = normal,
        levene_p = NA_real_,
        bartlett_p = NA_real_,
        data_type = "numeric",
        stringsAsFactors = FALSE
      )

    } else {
      shapiro_all <- res$tests$assumptions$shapiro
      factor_vec <- names(shapiro_all)
      shapiro_W <- sapply(shapiro_all, function(x) if (inherits(x, "htest")) x$statistic else NA_real_)
      shapiro_p <- sapply(shapiro_all, function(x) if (inherits(x, "htest")) x$p.value else NA_real_)
      normal <- ifelse(!is.na(shapiro_p), ifelse(shapiro_p > 0.05, "ja", "nein"), NA)

      data.frame(
        factor = factor_vec,
        granularity = granularity_label,
        shapiro_W = round(shapiro_W, 3),
        shapiro_p = round(shapiro_p, 4),
        normal = normal,
        levene_p = round(res$tests$assumptions$levene_p, 4),
        bartlett_p = round(res$tests$assumptions$bartlett_p, 4),
        data_type = "numeric",
        stringsAsFactors = FALSE
      )
    }
  }



  df_day <- get_df(res_day, "day")
  df_week <- get_df(res_week, "week")
  combined_df <- dplyr::bind_rows(df_day, df_week)
  print(combined_df)

  #QQ
  if (plot) {
    if (!requireNamespace("ggpubr", quietly = TRUE)) {
      stop("Bitte installieren Sie das Paket 'ggpubr' fuer QQ-Plots.")
    }

    df_plot_day <- res_day$data %>% dplyr::mutate(granularity = "day")
    df_plot_week <- res_week$data %>% dplyr::mutate(granularity = "week")
    df_plot <- dplyr::bind_rows(df_plot_day, df_plot_week)

    group_col <- res_day$tests$group_col

    if (!group_col %in% colnames(df_plot)) {
      stop(paste("Die Spalte", group_col, "existiert nicht im Datensatz."))
    }
    df_plot[[group_col]] <- as.factor(df_plot[[group_col]])
    df_plot$granularity <- factor(df_plot$granularity, levels = c("day", "week"))

    facet_formula <- c(group_col, "granularity")
    p <- ggpubr::ggqqplot(
      df_plot,
      x = ".value",
      facet.by = facet_formula,
      color = "granularity",
      line = "qq"
    ) +
      ggtitle("QQ-Plots nach Jahr und Granularitaet") +
      theme_minimal()

    print(p)

    if (save_plot) {
      ggplot2::ggsave(file.path(save_path, paste0("qqplot_by_granularity_", Sys.Date(), ".png")), p, width = 10, height = 6)
    }
  }

  invisible(combined_df)
}
