#' @title (Legacy) Old version of explore_poisson_trend()
#' @description
#' Legacy function retained for compatibility.
#' This is the previous version of \code{\link{explore_poisson_trend}} prior to the GAM extension and diagnostics update.
#'
#' Alte Version der Funktion \code{\link{explore_poisson_trend}}, nur aus Kompatibilitaetsgruenden behalten.
#'
#' @inherit explore_poisson_trend
#' @keywords internal
#' @export

explore_poisson_trend_Legacy <- function(data, datum_col, value_col, group_col = NULL,
                                  df_spline = 4, family = c("auto", "poisson", "negbin"),
                                  phi_threshold = 1.5, return_formula = FALSE,
                                  verbose = FALSE) {

  warning("Diese Funktion ist veraltet. Bitte verwenden Sie `explore_count_trend()`.", call. = FALSE)
  .log_msg <- function(...) if (verbose) message(...)

  .fit_glm_robustly <- function(formula, data, type) {
    model_res <- list(model = NULL, status = "error", message = NULL)
    tryCatch({
      model_res$model <- if (type == "poisson") stats::glm(formula, data = data, family = poisson()) else MASS::glm.nb(formula, data = data)
      model_res$status <- if (model_res$model$converged) "success" else "warning"
      model_res$message <- paste0(ifelse(type == "poisson", "Poisson", "Negativ-Binomial"),
                                  "-Modell: Konvergenzstatus = ", model_res$model$converged)
    }, warning = function(w) {
      model_res$message <- paste0("Warnung bei der Anpassung des ",
                                  ifelse(type == "poisson", "Poisson", "Negativ-Binomial"),
                                  "-Modells: ", w$message)
      model_res$status <- "warning"
    }, error = function(e) {
      model_res$message <- paste0("Fehler bei der Anpassung des ",
                                  ifelse(type == "poisson", "Poisson", "Negativ-Binomial"),
                                  "-Modells: ", e$message)
      model_res$status <- "error"
    })
    return(model_res)
  }

  .make_plot <- function(data, model_data, model, datum_col, value_col, group_col, family_label, selection_info) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Das Paket 'ggplot2' ist nicht verfuegbar, Plot wird uebersprungen.")
      return(NULL)
    }

    model_data$fitted <- predict(model, newdata = model_data, type = "response")
    group_var <- if (!is.null(group_col)) rlang::sym(group_col) else NULL
    x_var <- rlang::sym(datum_col)
    y_var <- rlang::sym(value_col)

    p <- ggplot2::ggplot(model_data, ggplot2::aes(x = !!x_var, y = !!y_var))
    if (!is.null(group_col)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = !!group_var), alpha = 0.4) +
        ggplot2::geom_line(ggplot2::aes(y = fitted, color = !!group_var))
    } else {
      p <- p + ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_line(ggplot2::aes(y = fitted), color = "steelblue", linewidth = 1)
    }

    p <- p + ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste0(family_label, " GAM-Anpassung"),
        subtitle = selection_info,
        x = "Time",
        y = rlang::as_name(y_var)
      )
    return(p)
  }

  if (!is.data.frame(data)) stop("'data' muss ein Data Frame sein.")
  if (!is.character(datum_col) || !(datum_col %in% names(data))) stop("'datum_col' muss ein gueltiger Spaltenname im Data Frame sein.")
  if (!is.character(value_col) || !(value_col %in% names(data))) stop("'value_col' muss ein gueltiger Spaltenname im Data Frame sein.")
  if (!inherits(data[[datum_col]], c("Date", "POSIXct"))) stop(paste0("'", datum_col, "' muss vom Typ 'Date' oder 'POSIXct' sein."))
  if (!is.numeric(data[[value_col]])) stop(paste0("'", value_col, "' muss numerisch sein."))
  if (any(data[[value_col]] < 0, na.rm = TRUE)) stop(paste0("'", value_col, "' darf keine negativen Werte enthalten."))
  if (!requireNamespace("splines", quietly = TRUE)) stop("Das Paket 'splines' wird benoetigt.")
  family <- match.arg(family)
  if ((family == "negbin" || family == "auto") && !requireNamespace("MASS", quietly = TRUE)) {
    stop("Das Paket 'MASS' wird fuer das Negative-Binomial-Modell benoetigt.")
  }

  if (!is.null(group_col)) {
    if (!(group_col %in% names(data))) stop(paste0("'", group_col, "' ist keine gueltige Spalte im Data Frame."))
    if (length(unique(data[[group_col]])) < 2) {
      warning(paste0("'", group_col, "' enthaelt weniger als zwei Gruppen, Interaktion wird ignoriert."), call. = FALSE)
      group_col <- NULL
    } else {
      data[[group_col]] <- as.factor(data[[group_col]])
    }
  }

  cols_to_select <- c(datum_col, value_col)
  if (!is.null(group_col)) cols_to_select <- c(cols_to_select, group_col)
  model_data <- stats::na.omit(data[, cols_to_select])

  time_numeric_col_name <- paste0(datum_col, "_numeric_for_spline")
  model_data[[time_numeric_col_name]] <- as.numeric(model_data[[datum_col]])

  time_val_range_diff <- diff(range(model_data[[time_numeric_col_name]]))
  if (time_val_range_diff == 0) {
    stop("Alle Zeitpunkte in den Daten sind identisch, Spline-Anpassung nicht moeglich.")
  }
  model_data$time_std <- (model_data[[time_numeric_col_name]] - min(model_data[[time_numeric_col_name]])) / time_val_range_diff

  if (nrow(model_data) < df_spline + 3) stop(paste0("Nicht genuegend Datenpunkte (", nrow(model_data), ") fuer Spline-Anpassung mit df = ", df_spline, ". Mindestens ", df_spline + 3, " Punkte erforderlich."))
  if (length(unique(model_data[[value_col]])) < 2) stop(paste0("'", value_col, "' enthaelt weniger als zwei unterschiedliche Werte."))
  if (all(model_data[[value_col]] == 0)) warning(paste0("Alle Werte in '", value_col, "' sind null, das Modell koennte degeneriert sein."), call. = FALSE)

  spline_term <- paste0("splines::ns(time_std, df = ", df_spline, ")")
  rhs <- if (!is.null(group_col)) paste(spline_term, "*", group_col) else spline_term
  fml <- stats::as.formula(paste(value_col, "~", rhs))
  if (return_formula) return(fml)

  model <- NULL; family_used <- ""; dispersion <- NA; selection_info <- ""; aic_comparison <- NULL; fit_messages <- character(0)

  if (family == "auto") {
    .log_msg("Poisson-Modell wird angepasst ...")
    poisson_res <- .fit_glm_robustly(fml, model_data, "poisson")
    fit_messages <- c(fit_messages, poisson_res$message)
    if (poisson_res$status == "error") stop(paste0("Poisson-Modellanpassung fehlgeschlagen: ", poisson_res$message))

    model <- poisson_res$model
    phi_poisson <- model$deviance / model$df.residual
    poisson_aic <- AIC(model)
    dispersion <- phi_poisson
    .log_msg("Dispersionsparameter (Phi): ", round(phi_poisson, 3))

    if (phi_poisson > phi_threshold) {
      .log_msg(paste0("Overdispersion erkannt (Phi = ", round(phi_poisson, 3), " > ", phi_threshold, "), versuche Negative-Binomial-Modell ..."))
      negbin_res <- .fit_glm_robustly(fml, model_data, "negbin")
      fit_messages <- c(fit_messages, negbin_res$message)

      if (negbin_res$status != "error" && !is.null(negbin_res$model)) {
        negbin_aic <- AIC(negbin_res$model)
        aic_comparison <- data.frame(Model = c("Poisson", "Negative Binomial"), AIC = c(poisson_aic, negbin_aic), Delta_AIC = c(poisson_aic, negbin_aic) - min(poisson_aic, negbin_aic))

        if (negbin_aic < poisson_aic) {
          model <- negbin_res$model
          family_used <- "Negative Binomial"
          dispersion <- model$theta
          selection_info <- paste0("Negativ-Binomial-Modell gewaehlt (niedrigerer AIC: ", round(negbin_aic, 2), " vs ", round(poisson_aic, 2), ").")
          .log_msg("Negativ-Binomial-Modell gewaehlt (niedrigerer AIC).")
        } else {
          family_used <- "Poisson"
          selection_info <- paste0("Poisson-Modell beibehalten trotz Overdispersion (NB-AIC war hoeher: ", round(negbin_aic, 2), " vs ", round(poisson_aic, 2), ").")
          .log_msg("Poisson-Modell beibehalten (NB-AIC war hoeher).")
        }
      } else {
        family_used <- "Poisson"
        selection_info <- paste0("Negative-Binomial-Anpassung fehlgeschlagen (", negbin_res$message, "), Poisson-Modell wird verwendet. Phi = ", round(phi_poisson, 3))
        .log_msg("Negative-Binomial-Anpassung fehlgeschlagen, Poisson-Modell wird verwendet.")
      }
    } else {
      family_used <- "Poisson"
      selection_info <- paste0("Keine Overdispersion erkannt (Phi = ", round(phi_poisson, 3), " <= ", phi_threshold, "), Poisson-Modell wird verwendet.")
      .log_msg("Keine Overdispersion erkannt, Poisson-Modell wird verwendet.")
    }
  } else {
    .log_msg(paste0("Passe ", ifelse(family == "poisson", "Poisson", "Negativ-Binomial"), "-Modell an ..."))
    res <- .fit_glm_robustly(fml, model_data, family)
    fit_messages <- c(fit_messages, res$message)
    if (res$status == "error") stop(paste0("Anpassung des ", family, "-Modells fehlgeschlagen: ", res$message))

    model <- res$model
    family_used <- ifelse(family == "poisson", "Poisson", "Negative Binomial")
    dispersion <- ifelse(family == "poisson", model$deviance / model$df.residual, model$theta)
    selection_info <- paste0("Benutzerdefiniertes ", family_used, "-Modell.")
  }

  if (is.null(model)) stop("Modellanpassung fehlgeschlagen, kein gueltiges Modell verfuegbar.")

  return(list(
    model = model,
    formula = fml,
    summary = summary(model),
    plot = .make_plot(data, model_data, model, datum_col, value_col, group_col, family_used, selection_info),
    dispersion_parameter = dispersion,
    model_family_used = family_used,
    model_selection_info = selection_info,
    aic_comparison = aic_comparison,
    messages = paste(unique(fit_messages), collapse = "; ")
  ))
}
