#' Explore time-based GAM for count data trend with automatic model selection /
#' Zeitbasierte GAM-Trendanalyse fuer Zaehldaten mit automatischer Modellauswahl
#'
#' Fits a Generalized Additive Model (GAM) with time-based splines to count data, supporting automatic selection between Poisson and Negative Binomial families. /
#' Passt ein Generalisiertes Additives Modell (GAM) mit zeitbasierten Splines an Zaehldaten an, inklusive automatischer Auswahl zwischen Poisson und Negativer Binomialverteilung.
#'
#' @encoding UTF-8
#'
#' @param data Data frame with time series count data. / Data Frame mit Zeitreihen-Zaehldaten.
#' @param datum_col Name of the time column (usually Date). / Name der Zeit-Spalte (normalerweise Date).
#' @param value_col Name of the count column (dependent variable). / Name der Zaehldaten-Spalte (abhaengige Variable).
#' @param group_col Optional. Name of grouping column for interaction. / Optional. Name der Gruppierungs-Spalte fuer Interaktion.
#' @param k_spline Basis dimension for smooth terms (default = 4). Larger k allows more complex curves. /
#' Basisdimension fuer glatte Terme (Standard = 4). Hoeheres k erlaubt komplexere Kurven.
#' @param family Specifies the GAM family: "auto" (default, chooses based on overdispersion), "poisson", or "negbin". /
#' Gibt die GAM-Familie an: "auto" (Standard, Auswahl basierend auf Overdispersion), "poisson" oder "negbin".
#' @param phi Overdispersion parameter threshold for model selection (default = 1.5). /
#' Schwellwert fuer Overdispersion zur Modellauswahl (Standard = 1.5).
#' @param return_formula If TRUE, returns the model formula instead of fitting. / Wenn TRUE, wird die Modellformel zurueckgegeben.
#' @param verbose Logical. Whether to print model fitting messages. / Ob Modellanpassungsmeldungen ausgegeben werden sollen.
#'
#' @return A list containing: / Eine Liste mit:
#' \describe{
#'   \item{model}{Fitted GAM model object / Angepasstes GAM-Modellobjekt}
#'   \item{summary}{Summary of the model / Modellzusammenfassung}
#'   \item{plot}{ggplot of observed vs fitted trend / ggplot mit beobachteten und geschaetzten Trends}
#'   \item{dispersion_parameter}{List with overdispersion info (phi or theta) / Liste mit Overdispersion-Informationen (phi oder theta)}
#'   \item{model_family_used}{Model family used / Verwendete Modellfamilie}
#'   \item{model_selection_info}{Explanation of model selection / Beschreibung der Modellauswahl}
#'   \item{effective_df}{Effective degrees of freedom of smooth term(s) / Effektive Freiheitsgrade der glatten Terme}
#'   \item{aic_comparison}{AIC comparison if applicable / AIC-Vergleich, falls zutreffend}
#'   \item{messages}{Vector of fitting messages and warnings / Meldungen und Warnungen zur Modellanpassung}
#'   \item{formula}{Model formula used / Verwendete Modellformel}
#' }
#'
#' @seealso [mgcv::gam()], [mgcv::nb()], [explore_zinb_trend()]
#'
#' @examples
#' # Simulierte Zeitreihen-Zaehldaten
#' set.seed(123)
#' df <- data.frame(
#'   datum = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
#'   value = rpois(100, lambda = 5)
#' )
#'
#' # Automatische Familienauswahl basierend auf Overdispersion
#' explore_poisson_trend(df, datum_col = "datum", value_col = "value", family = "auto")
#'
#' # Negative Binomial-GAM erzwingen
#' explore_poisson_trend(df, datum_col = "datum", value_col = "value", family = "negbin")
#'
#' @importFrom stats as.formula na.omit AIC predict residuals
#' @importFrom mgcv gam s nb gam.control
#' @importFrom ggplot2 ggplot geom_point geom_line theme_minimal labs
#' @importFrom rlang sym !!! as_name
#' @export
explore_poisson_trend <- function(data, datum_col, value_col, group_col = NULL,
                                  k_spline = 4, family = c("auto", "poisson", "negbin"),
                                  phi = 1.5, return_formula = FALSE,
                                  verbose = FALSE) {

  .log_msg <- function(...) if (verbose) message("[explore_poisson_trend] ", ...)

  .fit_gam_robustly <- function(formula, data, gam_family) {
    model_res <- list(model = NULL, status = "error", message = NULL)
    family_name <- if (inherits(gam_family, "family")) gam_family$family else "Unbekannt"
    tryCatch({
      fit <- mgcv::gam(formula, data = data, family = gam_family,
                       control = mgcv::gam.control(maxit = 50))
      model_res$model <- fit
      model_res$status <- if (fit$converged) "success" else "warning"
      model_res$message <- paste0("GAM mit ", family_name, " Familie: Konvergenz = ", fit$converged)
    }, warning = function(w) {
      model_res$message <- paste0("Warnung bei GAM-Anpassung (", family_name, "): ", w$message)
      model_res$status <- "warning"
    }, error = function(e) {
      model_res$message <- paste0("Fehler bei GAM-Anpassung (", family_name, "): ", e$message)
      model_res$status <- "error"
    })
    return(model_res)
  }

  .make_plot <- function(original_data, model_data, model, datum_col, value_col, group_col, family_label, selection_info) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Paket 'ggplot2' nicht verfuegbar, Plot wird uebersprungen.")
      return(NULL)
    }

    fitted_vals <- tryCatch(fitted(model), error = function(e) predict(model, type = "response"))
    if (length(fitted_vals) != nrow(model_data)) {
      warning("Fitted-Werte Laenge passt nicht. Plot uebersprungen.")
      return(NULL)
    }

    plot_data <- model_data
    plot_data$fitted <- fitted_vals

    x_var <- rlang::sym(datum_col)
    y_var <- rlang::sym(value_col)
    group_var <- if (!is.null(group_col)) rlang::sym(group_col) else NULL
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x_var, y = !!y_var))
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
  if (!is.character(datum_col) || !(datum_col %in% names(data)))
    stop("'datum_col' muss ein gueltiger Spaltenname sein.")
  if (!is.character(value_col) || !(value_col %in% names(data)))
    stop("'value_col' muss ein gueltiger Spaltenname sein.")
  if (!inherits(data[[datum_col]], c("Date", "POSIXct")))
    stop(paste0("'", datum_col, "' muss vom Typ 'Date' oder 'POSIXct' sein."))
  if (!is.numeric(data[[value_col]]))
    stop(paste0("'", value_col, "' muss numerisch sein."))
  if (any(data[[value_col]] < 0, na.rm = TRUE))
    stop(paste0("'", value_col, "' darf keine negativen Werte enthalten."))
  if (!requireNamespace("mgcv", quietly = TRUE))
    stop("Paket 'mgcv' wird benoetigt. Installieren mit: install.packages('mgcv')")

  family <- match.arg(family)
  cols_to_select <- c(datum_col, value_col)
  if (!is.null(group_col)) {
    if (!(group_col %in% names(data)))
      stop(paste0("'", group_col, "' ist keine gueltige Spalte."))
    cols_to_select <- c(cols_to_select, group_col)
  }

  model_data <- stats::na.omit(data[, cols_to_select])

  if (!is.null(group_col)) {
    if (length(unique(model_data[[group_col]])) < 2) {
      .log_msg(paste0("'", group_col, "' hat < 2 Gruppen. Interaktion wird ignoriert."))
      group_col <- NULL
      model_data <- stats::na.omit(data[, c(datum_col, value_col)])  # neu ohne group_col
    } else {
      model_data[[group_col]] <- as.factor(model_data[[group_col]])
    }
  }

  tnum <- as.numeric(model_data[[datum_col]])
  if (diff(range(tnum)) == 0)
    stop("Alle Zeitpunkte sind identisch Spline nicht moeglich.")

  model_data$.time_std_internal <- (tnum - min(tnum)) / diff(range(tnum))

  if (!is.null(group_col)) {
    fml_str <- paste0(value_col, " ~ ", group_col, " + s(.time_std_internal, k = ", k_spline, ", by = ", group_col, ")")
  } else {
    fml_str <- paste0(value_col, " ~ s(.time_std_internal, k = ", k_spline, ")")
  }
  fml <- stats::as.formula(fml_str)

  if (return_formula) {
    .log_msg("Formel zurueckgegeben (return_formula = TRUE).")
    return(fml)
  }

  model <- NULL
  dispersion_param <- list()
  selection_info <- ""
  aic_comparison <- NULL
  edf_used <- NA
  fit_messages <- character(0)

  if (family == "auto") {
    .log_msg("Passe Poisson-GAM an...")
    poisson_res <- .fit_gam_robustly(fml, model_data, poisson())
    fit_messages <- c(fit_messages, poisson_res$message)
    if (poisson_res$status == "error")
      stop("Poisson-GAM-Anpassung fehlgeschlagen: ", poisson_res$message)

    model <- poisson_res$model
    phi_poisson <- tryCatch({
      sum(residuals(model, type = "pearson")^2) / model$df.residual
    }, error = function(e) {
      .log_msg("Phi-berechnen fehlgeschlagen: ", e$message,"default value 1.0 benutzt")
      1.0
    })
    poisson_aic <- AIC(model)

    .log_msg("Overdispersion (Phi): ", round(phi_poisson, 3))

    if (phi_poisson > phi) {
      .log_msg("Overdispersion erkannt, versuche NB-GAM...")
      nb_res <- .fit_gam_robustly(fml, model_data, mgcv::nb())
      fit_messages <- c(fit_messages, nb_res$message)

      if (nb_res$status != "error" && !is.null(nb_res$model)) {
        nb_aic <- AIC(nb_res$model)
        aic_comparison <- data.frame(
          Model = c("Poisson GAM", "Negative Binomial GAM"),
          AIC = c(poisson_aic, nb_aic),
          Delta_AIC = c(poisson_aic, nb_aic) - min(poisson_aic, nb_aic)
        )

        if (nb_aic < poisson_aic) {
          model <- nb_res$model
          family_used <- "Negbin GAM (automatisch)"
          dispersion_param <- list(
            family = "negbin",
            overdispersion_phi = phi_poisson,
            theta = model$family$getTheta(TRUE),
            theta_se = model$family$getTheta(FALSE),
            message = paste("NB gewaehlt: AIC =", round(nb_aic, 2), "<", round(poisson_aic, 2))
          )
          selection_info <- paste0("NB-GAM gewaehlt (AIC: ", round(nb_aic, 2), " < ", round(poisson_aic, 2), ").")
          .log_msg("NB-GAM gewaehlt.")
        } else {
          family_used <- "Poisson GAM (automatisch)"
          dispersion_param <- list(
            family = "poisson",
            overdispersion_phi = phi_poisson,
            theta = NULL,
            message = paste("Poisson beibehalten: NB-AIC war hoeher (", round(nb_aic, 2), ")")
          )
          selection_info <- paste0("Poisson-GAM beibehalten (NB-AIC war hoeher: ", round(nb_aic, 2), ").")
        }
      } else {
        family_used <- "Poisson GAM (automatisch)"
        dispersion_param <- list(
          family = "poisson",
          overdispersion_phi = phi_poisson,
          theta = NULL,
          message = paste("NB-GAM fehlgeschlagen, Poisson verwendet. Phi =", round(phi_poisson, 3))
        )
        selection_info <- paste0("NB-GAM fehlgeschlagen, Poisson-GAM verwendet. Phi = ", round(phi_poisson, 3))
      }
    } else {
      family_used <- "Poisson GAM (automatisch)"
      dispersion_param <- list(
        family = "poisson",
        overdispersion_phi = phi_poisson,
        theta = NULL,
        message = paste("Keine Overdispersion (Phi =", round(phi_poisson, 3), ")")
      )
      selection_info <- paste0("Keine Overdispersion (Phi = ", round(phi_poisson, 3), "). Poisson-GAM verwendet.")
    }

  } else {
    fam_obj <- if (family == "poisson") poisson() else mgcv::nb()
    .log_msg(paste0("Passe ", toupper(family), "-GAM an..."))
    res <- .fit_gam_robustly(fml, model_data, fam_obj)
    fit_messages <- c(fit_messages, res$message)
    if (res$status == "error") stop(paste0(family, "-GAM fehlgeschlagen: ", res$message))

    model <- res$model
    if (family == "poisson") {
      family_used <- "Poisson GAM (benutzerdefiniert)"
      phi_val <- sum(residuals(model, type = "pearson")^2) / model$df.residual
      dispersion_param <- list(family = "poisson", overdispersion_phi = phi_val, theta = NULL)
    } else {
      family_used <- "Negbin GAM (benutzerdefiniert)"
      dispersion_param <- list(
        family = "negbin",
        overdispersion_phi = NULL,
        theta = model$family$getTheta(TRUE),
        theta_se = model$family$getTheta(FALSE)
      )
    }
    selection_info <- paste0("Benutzerdefiniertes ", toupper(family), "-GAM verwendet.")
  }

  edf_used <- tryCatch(sum(summary(model)$s.table[, "edf"]), error = function(e) NA)

  return(list(
    model = model,
    summary = tryCatch(summary(model), error = function(e) NULL),
    plot = .make_plot(data, model_data, model, datum_col, value_col, group_col,
                      family_used,
                      selection_info),
    dispersion_parameter = dispersion_param,
    model_family_used = family_used,
    model_selection_info = selection_info,
    effective_df = edf_used,
    aic_comparison = aic_comparison,
    messages = fit_messages
  ))
}
