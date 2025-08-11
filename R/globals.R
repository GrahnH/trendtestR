# globals.R

# Suppress R CMD check NOTE for non-standard evaluation variables
utils::globalVariables(
  c(
    "density",      # used in geom_density
    "datum",        # general datum variable
    ".data",        # used in tidyverse piping
    ".value",       # used in ggplot2 aes string evaluation
    "iso_year",     # used in time aggregation
    "jahr",         # year in German
    "jahr_group",   # custom grouping by year
    "jahr_woche",   # year-week identifier
    "monat",        # month in German
    "monat_num",    # numeric month
    "value"         # general numeric variable
  )
)
