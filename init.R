# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shinyWidgets", "shinythemes", "shinycssloaders", "openxlsx", "lubridate", "tidyverse",
                "openair", "stringi", "maptools", "modeest", "moments", "hms", "ggplot2", "ggseas", 
                "viridis", "shadowtext", "plotly", "leaflet", "rgdal", "gstat", "ggpubr", "gridExtra",
                "Metrics", "nortest")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))