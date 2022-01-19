# init.R
#
# Example R code to install packages if not already installed
#
#helpers.installPackages("rgdal")
#helpers.installPackages("gstat")
#helpers.installPackages("gridExtra")
#helpers.installPackages("ggpubr")

my_packages = c("shiny","shinyWidgets", "shinythemes", "shinycssloaders", "openxlsx", "lubridate", 
                "openair", "stringi", "maptools", "modeest", "moments", "hms", "ggplot2", "ggseas", 
                "viridis", "shadowtext", "plotly",
                "Metrics", "nortest","tidyverse")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))