# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c( "devtools","tidyverse","ggridges", "bslib","ggplot2","shiny")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, clean=TRUE, quiet=TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))
