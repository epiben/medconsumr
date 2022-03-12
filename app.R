# On a Mac this app can be invoked with this shell command
# Rscript -e "shiny::runGitHub('medicinforbrug', 'epiben', launch.browser = TRUE)"

# ==============================================================================

# Install packages if no available
pkgs <- c("shiny", "shinydashboard", "dplyr", "tidyr", "purrr", "readr",
          "stringr", "ggplot2", "lubridate")

for (p in pkgs) {
  if (isFALSE(requireNamespace(p, quietly = TRUE))) {
    message(p, " not available, now installing it.")
    install.packages(p, type = "binary", repos = "https://cran.rstudio.com/")
  }
}

# Must be done this way for shinyapps.io deployment to work
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(ggplot2)
library(lubridate)

# ==============================================================================

# Run the application
shinyApp(ui = ui, server = server)
