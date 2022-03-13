#' Launch the shiny app
#'
#' Later, perhaps put the logic into separate function and invoke them. For now,
#' all logic is just here.
#'
#' @param object not used, perhaps later used for passing results to the app.
#' @param rstudio logical, if `TRUE` the app uses the RStudio default, otherwise
#'   the browser is launched.
#' @param ... passed on to shiny::runApp
#'
#' Set host = "0.0.0.0" and port at your choosing to expose the app to others on
#' your LAN.
#'
#' @export

launch_app <- function(object, rstudio = TRUE, ...) {
	if (isTRUE(rstudio)) {
		launchBrowser <- getOption("shiny.launch.browser", interactive())
	} else {
		launchBrowser <- TRUE
	}

	# From other app, showing the logic needed if object to be fed into the app
	# .pkg_env$.RESULTS <- results
	# on.exit(rm(.RESULTS, pos = .pkg_env), add = TRUE, after = TRUE) # only needed while running shiny app

	shiny::runApp(system.file("shinyApp", package = "medconsumr"),
				  launch.browser = launchBrowser, ...)
}
