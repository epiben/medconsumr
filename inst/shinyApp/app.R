# On a Mac this app can be invoked with this shell command
# Rscript -e "shiny::runGitHub('medicinforbrug', 'epiben', launch.browser = TRUE)"

# To share on LAN, run the following (port is arbitrary but shouldn't be otherwise used)
# runApp(host = "0.0.0.0", port = 4131)
# Then others on the same LAN can access the app via (if the following is your computer's IP)
# 192.168.1.6:4131

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

# USER INTERFACE
body <- dashboardBody(
  fluidRow(
    column(6, fileInput("excel_data", "Upload Excel file",
                        accept = c(".xlsx", ".xls"), width = "100%")),
    column(3, uiOutput("dynamic_sheet")),
    column(3, numericInput("skip", "No. non-empty rows to skip", value = 0,
                           step = 1, min = 0, width = "100%"))
  ),

  fluidRow(
    uiOutput("box_with_plot") # must be dynamic so user can alter height
  ),

  fluidRow(
    uiOutput("dynamic_box_content"),
    uiOutput("dynamic_box_scales_panels"),
    uiOutput("dynamic_box_appearance")
  ),

  fluidRow(
    box(title = "Parsed data", collapsible = TRUE, collapsed = TRUE, width = 12,
        dataTableOutput("parsed_data")
    ),
  ),

  fluidRow(
    box(title = "Wrangled data", collapsible = TRUE, collapsed = TRUE, width = 12,
        dataTableOutput("wrangled_data")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "4131 medicinforbrugsapp"),
  dashboardSidebar(disable = TRUE),
  body
)


# SERVER LOGIC
server <- function(input, output) {

  # REACTIVE DATA ELEMENTS ====
  parsed_data <- reactive({
  	req(input$excel_data, input$sheet, input$skip)

    d <- read_excel(input$excel_data$datapath, col_names = FALSE, sheet = input$sheet,
                    skip = round(input$skip), .name_repair = ~ paste0("X", seq_along(.)))

    # Remove rows above that with period values (normally years)
    rows_to_remove <- seq_len(which(d$X3 == "Total") - 1)
    if (length(rows_to_remove) > 0) d <- slice(d, -rows_to_remove)

    # Carry on with the rest of parsing operations
    d %>%
      setNames(c("org_unit", "drug", .[1, -(1:2)])) %>%
      slice(-(1:2)) %>%
      select(-Total) %>%
      fill(org_unit, .direction = "down") %>%
      mutate(across(-c(org_unit, drug), parse_number)) %>% # locale should be redundant with read_excel
      filter(drug != "Total")
  })

  wrangled_data <- reactive({
    req(parsed_data())

    period_type <- input$period_type %||% "year" # no input field so far
    value_type <- input$value_type %||% "amount" # no input field so far

    convert_period <- function(period_var) {
      suffix <- switch(
        period_type,
        "year" = "-12-31",
        ""
      )
      paste0(period_var, suffix)
    }

    parsed_data() %>%
      pivot_longer(-c(org_unit, drug), names_to = "period", values_to = value_type) %>%
      mutate(period = parse_date(convert_period(period), "%Y-%m-%d"))
  })

  drug_names_replacements <- reactive({
  	req(parsed_data())

    drug_names <- list()
    for (n in names(input)) {
      if (str_starts(n, "drug_name_")) drug_names[[n]] <- input[[n]]
    }
    setNames(str_remove(unlist(drug_names), "drug_name_"),
             str_remove(names(drug_names), "drug_name_"))
  })

  # REACTIVE INPUT FIELDS ====
  output$dynamic_drug_names <- renderUI({
  	req(parsed_data())

    map(
      unique(parsed_data()$drug),
      ~ textInput(paste0("drug_name_", .), label = ., value = .)
    )
  })

  output$dynamic_n_cols <- renderUI({
  	req(parsed_data())

    n_drugs <- n_distinct(parsed_data()$drug)
    numericInput("n_cols", label = "Number of panel columns", value = min(2, n_drugs),
                 min = 1, max = n_drugs, step = 1)
  })

  output$dynamic_sheet <- renderUI({
  	req(input$excel_data)

  	choices <- "Upload file first"
  	try(choices <- excel_sheets(input$excel_data$datapath), silent = TRUE)
  	selectInput("sheet", "Sheet", choices = as.list(choices), width = "100%")
  })

  output$dynamic_y_label <- renderUI({
    req(input$sheet)

    textInput("y_label", label = "Label, y axis", value = input$sheet)
  })

  output$dynamic_trial_span <- renderUI({
  	req(wrangled_data())

  	max_value <- ymd(paste0(max(year(wrangled_data()$period)), "-12-31"))
  	dateRangeInput("trial_span", label = NULL, start = max_value,
  				   end = max_value, weekstart = 1)
  })


  # REACTIVE BOXES ====
  output$dynamic_box_content <- renderUI({
  	req(input$excel_data)

    box(title = "Customise content", collapsed = is.null(input$excel_data$datapath),
        collapsible = TRUE, width = 4,
    	h4("Trial start and end"),
    	uiOutput("dynamic_trial_span"),
    	h4("Rename drug names"),
        uiOutput("dynamic_drug_names")
    )
  })

  output$dynamic_box_scales_panels <- renderUI({
  	req(input$excel_data)

    box(title = "Scales and panels", collapsed = is.null(input$excel_data$datapath),
        collapsible = TRUE, width = 4,
        uiOutput("dynamic_y_label"),
        numericInput("y_scale", "Scale of y-axis values", value = 1),
        textInput("x_label", label = "Label, x axis", value = "Periode"),
        uiOutput("dynamic_n_cols"),
        selectInput("wrap_scales", "Scales of panels",
                    choices = list("free_y", "free_x", "free", "fixed"))
    )
  })

  output$dynamic_box_appearance <- renderUI({
  	req(input$excel_data)

    box(title = "Appearance", collapsed = is.null(input$excel_data$datapath),
        collapsible = TRUE, width = 4,
        numericInput("text_size", "Text size", value = 14, min = 0, step = 1),
        numericInput("point_size", "Point size", value = 2, min = 0.1, step = 0.1),
        numericInput("line_size", "Line thickness", value = 1, min = 0.1, step = 0.1),
        numericInput("line_alpha", "Line transparency", value = 0.25, min = 0, max = 1, step = 0.1),
        numericInput("plot_height", "Plot height, in pixels", value = 400, min = 0, step = 50)
    )
  })

  # OUTPUT ELEMENTS ====
  output$parsed_data <- renderDataTable({
  	req(parsed_data())
    parsed_data()
  }, options = list(scrollX = TRUE))

  output$wrangled_data <- renderDataTable({
  	req(wrangled_data())
    wrangled_data()
  }, options = list(scrollX = TRUE))

  output$history_plot <- renderPlot({
  	req(input$excel_data, input$y_scale, input$trial_span, input$line_alpha,
  		input$point_size, input$line_size, input$text_size, input$wrap_scales,
  		input$n_cols, input$plot_height)

    df <- mutate(wrangled_data(),
                 drug = str_replace_all(drug, drug_names_replacements()))

    ggplot(df, aes(x = period, y = amount * input$y_scale, colour = org_unit)) +
    	annotate("rect", ymin = -Inf, ymax = Inf, xmin = input$trial_span[1],
    				  xmax = input$trial_span[2], alpha = 0.1) +
    	geom_line(alpha = input$line_alpha, size = input$line_size, na.rm = TRUE) +
    	geom_point(size = input$point_size, na.rm = TRUE) +
    	theme_minimal() +
    	theme(legend.title = element_blank(),
    		  text = element_text(size = input$text_size)) +
    	scale_y_continuous(labels = scales::label_number()) +
    	labs(x = input$x_label,
    		 y = input$y_label) +
    	facet_wrap(~ drug, scales = input$wrap_scales, ncol = round(input$n_cols))
  }, height = function() if (is.null(input$excel_data$datapath)) 30 else input$plot_height)

  output$box_with_plot <- renderUI({
  	req(input$excel_data, input$plot_height)

    height <- if (is.null(input$excel_data$datapath)) 30 else input$plot_height
    box(plotOutput("history_plot"), width = 12, height = height + 40)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
