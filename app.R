pkgs <- c("shiny", "shinydashboard", "dplyr", "tidyr", "purrr", "readr",
          "stringr", "ggplot2", "lubridate")

for (p in pkgs) {
  if (isFALSE(requireNamespace(p, quietly = TRUE))) {
    message(p, " not available, now installing it.")
    install.packages(p, repos = "https://mirrors.dotsrc.org/cran/") # Danish mirror
  }
}

for (p in pkgs) {
  library(p, character = TRUE, repos = "http://cran.us.r-project.org")
}

dummy_data <- "		Total	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016	2017	2018	2019
Total	Total	36.236.573	5.126.129	6.241.894	6.304.735	6.253.449	5.521.455	3.576.395	1.301.713	517.134	539.275	328.389	274.743	133.215	118.047
Site1	Drug1	2.279.821	300.000	360.000	288.000	230.400	184.320	147.456	117.965	94.372	75.497	60.398	48.318	38.655	30.924
Site2	Drug1	11.102.357	1.200.000	1.440.000	1.152.000	921.600	737.280	589.824	471.859	377.487	301.990	241.592
Site3	Drug1	121.342	100.000	120.000	96.000	76.800	61.440	49.152
Site4	Drug1	8.487.857	1.300.000	1.560.000	1.248.000	998.400	798.720	638.976	511.181	408.945	327.156	261.725	209.380	167.504	134.003
Site5	Drug1	602.759	100.000	120.000	96.000	76.800	61.440	49.152	39.322	31.457					"

# Functions
parse_data <- function(data_as_string, delim = "\t", dec = ",", thousand = ".") {
  # Warnings about parsing errors is fine; stems from empty columns
  read_delim(data_as_string, col_names = FALSE, delim = delim, show_col_types = FALSE,
             col_types = cols(.default = "c")) %>%
    setNames(c("org_unit", "drug", .[1, -(1:2)])) %>%
    slice(-(1:2)) %>%
    select(-Total) %>%
    fill(org_unit, .direction = "down") %>%
    mutate(across(-c(org_unit, drug), parse_number,
                  locale = locale(decimal_mark = dec, grouping_mark = thousand))) %>%
    filter(drug != "Total")
}

wrangle_data <- function(df, period_type = "year", value_type = "amount") {
  convert_period <- function(period_var) {
    suffix <- switch(
      period_type,
      "year" = "-12-31",
      ""
    )
    paste0(period_var, suffix)
  }

  df %>%
    pivot_longer(-c(org_unit, drug), names_to = "period", values_to = value_type) %>%
    mutate(period = parse_date(convert_period(period), "%Y-%m-%d"))
}

# Define UI for application that draws a histogram
body <- dashboardBody(
  fluidRow(
    box(title = "Enter data", collapsible = TRUE, width = 12,
        textAreaInput("raw_data", label = NULL, value = dummy_data, width = "100%", rows = 5)
    )
  ),

  fluidRow(
    uiOutput("box_with_plot") # must be dynamic so user can alter height
  ),

  fluidRow(
    box(title = "Custom labels", collapsed = FALSE, collapsible = TRUE, width = 4,
      uiOutput("dynamic_drug_names")
    ),
    box(title = "Scales and panels", collapsed = FALSE, collapsible = TRUE, width = 4,
      textInput("y_label", label = "Label, y axis", value = NULL),
      numericInput("y_scale", "Scale of y-axis values", value = 1),
      textInput("x_label", label = "Label, x axis", value = "Periode"),
      uiOutput("dynamic_n_cols"),
      selectInput("wrap_scales", "Scales of panels",
                  choices = list("free_y", "free_x", "free", "fixed")),

    ),
    box(title = "Appearance", collapse = FALSE, collapsible = TRUE, width = 4,
      numericInput("text_size", "Text size", value = 16, min = 0.1, step = 1),
      numericInput("point_size", "Point size", value = 2, min = 0.1, step = 0.1),
      numericInput("line_size", "Line thickness", value = 1, min = 0.1, step = 0.1),
      numericInput("line_alpha", "Line transparency", value = 0.25, min = 0, max = 1, step = 0.1),
      numericInput("plot_height", "Plot height, in pixels", value = 400, min = 0, step = 50)
    )
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  # REACTIVE DATA ELEMENTS
  parsed_data <- reactive({
    if (input$raw_data == "") return(NULL)

    parse_data(input$raw_data)
  })

  wrangled_data <- reactive({
    if (input$raw_data == "") return(NULL)

    wrangle_data(parsed_data())
  })

  drug_names_replacements <- reactive({
    if (input$raw_data == "") return(NULL)

    drug_names <- list()
    for (n in names(input)) {
      if (str_starts(n, "drug_name_")) drug_names[[n]] <- input[[n]]
    }
    setNames(str_remove(unlist(drug_names), "drug_name_"),
             str_remove(names(drug_names), "drug_name_"))
  })

  # REACTIVE INPUT FIELDS
  output$dynamic_drug_names <- renderUI({
    if (input$raw_data == "") return(NULL)

    map(
      unique(parsed_data()$drug),
      ~ textInput(paste0("drug_name_", .), label = ., value = .)
    )
  })

  output$dynamic_n_cols <- renderUI({
    if (input$raw_data == "") return(NULL)

    n_drugs <- n_distinct(parsed_data()$drug)
    numericInput("n_cols", label = "Number of panel columns", value = min(2, n_drugs),
                min = 1, max = n_drugs, step = 1)
  })

  # OUTPUT ELEMENTS
  output$parsed_data <- renderDataTable({
    parsed_data()
  }, options = list(scrollX = TRUE))

  output$wrangled_data <- renderDataTable({
    wrangled_data()
  }, options = list(scrollX = TRUE))

  output$history_plot <- renderPlot({
    if (input$raw_data == "") return(ggplot() + theme_void())

    df <- mutate(wrangled_data(),
                 drug = str_replace_all(drug, drug_names_replacements()))

    ggplot(df, aes(x = period, y = amount * input$y_scale, colour = org_unit)) +
      geom_line(alpha = input$line_alpha, size = input$line_size) +
      geom_point(size = input$point_size) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            text = element_text(size = input$text_size)) +
      scale_y_continuous(labels = scales::label_number()) +
      labs(x = input$x_label,
           y = input$y_label) +
      facet_wrap(~ drug, scales = input$wrap_scales, ncol = round(input$n_cols))
  }, height = function() input$plot_height)

  output$box_with_plot <- renderUI({
    box(plotOutput("history_plot"), width = 12, height = input$plot_height + 40)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
