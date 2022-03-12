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
