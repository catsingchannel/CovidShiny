mod_assaydesign_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      h3(strong("Automatical RT-PCR assays design based on mutation profile",style = "color:black")),
      h5("This automatical assays design uses rprimer package"),
      selectInput(ns("country2"),
                  label = "Select the geographic location to present",
                  multiple = TRUE,
                  choices = countrylist,
                  selected = "Asia"),
      selectInput(ns("lineage2"),
                  label = "Select the lineage to present.",
                  multiple = TRUE,
                  choices = lineagelist,
                  selected = "all"),
      h5("If the total number of lineage exceed 50, only top 50 samples with the highest frequency of occurence would be chose"),
      selectInput(ns("target"),
                  label = "Select the target gene for assay design",
                  multiple = FALSE,
                  choices = protein_list,
                  selected = 'N'),
      dateRangeInput(ns("daterange_a"),
                     "Select date range:",
                     start = date_limit[2] - 365,
                     end = date_limit[2],
                     min = "2019-12-01",
                     max = date_limit[2]),
      actionButton(ns("ld_submit"), strong("Submit"), styleclass = "success")
    ),
    mainPanel(
      column(
        width = 12,
        box(title = strong("Plot generated assay information"), width = 14, solidHeader = T, status = "primary",
            withSpinner(DT::dataTableOutput(ns("assay_info")), type = 7)) 
      )
    )
    )
}

mod_assaydesign_server <- function(input, output, session){
  ns <- session$ns
  
  assay_param <- eventReactive(input$ld_submit, {
    plist <- paramInput()
    plist[[4]] <- c(as.numeric(covid_position[covid_position$Gene == plist[[4]], 'Start'] - 100):as.numeric(covid_position[covid_position$Gene == plist[[4]], 'Stop'] + 100))
    plist[[2]] <- as.character(plist[[2]])
    df <- obtain_assays(lineage = plist[[3]], country = plist[[1]], target = plist[[4]], date = plist[[2]])
    return(df)
  })
  
  paramInput <- reactive({
    palist <- list()
    palist[[1]] <- input$country2
    palist[[2]] <- input$daterange_a
    palist[[3]] <- input$lineage2
    palist[[4]] <- input$target
    return(palist)
  })
  
  output$assay_info <- renderDT({
    DT::datatable(assay_param(),
                  rownames = FALSE,
                  extensions = "FixedColumns",
                  filter = "bottom",
                  
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 1),
                    autoWidth = FALSE,
                    lengthMenu = c(5, 8, 10),
                    columnDefs = list(list(className = "dt-left", targets = "_all"))
                  ), escape = FALSE
    )
  })
}
