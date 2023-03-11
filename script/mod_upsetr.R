
mod_upsetr_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
      width = 4, 
      h3(strong("UpsetR plot for proteins", style = "color:black")),
      h5("In this panel, all SARS-CoV-2 mutation data regardless of sequence quality (complete, high coverage) are processed in order to track protein variants.",
         htmltools::a("Reference sequence: NC_045512.29(SARS-nCoV-19)", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
      title = "", status = "warning", solidHeader = TRUE,
      
      selectInput(ns("genes"),
                  label = "Select the gene for counting the mutations",
                  choices = protein_list,
                  selected = "S"),
      sliderInput(ns("top"), 
                  "Number of SNPs to present:", 
                  1, 25, 10),
      dateRangeInput(ns("daterange"),
                     "Select date range:",
                     start = date_limit[2] - 30,
                     end = date_limit[2],
                     min = "2019-12-01",
                     max = date_limit[2]),
      selectInput(ns("country"),
                  label = "Select the geographic location to present",
                  choices = countrylist,
                  selected = "global"),
      selectInput(ns("lineage"),
                  label = "Select the lineage to present",
                  choices = lineagelist,
                  selected = "all"),
      selectInput(ns("clade"),
                  label = "Select the GISAID clade to present",
                  choices = cladelist,
                  selected = "all"),
      actionButton(ns("submit"), 
                   strong("Submit"), 
                   styleclass = "success"),
      
      br(),
      h5(""),
      br()
    ),
    
    mainPanel(
      column(width = 12,
             box(title = strong("Mutation tracking for selected protein"), 
                 width = 14, 
                 solidHeader = T, 
                 status = "primary",
                 withSpinner(plotOutput(ns("plot1"), height = 600), type = 4)
                 )
             )
      )
    
  )
}


mod_upsetr_server <- function(input, output, session){
  ns <- session$ns
  
  submit_list <- eventReactive(input$submit, {
    L <- list()
    
    L[[1]] <- input$country
    L[[2]] <- input$genes
    L[[3]] <- input$top
    L[[4]] <- input$daterange
    L[[5]] <- input$lineage
    L[[6]] <- input$clade
    return(L)
  })
  
  output$plot1 <- renderPlot({
    date <- as.character(submit_list()[[4]])
    temp <- obtainids(submit_list()[[1]], submit_list()[[5]], submit_list()[[6]], date)
    upsetr_stat <- dbSendQuery(dbcon, 'SELECT protein,variant,ID FROM covid_annot WHERE [ID] = $ids')
    dbBind(upsetr_stat, temp)
    temp <- dbFetch(upsetr_stat)
    dbClearResult(upsetr_stat)
    upsetr(temp, top = submit_list()[[3]], protein = submit_list()[[2]])
  })
}