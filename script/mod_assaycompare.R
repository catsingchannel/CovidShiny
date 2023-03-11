
mod_assaycompare_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      
      h3(strong("Compare different RT-PCR assays mutation ratio",style = "color:black")),
      h5("Users can submit assay list in txt format to compare assays performance. Please make sure that the column name of your assay list contains 'Assay'(assay name) and 'F1', 'F2', 'R1', 'R2', 'P1','P2'(start and end of oligos in reference genome). 'F1'&'F2' represent start(5') and end(3') of forward primer, 'R1'&'R2' represent start(3') and end(5') of reverse primers, 'P1'&'P2' represent start and end of probe(5' and 3' depend on the direction)."),
      checkboxInput(ns("upload_sample"), "Upload Your Samples?", FALSE),
      conditionalPanel(
        condition = "input.upload_sample",
        ns = ns,
        fileInput(ns("sample"), "Upload samples.Leave blank for example run.",
                  multiple = FALSE,
                  accept = c(
                    ".txt"
                  ),
                  placeholder = "data/Assays.txt"
        ),
      ),
      br(),
      h4("Global mutation ratio in the SARS-CoV-2 genome is calculated for each assay, this figure is for comparison."),
      selectInput(ns("country2"), #下拉框
                  label = "Select the geographic location to present",
                  choices = countrylist,
                  selected = "global"),
      selectInput(ns("lineage2"),
                  label = "Select the lineage to present",
                  choices = lineagelist,
                  selected = "all"),
      selectInput(ns("clade2"),
                  label = "Select the GISAID clade to present",
                  choices = cladelist,
                  selected = "all"),
      dateRangeInput(ns("daterange_a"),
                     "Select date range:",
                     start = date_limit[2] - 30,
                     end = date_limit[2],
                     min = "2019-12-01",
                     max = date_limit[2]),
      
      actionButton(ns("ld_submit"), strong("Submit"), styleclass = "success")
      
    ),
    mainPanel(
      column(
        width = 12,
        box(title = strong("Plot assay barplot"), width = 14, solidHeader = T, status = "primary",
            withSpinner(plotOutput(ns("plot6")), type = 4)) 
      )
    )
  )
}

mod_assaycompare_server <- function(input, output, session){
  ns <- session$ns
  
  assay_compare <- eventReactive(input$ld_submit, {
    assaylist2 <- assayInput()
    return(assaylist2)
    })
  
  assayInput <- reactive({
    samplelist <- list()
    if (input$upload_sample) {
      samplelist[[1]] <- readNewData(fileinfo = input$sample)
    } else {
      samplelist[[1]] <- covid_assay
    }
    samplelist[[2]] <- input$country2
    samplelist[[3]] <- input$daterange_a
    samplelist[[4]] <- input$lineage2
    samplelist[[5]] <- input$clade2
    return(samplelist)
  })
  
  output$plot6 <- renderPlot({
    date <- as.character(assay_compare()[[3]])
    temp <- obtainids(assay_compare()[[2]], assay_compare()[[4]], assay_compare()[[5]], date)
    assaycopmare_stat6 <- dbSendQuery(dbcon, 'SELECT ID,rpos FROM nucmerr WHERE [ID] = $ids')
    if(assay_compare()[[2]] == 'global'){
      total <- homepagestat[1]
    }else{
      total <- length(temp[[1]])
    }
    dbBind(assaycopmare_stat6, temp)
    temp <- dbFetch(assaycopmare_stat6)
    dbClearResult(assaycopmare_stat6)
    AssayMutRatio(nucmerr = temp, assays = assay_compare()[[1]], totalsample = total, plotType = "logtrans", outputAssay = NULL, sub = FALSE)
  })
}
