mod_doubleAssay_ui <- function(id){
  ns <- NS(id)
  # Second tab content height = 420
  tagList(
    sidebarPanel(
      width = 4, #height = 503,
      h3(strong("Detection of co-occurring mutations using double-assay information", style = "color:black")),
      h5("Using double assays to cover more samples, as co-occurring mutations are much less than single-mutated samples. Users can submit assay list in txt format to choose pair assay to evaluate its performance. Please make sure that the column name of your assay list contains 'Assay'(assay name)and 'F1', 'F2', 'R1', 'R2', 'P1','P2'(start and end of oligos in reference genome). 'F1'&'F2' represent start(5') and end(3') of forward primer, 'R1'&'R2' represent start(3') and end(5') of reverse primers, 'P1'&'P2' represent start and end of probe(5' and 3' depend on the direction)."),
      title = "Controls for double-mutated sample detection", status = "warning", solidHeader = TRUE,
      
      checkboxInput(ns("upload_assay"), "Upload Your assay?", FALSE),
      conditionalPanel(
        condition = "input.upload_assay",
        ns = ns,
        fileInput(ns("double_assay"), "Upload samples.Leave blank for example run.",
                  multiple = FALSE,
                  accept = c(
                    ".txt"
                  ),
                  placeholder = "data/Assays.txt"
        ),
        
        numericInput(ns("assay1"), "Choose your 1st assay", value = "1"),
        verbatimTextOutput(ns("user_assay1")),
        numericInput(ns("assay2"), "Choose your 2nd assay", value = "1"),
        verbatimTextOutput(ns("user_assay2")),
        selectInput(ns("ld_snp_format"),
                    "Download data as:",
                    choices = available_data_formats,
                    selected = NULL,
                    selectize = TRUE
        )
        
        
      ),
      conditionalPanel(
        condition = "!input.upload_assay",
        ns = ns,
        selectInput(ns("eg_assay1"), #下拉框
                    label = "Select the first assay",
                    choices = demoAssay,
                    selected = "ChinaCDC-ORF1ab"),
        selectInput(ns("eg_assay2"), 
                    label = "Select the second assay",
                    choices = demoAssay,
                    selected = "ChinaCDC-N"),
        selectInput(ns("ld_snp_format"),
                    "Download data as:",
                    choices = available_data_formats,
                    selected = NULL,
                    selectize = TRUE
        )
      ),
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
      
      actionButton(ns("ld2_submit"), strong("Submit"), styleclass = "success")
      
      
    ),
    
    mainPanel(
      
      column(width = 12,
             box(title = strong("Plot double assay heatmap"), width = 14, solidHeader = T, status = "primary",collapsible = T,
                 withSpinner(plotOutput(ns("plot11"), height = 500), type = 4),
                 #除了homepage，其他用shiny的框架，注意修改
                 selectInput(ns("fig_format"),
                             "Download figure as:",
                             choices = available_fig_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 downloadButton(ns("double_download"), "Download double-assay validation"),
                 
             ),
             box(title = strong("Double assay vennplot"), width = 14, solidHeader = T, status = "primary",collapsible = T,
                 withSpinner(plotOutput(ns("plot12"), height = 400), type = 4) 
               
             ),
             
             
             h4("This table is dynamic corresponding to your assay output. Samples with double mutations are shown below."),
             box(title = strong("Samples carrying mutations in double assay"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                 footer = 
                   h5("sample: sample ID;", br(), "time: reported time;",
                      br(), "location: geographic location;", br(), "PM_type: mutation types in genomic position"),    
                 withSpinner(DT::dataTableOutput(ns("sub_nucmer_info")), type = 7),
                 br(),
                 selectInput(ns("ld_snp_format"),
                             "Download data as:",
                             choices = available_data_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 downloadButton(ns("ld_data_download"), "Download sub_nucmer Data"),
             )
      )
    )
  )
}

mod_doubleAssay_server <- function(input, output, session){
  ns <- session$ns
  double_assaylist<- eventReactive(input$ld2_submit, {
    assaylist <- list()
    if(input$upload_assay){
      assaylist[[1]] <- as.character(assayInput()$Assay[input$assay1])
      assaylist[[2]] <- as.character(assayInput()$Assay[input$assay2])
    }else{
      assaylist[[1]] <- input$eg_assay1
      assaylist[[2]] <- input$eg_assay2
    }
    date <- as.character(input$daterange)
    assaylist[[4]] <- input$daterange
    temp <- obtainids(input$country, input$lineage, input$clade, date)
    assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
    dbBind(assayheatmap_stat4, temp)
    assaylist[[3]] <- dbFetch(assayheatmap_stat4)
    assaylist[[5]] <- input$country
    assaylist[[6]] <- input$lineage
    assaylist[[7]] <- input$clade
    dbClearResult(assayheatmap_stat4)
    return(assaylist)
  })
  
  output$plot11 <- renderPlot({
    doubleAssay(nucmerr = double_assaylist()[[3]], assay = assayInput(), assay1 = double_assaylist()[[1]], assay2 = double_assaylist()[[2]], outdir = NULL, sub = FALSE)
  })
  
  output$plot12 <- renderPlot({
    re<- doubleAssay(nucmerr = double_assaylist()[[3]], assay = assayInput(), assay1 = double_assaylist()[[1]], assay2 = double_assaylist()[[2]], outdir = NULL, sub = FALSE, venn = TRUE)
  })
  
  output$user_assay1 <- renderText(as.character(assayInput()$Assay[input$assay1]))
  
  output$user_assay2 <- renderText(as.character(assayInput()$Assay[input$assay2]))
  
  assayInput <- reactive({
    if (input$upload_assay) {
      sample <- readNewData(fileinfo = input$double_assay)
    } else {
      sample <- covid_assay
    }
    return(sample)
  })
  
  output$double_download <- downloadHandler(
    filename = function() {
      glue::glue("{double_assaylist()[[1]]}_{double_assaylist()[[2]]}_doubleAssay.{input$fig_format}")
    },
    content = function(file) {#这里的file实际上指代filename
      withProgress(
        message = "Download in progress",
        detail = "Please wait a while ...",
        value = 0,
        {
          for (i in 1:10) {
            incProgress(1 / 10)
            Sys.sleep(0.01)
          }
          if (input$fig_format == "png") {
            png(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format == "pdf") {
            pdf(file, width = 12, height = 8, onefile = F)
          } else if (input$fig_format == "jpeg") {
            jpeg(file, width = 8 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format == "tiff") {
            tiff(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format == "bmp") {
            bmp(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else {
            svg(file)
          }
          doubleAssay(nucmerr = double_assaylist()[[3]], assay = assayInput(), assay1 = double_assaylist()[[1]], assay2 = double_assaylist()[[2]], outdir = NULL, sub = FALSE)
          dev.off()
        }
      )
    }
  )
  
  sub_nucmer <- eventReactive(input$ld2_submit,
                              {
                                date <- as.character(double_assaylist()[[4]])
                                temp <- obtainids(double_assaylist()[[5]], double_assaylist()[[6]], double_assaylist()[[7]], date)
                                doubleassay_statsub <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.PM_type,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
                                dbBind(doubleassay_statsub, temp)
                                temp <- dbFetch(doubleassay_statsub)
                                overlap<- doubleAssay(nucmerr = temp, assay = assayInput(), assay1 = double_assaylist()[[1]], assay2 = double_assaylist()[[2]], outdir = NULL, sub = TRUE)
                                overlap <- overlap[]
                                return(overlap)
                              }
                              
  )
  
  output$sub_nucmer_info <- renderDT({
    DT::datatable(sub_nucmer()[,c("ID", "country.x", "time.x","PM_type.x", "PM_type.y")],
                  colnames = c("ID", "location", "time", glue::glue("PM_type_{double_assaylist()[[1]]}"), glue::glue("PM_type_{double_assaylist()[[2]]}")),
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
  
  output$ld_data_download <- downloadHandler(
    filename = function() {
      glue::glue("{double_assaylist()[[1]]}_{double_assaylist()[[2]]}_doubleAssay.{input$ld_snp_format}")
    },
    content = function(file) {
      sub_nucmer<- sub_nucmer()[,c("sample", "country.x", "time.x","PM_type.x", "PM_type.y")]
      colnames(sub_nucmer) <- c("sample", "location", "time", "PM_type_assay1", "PM_type_assay2")
      if (input$ld_snp_format == "txt") {
        write.table(sub_nucmer, file, row.names = F, col.names = T, quote = F)
      } else if (input$ld_snp_format == "csv") {
        readr::write_csv(sub_nucmer, file, col_names = T)
      } else if (input$ld_snp_format == "tsv") {
        readr::write_tsv(sub_nucmer, file, col_names = T)
      } else {
        writexl::write_xlsx(sub_nucmer, file, col_names = T)
      }
    }
  )
}
