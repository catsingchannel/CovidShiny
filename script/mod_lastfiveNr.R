

mod_lastfiveNr_ui <- function(id){
  ns <- NS(id)
  # Second tab content height = 420
  tagList(
    sidebarPanel(
      
      width = 4, #height = 503,
      h3(strong("Evaluate sensitivity of different RT-PCR primers", style = "color:black")),
      h5("The global profile is for using the well established assays information to detect mutations in different SARS-CoV-2 genomic sites. The output can be series of figures presenting the mutation profile using a specific assay.", htmltools::a("Reference sequence: NC_045512.2", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
      title = "Controls for global profile", status = "warning", solidHeader = TRUE,

      checkboxInput(ns("upload_primer"), "Input Your primers?", FALSE),
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
      conditionalPanel(
        condition = "input.upload_primer",
        ns = ns,
        textInput(ns("primer1"), label = "Input your first primer sequence:", value = "AGTTGTGATCAACTCCGCGA"),
        textInput(ns("primer2"), label = "Input your second primer sequence:", value = "TAAGACGGGCTGCACTTACA"),
        verbatimTextOutput(ns("user_primer")),
        actionButton(ns("pr_submit"), strong("Submit"), styleclass = "success")
      ),
      conditionalPanel(
        condition = "!input.upload_primer",
        ns = ns,
        selectInput(ns("assayType2"), #下拉框
                    label = "Select the assay type to present",
                    choices = demoAssay,
                    selected = "ChinaCDC-ORF1ab"),
        actionButton(ns("default_submit"), strong("Submit"), styleclass = "success")
      ),
      br(),
      h5("The last five nucleotides of primers targeting SARS-CoV-2 genome is critical for primer binding capacity and RT-PCR specificity. Users can input their own primers to assess primers performance in silico. Please make sure that your primers are unique-aligned."),

      br()
    ),
    
    mainPanel(
      column(width = 12,
             box(title = strong("Plot assay heatmap"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                 withSpinner(plotOutput(ns("plot5")), type = 4),
                 selectInput(ns("fig_format"),
                             "Download figure as:",
                             choices = available_fig_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 conditionalPanel(
                   condition = "input.upload_primer",
                   ns = ns,
                   downloadButton(ns("primer_assay"), "Download primer validation") 
                 ),
                 conditionalPanel(
                   condition = "!input.upload_primer",
                   ns = ns,
                   downloadButton(ns("demo_assay"), "Download primer validation") 
                 )
                 
             ),
             h4("This table is dynamic corresponding to your assay output. All mutations shown on the figure are included in the table."),
             box(title = strong("Samples carrying mutations in single assay"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                 footer = 
                   h5("sample: sample ID;", br(), "time: reported time;",
                      br(), "country: geographic location;", br(), "M_type: mutation types", br(),
                      "PM_type: mutation types in genomic position"),    
                 
                 
                 
                 
                 conditionalPanel(
                   condition = "input.upload_primer",
                   ns = ns,
                   withSpinner(DT::dataTableOutput(ns("sub_nucmer_info1")), type = 7),
                   selectInput(ns("ld_snp_format1"),
                               "Download data as:",
                               choices = available_data_formats,
                               selected = NULL,
                               selectize = TRUE
                   ),
                   downloadButton(ns("ld_data_download"), "Download sub_nucmer Data")
                 ),
                 conditionalPanel(
                   condition = "!input.upload_primer",
                   ns = ns,
                   withSpinner(DT::dataTableOutput(ns("sub_nucmer_info2")), type = 7),
                   selectInput(ns("ld_snp_format2"),
                               "Download data as:",
                               choices = available_data_formats,
                               selected = NULL,
                               selectize = TRUE
                   ),
                   downloadButton(ns("ld_data_download_demo"), "Download sub_nucmer Data")
                 )
             )
)
    )
  )
}

mod_lastfiveNr_server <- function(input, output, session){
  ns <- session$ns

  assaylist2<- eventReactive(input$default_submit, {
    assay <- list()
    assay[[1]] <- input$assayType2
    assay[[2]] <- input$daterange
    assay[[3]] <- input$country
    assay[[4]] <- input$lineage
    assay[[5]] <- input$clade

    return(assay)
  })
  
  primer_align <- reactive({
    pattern1 <- input$primer1
    pattern2 <- input$primer2
    DNA_pair1<- matchPattern(pattern =pattern1,  refseq)@ranges
    DNA_pair2 <- matchPattern(pattern =pattern2,  refseq)@ranges
    if(length(DNA_pair1) > 1 | length((DNA_pair2)) >1){
      output$user_primer <- renderText("Your primer or probe is multi-aligned!")
    }
    if(length(DNA_pair1) == 1 | length((DNA_pair2)) == 1){
      output$user_primer <- renderText("Submitted successfully!")
      primer_loc<- primerAlign(pattern1 = pattern1, pattern2 = pattern2, pattern3 = NULL, refseq = refseq, probe_di = "not available")
    }
    return(primer_loc)
  })
  
  primer_list<- eventReactive(input$pr_submit, {
    primerlist <- list()
    if(input$upload_primer){
      primer_loc<- primer_align()
      primerlist[[1]] <- primer_loc
      primerlist[[2]] <- input$daterange
      primerlist[[3]] <- input$country
      primerlist[[4]] <- input$lineage
      primerlist[[5]] <- input$clade
      return(primerlist)
    }
  })

  sub_nucmer2 <- reactive({
                          date <- as.character(assaylist2()[[2]])
                          temp <- obtainids(assaylist2()[[3]], assaylist2()[[4]], assaylist2()[[5]], date)
                          
                          assayheatmap_statsub <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.M_type,nucmerr.PM_type,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
                          if(assaylist2()[[3]] == 'global'){
                            total <- homepagestat[1]
                          }else{
                            total <- length(temp[[1]])
                          }
                          dbBind(assayheatmap_statsub, temp)
                          temp <- dbFetch(assayheatmap_statsub)
                          dbClearResult(assayheatmap_statsub)
                          sub_nucmer<- LastfiveNrMutation(nucmerr = temp, assays = covid_assay, totalsample = homepagestat[[1]], outputAssay = assaylist2()[[1]], sub = TRUE)
                          return(sub_nucmer)
                          })
  
  sub_nucmer3 <- reactive({
                          date <- as.character(primer_list()[[2]])
                          temp <- obtainids(primer_list()[[3]], primer_list()[[4]], primer_list()[[5]], date)
                          
                          assayheatmap_statsub <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.M_type,nucmerr.PM_type,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
                          if(assaylist2()[[3]] == 'global'){
                            total <- homepagestat[1]
                          }else{
                            total <- length(temp[[1]])
                          }
                          dbBind(assayheatmap_statsub, temp)
                          temp <- dbFetch(assayheatmap_statsub)
                          dbClearResult(assayheatmap_statsub)
                          sub_nucmer<- UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = TRUE, country = "global", lastfiveNr = TRUE, totalsample = total)
                          return(sub_nucmer)
                          })

  output$plot5 <- renderPlot({
    if(input$upload_primer == FALSE){
      date <- as.character(assaylist2()[[2]])
      temp <- obtainids(assaylist2()[[3]], assaylist2()[[4]], assaylist2()[[5]], date)
      
      assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
      if(assaylist2()[[3]] == 'global'){
        total <- homepagestat[1]
      }else{
        total <- length(temp[[1]])
      }
      dbBind(assayheatmap_stat4, temp)
      temp <- dbFetch(assayheatmap_stat4)
      dbClearResult(assayheatmap_stat4)
      LastfiveNrMutation(nucmerr = temp, assays = covid_assay, totalsample = total, outputAssay = assaylist2()[[1]])
    }
    if(input$upload_primer){
      date <- as.character(primer_list()[[2]])
      temp <- obtainids(primer_list()[[3]], primer_list()[[4]], primer_list()[[5]], date)
      assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
      if(assaylist2()[[3]] == 'global'){
        total <- homepagestat[1]
      }else{
        total <- length(temp[[1]])
      }
      dbBind(assayheatmap_stat4, temp)
      temp <- dbFetch(assayheatmap_stat4)
      dbClearResult(assayheatmap_stat4)
      UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = FALSE, country = "global", lastfiveNr = TRUE, totalsample = total)
    }
    
  })
  
  output$demo_assay <- downloadHandler(
    filename = function() {
      glue::glue("Demo_{assaylist2()[[1]]}(LastfiveNR)_primer_Fig.{input$fig_format}")
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
            date <- as.character(assaylist2()[[2]])
            temp <- obtainids(assaylist2()[[3]], assaylist2()[[4]], assaylist2()[[5]], date)
            
            assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
            if(assaylist2()[[3]] == 'global'){
              total <- homepagestat[1]
            }else{
              total <- length(temp[[1]])
            }
            dbBind(assayheatmap_stat4, temp)
            temp <- dbFetch(assayheatmap_stat4)
            dbClearResult(assayheatmap_stat4)
            LastfiveNrMutation(nucmerr = temp, assays = covid_assay, totalsample = homepagestat[[1]], outputAssay = assaylist2()[[1]])
          dev.off()
        }
      )
    }
  )
  
  output$primer_assay <- downloadHandler(
    filename = function() {
      glue::glue("User_primer(LastfiveNR)_Fig.{input$fig_format}")
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
          
            date <- as.character(primer_list()[[3]])
            temp <- obtainids(primer_list()[[3]], primer_list()[[4]], primer_list()[[5]], date)
            assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
            if(assaylist2()[[3]] == 'global'){
              total <- homepagestat[1]
            }else{
              total <- length(temp[[1]])
            }
            dbBind(assayheatmap_stat4, temp)
            temp <- dbFetch(assayheatmap_stat4)
            dbClearResult(assayheatmap_stat4)
            UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = FALSE, country = "global", lastfiveNr = TRUE, totalsample = total)
         
          dev.off()
        }
      )
    }
  )
  
  output$sub_nucmer_info1 <- renderDT({
    DT::datatable(sub_nucmer3()[,c(1:5)],
                  colnames = c("sample", "time", "location", "M_type", "PM_type"),
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
  
  output$sub_nucmer_info2 <- renderDT({
    DT::datatable(sub_nucmer2()[,c(1:5)],
                  colnames = c("sample", "time", "location", "M_type", "PM_type"),
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
      glue::glue("User_primer(LastfiveNR)_sub_nucmer.{input$ld_snp_format1}")
      
    },
    content = function(file) {
      if (input$ld_snp_format1 == "txt") {
        write.table(sub_nucmer3()[,c(1:5)], file, row.names = F, col.names = T, quote = F)
      } else if (input$ld_snp_format1 == "csv") {
        readr::write_csv(sub_nucmer3()[,c(1:5)], file, col_names = T)
      } else if (input$ld_snp_format1 == "tsv") {
        readr::write_tsv(sub_nucmer3()[,c(1:5)], file, col_names = T)
      } else {
        writexl::write_xlsx(sub_nucmer3()[,c(1:5)], file, col_names = T)
      }
    }
  )
  
  output$ld_data_download_demo <- downloadHandler(
    filename = function() {
      glue::glue("Demo_{assaylist2()[[1]]}(LastfiveNR)_primer_sub_nucmer.{input$ld_snp_format2}")
    },
    content = function(file) {
      if (input$ld_snp_format2 == "txt") {
        write.table(sub_nucmer2()[,c(1:5)], file, row.names = F, col.names = T, quote = F)
      } else if (input$ld_snp_format2 == "csv") {
        readr::write_csv(sub_nucmer2()[,c(1:5)], file, col_names = T)
      } else if (input$ld_snp_format2 == "tsv") {
        readr::write_tsv(sub_nucmer2()[,c(1:5)], file, col_names = T)
      } else {
        writexl::write_xlsx(sub_nucmer2()[,c(1:5)], file, col_names = T)
      }
    }
  )
}
