mod_assayheatmap_ui <- function(id){
  ns <- NS(id)
  # Second tab content height = 420
  fluidPage(theme = shinythemes::shinytheme("cosmo"),
                tagList(
                  sidebarPanel(
                    width = 4, #height = 503,
                    h3(strong("Evaluate sensitivity of different RT-PCR assays", style = "color:black")),
                    h5("By using well-established assays information to detect mutations in specific SARS-CoV-2 genomic sites, users could predict whether RT-PCR primers/probes bind to mutation 'hotspots', which may impact assays efficiency. Series of figures presenting the mutation profile using a single assay (demo assay) are shown.
                       Users can submit their assays by giving the primer & probe sequences. Also, you can change the geographic location parameter to see your assay performance in other area. Please make sure that your sequences are not multi-mapping before you submit. ", htmltools::a("Reference sequence: NC_045512.2", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
                    title = "Controls for global profile", status = "warning", solidHeader = TRUE,
                    
                    checkboxInput(ns("upload_primer"), "Input Your primers?", FALSE),
                    conditionalPanel(
                      condition = "input.upload_primer",
                      ns = ns,
                      textInput(ns("primer1"), label = "Input your forward primer sequence:", value = "AGTTGTGATCAACTCCGCGA"),
                      textInput(ns("primer2"), label = "Input your reverse primer sequence:", value = "TAAGACGGGCTGCACTTACA"),
                      textInput(ns("probe"), label = "Input your probe sequence:", value = "CCATGCTTCAGTCAGCTGATGCACA"),
                      selectInput(ns("probe_di"), #下拉框
                                  label = "Select your probe direction:",
                                  choices = c("forward",
                                              "reverse"
                                  ),
                                  selected = "forward"),
                      verbatimTextOutput(ns("user_primer")),
                      selectInput(ns("country_user"), #下拉框
                                  label = "Select the geographic location to present",
                                  choices = countrylist,
                                  selected = "global"),
                      selectInput(ns("lineage_p"),
                                  label = "Select the lineage to present",
                                  choices = lineagelist,
                                  selected = "all"),
                      selectInput(ns("clade_p"),
                                  label = "Select the GISAID clade to present",
                                  choices = cladelist,
                                  selected = "all"),
                      dateRangeInput(ns("daterange_p"),
                                     "Select date range:",
                                     start = date_limit[2] - 30,
                                     end = date_limit[2],
                                     min = "2019-12-01",
                                     max = date_limit[2]),
                      actionButton(ns("pr_submit"), strong("Submit"), styleclass = "success")
                    ),
                    conditionalPanel(
                      condition = "!input.upload_primer",
                      ns = ns,
                      selectInput(ns("assayType1"), #下拉框
                                  label = "Select the demo assay to present",
                                  choices = demoAssay,
                                  selected = "ChinaCDC-ORF1ab"),
                      selectInput(ns("country_default"),
                                  label = "Select the geographic location to present",
                                  choices = countrylist,
                                  selected = "global"),
                      selectInput(ns("lineage_d"),
                                  label = "Select the lineage to present",
                                  choices = lineagelist,
                                  selected = "all"),
                      selectInput(ns("clade_d"),
                                  label = "Select the GISAID clade to present",
                                  choices = cladelist,
                                  selected = "all"),
                      dateRangeInput(ns("daterange_d"),
                                     "Select date range:",
                                     start = date_limit[2] - 30,
                                     end = date_limit[2],
                                     min = "2019-12-01",
                                     max = date_limit[2]),
                      actionButton(ns("default_submit"), strong("Submit"), styleclass = "success")
                    ),
                  ),
                  mainPanel(
                    #h2(strong("Plot assay heatmap")),
                    column(
                      width = 12,
                      box(title = strong("Plot assay heatmap"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                        withSpinner(plotOutput(ns("plot4")), type = 4),
                        #除了homepage，其他用shiny的框架，注意修改
                        #verbatimTextOutput("value"),
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
                        #downloadButton(ns("assay"), "Download assay validation")
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
              )
}

mod_assayheatmap_server <- function(input, output, session){
  ns <- session$ns
  primer_list<- eventReactive(input$pr_submit, {
    primerlist <- list()
    if(input$upload_primer){
      primer_loc<- primer_align()
      countryUser <- input$country_user
      primerlist[[1]] <- primer_loc
      primerlist[[2]] <- countryUser
      primerlist[[3]] <- input$daterange_p
      primerlist[[4]] <- input$lineage_p
      primerlist[[5]] <- input$clade_p
      return(primerlist)
    }
  })
  
  assaylist<- eventReactive(input$default_submit, {
    assaylist <- list()
      assaylist[[1]] <- input$assayType1
      assaylist[[2]] <- input$country_default
      assaylist[[3]] <- input$daterange_d
      assaylist[[4]] <- input$lineage_d
      assaylist[[5]] <- input$clade_d
      return(assaylist)
  })
  
  #primer submit reactive added:
  primer_align <- reactive({
    pattern1 <- input$primer1
    pattern2 <- input$primer2
    pattern3 <- input$probe
    DNA_pair1<- matchPattern(pattern =pattern1,  refseq)@ranges
    DNA_pair2 <- matchPattern(pattern =pattern2,  refseq)@ranges
    DNA_pair3 <- matchPattern(pattern =pattern3,  refseq)@ranges
    #if ranges length > 1 output error
    if(length(DNA_pair1) > 1 | length((DNA_pair2)) >1 |length(DNA_pair3) >1){
      output$user_primer <- renderText("Your primer or probe is multi-aligned!")
    }
    if(length(DNA_pair1) == 1 | length((DNA_pair2)) == 1 |length(DNA_pair3) == 1){
      output$user_primer <- renderText("Submitted successfully!")
      primer_loc<- primerAlign(pattern1 = pattern1, pattern2 = pattern2, pattern3 = pattern3, refseq = refseq, probe_di = input$probe_di)
      
    }
    return(primer_loc)
  })

    output$plot4 <- renderPlot({
      if(!input$upload_primer){
        date <- as.character(assaylist()[[3]])
        temp <- obtainids(assaylist()[[2]], assaylist()[[4]], assaylist()[[5]], date)
        assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
        if(assaylist()[[2]] == 'global'){
          total <- homepagestat[1]
        }else{
          total <- length(temp[[1]])
        }
        dbBind(assayheatmap_stat4, temp)
        temp <- dbFetch(assayheatmap_stat4)
        dbClearResult(assayheatmap_stat4)
        AssayMutRatio(nucmerr = temp, assays = covid_assay, totalsample = total, plotType = "no", outputAssay = assaylist()[[1]], sub = FALSE)
      }
      if(input$upload_primer){
        date <- as.character(primer_list()[[3]])
        temp <- obtainids(primer_list()[[2]], primer_list()[[4]], primer_list()[[5]], date)
        assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
        if(primer_list()[[2]] == 'global'){
          total <- homepagestat[1]
        }else{
          total <- length(temp[[1]])
        }
        dbBind(assayheatmap_stat4, temp)
        temp <- dbFetch(assayheatmap_stat4)
        dbClearResult(assayheatmap_stat4)
        UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = FALSE, lastfiveNr = FALSE, totalsample = total)
      }
    })

  sub_nucmer2 <- reactive({
                          date <- as.character(assaylist()[[3]])
                          temp <- obtainids(assaylist()[[2]], assaylist()[[4]], assaylist()[[5]], date)
                          assayheatmap_statsub <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.M_type,nucmerr.PM_type,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
                          dbBind(assayheatmap_statsub, temp)
                          temp <- dbFetch(assayheatmap_statsub)
                          dbClearResult(assayheatmap_statsub)
                          sub_nucmer<- AssayMutRatio(nucmerr = temp, assays = covid_assay, totalsample = homepagestat[1], plotType = "no", outputAssay = assaylist()[[1]], sub = TRUE)
                          return(sub_nucmer)
                               })
  
  sub_nucmer3 <- reactive({
                          date = as.character(primer_list()[[3]])
                          temp <- obtainids(primer_list()[[2]], primer_list()[[4]], primer_list()[[5]], date)
                          assayheatmap_statsub <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.M_type,nucmerr.PM_type,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
                          dbBind(assayheatmap_statsub, temp)
                          temp <- dbFetch(assayheatmap_statsub)
                          dbClearResult(assayheatmap_statsub)
                          sub_nucmer<- UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = TRUE, lastfiveNr = FALSE, totalsample = homepagestat[1])
                          return(sub_nucmer)
                               })
  
  output$primer_assay <- downloadHandler(
    filename = function() {
        glue::glue("User_primer_{primer_list()[[2]]}_Fig.{input$fig_format}")
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
            temp <- obtainids(primer_list()[[2]], primer_list()[[4]], primer_list()[[5]], date)
            assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
            if(primer_list()[[2]] == 'global'){
              total <- homepagestat[1]
            }else{
              total <- length(temp[[1]])
            }
            dbBind(assayheatmap_stat4, temp)
            temp <- dbFetch(assayheatmap_stat4)
            dbClearResult(assayheatmap_stat4)
            UserPrimer(nucmerr = temp, pr = primer_list()[[1]], sub = FALSE, lastfiveNr = FALSE)

          
          dev.off()
        }
      )
    }
  )
  
  output$demo_assay <- downloadHandler(
    filename = function() {
        glue::glue("Demo_{assaylist()[[1]]}_{assaylist()[[2]]}_Fig.{input$fig_format}")
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
          
            date <- as.character(assaylist()[[3]])
            temp <- obtainids(assaylist()[[2]], assaylist()[[4]], assaylist()[[5]], date)
            assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
            if(assaylist()[[2]] == 'global'){
              total <- homepagestat[1]
            }else{
              total <- length(temp[[1]])
            }
            dbBind(assayheatmap_stat4, temp)
            temp <- dbFetch(assayheatmap_stat4)
            dbClearResult(assayheatmap_stat4)
            AssayMutRatio(nucmerr = nucmerr, assays = covid_assay, totalsample = total, plotType = "no", outputAssay = assaylist()[[1]], sub = FALSE)
          
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
        glue::glue("User_primer_{primer_list()[[2]]}_sub_nucmer.{input$ld_snp_format1}")
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
        glue::glue("Demo_{assaylist()[[1]]}_{assaylist()[[2]]}_sub_nucmer.{input$ld_snp_format2}")
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