mod_globalSNP_ui <- function(id){
  ns <- NS(id)
  # Second tab content height = 420
  navbarPage(title = "",
             tabPanel("SNP profile",icon = icon("dna"),
               tagList(
                 sidebarPanel(
                   width = 4,
                   h3(strong("Global SNP profile")),
                   h5("In this section, we generated a global single nucleotide polymorphism (SNP) profile of SARS-CoV-2. In this profile, some mutations in specific loci occur at high frequency, shown as a significant line in the heatmap. You can change the location and number of top mutated SNPs in the control panel. ", htmltools::a("Reference sequence: NC_045512.2", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
                   br(),
                   selectInput(ns("globalfigure"), #下拉框
                               label = "Select the figure type for display",
                               choices = c("heatmap",
                                           "count"
                               ),
                               selected = "count"),
                   selectInput(ns("country_user"), #下拉框
                               label = "Select the geographic location to present",
                               choices = countrylist,
                               selected = "global"),
                   sliderInput(ns("top"), "Number of SNPs to present:", 1, 25, 8),
                   
                   dateRangeInput(ns("daterange"),
                                  "Select date range:",
                                  start = date_limit[2] - 21,
                                  end = date_limit[2],
                                  min = "2019-12-01",
                                  max = date_limit[2]),
                   selectInput(ns("lineage"),
                               label = "Select the lineage to present",
                               choices = lineagelist,
                               selected = "all"),
                   selectInput(ns("clade"),
                               label = "Select the GISAID clade to present",
                               choices = cladelist,
                               selected = "all"),
                   
                   br()
                 ),
                 mainPanel(
                   
                   column(
                     
                     width = 12,
                     box(title = strong("Plot SNP mutation profile"), width = 14, solidHeader = T, status = "primary",
                         withSpinner(plotOutput(ns("plot9")), type = 4),
                         img(src = "img/Genome.png", height = 100, width = "100%"),#how to adjust the width?
                         br(),
                         br(),
                         selectInput(ns("fig_format1"),
                                     "Download figure as:",
                                     choices = available_fig_formats,
                                     selected = NULL,
                                     selectize = TRUE
                         ),
                         downloadButton(ns("SNP_download"), "Download SNP mutation profile"),
                         
                     ),
                     h4("This table is dynamic corresponding to the SNP profile above. "),
                     box(title = strong("Samples carrying SNP mutations"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                         footer = 
                           h5("sample: sample ID;", br(), "time: reported time;",
                              br(), "country: geographic location;", br(), "M_type: mutation types", br(),
                              "PM_type: mutation types in genomic position"),
                         
                         
                         
                         
                         withSpinner(DT::dataTableOutput(ns("sub_nucmer_info1")), type = 7),
                         selectInput(ns("ld_snp_format1"),
                                     "Download data as:",
                                     choices = available_data_formats,
                                     selected = NULL,
                                     selectize = TRUE
                         ),
                         downloadButton(ns("ld_data_download1"), "Download sub_nucmer Data")
                     )
                   )
                 )
               )
             ),
             tabPanel("Protein profile",icon = icon("pills"),
                      tagList(
                        sidebarPanel(
                          width = 4,
                          h3(strong("Global protein mutation profile")),
                          h5("This heatmap is for the top mutated events occurred in proteins encoded by SARS-CoV-2 mRNA. These events may be considered as potential targets for clinical therapy, as some mutations are critical for virus infection and transmission. (e.g. D614G.). ", htmltools::a("Reference sequence: NC_045512.2", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
                          br(),
                          selectInput(ns("globalpro"), #下拉框
                                      label = "Select the figure type for display",
                                      choices = c("heatmap",
                                                  "count"
                                      ),
                                      selected = "count"),
                          selectInput(ns("country"), #下拉框
                                      label = "Select the geographic location to present",
                                      choices = countrylist,
                                      selected = "global"),
                          sliderInput(ns("Top_mutPro"),"Number of protein mutations:", 1, 30, 10),
                          
                          dateRangeInput(ns("daterange_2"),
                                         "Select date range:",
                                         start = date_limit[2] - 30,
                                         end = date_limit[2],
                                         min = "2019-12-01",
                                         max = date_limit[2]),
                          selectInput(ns("lineage_2"),
                                      label = "Select the lineage to present",
                                      choices = lineagelist,
                                      selected = "all"),
                          selectInput(ns("clade_2"),
                                      label = "Select the GISAID clade to present",
                                      choices = cladelist,
                                      selected = "all"),
                          
                          br()
                          
                        ),
                        
                        mainPanel(
                          
                          column(width = 12,
                                 box(title = strong("Plot protein mutation heatmap"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                                     withSpinner(plotOutput(ns("plot10")), type = 4),
                                     selectInput(ns("fig_format2"),
                                                 "Download figure as:",
                                                 choices = available_fig_formats,
                                                 selected = NULL,
                                                 selectize = TRUE
                                     ),
                                     downloadButton(ns("pro_download"), "Download protein mutation profile")
                                    
                                 ),
                                 h4("This table is dynamic corresponding to the protein profile above. "),
                                 box(title = strong("Samples carrying protein mutations"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                                     footer = 
                                       h5("sample: sample ID;", br(), "refpos: reference nucleotide position;",
                                          br(), "refvar: reference nucleotide in genome;", br(), "qvar: query nucleotide in genome", br(), "qpos: query nucleotide position", br(), "qlength: query genome length", br(), "protein: mutated protein", br(), "variant: mutation class for protein", br(), "varclass: mutation type for protein", br(), "annotation: protein name", br(), "pro_variant: variants in protein", br(), "ID: simplified sample ID"),    
                                     
                                     
                                     
                                     
                                     withSpinner(DT::dataTableOutput(ns("sub_nucmer_info")), type = 7),
                                     selectInput(ns("ld_snp_format"),
                                                 "Download data as:",
                                                 choices = available_data_formats,
                                                 selected = NULL,
                                                 selectize = TRUE
                                     ),
                                     downloadButton(ns("ld_data_download"), "Download sub_nucmer Data")
                                 )
                          )
                        )
                      )
             )
             )
}

mod_globalSNP_server <- function(input, output, session){
  ns <- session$ns
  
  output$plot9 <- renderPlot({  #The global heatmap costs more time
    date <- as.character(input$daterange)
    temp <- obtainids(input$country_user, input$lineage, input$clade, date)
    assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
    dbBind(assayheatmap_stat4, temp)
    temp <- dbFetch(assayheatmap_stat4)
    dbClearResult(assayheatmap_stat4)
    globalSNPprofile(nucmerr = temp, figure_Type = input$globalfigure, outdir = NULL, top = input$top)
  })
  
  output$plot10 <- renderPlot({
    date <- as.character(input$daterange_2)
    temp <- obtainids(input$country, input$lineage_2, input$clade_2, date)
    globalsnp_stat10 <- dbSendQuery(dbcon, 'SELECT pro_variant,ID,refpos FROM covid_annot WHERE [ID] = $ids')
    dbBind(globalsnp_stat10, temp)
    temp <- dbFetch(globalsnp_stat10)
    dbClearResult(globalsnp_stat10)
    globalProteinMut(covid_annot = temp, outdir = NULL, figure_Type = input$globalpro, top = input$Top_mutPro)
  })
  
  output$SNP_download <- downloadHandler(
    filename = function() {
      glue::glue("Top{input$top}_SNP_Mutation_{input$country_user}.{input$fig_format1}")
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
          if (input$fig_format1 == "png") {
            png(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format1 == "pdf") {
            pdf(file, width = 12, height = 8, onefile = F)
          } else if (input$fig_format1 == "jpeg") {
            jpeg(file, width = 8 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format1 == "tiff") {
            tiff(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format1 == "bmp") {
            bmp(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else {
            svg(file)
          }
          date <- as.character(input$daterange)
          temp <- obtainids(input$country_user, input$lineage, input$clade, date)
          assayheatmap_stat4 <- dbSendQuery(dbcon, 'SELECT ID,M_type,rpos FROM nucmerr WHERE [ID] = $ids')
          dbBind(assayheatmap_stat4, temp)
          temp <- dbFetch(assayheatmap_stat4)
          dbClearResult(assayheatmap_stat4)
          globalSNPprofile(nucmerr = temp, figure_Type = input$globalfigure, outdir = NULL, top = input$top)
          dev.off()
        }
      )
    }
  )
  
  output$pro_download <- downloadHandler(
    filename = function() {
      glue::glue("Top{input$Top_mutPro}_protein_Mutation_{input$country}.{input$fig_format2}")
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
          if (input$fig_format2 == "png") {
            png(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format2 == "pdf") {
            pdf(file, width = 12, height = 8, onefile = F)
          } else if (input$fig_format2 == "jpeg") {
            jpeg(file, width = 8 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format2 == "tiff") {
            tiff(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$fig_format2 == "bmp") {
            bmp(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else {
            svg(file)
          }
          date <- as.character(input$daterange_2)
          temp <- obtainids(input$country, input$lineage_2, input$clade_2, date)
          globalsnp_stat10 <- dbSendQuery(dbcon, 'SELECT pro_variant,ID,refpos FROM covid_annot WHERE [ID] = $ids')
          dbBind(globalsnp_stat10, temp)
          temp <- dbFetch(globalsnp_stat10)
          dbClearResult(globalsnp_stat10)
          globalProteinMut(covid_annot = covid_annot, outdir = NULL, figure_Type = input$globalpro, top = input$Top_mutPro, country = input$country)
          dev.off()
        }
      )
    }
  )
  
  sub_nucmer_tab <- reactive({
    date <- as.character(input$daterange_2)
    temp <- obtainids(input$country, input$lineage_2, input$clade_2, date)
    globalsnp_statsubp <- dbSendQuery(dbcon, 'SELECT ID,protein,variant,varclass,annotation,pro_variant FROM covid_annot WHERE [ID] = $ids')
    dbBind(globalsnp_statsubp, temp)
    temp <- dbFetch(globalsnp_statsubp)
    dbClearResult(globalsnp_statsubp)
    sub_cov<- globalProteinMut(covid_annot = temp, outdir = "return", figure_Type = input$globalpro, top = input$Top_mutPro)
    return(sub_cov)
  })
  
  sub_nucmer_tab1 <- reactive({
    date <- as.character(input$daterange)
    temp <- obtainids(input$country_user, input$lineage, input$clade, date)
    globalsnp_statsubn <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.time,meta.country,nucmerr.M_type,nucmerr.PM_type FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
    dbBind(globalsnp_statsubn, temp)
    temp <- dbFetch(globalsnp_statsubn)
    dbClearResult(globalsnp_statsubn)
    sub_cov1<- globalSNPprofile(nucmerr = temp, outdir = "return", figure_Type = "null", top = input$top)
    return(sub_cov1)
  })
  
  output$sub_nucmer_info <- renderDT({
    DT::datatable(sub_nucmer_tab()[,c(1:5)],
                  colnames = c("sample", "protein", "variant", "var_class", "annotation"),
                  rownames = FALSE,
                  extensions = "FixedColumns",
                  filter = "bottom",
                  
                  options = list(
                    pageLength = 5,
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
      glue::glue("Top{input$Top_mutPro}_protein_Mutation_{input$country}_sub_nucmer.{input$ld_snp_format}")
    },
    content = function(file) {
      if (input$ld_snp_format == "txt") {
        write.table(sub_nucmer_tab(), file, row.names = F, col.names = T, quote = F)
      } else if (input$ld_snp_format == "csv") {
        readr::write_csv(sub_nucmer_tab(), file, col_names = T)
      } else if (input$ld_snp_format == "tsv") {
        readr::write_tsv(sub_nucmer_tab(), file, col_names = T)
      } else {
        writexl::write_xlsx(sub_nucmer_tab(), file, col_names = T)
      }
    }
  )
  
  output$sub_nucmer_info1 <- renderDT({
    DT::datatable(sub_nucmer_tab1()[,c(1:5)],
                  colnames = c("sample", "time", "location", "M_type", "PM_type"),
                  rownames = FALSE,
                  extensions = "FixedColumns",
                  filter = "bottom",
                  
                  options = list(
                    pageLength = 5,
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 1),
                    autoWidth = FALSE,
                    lengthMenu = c(5, 8, 10),
                    columnDefs = list(list(className = "dt-left", targets = "_all"))
                  ), escape = FALSE
    )
  })
  
  output$ld_data_download1 <- downloadHandler(
    filename = function() {
      glue::glue("Top{input$top}_SNP_Mutation_{input$country_user}_sub_nucmer.{input$ld_snp_format1}")
      
    },
    content = function(file) {
      if (input$ld_snp_format1 == "txt") {
        write.table(sub_nucmer_tab1(), file, row.names = F, col.names = T, quote = F)
      } else if (input$ld_snp_format1 == "csv") {
        readr::write_csv(sub_nucmer_tab1(), file, col_names = T)
      } else if (input$ld_snp_format1 == "tsv") {
        readr::write_tsv(sub_nucmer_tab1(), file, col_names = T)
      } else {
        writexl::write_xlsx(sub_nucmer_tab1(), file, col_names = T)
      }
    }
  )
}
