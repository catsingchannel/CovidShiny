mod_coMutation_ui <- function(id){
  ns <- NS(id)
  # Second tab content height = 420
  tagList(
    sidebarPanel(
      width = 4, #height = 503,
      h3(strong("Detection of co-occurring mutations", style = "color:black")),
      selectInput(ns("mutationType1"), #下拉框
                  label = "Select the first mutation type (mut1):",
                  choices = c("protein",
                              "nucleotide"
                  ),
                  selected = "protein"),
      selectInput(ns("mutationType2"), #下拉框
                  label = "Select the second mutation type (mut2):",
                  choices = c("protein",
                              "nucleotide"
                  ),
                  selected = "protein"),
      conditionalPanel(
        condition = "input.mutationType1 == 'protein'",
        ns = ns,
        textInput(ns("mut1"), label = "Input protein mutation class (mut1, protein:mutation, e.g., S:D614G):", value = "S:D614G"),
        
        
        conditionalPanel(
          condition = "input.mutationType2 == 'nucleotide'",
          ns = ns,
          textInput(ns("mut2_refvar"), label = "Input reference nucleotide (refvar for mut2):", value = "GGG"),
          textInput(ns("mut2_qvar"), label = "Input query nucleotide (qvar for mut2):", value = "AAC"),
          textInput(ns("mut2_refpos"), label = "Input reference position (refpos for mut2):", value = "28881")
        ),
        conditionalPanel(
          condition = "input.mutationType2 == 'protein'",
          ns = ns,
          textInput(ns("mut2"), label = "Input protein mutation class (mut2, protein:mutation, e.g., S:D614G):", value = "S:D614G")
        )
        
      ),
      conditionalPanel(
        condition = "input.mutationType1 == 'nucleotide'",
        ns = ns,
        textInput(ns("mut1_refvar"), label = "Input reference nucleotide (refvar for mut1):", value = "GGG"),
        textInput(ns("mut1_qvar"), label = "Input query nucleotide (qvar for mut1):", value = "AAC"),
        textInput(ns("mut1_refpos"), label = "Input reference position (refpos for mut1):", value = "28881"),
        
        conditionalPanel(
          condition = "input.mutationType2 == 'nucleotide'",
          ns = ns,
          textInput(ns("mut2_refvar"), label = "Input reference nucleotide (refvar for mut2):", value = "GGG"),
          textInput(ns("mut2_qvar"), label = "Input query nucleotide (qvar for mut2):", value = "AAC"),
          textInput(ns("mut2_refpos"), label = "Input reference position (refpos for mut2):", value = "28881")
        ),
        conditionalPanel(
          condition = "input.mutationType2 == 'protein'",
          ns = ns,
          textInput(ns("mut2"), label = "Input protein mutation class (mut2, protein:mutation, e.g., S:D614G):", value = "S:D614G")
        )
        
      ),
      selectInput(ns("country"), #下拉框
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
      dateRangeInput(ns("daterange"),
                     "Select date range:",
                     start = date_limit[2] - 30,
                     end = date_limit[2],
                     min = "2019-12-01",
                     max = date_limit[2]),
      actionButton(ns("ld2_submit"), strong("Submit"), styleclass = "success"),
      h5("On the one hand, co-occurring mutations indicate that one variant may evolve from other strains as a byproduct of random mutation. On the other hand, no intersection suggest that two strains evolved separately. In the control panel, user can select proteins co-mutation or nucleotides co-mutation or nucleotide & protein co-mutation by specifying the first & second mutation type, and giving mutation classes (e.g., NSP3:F106F, S:D614G for protein, the protein name refers to the 'Single gene mutation counts' section in 'Mutation_statistics' module; 28881(refpos):GGG(refvar) to AAC(qvar), 25563:G to T for nucleotide)."),
      br(),
      br(),
    ),
    
    mainPanel(
      
      
      column(width = 12,
             box(title = strong("Co-mutations in SARS-CoV-2:"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                 withSpinner(plotOutput(ns("plot12")), type = 4),
                 #除了homepage，其他用shiny的框架，注意修改
                 selectInput(ns("fig_format"),
                             "Download figure as:",
                             choices = available_fig_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 downloadButton(ns("double_download"), "Download co-mutation")  
                 
             ),
             h4("This table is dynamic corresponding to the intersection of the venn plot above."),
             box(title = strong("Samples carrying co-mutations"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                 footer = 
                   h5(".x represents mut1, .y represents mut2", br(), "sample: sample ID;", br(), "refpos: reference position;",
                      br(), "refvar: reference nucleotide;", br(), "qvar: query nucleotide", br(), "qpos: query position", br(), "qlength: query genome length", br(), "protein: mutated protein", br(), "variant: mutation class for protein", br(), "varclass: mutation type for protein", br(), "annotation: protein name"),    
                 
                 
                 
                 
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
}

mod_coMutation_server <- function(input, output, session){
  ns <- session$ns
  
  mutlist<- eventReactive(input$ld2_submit, {
    mutlist <- list()
    if(input$mutationType1 == "protein"){
      if(input$mutationType2 == "protein"){
        mut1 <- input$mut1
        mut2 <- input$mut2
        refpos1 <- NA
        refpos2 <- NA
        variant <- "protein"
      }else{
        mut1 <- input$mut1
        mut2 <- c(input$mut2_refvar, input$mut2_qvar)
        refpos1 <- NA
        refpos2 <- input$mut2_refpos
        variant <- "pro&Nr"
      }

    }else{
      if(input$mutationType2 == "protein"){
        mut1 <- c(input$mut1_refvar, input$mut1_qvar)
        mut2 <- input$mut2
        refpos1 <- input$mut1_refpos
        refpos2 <- NA
        variant <- "Nr&pro"
      }else{
        mut1 <- c(input$mut1_refvar, input$mut1_qvar)
        mut2 <- c(input$mut2_refvar, input$mut2_qvar)
        refpos1 <- input$mut1_refpos
        refpos2 <- input$mut2_refpos
        variant <- "nucleotide"
      }
    }
    mutlist[[1]] <- mut1
    mutlist[[2]] <- mut2
    mutlist[[3]] <- refpos1
    mutlist[[4]] <- refpos2
    mutlist[[5]] <- variant
    mutlist[[6]] <- input$daterange
    mutlist[[7]] <- input$lineage
    mutlist[[8]] <- input$clade
    mutlist[[9]] <- input$country
    return(mutlist)
  })
  
  output$plot12 <- renderPlot({
    date <- as.character(mutlist()[[6]])
    temp <- obtainids(mutlist()[[9]], mutlist()[[7]], mutlist()[[8]], date)
    
    if(mutlist()[[5]] == "protein"){
      comutation_stat12p <- dbSendQuery(dbcon, 'SELECT pro_variant,ID FROM covid_annot WHERE [ID] = $ids')
      dbBind(comutation_stat12p, temp)
      temp <- dbFetch(comutation_stat12p)
      dbClearResult(comutation_stat12p)
    }
    else if (mutlist()[[5]] == "nucleotide"){
      comutation_stat12n <- dbSendQuery(dbcon, 'SELECT refvar,qvar,refpos,ID FROM covid_annot WHERE [ID] = $ids')
      dbBind(comutation_stat12n, temp)
      temp <- dbFetch(comutation_stat12n)
      dbClearResult(comutation_stat12n)
    }
    else if (mutlist()[[5]] == "pro&Nr" || mutlist()[[5]] == "Nr&pro"){
      comutation_stat12np <- dbSendQuery(dbcon, 'SELECT pro_variant,ID,refvar,qvar,refpos FROM covid_annot WHERE [ID] = $ids')
      dbBind(comutation_stat12np, temp)
      temp <- dbFetch(comutation_stat12np)
      dbClearResult(comutation_stat12np)
    }
    co_mutation_ev(results = temp, mut1 = mutlist()[[1]], mut2 = mutlist()[[2]], variant = mutlist()[[5]], refpos1 = mutlist()[[3]], refpos2 = mutlist()[[4]], outdir = NULL)
  })
  
  output$double_download <- downloadHandler(
    filename = function() {
      glue::glue("{mutlist()[[1]]}_{mutlist()[[2]]}_coMutation.jpg")
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
          
          date <- as.character(mutlist()[[6]])
          temp <- obtainids(mutlist()[[9]], mutlist()[[7]], mutlist()[[8]], date)
          if(mutlist()[[5]] == "protein"){
            #temp <- dbGetQuery(dbcon, "SELECT pro_variant,ID FROM covid_annot WHERE time BETWEEN ? AND ?", params = c(date[1], date[2]))
            comutation_stat12p <- dbSendQuery(dbcon, 'SELECT pro_variant,ID FROM covid_annot WHERE [ID] = $ids')
            dbBind(comutation_stat12p, temp)
            temp <- dbFetch(comutation_stat12p)
            dbClearResult(comutation_stat12p)
          }
          else if (mutlist()[[5]] == "nucleotide"){
            #temp <- dbGetQuery(dbcon, "SELECT refvar,qvar,refpos,ID FROM covid_annot WHERE time BETWEEN ? AND ?", params = c(date[1], date[2]))
            comutation_stat12n <- dbSendQuery(dbcon, 'SELECT refvar,qvar,refpos,ID FROM covid_annot WHERE [ID] = $ids')
            dbBind(comutation_stat12n, temp)
            temp <- dbFetch(comutation_stat12n)
            dbClearResult(comutation_stat12n)
          }
          else if (mutlist()[[5]] == "pro&Nr" || mutlist()[[5]] == "Nr&pro"){
            #temp <- dbGetQuery(dbcon, "SELECT pro_variant,ID,refvar,qvar,refpos FROM covid_annot WHERE time BETWEEN ? AND ?", params = c(date[1], date[2]))
            comutation_stat12np <- dbSendQuery(dbcon, 'SELECT pro_variant,ID,refvar,qvar,refpos FROM covid_annot WHERE [ID] = $ids')
            dbBind(comutation_stat12np, temp)
            temp <- dbFetch(comutation_stat12np)
            dbClearResult(comutation_stat12np)
          }
          co_mutation_ev(results = temp, mut1 = mutlist()[[1]], mut2 = mutlist()[[2]], variant = mutlist()[[5]], refpos1 = mutlist()[[3]], refpos2 = mutlist()[[4]], outdir = NULL)
          dev.off()
        }
      )
    }
  )
  
  sub_nucmer_tab <- reactive({
    date <- as.character(mutlist()[[6]])
    temp <- obtainids(mutlist()[[9]], mutlist()[[7]], mutlist()[[8]], date)
    comutation_statsub <- dbSendQuery(dbcon, 'SELECT * FROM covid_annot WHERE [ID] = $ids')
    dbBind(comutation_statsub, temp)
    temp <- dbFetch(comutation_statsub)
    dbClearResult(comutation_statsub)
    sub_cov<- co_mutation_ev(results = temp, mut1 = mutlist()[[1]], mut2 = mutlist()[[2]], variant = mutlist()[[5]], refpos1 = mutlist()[[3]], refpos2 = mutlist()[[4]], outdir = "return")
    return(sub_cov)
  })
  
  output$sub_nucmer_info <- renderDT({
    DT::datatable(sub_nucmer_tab(),
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
        glue::glue("{mutlist()[[1]]}_{mutlist()[[2]]}_sub_nucmer.{input$ld_snp_format}")
      
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
}
