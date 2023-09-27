mod_MutDis_ui <- function(id){
  ns <- NS(id)
  navbarPage(title = "", 
            tabPanel("Mutation density",icon = icon("chart-line"),
                     tagList(
                       sidebarPanel(
                         width = 4,
                         h3(strong("Mutation distribution")),
                         h5("Here you can change the number of locations (Top locations with the most recorded SARS-CoV-2 samples) to present the mutation density profile. Also, you can choose a range of genomic sites to investigate specific mutation hotspots.", span("e.g. 28831:28931 in SARS-CoV-2 genome. "), htmltools::a("Reference sequence: NC_045512.2", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/")),
                         br(),
                         sliderInput(ns("top1"), "Number of countries:", 1, 20, 10),#define the input 
                         sliderInput(ns("start"), 
                                     label = "Select the start site to plot",
                                     1, 20000, 1
                         ),
                         sliderInput(ns("end"), 
                                     label = "Select the end site to plot",
                                     1, 20000, 20000
                         ),
                         dateRangeInput(ns("daterange_d"),
                                        "Select date range:",
                                        start = date_limit[2] - 30,
                                        end = date_limit[2],
                                        min = "2019-12-01",
                                        max = date_limit[2])
                       ),
                       
                       mainPanel(
                         column(width = 12,
                                box(title = strong("Mutation density for each country"), width = 14, solidHeader = T, status = "primary",
                                    withSpinner(plotOutput(ns("plot1"), height = 600), type = 4)
                                )
                         )
                       )
                     )
                     ),
            tabPanel("Mutation classes", icon = icon("chart-bar"),
                     tagList(
                       sidebarPanel(
                         width = 4,
                         h3(strong("Basic descriptions for the mutational events", style = "color:black")),
                         h5("In this section, we displayed some mutation properties of SARS-CoV-2, including 
                         average mutations per sample(MutPerSample), counts of different mutation classes(VarClasses),
                         counts of different mutation types(VarType), barplot of different nucleotide variants(NucleoEvents),
                         and barplot of protein mutations(ProEvents), based on annotation of all mutations from the recorded 
                         samples on GISAID platform."),
                         br(),
                         selectInput(ns("figures"), #下拉框
                                     label = "Select the figure type for display",
                                     choices = c(
                                       "MutPerSample", "VarClasses",
                                       "VarType", "NucleoEvents",
                                       "ProEvents"
                                     ),
                                     selected = "MutPerSample"),
                         br(),
                         br(),
                         br(),
                         br(),
                         h3(strong("Basic descriptions for the mutational events for selected protein", style = "color:black")),
                         br(),
                         selectInput(ns("protein"), #下拉框
                                     label = "Select the protein name for display",
                                     choices = protein_list,
                                     selected = "NSP1"),
                         sliderInput(ns("top2"), "Number of top mutated events:", 1, 20, 10),
                         dateRangeInput(ns("daterange_2"),
                                        "Select date range:",
                                        start = date_limit[2] - 30,
                                        end = date_limit[2],
                                        min = "2019-12-01",
                                        max = date_limit[2])
                         
                       ),
                       mainPanel(
                         column(width = 12,
                                box(title = strong("Mutation statistics"), width = 14, solidHeader = T, status = "primary", 
                                    withSpinner(plotOutput(ns("plot7")), type = 4)
                                ),
                                box(title = strong("Mutation statistics for protein"), width = 14, solidHeader = T, status = "primary",
                                    withSpinner(plotOutput(ns("plot9")), type = 4) 
                                )
                         )
                       )
                     ),
                     ),
            tabPanel("Single gene mutation counts",icon = icon("dna"),
                     tagList(
                         sidebarPanel(
                           width = 4,
                           h3(strong("Mutation counts for each gene", style = "color:black")),
                           h5("We investigated mutation counts in each gene, 
                             as some gene regions are more susceptible to mutations. 
                             These mutations may impact SARS-CoV-2 infectivity and 
                             evolution. Users could select different genes to compare 
                             their mutation frequency."),
                           br(),
                           selectInput(ns("genes"),
                                       label = "Select the gene for counting the mutations",
                                       choices = protein_list,
                                       selected = "NSP1"),
                           dateRangeInput(ns("daterange_3"),
                                          "Select date range:",
                                          start = date_limit[2] - 30,
                                          end = date_limit[2],
                                          min = "2019-12-01",
                                          max = date_limit[2]),
                           sliderInput(ns("mtype_number"),
                                       "Number of mutation type to show",
                                       1, 20, 10)
                         ),
                         mainPanel(
                           column(width = 12,
                                  box(title = strong("Mutation counts for each gene"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                                      withSpinner(plotOutput(ns("plot8")), type = 4),
                                      br(),
                                  ),
                                  box(title = strong("Average mutation counts per base in genes"), width = 14, solidHeader = T, status = "primary", collapsible = T,
                                      withSpinner(plotOutput(ns("plotSum")), type = 4)
                                      )
                           )
                         )
                     )
                     )
  )
}

mod_MutDis_server <- function(input, output, session){
  ns <- session$ns
  
  output$plot1 <- renderPlot({
    date <- as.character(input$daterange_d)
    temp <- obtainids(date = date)
    mutdis_stat1 <- dbSendQuery(dbcon, 'SELECT meta.ID,meta.country,nucmerr.rpos FROM nucmerr INNER JOIN meta ON nucmerr.ID = meta.ID WHERE meta.ID = $ids')
    dbBind(mutdis_stat1, temp)
    temp <- dbFetch(mutdis_stat1)
    dbClearResult(mutdis_stat1)
    country_sample<-temp[!duplicated(temp$ID),c("country", "ID")]
    Top_country<- names(head(sort(table(country_sample$country),decreasing =T),n=input$top1))
    mutpos <- input$start:input$end
    rpos<- temp[temp$country %in% Top_country & temp$rpos %in% mutpos,]$rpos
    ggplot(data=temp[temp$country %in% Top_country & temp$rpos %in% mutpos,],aes(x=rpos, y = country, fill = ..density..))+
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.00, size = 0.3, labels = scales::comma) + 
      scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())+
      labs(x = "SARS-CoV-2 Genome Position (nt)")
  })
  
  output$approvalBox <- renderInfoBox({
    mutdis_statbox <- dbSendQuery(dbcon, 'SELECT ID,country FROM meta WHERE time BETWEEN ? AND ?')
    date <- as.character(input$daterange_d)
    temp <- c(date[1], date[2])
    dbBind(mutdis_statbox, temp)
    temp <- dbFetch(mutdis_statbox)
    dbClearResult(mutdis_statbox)
    Top_country<- names(head(sort(table(temp$country),decreasing =T),n=input$top1))
    infoBox(
      paste0("Top", input$top1, "countries", collapse = " "), value = paste0("In order of severity:", paste0(Top_country, collapse = " "), collapse = " "),
      color = "red"
    )
  })
  
  output$plot7 <- renderPlot({
    date <- as.character(input$daterange_2)
    
    if(input$figures == 'MutPerSample'){
      temp <- obtainids(date = date)
      mutdis_stat7_mps <- dbSendQuery(dbcon, 'SELECT ID FROM nucmerr WHERE [ID] = $ids')
      dbBind(mutdis_stat7_mps, temp)
      temp <- dbFetch(mutdis_stat7_mps)
      dbClearResult(mutdis_stat7_mps)
    } else if(input$figures == 'VarClasses'){
      temp <- obtainids(date = date)
      mutdis_stat7_varc <- dbSendQuery(dbcon, 'SELECT varclass FROM covid_annot WHERE [ID] = $ids')
      dbBind(mutdis_stat7_varc, temp)
      temp <- dbFetch(mutdis_stat7_varc)
      dbClearResult(mutdis_stat7_varc)
    } else if(input$figures == 'VarType'){
      temp <- obtainids(date = date)
      mutdis_stat7_vart <- dbSendQuery(dbcon, 'SELECT refvar,qvar FROM covid_annot WHERE [ID] = $ids')
      dbBind(mutdis_stat7_vart, temp)
      temp <- dbFetch(mutdis_stat7_vart)
      dbClearResult(mutdis_stat7_vart)
    } else if(input$figures == 'NucleoEvents'){
      temp <- obtainids(date = date)
      mutdis_stat7_nevent <- dbSendQuery(dbcon, 'SELECT refvar,qvar,refpos FROM covid_annot WHERE [ID] = $ids')
      dbBind(mutdis_stat7_nevent, temp)
      temp <- dbFetch(mutdis_stat7_nevent)
      dbClearResult(mutdis_stat7_nevent)
    } else if(input$figures == 'ProEvents'){
      temp <- obtainids(date = date)
      mutdis_stat7_pevent <- dbSendQuery(dbcon, 'SELECT protein,variant FROM covid_annot WHERE [ID] = $ids')
      dbBind(mutdis_stat7_pevent, temp)
      temp <- dbFetch(mutdis_stat7_pevent)
      dbClearResult(mutdis_stat7_pevent)
    }
    plotMutAnno(results = temp,figureType = input$figures)
  })
    
    output$plot8 <- renderPlot({
      date <- as.character(input$daterange_3)
      temp <- obtainids(date = date)
      mutdis_stat8 <- dbSendQuery(dbcon, 'SELECT rpos,M_type FROM nucmerr WHERE [ID] = $ids')
      dbBind(mutdis_stat8, temp)
      temp <- dbFetch(mutdis_stat8)
      dbClearResult(mutdis_stat8)
      MutByGene(nucmerr = temp, gff3 = covid_position, gene = input$genes, top = input$mtype_number)
    })
    
    output$plotSum <- renderPlot({
      date <- as.character(input$daterange_3)
      temp <- obtainids(date = date)
      assaycopmare_stat6 <- dbSendQuery(dbcon, 'SELECT ID,rpos FROM nucmerr WHERE [ID] = $ids')
      dbBind(assaycopmare_stat6, temp)
      temp <- dbFetch(assaycopmare_stat6)
      dbClearResult(assaycopmare_stat6)
      genelist<- GeneList(nucmerr = temp, gff3 = covid_position)
      gene <- factor(x = covid_position$Gene, levels = protein_list)
      ggplot(data=genelist,aes(x=gene,y = MutFrequency,fill=genelist$Gene))+
            geom_bar(stat="identity", show.legend = F)+
             theme_bw()+
             theme(axis.text.x = element_text(angle = 45,hjust = 1,size=12, face="bold"),
                   axis.text.y = element_text(size=12,face="bold"))+
        labs(x = "Genes in SARS-CoV-2 genome", y = "Mutation counts per base")
    })
    
    output$plot9 <- renderPlot({
      date <- as.character(input$daterange_2)
      temp <- obtainids(date = date)
      mutdis_stat7_pevent <- dbSendQuery(dbcon, 'SELECT protein,variant FROM covid_annot WHERE [ID] = $ids')
      dbBind(mutdis_stat7_pevent, temp)
      temp <- dbFetch(mutdis_stat7_pevent)
      dbClearResult(mutdis_stat7_pevent)
      plotMutProteins(object = temp,proteinName = input$protein, top = input$top2, outdir = NULL)
    })

}
