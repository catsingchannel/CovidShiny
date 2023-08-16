
mod_StrucView_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
      width = 4,
      h3(strong("Structure Viewer for Mutations in N or S protein", style = "color:black")),
      h5("In this panel, mutations in N or S protein will be marked on a 3D structure to display the effect on protein structure"),
      title = "", status = "warning", solidHeader = TRUE,
      
      checkboxInput(ns("custom"), "Input your custom mutations", FALSE),
      selectInput(ns("protein"), "Protein", c("S", "N"), selected = "S"),
      conditionalPanel(
        condition = "!input.custom",
        ns = ns,
        selectInput(ns("strain"), 
                    "Strain", 
                    c("top 20 mutations in last month",
                      "top 20 mutations in last 3 month", 
                      "Alpha", "Beta", "Gamma", "Delta", 
                      "Omicron(BA.1)", "Omicron(BA.2)", "Omicron(BA.5)", "Omicron(BA.2.75)"), 
                    selected = "Alpha"),
        actionButton(ns("submitL"), "Submit"),
        actionButton(ns("removeL"), "Remove")
      ),
      conditionalPanel(
        condition = "input.custom",
        ns = ns,
        textInput(ns("mutations"), "Mutations", "D614G;H655Y"),
        actionButton(ns("submitC"), "Submit"),
        actionButton(ns("removeC"), "Remove")
      ),
      
      br(),
      h5("The structure in left shows the wild-type structure and the right one shows the mutated structure built by homology modeling via SWISS-MODEL"),
      h5("The blue region is the C terminal domain(CTD), and the orange region is the N terminal domain(NTD) of this protein. mutated position is marked by red"),
      h5("Limited by the modeling efficiency, the structure for top mutations in recent months and custom mutations are not provided, WT structure will be displayed in right"),
      
      br(),
      h5(),
      br()
    ),
    
    mainPanel(
      column(width = 6,
             box(title = strong("Mutation tracking for selected protein (WT)"), 
                 width = 14, 
                 solidHeader = T, 
                 status = "primary",
                 withSpinner(NGLVieweROutput(ns("structure"), height = 400), type = 4),
             )
      ),
      column(width = 6,
             box(title = strong("Mutation tracking for selected protein (Mutated)"), 
                 width = 14, 
                 solidHeader = T, 
                 status = "primary",
                 withSpinner(NGLVieweROutput(ns("mutated"), height = 400), type = 4),
             )
      ),
    )
  )
}

mod_StrucView_server <- function(input, output, session){
  ns <- session$ns
  
  output$structure <- renderNGLVieweR({S})
  
  output$mutated <- renderNGLVieweR({S})
  
  observeEvent(input$protein, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("selp") %>%
      removeSelection("sell")
    
    if(input$protein == "N"){
      output$structure <- renderNGLVieweR({N})
      output$mutated <- renderNGLVieweR({N})
    } else if(input$protein == "S"){
      output$structure <- renderNGLVieweR({S})
      output$mutated <- renderNGLVieweR({S})
    }
  })
  
  observeEvent(input$submitL, {
    NGLVieweR_proxy(ns("structure")) %>%
      removeSelection("selp") %>%
      removeSelection("sell")
    
    if(!(input$strain %in% c("top 20 mutations in last month", "top 20 mutations in last 3 month"))){
      mutations <- unlist(strsplit(strains[input$strain, input$protein], split = ";"))
      positions <- paste0(":A and (", 
                          paste(str_extract(mutations, "\\(?[0-9]+\\)?"), collapse = " OR "),
                          ")")
    } else {
      if(input$strain == "top 20 mutations in last month"){
        date <- c(as.character(date_limit[2] - 30), as.character(date_limit[2]))
      } else {
        date <- c(as.character(date_limit[2] - 90), as.character(date_limit[2]))
      }
      temp <- obtainids(date = date)
      strucview_stat <- dbSendQuery(dbcon, 'SELECT protein,variant FROM covid_annot WHERE [ID] = $ids')
      dbBind(strucview_stat, temp)
      temp <- dbFetch(strucview_stat)
      dbClearResult(strucview_stat)
      pro <- temp[temp$protein == input$protein, ]
      
      ch <- sort(table(pro$variant), decreasing = TRUE)
      ch <- names(ch)
      ch <- ch[1:20]
      ch <- paste(ch, collapse = ";")
      
      mutations <- unlist(strsplit(ch, split = ";"))
      positions <- paste0(":A and (", 
                          paste(str_extract(mutations, "\\(?[0-9]+\\)?"), collapse = " OR "),
                          ")")
    }
    
    NGLVieweR_proxy("structure") %>%
      addSelection("surface",
                   param = list(
                     name = "selp",
                     sele = positions,
                     colorValue = "red"
                   )) %>%
      addSelection("label", 
                   param = list(
                     name = "sell",
                     sele = positions,
                     labelType = "format",
                     labelFormat = "%(resname)s%(resno)s",
                     labelGrouping = "residue",
                     color = "red",
                     fontFamiliy = "sans-serif",
                     xOffset = 1,
                     yOffset = 1,
                     zOffset = 1,
                     fixedSize = TRUE,
                     radiusType = 1,
                     radiusSize = 1.5,
                     showBackground = TRUE,
                     showBorder = TRUE,
                     borderColor = "red"
                   )
      )
    
    if(input$protein == "S"){
      if(input$strain == "Alpha"){
        output$mutated <- renderNGLVieweR({S_mutations[[1]]})
      }else if (input$strain == "Beta"){
        output$mutated <- renderNGLVieweR({S_mutations[[2]]})
      }else if (input$strain == "Gamma"){
        output$mutated <- renderNGLVieweR({S_mutations[[3]]})
      }else if (input$strain == "Delta"){
        output$mutated <- renderNGLVieweR({S_mutations[[4]]})
      }else if (input$strain == "Omicron(BA.1)"){
        output$mutated <- renderNGLVieweR({S_mutations[[5]]})
      }else if (input$strain == "Omicron(BA.2)"){
        output$mutated <- renderNGLVieweR({S_mutations[[6]]})
      }else if (input$strain == "Omicron(BA.5)"){
        output$mutated <- renderNGLVieweR({S_mutations[[7]]})
      }else if (input$strain == "Omicron(BA.2.75)"){
        output$mutated <- renderNGLVieweR({S_mutations[[8]]})
      }else{
        output$mutated <- renderNGLVieweR({
          S %>%
            addRepresentation("surface",
                              param = list(
                              name = "selp",
                              sele = positions,
                              colorValue = "red"
                              )) %>%
            addRepresentation("label", 
                              param = list(
                              name = "sell",
                              sele = positions,
                              labelType = "format",
                              labelFormat = "%(resname)s%(resno)s",
                              labelGrouping = "residue",
                              color = "red",
                              fontFamiliy = "sans-serif",
                              xOffset = 1,
                              yOffset = 1,
                              zOffset = 1,
                              fixedSize = TRUE,
                              radiusType = 1,
                              radiusSize = 1.5,
                              showBackground = TRUE,
                              showBorder = TRUE,
                              borderColor = "red"
                              )
                              )
          })
      } 
    } else if (input$protein == "N"){
      if(input$strain == "Alpha"){
        output$mutated <- renderNGLVieweR({N_mutations[[1]]})
      }else if (input$strain == "Beta"){
        output$mutated <- renderNGLVieweR({N_mutations[[2]]})
      }else if (input$strain == "Gamma"){
        output$mutated <- renderNGLVieweR({N_mutations[[3]]})
      }else if (input$strain == "Delta"){
        output$mutated <- renderNGLVieweR({N_mutations[[4]]})
      }else if (input$strain == "Omicron(BA.1)"){
        output$mutated <- renderNGLVieweR({N_mutations[[5]]})
      }else if (input$strain == "Omicron(BA.2)"){
        output$mutated <- renderNGLVieweR({N_mutations[[6]]})
      }else if (input$strain == "Omicron(BA.5)"){
        output$mutated <- renderNGLVieweR({N_mutations[[7]]})
      }else if (input$strain == "Omicron(BA.2.75)"){
        output$mutated <- renderNGLVieweR({N_mutations[[8]]})
      }else{
        output$mutated <- renderNGLVieweR({
          N %>%
            addRepresentation("surface",
                              param = list(
                                name = "selp",
                                sele = positions,
                                colorValue = "red"
                              )) %>%
            addRepresentation("label", 
                              param = list(
                                name = "sell",
                                sele = positions,
                                labelType = "format",
                                labelFormat = "%(resname)s%(resno)s",
                                labelGrouping = "residue",
                                color = "red",
                                fontFamiliy = "sans-serif",
                                xOffset = 1,
                                yOffset = 1,
                                zOffset = 1,
                                fixedSize = TRUE,
                                radiusType = 1,
                                radiusSize = 1.5,
                                showBackground = TRUE,
                                showBorder = TRUE,
                                borderColor = "red"
                              )
            )
        })
      }
    }
  })
  
  observeEvent(input$submitC, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("selp") %>%
      removeSelection("sell")
    
    mutations <- unlist(strsplit(input$mutations, split = ";"))
    positions <- paste0(":A and (", 
                        paste(str_extract(mutations, "\\(?[0-9]+\\)?"), collapse = " OR "), 
                        ")")
    
    NGLVieweR_proxy("structure") %>%
      addSelection("surface",
                   param = list(
                     name = "selp",
                     sele = positions,
                     colorValue = "red"
                   )) %>%
      addSelection("label", 
                   param = list(
                     name = "sell",
                     sele = positions,
                     labelType = "format",
                     labelFormat = "%(resname)s%(resno)s",
                     labelGrouping = "residue",
                     color = "red",
                     fontFamiliy = "sans-serif",
                     xOffset = 1,
                     yOffset = 1,
                     zOffset = 1,
                     fixedSize = TRUE,
                     radiusType = 1,
                     radiusSize = 1.5,
                     showBackground = TRUE,
                     showBorder = TRUE,
                     borderColor = "red"
                   ))
  })
  
  observeEvent(input$removeL, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("selp") %>%
      removeSelection("sell")
  })
  
  observeEvent(input$removeC,{
    NGLVieweR_proxy("structure") %>%
      removeSelection("selp") %>%
      removeSelection("sell")
  })

}