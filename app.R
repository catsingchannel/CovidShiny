#Create interactive web graphics from 'ggplot2' graphs
#library(semantic.dashboard)

source("global.R")

ui <- fluidPage(theme = shinytheme("yeti"),
  
  div(img(src = "img/shiny.png", height = "100%",width = "100%"), align="center"),
  includeCSS("www/css/custom.css"),
  includeCSS("www/css/footer.css"),
  disconnectMessage(
    text = "Your session timed out, reload the application!",
    refresh = "Reload now",
    background = "#f89f43",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.75,
    top = 250,
    refreshColour = "brown"
  ),
  navbarPage(
    title = NULL,
    tags$style(HTML(".navbar-nav { width: 100%; text-align: center }")),
    windowTitle = "CovidShiny for SARS-CoV-2 diagnostic analysis",
    tabPanel("Home", homepage, icon = icon("home")),
    tabPanel("Mutation statistics", mod_MutDis_ui("dis"), icon = icon("chart-bar")),
    tabPanel("SNP&Protein profile", mod_globalSNP_ui("SNP"), icon = icon("dna")),

    tabPanel("Assay profiles",
             navbarPage(title = "",
                        tabPanel("Assay profile", mod_assayheatmap_ui("assay"), icon = icon("vial")),
                        tabPanel("Assays comparison", mod_assaycompare_ui("compare"), icon = icon("vial")),
                        tabPanel("LastfiveNr profile", mod_lastfiveNr_ui("primer"), icon = icon("syringe")),
                        tabPanel("Double assay", mod_doubleAssay_ui("double"), icon = icon("vials")),
                        tabPanel("RT-PCR assay design", mod_assaydesign_ui("design"), icon = icon("vial"))
                        ),
             icon = icon("vial")
             ),
    tabPanel("Mutation viewers",
             navbarPage(title = "",
                        tabPanel("UpsetR", mod_upsetr_ui("upset"), icon = icon("dna")),
                        tabPanel("StrucView", mod_StrucView_ui("struc"), icon = icon("disease")),
                        tabPanel("Co_mutation", mod_coMutation_ui("coMu"), icon = icon("disease"))
                        ),
             icon = icon("dna")
             ),
    tabPanel("Documentation", mod_about_ui("doc"), icon = icon("file-text")),
    footer = footerTagList  #common footer
  )
)

server <- function(input, output, session) {
  callModule(mod_MutDis_server, "dis")
  callModule(mod_globalSNP_server, "SNP")
  callModule(mod_assayheatmap_server, "assay")
  callModule(mod_assaycompare_server, "compare")
  callModule(mod_lastfiveNr_server, "primer")
  callModule(mod_doubleAssay_server, "double")
  callModule(mod_coMutation_server, "coMu")
  callModule(mod_upsetr_server, "upset")
  callModule(mod_StrucView_server, "struc")
  callModule(mod_assaydesign_server, "design")
  observeEvent(input$disconnect, {
    session$close()
  })
}

onStop(
  function(){
    dbDisconnect(dbcon)
  }
)

shinyApp(ui, server)
