homepage <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    column(
      width = 10,
      offset = 1,
      box(title = strong("CovidShiny: integrated analysis of SARS-CoV-2 mutations guiding diagnostic therapy"), status = "primary", solidHeader = TRUE, width = 12)
    ),
    column(
      width = 10,
      offset = 1,
      box(
        width = 12,
        p("The CovidShiny is an interactive web-based platform and set of 
          analytic tools for effcient analysis of mutations among thousands of 
          SARS-CoV-2 samples and providing in-silico evaluation of RT-PCR assays 
          based on the primer & probe design reported by previous research (", htmltools::a("FIND EVALUATION UPDATE: SARS-COV-2 MOLECULAR DIAGNOSTICS", href = "https://www.finddx.org/covid-19/sarscov2-eval-molecular/"), ").")
      )
    ),

    column(
      width = 10,
      offset = 1,
      box(status = "primary", 
          width = 12,
        title = strong("What's Inside"),           
        solidHeader = TRUE,
        collapsible = TRUE,
        #status = "warning",
        fluidRow(
          valueBox(homepagestat[1], "SARS-CoV-2 samples", width = 4),
          valueBox(homepagestat[2], "Average mutations per sample", color = "purple", width = 4),
          valueBox(114, "Locations", color = "yellow", width = 4)
        ),
        fluidRow(
          valueBox(13, "Available default RT-PCR assays", color = "fuchsia", width = 4),
          valueBox(homepagestat[3], "Protein variant types", color = "navy", width = 4),
          valueBox(homepagestat[4], "Nucleotides variant types", color = "olive", width = 4),
        )
      )
    ),

    column(
      width = 10,
      offset = 1,
      box(status = "primary", 
          width = 12,
        title = strong("Analysis Modules"),           
        solidHeader = TRUE,
        collapsible = TRUE,
        #status = "warning",
        box(
          width = 12,
          status = "primary",
          p("We here present 8 modules to analyze the SARS-CoV-2 dataset. 
            Within the first two modules, the user can select different parameters 
            to analyze the mutation profile of SARS-CoV-2 dataset; the next
            three modules are designed for users to submit their RT-PCR assays
            information to pre-evaluate potential efficiency of customed assays;
            the last three modules display mutation combinations and labeled
            in protein structure of SARS-CoV-2 N and S protein.
            You can find some example figures that these modules can generate in 
            the manuscript. The SARS-CoV-2 sample sequencs are retrieved from GISAID platform(", htmltools::a(em("Elbe et al. (2017)"), href = "https://doi.org/10.1002/gch2.1018", target = "_blank"), ")."), 
          fluidRow(
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("Mutation statistics"),
              h5("This module provides an overview of SARS-CoV-2 mutation features and displays mutation statistics in terms of geographical distribution, mutation hotspots, etc."),
              p(img(src = "img/stats.png", height = 170, width = 270), align = "center")
              
              
            ),
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("SNP&Protein profile"),
              h5("This module displays SNPs and proteins mutation profile in SARS-CoV-2 whole genome as heatmap or counts figure."),
              p(img(src = "img/snp.png", height = 170, width = 270), align = "center")
              
              
            )
          ),
          fluidRow(
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("Assay profile"),
              h5("This module implements the functionality to perform user-specified RT-PCR assay evaluation and comparison."),
              p(img(src = "img/assay.png", height = 170, width = 270), align = "center")
              
            ),
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("Last five nucleotides of primers"),
              h5("This module provides the functionality to calculate the mutation ratio in SARS-CoV-2 genomic binding sites of last five nucleotides of RT-PCR primers."),
              p(img(src = "img/primer.png", height = 170, width =270), align = "center")
              
            )
          ),
          fluidRow(
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("Double assay evaluation"),
              h5("This module allows user to submit RT-PCR assay list in txt format to choose a pair of assays to evaluate the coverage of double assay in all SARS-CoV-2 samples."),
              p(img(src = "img/double.png", height = 170, width =270), align = "center")
            ),
            box(solidHeader = T,
              width = 6,
              status = "primary",
              title = strong("Co-mutation analysis"),
              h5("This module allows user to speculate whether some variants evolved separately or derived from other variants (e.g., S:D614G accompanied by some 'passenger' mutations)."),
              p(img(src = "img/overlap_829.png", height = 170, width =270), align = "center")
            )
          ),
          fluidRow(
            box(solidHeader = T,
                width = 6,
                status = "primary",
                title = strong("UpSetR"),
                h5("This module uses UpSetR plot to display mutation combinations for tracking SARS-CoV-2 protein variants."),
                p(img(src = "img/upset.png", height = 170, width =270), align = "center")
            ),
            box(solidHeader = T,
                width = 6,
                status = "primary",
                title = strong("StrucView"),
                h5("This module displays 3D protein structure for N or S protein and marked mutation positions."),
                p(img(src = "img/mark.png", height = 170, width =270), align = "center")
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 10,
        offset = 1,
        box(
          title = strong("More about CovidShiny"),
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          includeMarkdown("www/md/home.md")
        )
      )
    )
  )
)
