options(warn = -1)
options(download.file.method="libcurl")

library(seqinr)
library(Biostrings)
library(shiny)
library(shinydisconnect)
library(ggplot2)
library(ggsci)
library(stringr)
library(dplyr)
library(tidyr)
library(forcats)
library(patchwork)
library(glue)
library(ggpubr)
library(writexl)
library(gridExtra)
library(grid)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(plotly)
library(dashboardthemes)
library(venn)
library(DBI)
library(UpSetR)
library(NGLVieweR)
library(ggridges)
library(RColorBrewer)
library(msa)
library(rprimer)

footerTagList <- list(
  tags$footer(id = "myFooter",
              shiny::includeHTML("www/md/footer.html")
  )
)

load("./data/covid_assays.rda")
load("./data/covid_position.rda")
load("./data/strains.rda")

dbcon <- dbConnect(RSQLite::SQLite(), "./data/CovidShiny.sqlite")

available_data_formats <- c("txt", "xlsx", "csv", "tsv")
available_fig_formats <- c("png", "pdf", "jpeg", "tiff", "bmp", "svg")

homepagestat <- readLines('./data/homepagestat.txt')
date_limit <- c(as.Date(homepagestat[6]), as.Date(homepagestat[5]))
homepagestat <- as.numeric(homepagestat)

demoAssay <- covid_assay$Assay
protein_list <- covid_position$Gene
countrylist <- read.csv("./data/countrylist.txt", header = F, stringsAsFactors = F)$V1
lineagelist <- read.csv("./data/lineagelist.txt", header = F, stringsAsFactors = F)$V1
cladelist <- read.csv("./data/cladelist.txt", header = F, stringsAsFactors = F)$V1
chinalist <-read.csv("data/China.txt", header = F)

refseq<-read.fasta("./data/NC_045512.2.fa",forceDNAtolower=FALSE)[[1]]
refseq<-DNAString(paste0(refseq,collapse=""))

N_mutations <- readRDS("./data/NStruc.RData")
S_mutations <- readRDS("./data/SStruc.RData")
S <- NGLVieweR("./data/pdbs/SWTT.pdb") %>%
	addRepresentation("cartoon",
			  param = list(name = "cartoon", sele = ":A", color = "white")) %>%
	addRepresentation("cartoon",
			  param = list(name = "NTD", sele = ":A AND 14-303", color = "orange")) %>%
	addRepresentation("cartoon",
			  param = list(name = "CTD", sele = ":A AND 334-527", color = "skyblue"))

N <- NGLVieweR("./data/pdbs/NWTT.pdb") %>%
	addRepresentation("cartoon",
			  param = list(name = "cartoon", sele = "*", color = "white")) %>%
	addRepresentation("cartoon",
			  param = list(name = "NTD", sele = "48-174", color = "orange")) %>%
	addRepresentation("cartoon",
			  param = list(name = "CTD", sele = "247-364", color = "skyblue"))

source("script/obtainids.R")
source("script/AssayMutRatio.R")
source("script/globalProteinMut.R")
source("script/globalSNPprofile.R")
source("script/LastfiveNrMutation.R")
source("script/MutByGene.R")
source("script/MutStat.R")
source("script/plotMutAnno.R")
source("script/homepage.R")
source("script/mod_assayheatmap.R")
source("script/mod_assaycompare.R")
source("script/mod_globalSNP.R")
source("script/mod_lastfiveNr.R")
source("script/mod_MutDis.R")
source("script/readNewData.R")
source("script/doubleAssay.R")
source("script/mod_doubleAssay.R")
source("script/mod_coMutation.R")
source("script/co_mutation_ev.R")
source("script/primerAlign.R")
source("script/UserPrimer.R")
source("script/plotMutProteins.R")
source("script/UpsetR_NS.R")
source("script/mod_upsetr.R")
source("script/mod_about.R")
source("script/mod_StrucView.R")
source("script/QPD.R")
source("script/mod_assaydesign.R")