library(plyr)
library(readr)
library(tidyr)
library(stringr)
library(Rcpp)
library(DBI)
library(r2r)

sourceCpp("./mergeTSV.cpp")

chinalist <-read.csv("../data/China.txt", header = F)

process <- function(metapath = metapath, dbpath = dbpath){
  fs <- list.files(metapath, pattern = "\\.csv$")
  annot <- read_csv(file.path(metapath, fs))
  
  #{metadeal}
  print("dealing meta data")
  fs <- list.files(metapath, pattern = "\\.tsv$")
  tsvs <- lapply(fs, function(x){a = read.csv(file.path(metapath, x), sep = '\t')})
  meta <- ldply(tsvs, data.frame)
  
  #{metafix}
  print("fixing metadata")
  mergeTSV(annot, meta)
  
  #{coldeal}
  print("dealing column creation and transform")
  annot$pro_variant <- str_c(annot$protein, ':', annot$variant)
  annot$ID <- vapply(strsplit(as.character(annot$sample), "[|]"), function(x) x[2], character(1))
  
  nucmerr <- annot[, c(2,3,4,5,12)]
  colnames(nucmerr) <- c("rpos", "rvar", "qvar", "qpos", "ID")
  nucmerr$M_type <-str_c(nucmerr$rvar,nucmerr$qvar,sep ="->")
  nucmerr$PM_type <-str_c(nucmerr$rpos,nucmerr$M_type,sep =":")
  
  #{NAdeal}
  print("dealing NAs")
  annot$variant[is.na(annot$variant)] <- "NoArg"
  annot$varclass[is.na(annot$varclass)] <- "NoArg"
  annot$annotation[is.na(annot$annotation)] <- "extragenic"
  annot %>% drop_na(ID)
  annot$sample <- as.character(annot$sample)
  
  nucmerr %>% drop_na(ID)
  nucmerr$ID <- as.character(nucmerr$ID)
  
  #{insertandredun}
  print("Inserting and deredundunt")
  #sample <- annot[, c("sample", "refpos", "refvar", "qvar", "qpos", "qlength", "ID")]
  mt <- meta[, c("strain","length","gisaid_epi_isl","date","country","region","pangolin_lineage","GISAID_clade")]
  colnames(mt) <- c("sample","qlength","ID","time","country","region","lineage","Gclade")
  annot <- annot[, c("ID","protein","variant","varclass", "pro_variant", "annotation", "refpos", "refvar", "qvar", "qpos")]
  
  con <- dbConnect(RSQLite::SQLite(), dbpath)
  dbWriteTable(con, "covid_annot", annot, append = TRUE)
  dbWriteTable(con, "nucmerr", nucmerr, append = TRUE)
  dbWriteTable(con, "meta", mt, append = TRUE)
  #dbWriteTable(con, "mutdis", mutdis, append = TRUE)
  #dbWriteTable(con, "sample", sample, append = TRUE)
  dbExecute(con, "DELETE FROM covid_annot WHERE rowid NOT IN (SELECT min(rowid) FROM covid_annot GROUP BY ID, refpos, qpos, refvar, qvar)")
  dbExecute(con, "DELETE FROM nucmerr WHERE rowid NOT IN (SELECT min(rowid) FROM nucmerr GROUP BY ID, rpos, qpos, rvar, qvar)")
  dbExecute(con, "DELETE FROM meta WHERE rowid NOT IN (SELECT min(rowid) FROM meta GROUP BY ID)")
  #dbExecute(con, "DELETE FROM mutdis WHERE rowid NOT IN (SELECT min(rowid) FROM mutdis GROUP BY ID, refpos, qpos, refvar, qvar)")
  #dbExecute(con, "DELETE FROM sample WHERE rowid NOT IN (SELECT min(rowid) FROM sample GROUP BY ID, refpos, qpos, refvar, qvar)")
  
  print("update statistics")
  hpsfd <- file("../data/homepagestat.txt")
  llgfd <- file("../data/lineagelist.txt")
  cldfd <- file("../data/cladelist.txt")
  cotfd <- file("../data/countrylist.txt")
  homepagestat <- c()
  t <- dbGetQuery(con, "SELECT COUNT(*) FROM meta")
  homepagestat <- c(homepagestat, t[[1]])
  t <- dbGetQuery(con, "SELECT COUNT(*) FROM nucmerr")
  homepagestat <- c(homepagestat, round(t[[1]] / homepagestat[1], 4))
  t <- dbGetQuery(con, "SELECT COUNT(DISTINCT pro_variant) FROM covid_annot")
  homepagestat <- c(homepagestat, t[[1]])
  t <- dbGetQuery(con, "SELECT COUNT(DISTINCT PM_type) FROM nucmerr")
  homepagestat <- c(homepagestat, t[[1]])
  t <- dbGetQuery(con, "SELECT time FROM meta ORDER BY time DESC LIMIT 1")
  homepagestat <- c(homepagestat, t[[1]])
  t <- dbGetQuery(con, "SELECT time FROM meta ORDER BY time ASC LIMIT 1")
  homepagestat <- c(homepagestat, t[[1]])
  homepagestat <- as.character(homepagestat)
  writeLines(homepagestat, hpsfd)
  close(hpsfd)
  
  t <- dbGetQuery(con, "SELECT DISTINCT(lineage) FROM meta")
  t <- sort(t$lineage)
  t <- c(t, "all")
  writeLines(t, llgfd)
  close(llgfd)
  
  t <- dbGetQuery(con, "SELECT DISTINCT(Gclade) FROM meta")
  t <- sort(t$Gclade)
  t <- c(t, "all")
  writeLines(t, cldfd)
  close(cldfd)
  
  t <- dbGetQuery(con, "SELECT DISTINCT(region) FROM meta")
  tt <- dbGetQuery(con, "SELECT DISTINCT(country) FROM meta")
  tt <- sort(tt$country)
  t <- c(t$region, tt, "global")
  writeLines(t, cotfd)
  close(cotfd)
  
  dbDisconnect(con)
  print("done")
}
