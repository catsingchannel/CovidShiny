obtain_simu_seq <- function(nucmerr, refseq){
  nuc <- nucmerr
  total <- length(unique(nucmerr$ID))
  PM <- sort(table(nucmerr$PM_type), decreasing = TRUE)
  PM <- PM[PM >= round(total * 0.001)]
  lrf <- refseq
  
  nuc <- nucmerr[nucmerr$PM_type %in% names(PM),]
  nuc <- nuc[!duplicated(nuc[,'PM_type']),]
  nuc$Freq <- 0
  for (i in 1:nrow(nuc)) {
    nuc[i, 'Freq'] <- PM[nuc[i, 'PM_type']]
  }
  nuc <- nuc[order(nuc[,'Freq'], decreasing = TRUE),]
  nuc <- nuc[!duplicated(nuc[,'rpos']),]
  
  nuc$rend <- nuc$rpos + nchar(nuc$rvar) - 1
  nuc$qend <- nuc$rpos + nchar(nuc$qvar) - 1
  nuc$lb <- TRUE
  for(i in 1:nrow(nuc)){
    if(nuc[i, 'lb'] == FALSE) next
    
    if(nuc[i, 'rpos'] != nuc[i, 'qend']){
      nuc[nuc$rvar == '.' & nuc$qend >= nuc[i, 'rpos'] & nuc$rpos <= nuc[i, 'qend'],'lb'] <- FALSE
      nuc[i, 'lb'] <- TRUE
      next
    }
    
    nuc[nuc$rend >= nuc[i, 'rpos'] & nuc$rpos <= nuc[i, 'rend'],'lb'] <- FALSE
    nuc[i, 'lb'] <- TRUE
  }
  nuc <- nuc[nuc$lb, c('rpos', 'rvar', 'qvar', 'rend')]
  
  for(i in 1:nrow(nuc)){
    if(nuc[i,'rvar'] == '.'){
      lrf[nuc[i,'rpos']] <- paste0(nuc[i, 'qvar'], lrf[nuc[i,'rpos']])
      next
    }
    
    if(nuc[i,'qvar'] == '.') {
      lrf[nuc[i,'rpos']:nuc[i,'rend']] <- ''
      next
    }
    
    if(nchar(nuc[i,'rvar']) > 1){
      lrf[nuc[i,'rpos']:nuc[i,'rend']] <- ''
      lrf[nuc[i,'rpos']] <- nuc[i, 'qvar']
      next
    }
    
    lrf[nuc[i, 'rpos']] <- nuc[i, 'qvar']
  }
  lrf <- paste(lrf, collapse = '')
  
  lrf
}

obtain_assays <- function(lineage = 'all', country='global', target, date){
  nucmerr <- dbGetQuery(dbcon, 'SELECT ID, country, region, lineage FROM meta WHERE time BETWEEN ? AND ?', params = c(date[1], date[2]))
  
  if(!('all' %in% lineage)){
    nucmerr <- nucmerr[nucmerr$lineage %in% lineage,]
  }
  
  if(!('global' %in% country)){
    if(any(countrylist[1:6] %in% country)){
      nucmerr <- nucmerr[nucmerr$region %in% country | nucmerr$country %in% country,]
    } else {
      nucmerr <- nucmerr[nucmerr$country %in% country,]
    }
  }
  
  nucmerr <- dbGetQuery(dbcon, 'SELECT meta.ID, meta.lineage, nucmerr.rpos, nucmerr.qpos, nucmerr.rvar, nucmerr.qvar, nucmerr.PM_type FROM meta 
                           INNER JOIN nucmerr ON meta.ID == nucmerr.ID 
                           WHERE meta.ID == ?', list(nucmerr$ID))
  nucmerr <- nucmerr[nucmerr$rpos %in% target,]
  gc()
  
  li <- names(sort(table(nucmerr$lineage), decreasing = TRUE))
  li <- li[1:min(50, length(li))]
  rf <- unlist(strsplit(as.character(refseq), ''))
  seqs <- c()
  for(i in 1:length(li)){
    sub_nucmerr <- nucmerr[nucmerr$lineage == li[i],]
    if(nrow(sub_nucmerr) < 1) {
      seqs[i] <- ""
    } else {
      seqs[i] <- obtain_simu_seq(sub_nucmerr, rf) 
    }
  }
  
  seqs <- seqs[nzchar(seqs)]
  seqs <- DNAStringSet(seqs, start = target[1], end = target[length(target)])
  seqs <- msa(seqs)
  
  consensus_profile <- consensusProfile(seqs, ambiguityThreshold = 0.05)
  oligos <- designOligos(consensus_profile, 
                         lengthPrimer = c(17, 25), 
                         gcPrimer = c(0.4, 0.6), 
                         tmPrimer = c(50, 60), 
                         lengthProbe = c(15, 30))
  myassays <- designAssays(oligos, 
                         length = c(90, 150),
                         tmDifferencePrimers = 5)
  myassays <- myassays[myassays$score == min(myassays$score), ]
  
  myassays <- as.data.frame(myassays)
  myassays$tmi <- abs(myassays$tmMeanFwd - myassays$tmMeanRev)
  myassays$tmmd <- abs(50 - (myassays$tmMeanFwd + myassays$tmMeanRev) / 2)
  myassays$gci <- abs(myassays$gcContentMeanFwd - myassays$gcContentMeanRev)
  myassays$gcmd <- abs(50 - (myassays$gcContentMeanFwd + myassays$gcContentMeanRev) / 2)
  myassays$vas <- 0
  for(i in 1:nrow(myassays)){
    myassays[i, 'sequenceFwd'] <- myassays[i, 'sequenceFwd'][[1]][1]
    myassays[i, 'gcContentFwd'] <- myassays[i, 'gcContentFwd'][[1]][1]
    myassays[i, 'tmFwd'] <- myassays[i, 'tmFwd'][[1]][1]
    myassays[i, 'sequenceRev'] <- myassays[i, 'sequenceRev'][[1]][1]
    myassays[i, 'gcContentRev'] <- myassays[i, 'gcContentRev'][[1]][1]
    myassays[i, 'tmRev'] <- myassays[i, 'tmRev'][[1]][1]
    myassays[i, 'sequencePr'] <- myassays[i, 'sequencePr'][[1]][1]
    myassays[i, 'sequenceRcPr'] <- myassays[i, 'sequenceRcPr'][[1]][1]
    myassays[i, 'gcContentPr'] <- myassays[i, 'gcContentPr'][[1]][1]
    myassays[i, 'tmPr'] <- myassays[i, 'tmPr'][[1]][1]
    myassays[i, 'vas'] <- length(myassays[i, 'sequenceFwd'][[1]]) + length(myassays[i, 'sequenceRev'][[1]]) + length(myassays[i, 'sequencePr'][[1]])
  }
  
  myassays <- myassays[order(myassays[,'vas']),]
  myassays <- myassays[1:(min(100, nrow(myassays))),]
  myassays <- myassays[order(myassays[,'tmi']),]
  myassays <- myassays[1:(min(10, nrow(myassays))),]
  myassays <- myassays[order(myassays[,'gci']),]
  myassays <- myassays[1:(min(5, nrow(myassays))),]
  myassays <- myassays[order(myassays[,'tmmd']),]
  myassays <- myassays[1:(min(3, nrow(myassays))),]
  myassays <- myassays[order(myassays[,'gcmd']),]
  myassays$startFwd <- myassays$startFwd - 1 + target[1]
  myassays$endFwd <- myassays$endFwd - 1 + target[1]
  myassays$startRev <- myassays$startRev - 1 + target[1]
  myassays$endRev <- myassays$endRev - 1 + target[1]
  myassays$startPr <- myassays$startPr - 1 + target[1]
  myassays$endPr <- myassays$endPr - 1 + target[1]
  
  rt <- as.data.frame(c("Foward Primer", "Reverse Primer", "Probe", "Reverse Probe"))
  angle <- paste0("Product Length:", myassays[1,'length'], "bp")
  rt$Sequence <- c(myassays[1,'sequenceFwd'], myassays[1,'sequenceRev'], myassays[1,'sequencePr'], myassays[1,'sequenceRcPr'])
  rt$Tm <- c(myassays[1,'tmFwd'], myassays[1,'tmRev'], myassays[1,'tmPr'], myassays[1,'tmPr'])
  rt$Tm <- round(as.numeric(rt$Tm), 2)
  rt$GcContent <- c(myassays[1, 'gcContentFwd'], myassays[1, 'gcContentRev'], myassays[1, 'gcContentPr'], myassays[1, 'gcContentPr'])
  rt$GcContent <- round(as.numeric(rt$GcContent), 2)
  rt$PrimerLength <- c(myassays[1, 'lengthFwd'], myassays[1, 'lengthRev'], myassays[1, 'lengthPr'], myassays[1, 'lengthPr'])
  rt$StartPosition <- c(myassays[1, 'startFwd'], myassays[1, 'startRev'], myassays[1, 'startPr'], myassays[1, 'startPr'])
  rt$EndPosition <- c(myassays[1, 'endFwd'], myassays[1, 'endRev'], myassays[1, 'endPr'], myassays[1, 'endPr'])
  colnames(rt)[1] <- angle
  rt
}