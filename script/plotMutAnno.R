plotMutAnno <- function(results = results,figureType = "MostMut"){
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))# resetting the par when exit function
  if(figureType == "MostMut"){
    # Most mutated samples
    occ<-sort(table(results$ID),decreasing=TRUE)[1:20]
    par(las=2,mar=c(15,5,5,1))
    barplot(occ,ylab="nr of mutations",main="Most mutated samples",col=heat.colors(length(occ)))
    # dev.off()
  }
  if(figureType == "MutPerSample"){
    occ<-table(table(results$ID))
    par(las=2,mar=c(5,5,5,1))
    barplot(occ,xlab="nr of mutations",main="Overall mutations per sample",col="cornflowerblue")
  }
  if(figureType == "VarClasses"){
    occ<-sort(table(results$varclass),decreasing=TRUE)
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of events",main="Most frequent events per class",col=heat.colors(length(occ)))
  }
  if(figureType == "VarType"){
    occ<-sort(table(apply(results[,c("refvar","qvar")],1,paste0,collapse=">")),decreasing=TRUE)[1:20]
    par(las=2,mar=c(5,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent per type",col=heat.colors(length(occ)))
  }
  if(figureType == "NucleoEvents"){
    occ<-sort(table(apply(results[,c("refvar","refpos","qvar")],1,paste0,collapse="")),decreasing=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (nucleotide)",col=heat.colors(length(occ)))
  }
  if(figureType == "ProEvents"){
    occ<-sort(table(apply(results[,c("protein","variant")],1,paste0,collapse=":")),decreasing=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (protein)",col=terrain.colors(length(occ)))
  }
}
