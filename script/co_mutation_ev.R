co_mutation_ev <- function(results = covid_annot, mut1 = mut1, mut2 = mut2, variant = "protein", refpos1 = "28881", refpos2 = "13342", outdir = NULL){
  if(variant == "protein"){
    sub1<- subset(results, results$pro_variant == mut1)
    sub2 <- subset(results, results$pro_variant == mut2)
  }
  if(variant == "nucleotide"){
    sub1<- subset(results, results$refvar == mut1[1] & results$qvar == mut1[2] & results$refpos == refpos1)
    sub2 <- subset(results, results$refvar == mut2[1] & results$qvar == mut2[2] & results$refpos == refpos2)
    mut1 <- paste(mut1[1], "->", mut1[2], sep = "")
    mut2 <- paste(mut2[1], "->", mut2[2], sep = "")
  }
  if(variant == "pro&Nr"){
    sub1<- subset(results, results$pro_variant == mut1)
    sub2 <- subset(results, results$refvar == mut2[1] & results$qvar == mut2[2] & results$refpos == refpos2)
    mut2 <- paste(refpos2, ":", mut2[1], "->", mut2[2], sep = "")
  }
  if(variant == "Nr&pro"){
    sub1 <- subset(results, results$refvar == mut1[1] & results$qvar == mut1[2] & results$refpos == refpos1)
    sub2<- subset(results, results$pro_variant == mut2)
    mut1 <- paste(refpos1, ":", mut1[1], "->", mut1[2], sep = "")
  }
  overlap<- inner_join(sub1,sub2,by = "ID")
  
  if(is.null(outdir) == TRUE){
    co_occur<- length(unique(overlap$ID))
    Mutation_Ratio1<- round(co_occur/length(unique(sub1$ID))*100,5) 
    Mutation_Ratio2<- round(co_occur/length(unique(sub2$ID))*100,5)
    venn(list(unique(sub1$ID), unique(sub2$ID)), zcolor = "style", box = F, snames = c(mut1, mut2), plotsize = 25, lty = 5, ilcs = 1, sncs = 1)
    
  }
  if(is.null(outdir) == FALSE){
    return(overlap)
  }
}