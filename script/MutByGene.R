MutByGene <- function(nucmerr = nucmerr, gff3 = gff3, gene = gene, top = 10){
  #Requirements: gff3 includes: Gene, Start, Stop
  
  P1 = gff3[gff3$Gene == gene,"Start"]
  P2 = gff3[gff3$Gene == gene,"Stop"]
  narrow_nucmer <- nucmerr[nucmerr$rpos %in% P1:P2,]
  narrow_nucmer <- narrow_nucmer[narrow_nucmer$M_type %in% names(sort(table(narrow_nucmer$M_type), decreasing = T))[1:top],]
  rpos <- narrow_nucmer$rpos
  M_type <- narrow_nucmer$M_type
  
  p<-ggplot(data=narrow_nucmer,aes(x=rpos, color=M_type))+
    geom_point(stat="count",size=2, alpha=2/3)+
    theme_bw()+
    scale_y_continuous(trans='log10')+
    labs(x="SARS-CoV-2 Genomic Postion",
         title = paste0(gene,'_Mutation_Count'))
  print(p)
}

GeneList <- function(nucmerr = nucmerr, gff3 = gff3){
  #Requirements: gff3 includes: Gene, Start, Stop
  mut_perLoci <- list()
  gene_name <- c()
  mut_frequency <- c()
  for ( i in 1:length(gff3$Gene)){
    P1=gff3[i,"Start"]
    P2=gff3[i,"Stop"]
    gene_name <- c(gene_name, gff3[i,"Gene"])
    geneLength <- P2-P1
    mut_frequency <- c(mut_frequency,
                       length(unique(nucmerr[nucmerr$rpos %in% P1:P2,]$ID))/geneLength)
  }
    
  Mutlist<- data.frame(Gene = gene_name, 
                       MutFrequency = mut_frequency)
  return(Mutlist)
}