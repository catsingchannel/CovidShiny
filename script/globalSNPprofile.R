globalSNPprofile <- function(nucmerr = nucmerr, outdir = NULL, figure_Type = "heatmap", country = "global", top = 5){
  if(is.null(outdir) == FALSE){
    if(country != "global"){
      sub_nucmer<-nucmerr[nucmerr$country == country, ]
      sub_nucmer<- sub_nucmer[sub_nucmer$M_type %in% names(head(sort(table(sub_nucmer$M_type),decreasing=TRUE),n=top)),]
    }
    if(country == "global"){
      sub_nucmer <- nucmerr
      sub_nucmer<- sub_nucmer[sub_nucmer$M_type %in% names(head(sort(table(sub_nucmer$M_type),decreasing=TRUE),n=top)),]
    }
    return(sub_nucmer)
  }
  if(is.null(outdir) == TRUE){
    if(country != "global"){
      sub_nucmer<-nucmerr[nucmerr$country == country, ]
      rpos <- sub_nucmer$rpos
      sample <- sub_nucmer$ID
      M_type <- sub_nucmer$M_type
      sub_nucmer<- sub_nucmer[sub_nucmer$M_type %in% names(head(sort(table(sub_nucmer$M_type),decreasing=TRUE),n=top)),]
      
      if(figure_Type == "heatmap"){
        p<-ggplot(data=sub_nucmer,aes(x=rpos, y=sample))+
          geom_point(size=0.001, alpha=2/3,aes(color=M_type))+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")
      }
      if(figure_Type == "count"){
        p<-ggplot(data=sub_nucmer,aes(x=rpos,color=M_type))+
          geom_point(stat="count",size=2, alpha=2/3)+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")+
          scale_y_continuous(trans='log10')+
          labs(x="SARS-CoV-2 Genomic Postion",
               title = 'SARS-CoV-2: Mutation_Count')
      }
    }
    if(country == "global"){
      sub_nucmer <- nucmerr
      sub_nucmer<- sub_nucmer[sub_nucmer$M_type %in% names(head(sort(table(sub_nucmer$M_type),decreasing=TRUE),n=top)),]
      rpos <- sub_nucmer$rpos
      sample <- sub_nucmer$ID
      M_type <- sub_nucmer$M_type
      
      if(figure_Type == "heatmap"){
        p<-ggplot(data=sub_nucmer,aes(x=rpos, y=ID))+
          geom_point(size=0.001, alpha=2/3,aes(color=M_type))+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")

        if(length(unique(sample)) > 25){
          p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())
        }
      }
      if(figure_Type == "count"){
        p<-ggplot(data=sub_nucmer,aes(x=rpos,color=M_type))+
          geom_point(stat="count",size=2, alpha=2/3)+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")+
          scale_y_continuous(trans='log10')+
          labs(x="SARS-CoV-2 Genomic Postion",
               title = 'SARS-CoV-2: Mutation_Count')
      }
    }
    print(p)
  }
}
