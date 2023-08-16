globalProteinMut <- function(covid_annot = covid_annot, outdir = NULL, figure_Type = "heatmap", top = 10, country = "global"){
  if(is.null(outdir) == FALSE){
    if(country != "global"){
      covid_annot<-covid_annot[covid_annot$country == country, ]
      covid_annot<- covid_annot[covid_annot$pro_variant %in% names(head(sort(table(covid_annot$pro_variant),decreasing=TRUE),n=top)),]
    }
    if(country == "global"){
      covid_annot<- covid_annot[covid_annot$pro_variant %in% names(head(sort(table(covid_annot$pro_variant),decreasing=TRUE),n=top)),]
    }
    return(covid_annot)
  }
  if(is.null(outdir) == TRUE){
    if(country != "global"){
      covid_annot<-covid_annot[covid_annot$country == country, ]
      covid_annot<- covid_annot[covid_annot$pro_variant %in% names(head(sort(table(covid_annot$pro_variant),decreasing=TRUE),n=top)),]
      if(figure_Type == "heatmap"){
        refpos <- covid_annot$refpos
        sample <- covid_annot$ID
        pro_variant <- covid_annot$pro_variant
        p <- ggplot(data=covid_annot,aes(x=refpos, y=ID))+
          geom_point(size=0.001, alpha=2/3,aes(color=pro_variant))+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")
        
      }
      if(figure_Type == "count"){
        refpos <- covid_annot$refpos
        sample <- covid_annot$ID
        pro_variant <- covid_annot$pro_variant
        p<-ggplot(data=covid_annot,aes(x=refpos,color=pro_variant))+
          geom_point(stat="count",size=2, alpha=2/3)+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")+
          scale_y_continuous(trans='log10')+
          labs(x="SARS-CoV-2 Genomic Postion",
               title = 'SARS-CoV-2: Mutation_Count')
        
        
      }
    }
    if(country == "global"){
      covid_annot<- covid_annot[covid_annot$pro_variant %in% names(head(sort(table(covid_annot$pro_variant),decreasing=TRUE),n=top)),]
      if(figure_Type == "heatmap"){
        refpos <- covid_annot$refpos
        sample <- covid_annot$ID
        pro_variant <- covid_annot$pro_variant
        p <- ggplot(data=covid_annot,aes(x=refpos, y=ID))+
          geom_point(size=0.001, alpha=2/3,aes(color=pro_variant))+
          theme_bw()+
          labs(x="SARS-CoV-2 Genomic position")
        
      }
      if(figure_Type == "count"){
        refpos <- covid_annot$refpos
        sample <- covid_annot$ID
        pro_variant <- covid_annot$pro_variant
        p<-ggplot(data=covid_annot,aes(x=refpos,color=pro_variant))+
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
