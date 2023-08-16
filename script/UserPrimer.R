UserPrimer <- function(nucmerr = nucmerr, pr = pr, sub = FALSE, country = "global", lastfiveNr = FALSE, totalsample = totalsample){
  if(country != "global"){
    sub_nucmer<-nucmerr[nucmerr$country == country, ]
    F1=pr$F1
    F2=pr$F2
    R1=pr$R1
    R2=pr$R2
    P1=pr$P1
    P2=pr$P2
    
    sub_nucmer<-subset(sub_nucmer, sub_nucmer$rpos %in% c(F1:F2,P1:P2,R1:R2))
    if(sub == TRUE){
      return(sub_nucmer)
    }
    if(sub == FALSE){
      TMN<- length(unique(sub_nucmer$sample))
      Mutation_Ratio<- round(TMN/totalsample*100,5)
      rpos <- sub_nucmer$rpos
      sample<- sub_nucmer$sample
      M_type<- sub_nucmer$M_type
      p <- ggplot(data=sub_nucmer,aes(x=rpos, y=sample,color=M_type))+
        geom_point(size=2)+
        theme_bw()+
        scale_x_continuous(breaks=seq(F1,R2,2),limits =c(F1,R2))+
        geom_vline(aes(xintercept=F1),color="blue", linetype="dashed", size=0.5)+
        geom_vline(aes(xintercept=F2),color="blue", linetype="dashed", size=0.5)+
        geom_vline(aes(xintercept=R1),color="red", linetype="dashed", size=0.5)+
        geom_vline(aes(xintercept=R2),color="red", linetype="dashed", size=0.5)+
        geom_vline(aes(xintercept=P1),color="gray", linetype="solid", size=0.5)+
        geom_vline(aes(xintercept=P2),color="gray", linetype="solid", size=0.5)+
        theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
        labs(x="SARS-CoV-2 Genomic position",
             title=paste0("User_assay","-Total Mutant Samples:", TMN,"/Mutation_Ratio:",Mutation_Ratio,"%")) 
      print(p)
      
    }
   
  }
  if(country == "global"){
    if(lastfiveNr == TRUE){
      F1=pr$F1
      F2=pr$F2
      F2five=pr$F2-5
      R1=pr$R1
      R1five=pr$R1+5
      R2=pr$R2
      
      sub_nucmer= nucmerr[nucmerr$rpos %in% c(F2five:F2,R1:R1five),]
      if(sub == TRUE){
        return(sub_nucmer)
      }
      if(sub == FALSE){
        TMN<- length(unique(sub_nucmer$ID))
        Mutation_Ratio<- round(TMN/totalsample*100,5)
        
        p <- ggplot(data =sub_nucmer,aes(x=rpos,y=ID, color=M_type))+
          geom_point()+
          theme_bw()+
          scale_x_continuous(breaks=seq(F1,R2,2),limits =c(F1,R2))+
          labs(x="SARS-CoV-2 Genomic position",
               title=paste0("User_primer","(Samples:", TMN,"/Ratio:",Mutation_Ratio,"%)"))+
          theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
                axis.text.y=element_blank(),
                axis.title.y=element_blank(),
                plot.title = element_text(size=8),
                legend.position="none")+
          geom_vline(aes(xintercept=F1),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=F2),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R1),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R2),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=F2five),color="gray", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R1five),color="gray", linetype="dashed", size=0.5)
        print(p)
        
      }
      
      
    }else{
      F1=pr$F1
      F2=pr$F2
      R1=pr$R1
      R2=pr$R2
      P1=pr$P1
      P2=pr$P2
      
      sub_nucmer<-subset(nucmerr, nucmerr$rpos %in% c(F1:F2,P1:P2,R1:R2))
      if(sub == TRUE){
        return(sub_nucmer)
      }
      if(sub == FALSE){
        TMN<- length(unique(sub_nucmer$ID))
        Mutation_Ratio<- round(TMN/totalsample*100,5)
        rpos <- sub_nucmer$rpos
        sample<- sub_nucmer$ID
        M_type<- sub_nucmer$M_type
        p <- ggplot(data=sub_nucmer,aes(x=rpos, y=sample,color=M_type))+
          geom_point(size=2)+
          theme_bw()+
          scale_x_continuous(breaks=seq(F1,R2,2),limits =c(F1,R2))+
          geom_vline(aes(xintercept=F1),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=F2),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R1),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R2),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=P1),color="gray", linetype="solid", size=0.5)+
          geom_vline(aes(xintercept=P2),color="gray", linetype="solid", size=0.5)+
          theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
          labs(x="SARS-CoV-2 Genomic position",
               title=paste0("User_assay","-Total Mutant Samples:", TMN,"/Mutation_Ratio:",Mutation_Ratio,"%")) 
        print(p)
      }
      
    }
    
  }
  
}
