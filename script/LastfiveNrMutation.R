
## bacth assay analysis for last five Nr of primers

LastfiveNrMutation <- function(nucmerr = nucmerr, assays = assays, totalsample = totalsample, outputAssay = "ZS-ORf1ab", sub = FALSE){
  assays$MR_Five=0
  
  for (i in 1:length(assays$Assay)){
    if(outputAssay == assays$Assay[i]){
      F1=assays[i,"F1"]
      F2five=as.numeric(assays[i,"F2"])-5
      F2=assays[i,"F2"]
      R1=assays[i,"R1"]
      R1five=as.numeric(assays[i,"R1"])+5
      R2=assays[i,"R2"]
      
      F1 = as.numeric(F1)
      F2 = as.numeric(F2)
      R1 = as.numeric(R1)
      R2 = as.numeric(R2)
      
      sub_nucmer= nucmerr[nucmerr$rpos %in% c(F2five:F2,R1:R1five),]
      if(sub == TRUE){
        return(sub_nucmer)
      }
      if(sub == FALSE){
        TMN<- length(unique(sub_nucmer$ID))
        Mutation_Ratio<- round(TMN/totalsample*100,5)
        
        assays[i,"MR_Five"]<- Mutation_Ratio
        
        p <- ggplot(data =sub_nucmer,aes(x=rpos,y=ID, color=M_type))+
          geom_point()+
          theme_bw()+
          scale_x_continuous(breaks=seq(F1,R2,2),limits =c(F1,R2))+
          labs(x="SARS-CoV-2 Genomic position", y="samples",
               title=paste0(assays$Assay[i],"(Samples:", TMN,"/Ratio:",Mutation_Ratio,"%)"))+
          theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
                plot.title = element_text(size=8),
                legend.position="none")+
          geom_vline(aes(xintercept=F1),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=F2),color="blue", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R1),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R2),color="red", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=F2five),color="gray", linetype="dashed", size=0.5)+
          geom_vline(aes(xintercept=R1five),color="gray", linetype="dashed", size=0.5)

        if(TMN > 25){
          p <- p + theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
        }

        print(p)
      }
    }
  }
}
