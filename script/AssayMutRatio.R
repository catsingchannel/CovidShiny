AssayMutRatio <- function(nucmerr = nucmerr, assays = assays, totalsample = totalsample, plotType = "barplot", outputAssay = "ZS-ORf1ab", sub = FALSE, country = "global"){
  if(is.null(outputAssay) == FALSE){
    if(country != "global"){
      
      for (i in 1:length(assays$Assay)){
        if(outputAssay == assays$Assay[i]){
          
          sub_nucmer<-nucmerr[nucmerr$country == country, ]
          totalcountry <- length(unique(sub_nucmer$sample))
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(sub_nucmer, sub_nucmer$rpos %in% c(F1:F2,P1:P2,R1:R2))
          if(sub == TRUE){
            return(sub_nucmer)
          }
          if(sub == FALSE){
            TMN<- length(unique(sub_nucmer$sample))
            Mutation_Ratio<- round(TMN/totalcountry*100,5)
            assays[i,"Mutation_Ratio"]<- Mutation_Ratio
            
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
                   title=paste0(assays$Assay[i], "-",country, "-Total Mutant Samples:", TMN,"/", totalcountry, "-Mutation_Ratio:",Mutation_Ratio,"%")) 
            print(p)#for ggplot2 figure, source will not present the figure!
          }
        }
        #输入plotType则输出global figure，不输入则单独的figure
      }
    }
    if(country == "global"){
      for (i in 1:length(assays$Assay)){
        if(outputAssay == assays$Assay[i]){
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(nucmerr, nucmerr$rpos %in% c(F1:F2,P1:P2,R1:R2))
          if(sub == TRUE){
            return(sub_nucmer)
          }
          if(sub == FALSE){
            TMN<- length(unique(sub_nucmer$ID))
            Mutation_Ratio<- round(TMN/totalsample*100,5)
            assays[i,"Mutation_Ratio"]<- Mutation_Ratio
            
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
                   title=paste0(assays$Assay[i],"-Total Mutant Samples:", TMN,"/Mutation_Ratio:",Mutation_Ratio,"%")) 
            print(p)#for ggplot2 figure, source will not present the figure!
          }
        }
        #输入plotType则输出global figure，不输入则单独的figure
      }
    }
  }
  if(is.null(outputAssay) == TRUE){
    if(country != "global"){
      
      if(plotType == "barplot"){
        for (i in 1:length(assays$Assay)){
          sub_nucmer<-nucmerr[nucmerr$country == country, ]
          totalcountry <- length(unique(sub_nucmer$sample))
          
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(sub_nucmer, sub_nucmer$rpos %in% c(F1:F2,P1:P2,R1:R2))

          TMN<- length(unique(sub_nucmer$sample))
          Mutation_Ratio<- round(TMN/totalcountry*100,5)
          assays[i,"Mutation_Ratio"]<- Mutation_Ratio}
        
        barplot(data=assays, log2(assays$Mutation_Ratio) ~ assays$Assay, col=heat.colors(12), xlab = "Assays", ylab = "Mutation Ratio%" )
      }
      if(plotType == "logtrans"){
        for (i in 1:length(assays$Assay)){
          sub_nucmer<-nucmerr[nucmerr$country == country, ]
          totalcountry <- length(unique(sub_nucmer$sample))
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(sub_nucmer, sub_nucmer$rpos %in% c(F1:F2,P1:P2,R1:R2))

          TMN<- length(unique(sub_nucmer$sample))
          Mutation_Ratio<- round(TMN/totalcountry*100,5)
          assays[i,"Mutation_Ratio"]<- Mutation_Ratio}
        order_assay=assays[order(assays$Mutation_Ratio),]$Assay
        
        assays$Assay=factor(assays$Assay, levels=order_assay)
        p<- ggplot(data=assays,aes(x=assays$Assay,y=assays$Mutation_Ratio,fill=assays$Assay))+
          geom_bar(stat="identity")+
          geom_text(aes(label=assays$Mutation_Ratio),vjust=0,angle = 90)+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45,hjust = 1,size=12, face="bold"),
                axis.text.y = element_text(size=12,face="bold"))+
          scale_y_sqrt()+labs(x = "Assays", y = "Mutation Ratio%", title = paste("Mutation Ratio in ", country, sep = ""))
        scale_y_continuous(trans='log10')
        print(p)
      }
    }
    if(country == "global"){
      if(plotType == "barplot"){
        for (i in 1:length(assays$Assay)){
          
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(nucmerr, nucmerr$rpos %in% c(F1:F2,P1:P2,R1:R2))

          TMN<- length(unique(sub_nucmer$sample))
          Mutation_Ratio<- round(TMN/totalsample*100,5)
          assays[i,"Mutation_Ratio"]<- Mutation_Ratio}
        
        barplot(data=assays, log2(assays$Mutation_Ratio) ~ assays$Assay, col=heat.colors(12), xlab = "Assays", ylab = "Mutation Ratio%")
      }
      if(plotType == "logtrans"){
        for (i in 1:length(assays$Assay)){
          
          F1=assays[i,"F1"]
          F2=assays[i,"F2"]
          R1=assays[i,"R1"]
          R2=assays[i,"R2"]
          P1=assays[i,"P1"]
          P2=assays[i,"P2"]
          
          F1 = as.numeric(F1)
          F2 = as.numeric(F2)
          R1 = as.numeric(R1)
          R2 = as.numeric(R2)
          P1 = as.numeric(P1)
          P2 = as.numeric(P2)
          
          sub_nucmer<-subset(nucmerr, nucmerr$rpos %in% c(F1:F2,P1:P2,R1:R2))

          TMN<- length(unique(sub_nucmer$ID))
          Mutation_Ratio<- round(TMN/totalsample*100,5)
          assays[i,"Mutation_Ratio"]<- Mutation_Ratio}
        order_assay=assays[order(assays$Mutation_Ratio),]$Assay
        
        assays$Assay=factor(assays$Assay, levels=order_assay)
        p<- ggplot(data=assays,aes(x=assays$Assay,y=assays$Mutation_Ratio,fill=assays$Assay))+
          geom_bar(stat="identity")+
          geom_text(aes(label=assays$Mutation_Ratio),vjust=0,angle = 90)+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45,hjust = 1,size=12, face="bold"),
                axis.text.y = element_text(size=12,face="bold"))+
          theme(plot.margin = unit(c(1,1,1,1), "cm"))+
          scale_y_sqrt()+labs(x = "Assays", y = "Mutation Ratio%", title = "Mutation Ratio in global")
        scale_y_continuous(trans='log10')
        print(p)
      }
    }
  }
}
