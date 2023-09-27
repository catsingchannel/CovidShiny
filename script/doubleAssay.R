doubleAssay<- function(nucmerr = nucmerr, assay = assay, assay1 = assay1, assay2 = assay2, outdir = NULL, sub = FALSE, venn = FALSE){
  #assay1:
  F1=assay[assay$Assay == assay1,"F1"]
  F2=assay[assay$Assay == assay1,"F2"]
  R1=assay[assay$Assay == assay1,"R1"]
  R2=assay[assay$Assay == assay1,"R2"]
  P1=assay[assay$Assay == assay1,"P1"]
  P2=assay[assay$Assay == assay1,"P2"]
  
  F1 = as.numeric(F1)
  F2 = as.numeric(F2)
  R1 = as.numeric(R1)
  R2 = as.numeric(R2)
  P1 = as.numeric(P1)
  P2 = as.numeric(P2)
  
  #assay2:
  f1=assay[assay$Assay == assay2,"F1"]
  f2=assay[assay$Assay == assay2,"F2"]
  r1=assay[assay$Assay == assay2,"R1"]
  r2=assay[assay$Assay == assay2,"R2"]
  p1=assay[assay$Assay == assay2,"P1"]
  p2=assay[assay$Assay == assay2,"P2"]
  
  f1 = as.numeric(f1)
  f2 = as.numeric(f2)
  r1 = as.numeric(r1)
  r2 = as.numeric(r2)
  p1 = as.numeric(p1)
  p2 = as.numeric(p2)
  
  sub_nucmer<-subset(nucmerr, nucmerr$rpos %in% c(F1:F2,P1:P2,R1:R2))
  sub_nucmer2 <- subset(nucmerr, nucmerr$rpos %in% c(f1:f2,p1:p2,r1:r2))
  rpos.x <- sub_nucmer$rpos.x
  M_type.x<- sub_nucmer$M_type.x
  
  overlap<- inner_join(sub_nucmer,sub_nucmer2,by = "ID")
  if(sub == FALSE){
    
    co_occur<- length(unique(overlap$ID))
    Mutation_Ratio1<- round(co_occur/length(unique(sub_nucmer$ID))*100,5)
    Mutation_Ratio2<- round(co_occur/length(unique(sub_nucmer2$ID))*100,5)
    plist <- list()
    plist[[1]]<-ggplot(data=overlap,aes(x=rpos.x, y=ID,color=M_type.x))+
      geom_point(size=2)+
      theme_bw()+
      scale_x_continuous(breaks=seq(F1,R2,2),limits =c(F1,R2))+
      geom_vline(aes(xintercept=F1),color="blue", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=F2),color="blue", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=R1),color="red", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=R2),color="red", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=P1),color="gray", linetype="solid", size=0.5)+
      geom_vline(aes(xintercept=P2),color="gray", linetype="solid", size=0.5)+
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
      labs(x="SARS-CoV-2 Genomic position",
           title=paste0(assay1,"-Co-occurring Mutant Samples:", co_occur,"/co_Mutation_Ratio:",Mutation_Ratio1,"%"))#+

    if(length(unique(sub_nucmer$ID)) > 25) {
        plist[[1]] <- plist[[1]] + theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }

    rpos.y <- sub_nucmer$rpos.y
    M_type.y<- sub_nucmer$M_type.y
    plist[[2]]<-ggplot(data=overlap,aes(x=rpos.y, y=ID,color=M_type.y))+
      geom_point(size=2)+
      theme_bw()+
      scale_x_continuous(breaks=seq(f1,r2,2),limits =c(f1,r2))+
      geom_vline(aes(xintercept=f1),color="blue", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=f2),color="blue", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=r1),color="red", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=r2),color="red", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=p1),color="gray", linetype="solid", size=0.5)+
      geom_vline(aes(xintercept=p2),color="gray", linetype="solid", size=0.5)+
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
      labs(x="SARS-CoV-2 Genomic position",
           title=paste0(assay2,"-Co-occurring Mutant Samples:", co_occur,"/co_Mutation_Ratio:",Mutation_Ratio2,"%"))#+

    if(length(unique(sub_nucmer$ID)) > 25) {
        plist[[2]] <- plist[[2]] + theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }

    p<- ggarrange(plist[[1]],plist[[2]], ncol=1,nrow=2,labels=c("A","B"))
    if(is.null(outdir) == TRUE){
      print(p)
      
      
    }
    if(is.null(outdir) == FALSE){
      ggsave(p,filename=paste0("doubleAssay",'.png'),width = 18, height = 12, dpi=300,path = outdir)
      write.csv(overlap, file.path(outdir, "overlap_samples.csv"))
    }
  }
  
  if(sub == TRUE){
    return(overlap)
  }
  if(venn == TRUE){
    venn(list(unique(sub_nucmer$ID), unique(sub_nucmer2$ID)), zcolor = "style", box = F, snames = c(assay1, assay2), plotsize = 25, lty = 5, ilcs = 1, sncs = 1)
  }
}
