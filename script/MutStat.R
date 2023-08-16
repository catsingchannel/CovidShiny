MutStat <- function(nucmerr = nucmerr, figure_Type = "TopMuSample", type_top = 10, mutpos = NULL){
  # mutation statics for Nucleiotide
  if(figure_Type == "TopMuSample"){
    ##2 Most mutated sample
    MMS<-head(sort(table(nucmerr$sample),decreasing=TRUE),n=type_top)
    #par(las=3,mar=c(15,5,5,1))#par should be adjusted
    barplot(MMS,ylab="nr of mutations",main=paste("Top", type_top, "mutated samples"),col="lightblue")
  }
  ##3 Averagemutation per sample
  if(figure_Type == "AverageMu"){
    MPS<- table(table(nucmerr$sample))
    #par(las=1,mar=c(5,5,5,2))
    barplot(MPS, ylab="Case Number",xlab="NR of Mutation",main="Summarized Mutation Per Sample",col="lightblue")
  }
  if(figure_Type == "TopMuPos"){
    ##4.4 Top30 Mutation Sites
    PM_Site_top <-head(sort(table(nucmerr$PM_type), decreasing = T),n=type_top)
    #par(las=2,mar=c(8,5,5,2))
    barplot(PM_Site_top,ylab="number", main=paste("Top", type_top, "Mutation site"),col=rainbow(length(PM_Site_top)))
  }

    if(figure_Type == "CountryMutCount"){
      ### mutation country distribution
      
      country_sample<-nucmerr[!duplicated(nucmerr$sample),c("country", "sample")]
      
      #table(country_sample$country=="China")
      #par(las=2,mar=c(8,5,5,2))
      barplot(head(sort(table(country_sample$country),decreasing =T),n = type_top),
              main=paste("Top", type_top, "Countries Mutation counts"), col=rainbow(1:length(country_sample$sample)))
    }
    if(figure_Type == "TopCountryMutDens"){
      # country mutation type in Top10 country
      if(is.null(mutpos) == TRUE){
        country_sample<-nucmerr[!duplicated(nucmerr$sample),c("country", "sample")]
        Top_country<- names(head(sort(table(country_sample$country),decreasing =T),n=type_top))
        rpos<- nucmerr[nucmerr$country %in% Top_country,]$rpos
        ggplot(data=nucmerr[nucmerr$country %in% Top_country,],aes(x=rpos))+
          geom_density()+
          theme_bw()+
          facet_grid(country~ .)
      }
      if(is.null(mutpos) == FALSE){
        country_sample<-nucmerr[!duplicated(nucmerr$sample),c("country", "sample")]
        Top_country<- names(head(sort(table(country_sample$country),decreasing =T),n=type_top))
        rpos<- nucmerr[nucmerr$country %in% Top_country & nucmerr$rpos %in% mutpos,]$rpos
        ggplot(data=nucmerr[nucmerr$country %in% Top_country & nucmerr$rpos %in% mutpos,],aes(x=rpos))+
          geom_density()+
          theme_bw()+
          facet_grid(country~ .)
      }
    }
}