upsetr <- function(covid_annot = covid_annot, top = 10, protein = "N"){
  pro <- covid_annot[covid_annot$protein == protein, ]
  ch <- sort(table(pro$variant), decreasing = TRUE)
  ch <- names(ch)
  t <- min(c(top, length(ch)))
  
  L <- list()
  for(i in 1:t){
    L[[i]] <- pro[pro$variant == ch[i], ]$ID
  }
  names(L) <- c(ch[1:t])
  
  if(t < 2){
    return
  }
  
  p <- upset(fromList(L), nsets = t, order.by = "freq")
  print(p)
}