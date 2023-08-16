primerAlign <- function(pattern1 = pattern1, pattern2 = pattern2, pattern3 = pattern3, refseq = refseq, probe_di = "forward"){
  reverse<- reverseComplement(refseq)
  DNA_pair_F<- matchPattern(pattern = pattern1,  refseq)
  F1<- DNA_pair_F@ranges@start
  F2 <- DNA_pair_F@ranges@start+DNA_pair_F@ranges@width-1
  
  DNA_pair_R<- matchPattern(pattern = pattern2,  reverse)
  R2<- 29903-DNA_pair_R@ranges@start+1
  R1 <- R2 - DNA_pair_R@ranges@width+1
  if(probe_di == "not available"){
    pr <- data.frame(F1 = F1,
                     F2 = F2, 
                     R1 = R1,
                     R2 = R2) 
  } else if (probe_di == "reverse") {
    DNA_pair_P <- matchPattern(pattern = pattern3,  reverse)
    P2<- 29903-DNA_pair_P@ranges@start+1
    P1 <- P2 - DNA_pair_P@ranges@width+1
  } else if (probe_di == "forward") {
    DNA_pair_P <- matchPattern(pattern = pattern3,  refseq)
    P1<- DNA_pair_P@ranges@start
    P2 <- DNA_pair_P@ranges@start+DNA_pair_P@ranges@width-1
  }

  if(probe_di == "reverse" | probe_di == "forward"){
    pr <- data.frame(F1 = F1,
                     F2 = F2, 
                     R1 = R1,
                     R2 = R2,
                     P1 = P1,
                     P2 = P2) 
  }
  
  return(
    pr
  )
  
}
