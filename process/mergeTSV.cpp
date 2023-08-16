#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <iostream>
//#include <time.h>
using namespace Rcpp;

// [[Rcpp::export]]

void mergeTSV(List nucmer, List meta){
  //clock_t start, end;
  //double totaltime;
  //start = clock();
  std::unordered_map<std::string, std::string> hash_meta_;
  List ret = nucmer;
  List strain = meta[0];
  List epi = meta[2];
  //List date = meta[4];
  List temp = nucmer[0];
  std::string key, value;
  
  for(uint i = 0; i < strain.size(); i++){
    key = as<std::string>(strain[i]);
    value = "|";
    value.append(as<std::string>(epi[i]));
    //value.append("|");
    //value.append(as<std::string>(date[i]));
    hash_meta_.insert(std::pair<std::string, std::string>(key, value));
  }
  
  CharacterVector t(temp.size());
  for(uint i = 0; i < temp.size(); i++){
    key = as<std::string>(temp[i]);
    auto it = hash_meta_.find(key);
    if(it != hash_meta_.end()){
      key.append(it->second);
      CharacterVector combine(key);
      
      //temp[i] = combine;
      temp[i] = combine;
      if(i % 100000 == 0){
        std::cout<<i<<std::endl;
      }
    } else {
      //std::cout<<key<<std::endl;
      continue;
    }
  }
  
  nucmer[0] = temp;
  //end = clock();
  //totaltime = (double)(end - start) / CLOCKS_PER_SEC;
  //std::cout<<totaltime<<std::endl;
  //return ret;
}
