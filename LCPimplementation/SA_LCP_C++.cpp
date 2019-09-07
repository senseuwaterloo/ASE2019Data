#include <Rcpp.h>
#include <vector>
#include <string>
#include <numeric>
#include <iostream>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
CharacterVector suffixArray(CharacterVector x){
  CharacterVector strs;
  std::string str;
  for (CharacterVector::iterator p = x.begin(); p!=x.end(); ++p) {
    str += *p;
  }
  strs.push_back(str);
  int length = x.size();
  for(int i=0; i<length-2;++i){
    x.erase(x.begin());
    str.clear();
    for (CharacterVector::iterator p = x.begin(); p!=x.end(); ++p) {
      str += *p;
    }
    strs.push_back(str);
  }
  std::sort(strs.begin(), strs.end());
  return strs; 
}

// [[Rcpp::export]]
List SuffixArrayByList(const List & list) { //vector
  List l;
  int length_l = list.size();
  std::string str;
  for(int i=0; i<length_l; ++i){
    CharacterVector seqs = list[i];
    CharacterVector strs;
    str.clear();
    for (CharacterVector::iterator p = seqs.begin(); p!=seqs.end(); ++p) {
      str += *p;
    }
    strs.push_back(str);
    int length_seqs = seqs.size();
    for(int j=0; j<length_seqs-2;++j){
      seqs.erase(seqs.begin());
      str.clear();
      for (CharacterVector::iterator p = seqs.begin(); p!=seqs.end(); ++p) {
        str += *p;
      }
      strs.push_back(str);
    }
    std::sort(strs.begin(), strs.end());
    l.push_back(strs);
  }
  return l;
}

// [[Rcpp::export]]
List SuffixArrayByCharacter(CharacterVector vectors) { //string
  std::string seq;
  int length_v = vectors.size();
  List l(length_v);
  for(int i=0; i<length_v; ++i){
    CharacterVector strs;
    seq = vectors[i];
    int length_str = seq.length();
    //Rcout << length_str << std::endl;
    for(int j=0; j<length_str-4; j = j+2){ // note: the step is 2, a1, a2
      //Rcout << (&(seq[j])) << std::endl;
      // strs.push_back(&(seq[j]));
      strs.push_back(seq.substr(j,60)); //handle only 30 events
    }
    //Rcout << strs << std::endl;
    //std::sort(strs.begin(), strs.end()); //error
    strs.sort();
    l[i] = strs;
  }
  return l;
}

// [[Rcpp::export]]
NumericVector ComputeLCPC(const List & list){
  int length_l = list.size();
  Rcout << length_l << std::endl;
  NumericVector lcp;
  std::string a;
  std::string b;
  for(int i=0; i<length_l; ++i){
    CharacterVector suffixs = list[i];
    int length_suffix = suffixs.size();
    lcp.push_back(0); //the first common prefix is 0
    for(int j=0; j<length_suffix-1; ++j){
      a = suffixs[j];
      b = suffixs[j+1];
      // Rcout << a << std::endl;
      // Rcout << b << std::endl;
      int k=0;
      while(a[k] != '\0' && b[k] != '\0' && a[k] == b[k])
       ++k;
      lcp.push_back(k);
    }
  }
  return lcp;
}

// [[Rcpp::export]]
int LCPrefixCPP(std::string a, std::string b){
  int k=0;
  while(a[k] == b[k] && a[k] != '\0' && b[k] != '\0')
    ++k;
  return k;
}

// [[Rcpp::export]]
NumericVector minePattern(NumericVector lcp){
  int lcp_len = lcp.length();
  NumericVector patterns(lcp_len);
  patterns[0] = 0;
  for(int i=1; i<lcp_len; ++i){
    int count = 0;
    if(lcp[i] <= 2){ //a1, only one event match
      patterns[i] = 0;
      continue;
    }
    int j = i;
    //Scan lcp values after current index
    while( (j<lcp_len) && (lcp[i] <= lcp[j])){
      ++count;
      ++j;
    }
    //Scan lcp values before current index
    j = i;
    while(j>1 && (lcp[i] <= lcp[j-1])){
      ++count;
      --j;
    }
    //The aggregate of two counts + 2 is the frequency count of the current pattern.
    patterns[i] = count+1;
  }
  return patterns;
}
