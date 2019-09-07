#-----------------------------------------------------------------------------
#function: construct suffix array from a string
#param1: event_vec, characters vector, like c("a1","a2","b2","b3")
#param2: sa_min_len, remove the suffix array that length is less than sa_min_len
#return suffix array
#-----------------------------------------------------------------------------
constructSuffixArray <- function(event_vec,sa_min_len = 2){
  # nchar.value <- nchar(event_str)/2 #every two letters is one event, devide by 2
  # suffixs <- numeric(nchar.value)
  # for(i in 1:nchar.value)
  #   suffixs[i] <- substr(event_str,i*2-1,nchar.value*2) #performance issue
  #1. construct suffix array based on the original data, vector not string
  # event_vec.length <- length(event_vec)
  # suffixs <- numeric(event_vec.length)
  # for(i in 1:event_vec.length){
  #   suffixs[i] <- paste(event_vec[i:event_vec.length],collapse = "")
  # }
  #2. construct suffix array based on the original data, remove loop, equal to 1 using loop
  # suffixs <- lapply(seq_along(event_vec), function(i) paste0(event_vec[-(1:i)],collapse = ""))
  suffixs <- lapply(seq_along(event_vec), function(i) str_c(event_vec[-(1:i)],collapse = "")) # faster
  suffixs <- unlist(unname(suffixs))
  suffixs <- append(suffixs,str_c(event_vec,collapse = "")) #add the first element, contain all events
  suffixs <- suffixs[nchar(suffixs) > sa_min_len]  #remove suffix array that contains only one event
  suffixs <- sort(suffixs)
  return(suffixs)
}
#construct suffix array using C++ implementation, improve efficient
constructSuffixArrayC <- function(event_vec){
  suffixs <- suffixArray(event_vec)
  return(suffixs)
}
#-----------------------------------------------------------------------------
#function: construct lcp (longest common preffix)
#param1: suffix array, characters
#return LCP array
#Note: lcPrefixC is faster than lcprefix
#-----------------------------------------------------------------------------
constructLCP <- function(suffix_array){
  #compare the neibour two suffix array
  # current_suffix <- suffix_array[i]
  # previous_suffix<- suffix_array[i-1]
  ######rollapply function can handle two elements in one vector once#####
  lcp <- rollapply(suffix_array,2,function(tuple_suffix){
    # current_suffix <- tuple_suffix[1]
    # previous_suffix<- tuple_suffix[2]
    # lcprefix(tuple_suffix[1],tuple_suffix[2]) # this is too slow
    # nchar(lcPrefixC(c(tuple_suffix[1],tuple_suffix[2]))) #second faster
    LCPrefixCPP(tuple_suffix[1],tuple_suffix[2]) #faster
  })
  lcp <- append(lcp,0,0) #insert 0 to the first lcp
  return(lcp)
}

#-----------------------------------------------------------------------------
#function: mine patterns based on lcp (longest common preffix)
#param1: lcp array, numeric vector
#return patterns
#due to performance issue, we transform to Rcpp program
#-----------------------------------------------------------------------------
findPattern <- function(lcp){
  length.value <- length(lcp)
  pattern_count <- numeric(length.value)
  pattern_count[1] <- 0
  if(length.value < 2)
    return(pattern_count)
  for(i in 2:length.value){
    count <- 0
    if(lcp[i] < 2){
      pattern_count[i] <- 0
      next
    }
    #Scan lcp values after current index
    j <- i
    while ( (j < length.value) && (lcp[i] <= lcp[j+1]) ) {
      count <- count + 1
      if(lcp[i] == lcp[j+1])
        lcp[j+1] <- (-lcp[j+1])
      j <- j + 1
    }
    #Scan lcp values before current index
    j <- i
    while(j > 1 && lcp[i] <= abs(lcp[j-1])){
      if(lcp[i] == 0)
        break
      count <- count + 1
      j <- j - 1
    }
    #The aggregate of two counts + 2 is the frequency count of the current pattern.
    pattern_count[i] <- (count + 1)
  }
  return(pattern_count)
}


