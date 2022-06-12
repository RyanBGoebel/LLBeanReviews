# ==============================================================================
# FUNCTION DEFINITION
# ==============================================================================
list.of.packages <- c("tm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tm)


clean_corp <- function(feature,stop_words=NULL,comb_words=NULL) {
  # Take the variable and create a corpus object
  corp <- Corpus(VectorSource(feature))
  
  # Create Random Number for sample definition
  rand <- round(runif(1, 1, 1000),0)
  print("Sample Pre-Cleaned Review Comment:")
  print(corp[[rand]]$content)
  
  # Strip whitespace
  corp <- tm_map(corp, stripWhitespace)

  # Remove numbers
  corp <- tm_map(corp, removeNumbers)
  
  # Remove punctuation
  corp <- tm_map(corp, removePunctuation)
  
  # Convert to lower case
  corp <- tm_map(corp, tolower) 
  
  # Remove non ascii characters
  corp <- tm_map(corp, function(x) iconv(x, "latin1", "ASCII", sub=""))  
  
  # Combine words that should stay together
  if (length(comb_words)>0){
    for (j in seq(corp)) {
      for (k in 1:nrow(comb_words)){
        corp[[j]] <- gsub(comb_words[k,1], comb_words[k,2], corp[[j]])
      }
    }
  }
  
  # Stem the document  
  corp <- tm_map(corp, stemDocument)   

  # Remove Stopwords
  stopw <- c(stopwords("english"),stop_words)
  corp <- tm_map(corp, removeWords, stopw)
  rm(stopw)
  
  # Strip whitespace
  corp <- tm_map(corp, stripWhitespace)
  
  # Validate cleaning operations by re-printing OG text
  print("                                                                    ")
  print("Post-Cleaned Comment Results:")
  print(corp[[rand]]$content)
  print("====================================================================")
  
  # Remove temp variables
  rm(rand)

  return(corp)
}