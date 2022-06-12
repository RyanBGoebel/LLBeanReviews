
# ALY 6040 Project
# LL Bean 
# NLP/Text Mining

list.of.packages <- c("qdap","dplyr","tm","wordcloud","ggplot2","ggthemes",
                      "reshape2","quanteda","udpipe","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages to library
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
#library(plotrix)
#library(dendextend)
library(ggplot2)
library(ggthemes)
#library(RWeka)
library(reshape2)
library(quanteda)
library(udpipe) # NLP tokenizing package
library(stringr)


# Save old graphics parameters prior to start
opar <- par(no.readonly = TRUE)

# Read csv to df
bean <- read.csv("LLB.csv")
names(bean)

# Random sample of 20,000 rows
#bean <- bean[sample(nrow(bean), 20000), ]

# Remove review_title and review_text (both are already combined in "review")
names(bean)[names(bean) == "reviewText"] <- "review_text"
names(bean)[names(bean) == "reviewTitle"] <- "review_title"
names(bean)[names(bean) == "itemGroup"] <- "description"

bean <- subset(bean, select = -c(review_text, review_title))

# Convert bi_rating to factor
bean$bi_rating <- as.factor(bean$bi_rating)
summary(bean$bi_rating)

# Separate bean data into positive and negative reviews
beanpos <- bean[bean$bi_rating == "positive",]
beanneg <- bean[bean$bi_rating == "negative",]

## Make a vector source and a corpus
pcorp <- Corpus(VectorSource(beanpos$review))
ncorp <- Corpus(VectorSource(beanneg$review))

# Check random reviews
prand <- round(runif(1, 1, nrow(beanpos)),0)
print("====================================================================")
print("Random Positive Review:")
print(pcorp[[prand]]$content)
print("")
nrand <- round(runif(1, 1, nrow(beanneg)),0)
print("Random Negative Review:")
ncorp[[nrand]]$content #check
print("====================================================================")


# Convert to lower case 
pcorp <- tm_map(pcorp, tolower)   
print("====================================================================")
print("Random Positive Review - Converted to Lower Case")
print(pcorp[[prand]]$content)
print("")
ncorp <- tm_map(ncorp, tolower)   
print("Random Negative Review - Converted to Lower Case")
ncorp[[nrand]]$content #check
print("====================================================================")



# Remove numbers
pcorp <- tm_map(pcorp, removeNumbers)  
print("====================================================================")
print("Random Positive Review - Numbers Removed")
print(pcorp[[prand]]$content)
print("")
ncorp <- tm_map(ncorp, removeNumbers)  
print("Random Negative Review - Numbers Removed")
ncorp[[nrand]]$content #check
print("====================================================================")

# Add words to stop words list
addstop <- c("will", "shall", "can", "also", "bean", "llbean", "ll", "ve", "s",
             "one", "just", "year", "pair", "order", "purchase", "bought", "buy",
             "purchased", "purchasing", "purchases", "buying", "year", "years")

# Add to stop words
stopw <- c(stopwords("english"), addstop)
#stopw # for checking updated stop word list

# Remove stopwords
pcorp <- tm_map(pcorp, removeWords, stopw)   
print("====================================================================")
print("Random Positive Review - Stop Words Removed")
print(pcorp[[prand]]$content)
print("")
ncorp <- tm_map(ncorp, removeWords, stopw)   
print("Random Negative Review - Stop Words Removed")
ncorp[[nrand]]$content #check
print("====================================================================")

#Remove punctuation
pcorp <- tm_map(pcorp, removePunctuation)
ncorp <- tm_map(ncorp, removePunctuation)
pcorp <- tm_map(pcorp, removePunctuation, ucp=TRUE) # Remove additional unicode punctuation
ncorp <- tm_map(ncorp, removePunctuation, ucp=TRUE) # Remove additional unicode punctuation
print("====================================================================")
print("Random Positive Review - Punctuation Removed")
print(pcorp[[prand]]$content)
print("")
print("Random Negative Review - Punctuation Removed")
ncorp[[nrand]]$content #check
print("====================================================================")


# Combine various spellings of "tshirt"
for (j in seq(pcorp)) {
  pcorp[[j]] <- gsub("tee shirt| t shirt|teeshirt|tee shirts| t shirts|teeshirts|tees|tshirts|tee", "tshirt", pcorp[[j]])
  #print(j) # for monitoring
}
print("====================================================================")
print("Random Positive Review - Various spellings of 'tshirt' consolidated")
print(pcorp[[prand]]$content)
print("")

for (j in seq(ncorp)) {
  ncorp[[j]] <- gsub("tee shirt| t shirt|teeshirt|tee shirts| t shirts|teeshirts|tees|tshirts|tee", "tshirt", ncorp[[j]])
  #print(j) # for monitoring
}
print("Random Negative Review - Various spellings of 'tshirt' consolidated")
ncorp[[nrand]]$content #check
print("====================================================================")

## Stem document
pcorp <- tm_map(pcorp, stemDocument)
print("====================================================================")
print("Random Positive Review - Words stemmed")
print(pcorp[[prand]]$content)
print("")
ncorp <- tm_map(ncorp, stemDocument)
print("Random Negative Review - Words stemmed")
ncorp[[nrand]]$content #check
print("====================================================================")

# strip whitespace
pcorp <- tm_map(pcorp, stripWhitespace)
print("====================================================================")
print("Random Positive Review - White space stripped")
print(pcorp[[prand]]$content)
print("")
ncorp <- tm_map(ncorp, stripWhitespace)
print("Random Negative Review - White space stripped")
ncorp[[nrand]]$content #check
print("====================================================================")

# Remove extraneous objects
rm(list = c("j", "stopw", "addstop"))

# Create document term matrices
pdtm <- DocumentTermMatrix(pcorp)   
pdtm 
ndtm <- DocumentTermMatrix(ncorp)   
ndtm 

# Correlation between return and size? ==> no
# If words always appear together in a document, then correlation=1.0
findAssocs(ndtm, c("return", "size"), corlimit=0.1) # correlation limit of 0.67
findAssocs(ndtm, c("fabric", "thin"), corlimit=0.1)


# Remove sparse terms (set sparse to lower number to remove more terms)
pdtms <- removeSparseTerms(pdtm, 0.99) 
pdtms
ndtms <- removeSparseTerms(ndtm, 0.99) 
ndtms

# most frequent words 
pfreqw <- sort(colSums(as.matrix(pdtms)), decreasing=TRUE)   
#head(pfreqw, 25)
nfreqw <- sort(colSums(as.matrix(ndtms)), decreasing=TRUE)   
#head(nfreqw, 25)

# Barplot of 25 most frequent words in positive reviews
# Create a data frame with words and frequencies for plotting
wordfreqp <- data.frame(word=names(pfreqw), freq=pfreqw) 
rownames(wordfreqp) <- NULL # change row names from words to rank numbers
head(wordfreqp, 25) # check
ggplot(wordfreqp[1:25, ], aes(x = reorder(word, freq), y = freq, fill=freq)) + 
  geom_bar(stat = "identity", width=0.8, col="gray80") + 
  geom_text(aes(label=word), hjust=-0.2, vjust=0.5, size=5) +
  coord_flip() +
  scale_fill_gradient(low="#F7FCF5", high="#005A32", space="Lab") +
  scale_y_continuous(limits = c(0,120000), expand=c(0,0)) +
  labs(title="Top 25 Words Used in Positive LL Bean Reviews",
       x="", y="", fill="Frequency") +
  theme_light() +
  theme(
    plot.title = element_text(hjust=0.5, size=18, face="bold"),
    axis.title = element_text(size=16, face="bold"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    #panel.background = element_rect(fill="#FDFEF0"),
    legend.position = c(0.9, 0.3),
    #legend.background = element_rect(fill="#EDEEE0", color="gray90")
  )

# Barplot of 25 most frequent words in negative reviews
# Create a data frame with words and frequencies for plotting
wordfreqn <- data.frame(word=names(nfreqw), freq=nfreqw) 
rownames(wordfreqn) <- NULL # change row names from words to rank numbers
head(wordfreqn, 25) # check
ggplot(wordfreqn[1:25, ], aes(x = reorder(word, freq), y = freq, fill=freq)) + 
  geom_bar(stat = "identity", width=0.8, col="gray80") + 
  geom_text(aes(label=word), hjust=-0.2, vjust=0.5, size=5) +
  coord_flip() +
  scale_fill_gradient(low="#FFF5F0", high="#99000D", space="Lab") +
  scale_y_continuous(limits = c(0,25000), expand=c(0,0)) +
  labs(title="Top 25 Words Used in Negative LL Bean Reviews",
       x="", y="", fill="Frequency") +
  theme_light() +
  theme(
    plot.title = element_text(hjust=0.5, size=18, face="bold"),
    axis.title = element_text(size=16, face="bold"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    #panel.background = element_rect(fill="#FDFEF0"),
    legend.position = c(0.9, 0.3),
    #legend.background = element_rect(fill="#EDEEE0", color="gray90")
  )

## Combine both corpora: all reviews
# Effectively, making all positive reviews into one document, and all negative reviews into another
pos <- paste(pcorp, collapse = "")
neg <- paste(ncorp, collapse = "")
pn <- c(pos, neg)

## Creating corpus for combination
allcorp <- Corpus(VectorSource(pn)) 

#Remove punctuation (quotes and slashes introduced between documents)
allcorp <- tm_map(allcorp, removePunctuation)

# Create tdm with allcorp
tdmall <- TermDocumentMatrix(allcorp)
#inspect(tdmall)
allmtrx = as.matrix(tdmall)
colnames(allmtrx)=c("Positive Reviews","Negative Reviews")
summary(allmtrx)

# #Make commonality cloud
# par(mar = rep(0, 4)) 
# par(bg=NA)
# commonality.cloud(allmtrx, 
#                   colors = "brown",
#                   max.words = 80,
#                   scale=c(8,0.5),
#                   random.order = FALSE
#                   )

# Create comparison cloud
par(mar = rep(0, 4)) 
par(bg=NA)
comparison.cloud(allmtrx[,1:2],
                 colors = c("darkseagreen4", "firebrick2"),
                 max.words = 75,
                 scale=c(8,0.5),
                 random.order = FALSE,
                 title.bg.colors=c("darkseagreen4", "firebrick2")
                 )
par(opar)

allmtrx <- as.data.frame(allmtrx)
head(allmtrx)
allmtrx$Difference <- (allmtrx$`Positive Reviews`)/(sum(allmtrx$`Positive Reviews`)) - (allmtrx$`Negative Reviews`)/(sum(allmtrx$`Negative Reviews`))
allmtrx$Difference <- round(allmtrx$Difference,4)
allmtrx <- allmtrx[order(-allmtrx$Difference), ]
names(allmtrx)[names(allmtrx) == "Difference"] <- "Rel Freq Diff"

# Display top pos and neg words, based on relative freq
allmtrx[order(allmtrx[,3]), ][1:8, ]
allmtrx[order(allmtrx[,3], decreasing = TRUE), ][1:8, ]

# Check percent of reviews that contain "thin(ner) fabric/material"
length(beanpos$review[grepl("fabric|Fabric|material|Material", beanpos$review) ])/nrow(beanpos)
length(beanneg$review[grepl("fabric|Fabric|material|Material", beanneg$review) ])/nrow(beanneg)


# Remove extraneous objects 
#rm(list = c("pcorp", "ncorp", "allcorp", "allmtrx", "ndtm", "ndtms", "pdtm", 
#            "pdtms", "wordfreqn", "wordfreqp"))


# Fabric and Material showing up in negative review words
# Dig deeper into negative reviews with these terms
# Subset negative reviews that mention "fabric" or "material"

# Copy neg and pos reviews
fabmatneg <- beanneg
fabmatpos <- beanpos

# Convert reviews to all lower
fabmatneg$review_text <- tolower(fabmatneg$review)
fabmatpos$review_text <- tolower(fabmatpos$review)

# Filter each for "fabric" or "material"
fabmatneg <- fabmatneg %>% filter(grepl("fabric|material", review))
fabmatpos <- fabmatpos %>% filter(grepl("fabric|material", review))

thinfabneg <- fabmatneg # used later in the code
thinfabpos <- fabmatpos # used later

# Substitute "fabric" for "material"
fabmatneg$review <- gsub("material", "fabric", fabmatneg$review)
fabmatpos$review <- gsub("material", "fabric", fabmatpos$review)

# Substitute "thin" for "thinner"?

## udpipe dependency parsing
## Make udpipe model and annotate the text (WARNING:  SLOW)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
xp <- udpipe_annotate(ud_model, x = fabmatpos$review)
xp <- as.data.frame(xp)
xn <- udpipe_annotate(ud_model, x = fabmatneg$review)
xn <- as.data.frame(xn)


# Use dependency parsing output to get the nominal subject and the adjective of it
# In this way we can combine what are people talking about with the adjective they use when they talk about the subject.

# Positive reviews than mention fabric/material
statsp <- merge(xp, xp, 
                by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                all.x = TRUE, all.y = FALSE, 
                suffixes = c("", "_parent"), sort = FALSE)
statsp <- subset(statsp, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
statsp$term <- paste(statsp$lemma_parent, statsp$lemma, sep = " ")
statsp <- txt_freq(statsp$term)
# par(mar = rep(0, 4)) 
# par(bg=NA)
# #par(bg="#FDFEF0")
# wordcloud(words = statsp$key, freq = statsp$freq, 
#           min.freq = 3, max.words = 150,
#           scale=c(5.5,0.5),
#           random.order = FALSE, 
#           colors = c("#A1D99B", "#74C476", "#41AB5D","#238B45","#006D2C","green")
#           )
# par(opar)

# Negative reviews than mention fabric/material
statsn <- merge(xn, xn, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
statsn <- subset(statsn, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
statsn$term <- paste(statsn$lemma_parent, statsn$lemma, sep = " ")
statsn <- txt_freq(statsn$term)
# par(mar = rep(0, 4)) 
# par(bg=NA)
# wordcloud(words = statsn$key, freq = statsn$freq, 
#           min.freq = 3, max.words = 150,
#           random.order = FALSE, 
#           scale=c(5.5,0.5),
#           colors = c("#FC9272", "#FB6A4A", "#EF3B2C", "red")
#           #colors = brewer.pal(6, "Dark2")
#           )
# par(opar)

# Merge pos and neg fab stats
posneg <- merge(statsp, statsn, by="key", all=TRUE)
posneg <- posneg[c("key", "freq.x", "freq.y")]
posneg[is.na(posneg)] <- 0
names(posneg) <- c("Phrase", "Positive Reviews", "Negative Reviews")


# Checking some common phrases
posneg[posneg$Phrase == 'thin fabric',]
posneg[posneg$Phrase == 'nice fabric',]
posneg[posneg$Phrase == 'great fabric',]

# Rename rows with Phrase itself
nums <- posneg[-1]
row.names(nums) <- posneg$Phrase

# Convert to matrix
posnegmatrx <- as.matrix(nums)
summary(posnegmatrx)

# Comparison Cloud
par(mar = rep(0, 4))
#par(bg="#FDFEF0")
par(bg=NA)
comparison.cloud(posnegmatrx,
                 colors = c("darkseagreen4", "firebrick2"),
                 max.words = 150,
                 scale=c(5.5,0.5),
                 random.order = FALSE,
                 title.bg.colors=c("darkseagreen4","firebrick2")
                 )
par(opar)

posneg$Difference <- posneg$`Positive Reviews` - posneg$`Negative Reviews`
head(posneg[order(posneg$Difference),],10)
head(posneg[order(-posneg$Difference),],10)

# Let's figure out what products have thin + fabric in the negative reviews:
# Filter neg reviews for "thin" AND "fabric" or "thin" AND "material"
# already filtered for "fabric" or "material"
# filter for "thin"
thinfabneg <- thinfabneg %>% filter(grepl(" thin | thinner ", review_text))
thinfabpos <- thinfabpos %>% filter(grepl(" thin | thinner ", review_text))

# Top 10 products with thin fabric in negative reviews
top10thin <- as.data.frame(sort(table(thinfabneg$description), decreasing = TRUE )[1:10])
top10thin
names(top10thin) <- c("Product", "Frequency")

# Make plot of products with "thin fabric/material" in negative reviews
ggplot(top10thin, aes(x = reorder(Product, Frequency), y = Frequency, fill=Frequency)) + 
  geom_bar(stat = "identity", width=0.3, col="gray80") +
  geom_text(aes(label=Frequency), hjust=-0.3, vjust=0.5, size=6, fontface="bold") +
  coord_flip() +
  scale_fill_gradient(low="lightsalmon1", high="salmon4", space="Lab") +
  scale_y_continuous(limits = c(0,62), expand=c(0,0)) +
  labs(title="Products With The Most Negative Reviews \nMentioning 'Thin(ner) Fabric' or 'Thin(ner) Material' ",
       x="", y="", fill="Frequency") +
  #labs(title="",
  #     x="", y="", fill="Frequency") +
  theme_light() +
  theme(
    plot.title = element_text(hjust=0.5, size=22, face="bold"),
    axis.title = element_text(size=16, face="bold"),
    axis.text.y = element_text(vjust=-1, hjust=0, margin=ggplot2::margin(l=10, r=-420), size=18),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    #panel.background = element_rect(fill="#FDFEF0"),
    legend.position = "none"
    #legend.background = element_rect(fill="#EDEEE0", color="gray90")
  )

length(bean$description[grepl("Women's Perfect Fit Pants", bean$description) ]) #17086 total reviews
length(beanpos$description[grepl("Women's Perfect Fit Pants", beanpos$description) ]) # 13188 pos 
length(beanneg$description[grepl("Women's Perfect Fit Pants", beanneg$description) ]) # 3898 neg
length(thinfabpos$description[grepl("Women's Perfect Fit Pants", thinfabpos$description) ]) #73 thin fab
length(thinfabneg$description[grepl("Women's Perfect Fit Pants", thinfabneg$description) ]) #185 thin fab

# Women's Perfect Fit Pants seem to have lots of complaints about thin fabric
# ~1.1% of Women's perfect fit pants have negative reviews with "thin fabric/material"

# Let's see what customer's are saying about these pants

# Subset thinfab to only include Women's Perfect Fit Pants
wpfp <- thinfabneg[grepl("Women's Perfect Fit Pants", thinfabneg$description), ]

wpfpthin <- str_extract(wpfp$review_text, ".{0,35} thin .{0,30}|.{0,35} thinner .{0,30}")
wpfpthin
sample(wpfpthin, 20)

par(opar)


# # Sources: https://medium.com/analytics-vidhya/customer-review-analytics-using-text-mining-cd1e17d6ee4e
# # https://rstudio-pubs-static.s3.amazonaws.com/118348_a00ba585d2314b3c937d6acd4f4698b0.html
# # https://www.r-bloggers.com/2018/04/an-overview-of-keyword-extraction-techniques/
# # https://universaldependencies.org/u/dep/index.html