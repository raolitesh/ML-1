# codes are written in R Studio
```r
# installing packages
install.packages("rvest") # for web scraping
install.packages("magrittr") # for  using pipe operator %>%, which allows to chain multiple operations together
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("slam") #  for operations on sparse matrices
install.packages("quanteda") # for text analysis
install.packages("Matrix") # for dense matrices
```
```r
#loading the packages
library("rvest")
library("magrittr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("slam")
library("quanteda")
library("Matrix")
```
```r
# Specify the base URL for the iPhone 6 reviews
url <- "https://www.amazon.in/Apple-iPhone-Space-Grey-32GB/product-reviews/B01NCN4ICO/ref=cm_cr_othr_d_paging_btm_2?showViewpoints=1&pageNumber"
```
```r
# Initialize an empty vector to store the reviews
review <- NULL

# reviews from first 40 pages
for (i in 1:40){murl <- read_html(as.character(paste(url, i, sep="="))) # Read the HTML content of the page
rev <- murl%>% html_nodes(".review-text")%>% # Extract review texts
  html_text()
review <- c(review, rev) # Append the reviews to the vector
}

# Save the reviews to a text file
write.table(review, "Apple iPhone 6 (Space Grey, 32GB).txt")

# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
```
```r
# Loading the text file to be analysed 
x = readLines(file.choose()) 	# first, read-in data from "Apple iPhone 6 (Space Grey, 32GB).txt"

# Preparing corpus from the text document 
x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input

# cleaning the text data
x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords("english"))) # removes english common stopwords
x1 = tm_map(x1, stemDocument) # reduces words to root form
```

```r
# Term document frequency matrix
tdm0 <- TermDocumentMatrix(x1)

# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(x1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))#,stemming=T))
inspect(tdm1)
```
```r
a0 <- NULL # Initialize an vector to store the empty words
a1 <- NULL # Initialize an vector to store the empty words
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)
```
```r
#word cloud function
makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")


# words barplot function
words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)

# Word cloud - TFIDF - Unigram
makewordc(tdm1)

# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)
```
```r
# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Positive word cloud - TF - Unigram
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")

# positive words bar plot
pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}

# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)
```
```r
# Making negative wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching negative words
  neg.matches = match(names(freq), c(neg.words,"disapprovals"))
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Negative word cloud - TF - unigam
makenegwordc(tdm0) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")

# negative words barplot function
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab(" negative words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}

# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)

# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")

# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)
```
```r
# sentiment analysis using Syuzhet package
syuzhet_vector <- get_sentiment(x1, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# sentiment analysis using bing package
bing_vector <- get_sentiment(x1, method="bing")
head(bing_vector)
summary(bing_vector)

# sentiment analysis using affin package
afinn_vector <- get_sentiment(x1, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)
```
```r
#Emotion analysis
d<-get_nrc_sentiment(x)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)


# frequency of words in the text, associated with each of the eight emotions.
#transpose
td<-data.frame(t(d))
dim(td)
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:167]))
td_new
#Transformation and cleaning
names(td_new)[1] <- "count"
names(td_new)[1]
td_new <- cbind("sentiment" = rownames(td_new), td_new)
td_new
dim(td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot - percentage of words associated with each sentiment 
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
```
```r
# Bi gram document term frequency 
dtm0_2 <- dfm(unlist(x1),ngrams=2,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm0_2 <- t(tdm0_2)

# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")

# Bi gram barplot on TF
words_bar_plot(tdm0_2)

## Bi gram on TFIDF

dtm1_2 <- dfm_tfidf(dtm0_2)
tdm1_2 <- t(dtm1_2)
a0 = NULL
for (i1 in 1:ncol(tdm1_2)){ if (sum(tdm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm1_2 = tdm1_2[, -a0]} else {tdm1_2 = tdm1_2};	dim(tdm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm1_2 <- t(tdm1_2)

# Bi gram word cloud for TFIDF
makewordc(tdm1_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TFIDF")

# Bigram barplot on TFIDF
words_bar_plot(tdm1_2)
```
```r
# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  plot(fit) # display dendogram
} # clusdend() func ends

# Cluster dendrogram on Uni gram - TF
clusdend(dtm0)
title(sub = "Dendrogram using TF")

# Cluster dendrogram on Uni gram - TFIDF
clusdend(dtm1)
title(sub = "Dendrogram using TFIDF")
```
















