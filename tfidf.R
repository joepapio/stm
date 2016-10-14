library(topicmodels)
library(textir)
library(tm)

data(congress109)

covars <- data.frame(gop=congress109Ideology$party=="R",
                     cscore=congress109Ideology$cs1)
covars$cscore <- covars$cscore - tapply(covars$cscore,covars$gop,mean)[covars$gop+1]
rownames(covars) <- rownames(congress109Ideology)

## cl=NULL implies a serial run.
## To use a parallel library fork cluster,
## uncomment the relevant lines below.
## Forking is unix only; use PSOCK for windows
cl <- NULL
# cl <- makeCluster(detectCores(), type="FORK")
fitCS <- mnlm(cl, covars, congress109Counts, bins=5,gamma=1)
# stopCluster(cl)

# plot the fit
par(mfrow=c(1,2))
for(j in c("estate.tax","death.tax")){
  plot(fitCS[[j]], col=c("red","green"))
  mtext(j,line=2) }
legend("topright",bty="n",fill=c("red","green"),legend=names(covars))
## plot the IR sufficient reduction space
Z <- srproj(fitCS, congress109Counts)
par(mfrow=c(1,1))
plot(Z, pch=21, bg=c(4,3,2)[congress109Ideology$party], main="SR projections")
## two pols
Z[c(68,388),]



############
data(we8there)
## 20 high-variance tf-idf terms
colnames(we8thereCounts)[order(-sdev(tfidf(we8thereCounts)))[1:20]]

?tfidf

##using TM package

#read 1000 txt articles from directory data/txt
corpus  <-Corpus(DirSource("data/txt"), readerControl = list(blank.lines.skip=TRUE));
#some preprocessing
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language="english")
#creating term matrix with TF-IDF weighting
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#or compute cosine distance among documents
dissimilarity(tdm, method = "cosine")



presDfm <- trim(presDfm, minCount=5, minDoc=3)
# hierarchical clustering - get distances on normalized dfm
presDistMat <- dist(as.matrix(weight(presDfm, "relFreq")))
# hiarchical clustering the distance object
presCluster <- hclust(presDistMat)
# label with document names
presCluster$labels <- docnames(presDfm)
# plot as a dendrogram
plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")


library(dplyr)
# library(janeaustenr)
# library(tidytext)
#   book_words <- austen_books() %>%
#     unnest_tokens(word, text) %>%
#   count(book, word, sort = TRUE) %>%
#   ungroup()
# 
#   head(book_words, 20)


# list of all words. no stop words removed, no stemming.
# counts are by "group"

 xWords  <- dfTranscripts %>%
   unnest_tokens(word, text) %>%
   count(group, word, sort= TRUE) %>%
   ungroup()

 #counts of total words in each group (ie across documents)
 total_words <- xWords %>% group_by(group) %>% summarize(total = sum(n))

 #join counts for each group to counts of individual words for each group
 xWords <- left_join(xWords, total_words)
  
 xWords
 
 #computes term frequency for each word out of total for group
 xWords <- xWords %>%
   bind_tf_idf(word, group, n)
 xWords

 
 #"penalize" words that occur very frequently 
 xWords %>%
   select(-total) %>%
   arrange(desc(tf_idf))
 
 xWords %>%
   filter(group == "3") %>%
   select(-total) %>%
   arrange(desc(tf_idf))
 
 
# total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
# book_words <- left_join(book_words, total_words)
# book_words

library(ggplot2)
ggplot(xWords, aes(n/total, fill = group)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~group, ncol = 2, scales = "free_y")

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  filter(book == "Pride & Prejudice") %>%
  select(-total) %>%
  arrange(desc(tf_idf))

