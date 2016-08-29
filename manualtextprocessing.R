install.packages("ngram")
library(tm)
library(ngram)

#read in text
xTran <- Corpus(VectorSource(dfTranscripts$text))
#xTran

#inspect a few documents in the corpus
#inspect(xTran[1:4])

#examine a single line of the corpus
writeLines(as.character(xTran[[1]]))
writeLines(as.character(xTran[[2]]))

#store to demonstrate chnages as text processing progresses
ex1a <-as.character(xTran[[1]])
ex2a <-as.character(xTran[[2]])

#convert to lower case
xTran <- tm_map(xTran, content_transformer(tolower))

ex1b <-as.character(xTran[[1]])
ex2b <-as.character(xTran[[2]])

#remove stopwords, including "alt-title", which is in most of the comics as the mouse over text
xTran <- tm_map(xTran, removeWords, c(stopwords("english"), "alt-title", "alt", "title"))

ex1c <-as.character(xTran[[1]])
ex2c <-as.character(xTran[[2]])

#remove punctuation and numbers
xTran <- tm_map(xTran, removePunctuation)
xTran <- tm_map(xTran, removeNumbers)

ex1d <-as.character(xTran[[1]])
ex2d <-as.character(xTran[[2]])

#stemming
xTran <- tm_map(xTran, stemDocument)

ex1e <-as.character(xTran[[1]])
ex2e <-as.character(xTran[[2]])

#remove any unnecessary white space
xTran <- tm_map(xTran, stripWhitespace)

ex1f <-as.character(xTran[[1]])
ex2f <-as.character(xTran[[2]])

#display example of steps
ex1a
ex1b
ex1c
ex1d
ex1e
ex1f

ex2a
ex2b
ex2c
ex2d
ex2e
ex2f

#word cloud viz of corpus
# library(SnowballC)
# library(wordcloud)
# 
# dtm = DocumentTermMatrix(xTran)
# freq = sort(colSums(as.matrix(dtm)), decreasing=T)
# wordcloud(names(freq),freq,scale=c(5,1), max.words=50, random.order = F, colors = brewer.pal(8,"Dark2"), rot.per=0.35,use.r.layout = F)

#using ngram package
str <- "A B A C A B B"
ng <- ngram(str)
get.phrasetable(ng)
get.ngrams(ng)

