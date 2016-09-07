
library(tm)
library(ngram)
library(RWeka)
library(quanteda)
library(RColorBrewer)
library(stm)
library(stringi)
library(Rtsne)
library(geometry)

#using TM package - probably need to redo this using the stringi package, since quanteda uses functions from
#that package instead of TM package

#read in text
xTran <- Corpus(VectorSource(dfTranscripts$text))
#xTran

#inspect a few documents in the corpus
#inspect(xTran[1:4])

#examine a single line of the corpus
writeLines(as.character(xTran[[30]]))
writeLines(as.character(xTran[[612]]))

#store to demonstrate chnages as text processing progresses
ex1a <-as.character(xTran[[30]])
ex2a <-as.character(xTran[[612]])

ex2a <-as.character(xTran[[30]])

#convert to lower case
xTran <- tm_map(xTran, content_transformer(tolower))

ex1b <-as.character(xTran[[30]])
ex2b <-as.character(xTran[[612]])

#remove stopwords, including "alt-title", which is in most of the comics as the mouse over text
xTran <- tm_map(xTran, removeWords, c(stopwords("english"), "alt-title", "alt", "title"))

ex1c <-as.character(xTran[[30]])
ex2c <-as.character(xTran[[612]])

#remove punctuation and numbers
xTran <- tm_map(xTran, removePunctuation)
xTran <- tm_map(xTran, removeNumbers)

ex1d <-as.character(xTran[[30]])
ex2d <-as.character(xTran[[612]])

#stemming
xTran <- tm_map(xTran, stemDocument)

ex1e <-as.character(xTran[[30]])
ex2e <-as.character(xTran[[612]])

#remove any unnecessary white space
xTran <- tm_map(xTran, stripWhitespace)

ex1f <-as.character(xTran[[30]])
ex2f <-as.character(xTran[[612]])

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


###
##attempting to use quanteda package instead - easier to get from corpus to tokens to doc-term-matrix, 
#quanteda also includes a function to convert dtm to an object which stm package can use
myCorpus <- corpus(dfTranscripts$text)

summary(myCorpus, n=5)


#add dummy meta data
docvars(myCorpus, "group") <- as.factor(rep(1:11,each=115))
summary(myCorpus, n=5, showmeta=TRUE)

head(docvars(myCorpus))

##call a particular document from the corpus
#texts(myCorpus)[2]

# ##Tokenizing text into a document term matrix
# tokens <- tokenize(myCorpus, removeNumbers=TRUE, removePunct=TRUE,removeSeparators=TRUE)
# ngram <-ngrams(tokens, n=1:2)

#create document term matrix, with 1-grams and 2-grams
mydfm <- dfm(myCorpus, stem=TRUE, ngrams=c(1,2), 
             removeNumbers=TRUE, removePunct=TRUE,
             removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title"))

#remove words with less than 4 occurances and in only one document
mydfm <- trim(mydfm, minCount=4, minDoc=2)

#further trim to include 5 or more occurances and in at least two docs
mydfm <- trim(mydfm, minCount=5, minDoc=3)

#review dtm
topfeatures(mydfm, 20)
head(mydfm)

#Plot wordcloud
plot(mydfm, max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))



# mydfmG <- dfm(myCorpus, groups = "group", ignoredFeatures=c(stopwords("english"), "title"), stem=TRUE, ngrams=c(1,2), 
#              removeNumbers=TRUE, removePunct=TRUE,removeSeparators=TRUE)
# 
# mydfmG <- trim(mydfmG, minCount = 4)

#convert document feture matrix into an STM object
stmdfm <- convert(mydfm, to="stm",
                  docvars=data.frame(group=docvars(myCorpus, "group")))

#fit STM for corpus
#init.type = "Spectral" is much faster than the default init.type=LDA"
#need to read more about how "spectral" works
stmFit <- stm(stmdfm$documents, stmdfm$vocab, K=10, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )




head(stmFit$vocab,20)
length(stmFit$vocab)

#still getting words like "aaaaa" and "aaaaaaaa", which i would think would be stemmed to a single feature
#need to explore how stemming works within the stringi/quanteda packages

labelTopics(stmFit, topics=1:10)

# "estimated topic proportions"
plot.STM(stmFit, type = "summary", xlim = c(0, .3))

# thought <- findThoughts(stmFit, topics=c(1,3), n=2, texts=stmFit$documents)
# 
# plotQuote(thought$docs[[10]], width=30)

#see footnote 11 on page 10 of STM vignette
#setting K=0 uses an algorithm from another paper (Lee/Mimno 2014), allowing 
#automatic selection of number of topics
stmFit0 <- stm(stmdfm$documents, stmdfm$vocab, K=0, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )
#when I rand this option, it fit a model with 63 topics, 150 iterations

#this gives all topics, need to figure out how to limit to maybe top ten or so most commmon topics
plot.STM(stmFit0, type = "summary",  xlim = c(0, .3))
#labelTopics(stmFit0, topics=1:10)

as.matrix(mydfm)[c(30,612),40:50]
