
library(tm)
library(ngram)
#library(RWeka)
library(quanteda)
library(RColorBrewer)
library(stm)
library(stringi)
library(Rtsne)
library(geometry)
#library(topicmodels)
library(stmBrowser)

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
##group as a continuous rather than categorical variable
docvars(myCorpus, "group") <- as.numeric(rep(1:11,each=115))
##group as a categorical variable
#docvars(myCorpus, "group") <- as.factor(rep(1:11,each=115))
summary(myCorpus, n=5, showmeta=TRUE)

head(docvars(myCorpus))

##call a particular document from the corpus
#texts(myCorpus)[2]

# ##Tokenizing text into a document term matrix
# tokens <- tokenize(myCorpus, removeNumbers=TRUE, removePunct=TRUE,removeSeparators=TRUE)
# ngram <-ngrams(tokens, n=1:2)

#create document term matrix, with 1-grams and 2-grams
mydfm <- dfm(myCorpus, stem=TRUE, #ngrams=c(1,2), 
             removeNumbers=TRUE, removePunct=TRUE,
             removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text"))

#remove words with less than 4 occurances and in only one document
mydfm <- trim(mydfm, minCount=4, minDoc=2)

#further trim to include 5 or more occurances and in at least two docs
mydfm <- trim(mydfm, minCount=5, minDoc=3)

#review dtm
topfeatures(mydfm, 20)
head(mydfm)

#Plot wordcloud
plot(mydfm, max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))



# mydfmG <- dfm(myCorpus, groups = "group", ignoredFeatures=c(stopwords("english"), "title", "text", "alt"), stem=TRUE, ngrams=c(1,2),
#              removeNumbers=TRUE, removePunct=TRUE,removeSeparators=TRUE)
# 
# mydfmG <- trim(mydfmG, minCount = 4)

##clumps documents by groups, looks at frequency within each group
#sort(mydfmG)[, 1:20]

#plot(mydfmG, max.words = 100, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))

#convert document feture matrix into an STM object
stmdfm <- convert(mydfm, to="stm",
                  docvars=data.frame(group=docvars(myCorpus, "group")))

#fit STM for corpus
#init.type = "Spectral" is much faster than the default init.type=LDA"
#need to read more about how "spectral" works
stmFit <- stm(stmdfm$documents, stmdfm$vocab, K=10, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )

stmFit <- stm(stmdfm$documents, stmdfm$vocab, K=10, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )

stmCfit

head(stmFit$vocab,20)
length(stmFit$vocab)

#still getting words like "aaaaa" and "aaaaaaaa", which i would think would be stemmed to a single feature
#need to explore how stemming works within the stringi/quanteda packages

labelTopics(stmFit, topics=1:10)

# "estimated topic proportions"
plot.STM(stmFit, type = "summary", xlim = c(0, .3),)

# thought <- findThoughts(stmFit, topics=c(1,3), n=2, texts=stmFit$documents)
# 
# plotQuote(thought$docs[[10]], width=30)

stor <- estimateEffect(1:10 ~ group, stmFit, meta=stmdfm$meta)

#names(stor)

plot.estimateEffect(stor, covariate="group", topics = c(1,2), model=stmFit)
plot.estimateEffect(stor, covariate="group", topics = c(3,4), model=stmFit)
plot.estimateEffect(stor, covariate="group", topics = c(5,6), model=stmFit)
plot.estimateEffect(stor, covariate="group", topics = c(7,8), model=stmFit)
plot.estimateEffect(stor, covariate="group", topics = c(9,10), model=stmFit)


#see footnote 11 on page 10 of STM vignette
#setting K=0 uses an algorithm from another paper (Lee/Mimno 2014), allowing 
#automatic selection of number of topics
stmFit0 <- stm(stmdfm$documents, stmdfm$vocab, K=0, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )
#when I rand this option, it fit a model with 63 topics, 150 iterations

#this gives all topics, need to figure out how to limit to maybe top ten or so most commmon topics
plot.STM(stmFit0, type = "summary",  xlim = c(0, .3))
#labelTopics(stmFit0, topics=1:10)

as.matrix(mydfm)[c(30,612),40:50]


#trying four topics
stmFit4 <- stm(stmdfm$documents, stmdfm$vocab, K=4, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )
labelTopics(stmFit4, topics=1:4)
plot.STM(stmFit4, type = "summary",  xlim = c(0, .5))


stmBrowser(stmFit, data=stmdfm$meta, c("group"), text="text")




#### play with tfidf
colnames(mydfm)[order((tfidf(mydfm)))[1:20]]

mytfidf <- tfidf(mydfm)

#this does the same thing
topfeatures(mydfm, 20)


tfidf(mydfm)


mydfm.gram <- dfm(myCorpus, stem=TRUE, ngrams=c(2,3,4,5), 
             removeNumbers=TRUE, removePunct=TRUE,
             removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))

mydfm.gram <- trim(mydfm.gram, minCount=4, minDoc=2)

tfidf.gram <- tfidf(mydfm.gram)

topfeatures(mydfm.gram, 40)

colnames(mydfm.gram)[order(tfidf.gram)[1:20]]

##how to see frequencies tho..?

########

myDistMat <- dist(as.matrix(weight(mydfm, "relFreq")))
myCluster <- hclust(myDistMat)
myCluster$labels <- docnames(mydfm)
plot(myCluster, xlab="", sub="", main="euclidean distance on normalized token frequenc")

similarity(mydfm, c("man", "person", "woman"), method="cosine", margin = "features", n=20)





####plotting experiments

plot.estimateEffect(stor, covariate="group", topics = c(7,8), 
                    model=stmFit, labeltype = "custom", #linecol=c(red, blue),
                    xlab="Topic Proportion", main = "Change in topic proportions over time",
                    custom.labels=c('T7G1',
                                    'T8G1',
                                    'T7G2',
                                    'T8G2',
                                    'T7G3',
                                    'T8G3',
                                    'T7G4',
                                    'T8G4',
                                    'T7G5',
                                    'T8G5',
                                    'T7G6',
                                    'T8G6',
                                    'T7G7',
                                    'T8G7',
                                    'T7G8',
                                    'T8G8',
                                    'T7G9',
                                    'T8G9',
                                    'T7G10',
                                    'T8G10',
                                    'T7G11',
                                    'T8G11'))

