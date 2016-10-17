library(lda)
library(tm)
library(stm)
library(topicmodels)
library(Matrix)
library(quanteda)
library(textir)

##does the preprocessing stuff already
#converts to lower case, removes stop words and numbers and punctuation, does stemming
#creates a list object
temp<-textProcessor(documents=gadarian$open.ended.response,metadata=gadarian) 


#pull objects from preprocessed text object
#creates 
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents


out <- prepDocuments(docs, vocab, meta)

docs<-out$documents
vocab<-out$vocab
meta <-out$meta



data(poliblog.documents) # word counts
data(poliblog.vocab) # words in vocab
data(poliblog.ratings) #conservativer or liberal

###
poliblog.ratings <- as.factor(ifelse(poliblog.ratings==-100, "Liberal", "Conservative"))

out <-prepDocuments(poliblog.documents, poliblog.vocab, poliblog.ratings)
poliblog.documents <- out$documents
poliblog.ratings <- out$meta
poliblog.vocab <- out$vocab

length(poliblog.documents) #there are 773 documents
length(poliblog.vocab) #and 1290 items in the vocab



#estimate the model
poliblogPrevFit <- stm(poliblog.documents,poliblog.vocab,K=10, prevalence=~poliblog.ratings)
#typeof(poliblogPrevFit)
#names(poliblogPrevFit)
head(poliblogPrevFit$vocab,20)
length(poliblogPrevFit$vocab)


#?selectModel
poliblogSelect <- selectModel(poliblog.documents,poliblog.vocab,K=10, prevalence=~poliblog.ratings,runs=20)

#graphical display of 4 models, each with k=10 topics, for deciding which model to go with
#plotted on semantic coherence by exclusivity
plotModels(poliblogSelect)

##i think this selects the first model of the four?? as far as i can tell, this step was left out of the 
#vignette, so i'm not sure if i'm doing this part right
poliblogFit <- poliblogSelect$runout[[1]]

#semantic coherence means "high probability words for a topic co-occur within documents"
#semantic exclusivity means "top words for a topic are unlikely to appear within top words of other topics"



#to see words associated with each topic
labelTopics(poliblogPrevFit, topics=c(1,3))


thought <- findThoughts(poliblogPrevFit, topics=c(1,3), n=3, texts=poliblog.documents)

#names(thought)

#head(thought$index)
#head(thought$doc)

#too noisy to know what to do with this...
plotQuote(thought$docs[[1]])


#estimating relationships between metadata and topic prevalance
prep <- estimateEffect(1:2~poliblog.ratings,poliblogFit)

#difference of prevelence of two topics
plot.estimateEffect(prep, "poliblog.ratings", model="poliblogFit", method="difference", 
                    cov.value1="Liberal", cov.value2 = "Conservative",
                    xlab="Liberal-Conservative",
                    main="Effect of Liberal vs Conservative")

#prevelence of indiviudal topics
plot.estimateEffect(prep, "poliblog.ratings", model="poliblogFit", method="pointestimate", 
                    cov.value1="Liberal", cov.value2 = "Conservative",
                    xlab="Liberal-Conservative",
                    main="Effect of Liberal vs Conservative")

#getting an error about needing finite xlim..?
plot.STM(poliblogFit, type="perspectives", topics=5)





### congress example - can't get this to work because temp$vocab is empty
data(congress109)

temp <- readCorpus(congress109Counts, type="Matrix")
documents.gs <- temp$documents
vocab.gs <- temp$vocab ###temp$vocab is empty...?
metadata.gs <- congress109Ideology #this is our metadata.
out <-prepDocuments(documents.gs, vocab.gs, metadata.gs)

# head(temp$documents)
# names(temp$documents)
# 
# temp$vocab
# 
# ?readCorpus

#############################
###### xkcd application

set.seed(11123)

dfTranscripts <- read.table("transcriptsB.csv",header=F,sep="\t",colClasses=c("character","character"),col.names=c("url","text"),quote="")
dfTranscripts$number<- seq(from=1,to=1265,by=1)
dfTranscripts$group<- rep(1:11,each=115)
dfTranscripts$group <- as.factor(dfTranscripts$group)
xkcd <- textProcessor(metadata = dfTranscripts, documents=dfTranscripts$text)



Xmeta<-xkcd$meta
Xvocab<-xkcd$vocab
Xdocs<-xkcd$documents

Xout <- prepDocuments(Xdocs, Xvocab, Xmeta)

docsX<-Xout$documents
vocabX<-Xout$vocab
metaX <-Xout$meta

xkcdPrevFit10 <- stm(docsX,vocabX,K=10, prevalence=~metaX$group)

xkcdSelect10 <- selectModel(docsX,vocabX, K=10, runs=10)

plotModels(xkcdSelect4)
plotModels(xkcdSelect5)
plotModels(xkcdSelect6)
plotModels(xkcdSelect7)
plotModels(xkcdSelect8)
plotModels(xkcdSelect9)
plotModels(xkcdSelect10)

xkcdFit <- xkcdSelect10$runout[[1]]

labelTopics(xkcdPrevFit4, topics=c(1,2,3,4))
labelTopics(xkcdPrevFit5, topics=c(1,2,3,4,5))
labelTopics(xkcdPrevFit6, topics=c(1,2,3,4,5,6))
labelTopics(xkcdPrevFit7, topics=c(1,2,3,4,5,6,7))
labelTopics(xkcdPrevFit8, topics=c(1,2,3,4,5,6,7,8))
labelTopics(xkcdPrevFit9, topics=c(1,2,3,4,5,6,7,8,9))
labelTopics(xkcdPrevFit10, topics=c(1,2,3,4,5,6,7,8,9,10))

# graphic list of topic words
plot.STM(xkcdFit, type="labels", topics=c(1,2,3,4,5))

#no meta data to use for estimating any effects
#prepX <- estimateEffect(1:2~metaX$group,xkcdFit)

group <- dfTranscripts$group

#plot.STM(xkcdFit, type="perspectives", topics=2)



###run model on individual groups
library(quanteda)

head(dfTranscripts)

t1 <- subset(dfTranscripts, group=="1")
t2 <- subset(dfTranscripts, group=="2")
t3 <- subset(dfTranscripts, group=="3")
t4 <- subset(dfTranscripts, group=="4")
t5 <- subset(dfTranscripts, group=="5")
t6 <- subset(dfTranscripts, group=="6")
t7 <- subset(dfTranscripts, group=="7")
t8 <- subset(dfTranscripts, group=="8")
t9 <- subset(dfTranscripts, group=="9")
t10 <- subset(dfTranscripts, group=="10")
t11 <- subset(dfTranscripts, group=="11")

corp1 <- corpus(t1$text)
corp2 <- corpus(t2$text)
corp3 <- corpus(t3$text)
corp4 <- corpus(t4$text)
corp5 <- corpus(t5$text)
corp6 <- corpus(t6$text)
corp7 <- corpus(t7$text)
corp8 <- corpus(t8$text)
corp9 <- corpus(t9$text)
corp10 <- corpus(t10$text)
corp11 <- corpus(t11$text)


dfm1 <- dfm(corp1, stem=TRUE, ngrams=c(1,2), 
             removeNumbers=TRUE, removePunct=TRUE,
             removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm2 <- dfm(corp2, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm3 <- dfm(corp3, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm4 <- dfm(corp4, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm5 <- dfm(corp5, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm6 <- dfm(corp6, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm7 <- dfm(corp7, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm8 <- dfm(corp8, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm9 <- dfm(corp9, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm10 <- dfm(corp10, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))
dfm11 <- dfm(corp11, stem=TRUE, ngrams=c(1,2), 
            removeNumbers=TRUE, removePunct=TRUE,
            removeSeparators=TRUE, ignoredFeatures=c(stopwords("english"), "title", "text", "alt"))

dfm1 <- trim(dfm1, minCount=4, minDoc=2)
dfm2 <- trim(dfm2, minCount=4, minDoc=2)
dfm3 <- trim(dfm3, minCount=4, minDoc=2)
dfm4 <- trim(dfm4, minCount=4, minDoc=2)
dfm5 <- trim(dfm5, minCount=4, minDoc=2)
dfm6 <- trim(dfm6, minCount=4, minDoc=2)
dfm7 <- trim(dfm7, minCount=4, minDoc=2)
dfm8 <- trim(dfm8, minCount=4, minDoc=2)
dfm9 <- trim(dfm9, minCount=4, minDoc=2)
dfm10 <- trim(dfm10, minCount=4, minDoc=2)
dfm11 <- trim(dfm11, minCount=4, minDoc=2)



### fit with no covariates..?
stmdfm1 <- convert(dfm1, to="stm")
stmdfm2 <- convert(dfm2, to="stm")
stmdfm3 <- convert(dfm3, to="stm")
stmdfm4 <- convert(dfm4, to="stm")
stmdfm5 <- convert(dfm5, to="stm")
stmdfm6 <- convert(dfm6, to="stm")
stmdfm7 <- convert(dfm7, to="stm")
stmdfm8 <- convert(dfm8, to="stm")
stmdfm9 <- convert(dfm9, to="stm")
stmdfm10 <- convert(dfm10, to="stm")
stmdfm11 <- convert(dfm11, to="stm")

stmFit1 <- stm(stmdfm1$documents, stmdfm1$vocab, K=0, data=stmdfm1$meta, init.type="Spectral" )
stmFit2 <- stm(stmdfm2$documents, stmdfm2$vocab, K=0, data=stmdfm2$meta, init.type="Spectral" )
stmFit3 <- stm(stmdfm3$documents, stmdfm3$vocab, K=0, data=stmdfm3$meta, init.type="Spectral" )
stmFit4 <- stm(stmdfm4$documents, stmdfm4$vocab, K=0, data=stmdfm4$meta, init.type="Spectral" )
stmFit5 <- stm(stmdfm5$documents, stmdfm5$vocab, K=0, data=stmdfm5$meta, init.type="Spectral" )
stmFit6 <- stm(stmdfm6$documents, stmdfm6$vocab, K=0, data=stmdfm6$meta, init.type="Spectral" )
stmFit7 <- stm(stmdfm7$documents, stmdfm7$vocab, K=0, data=stmdfm7$meta, init.type="Spectral" )
stmFit8 <- stm(stmdfm8$documents, stmdfm8$vocab, K=0, data=stmdfm8$meta, init.type="Spectral" )
stmFit9 <- stm(stmdfm9$documents, stmdfm9$vocab, K=0, data=stmdfm9$meta, init.type="Spectral" )
stmFit10 <- stm(stmdfm10$documents, stmdfm10$vocab, K=0, data=stmdfm10$meta, init.type="Spectral" )
stmFit11 <- stm(stmdfm11$documents, stmdfm11$vocab, K=0, data=stmdfm11$meta, init.type="Spectral" )

plot.STM(stmFit1, type = "summary", xlim = c(0, .3), main= "Group 1 Top Topics") 
##converges to model with 41 topics on first attempt
summary(stmFit1)

plot.STM(stmFit2, type = "summary", xlim = c(0, .3), main= "Group 2 Top Topics") 
##converges to model with 45 topics on first attempt
summary(stmFit2)

plot.STM(stmFit3, type = "summary", xlim = c(0, .3), main= "Group 3 Top Topics") 
##converges to model with 37 topics on first attempt
summary(stmFit3)

plot.STM(stmFit4, type = "summary", xlim = c(0, .3), main= "Group 4 Top Topics") 
##converges to model with 43 topics on first attempt
summary(stmFit4)

plot.STM(stmFit5, type = "summary", xlim = c(0, .3), main= "Group 5 Top Topics") 
##converges to model with 45 topics on first attempt
summary(stmFit5)

plot.STM(stmFit6, type = "summary", xlim = c(0, .3), main= "Group 6 Top Topics") 
##converges to model with 34 topics on first attempt
summary(stmFit6)

plot.STM(stmFit7, type = "summary", xlim = c(0, .3), main= "Group 7 Top Topics") 
##converges to model with 34 topics on first attempt
summary(stmFit7)

plot.STM(stmFit8, type = "summary", xlim = c(0, .3), main= "Group 8 Top Topics") 
##converges to model with 33 topics on first attempt
summary(stmFit8)

plot.STM(stmFit9, type = "summary", xlim = c(0, .3), main= "Group 9 Top Topics") 
##converges to model with 40 topics on first attempt
summary(stmFit9)

plot.STM(stmFit10, type = "summary", xlim = c(0, .3), main= "Group 10 Top Topics") 
##converges to model with 39 topics on first attempt
summary(stmFit10)

plot.STM(stmFit11, type = "summary", xlim = c(0, .3), main= "Group 11 Top Topics") 
##converges to model with 41 topics on first attempt
summary(stmFit11)

#at least fitting each group manually, doesn't appear to be a particular direction as far as how many topics

#fit STM for corpus
#init.type = "Spectral" is much faster than the default init.type=LDA"
#need to read more about how "spectral" works
stmFit <- stm(stmdfm$documents, stmdfm$vocab, K=10, prevalence= ~group, data=stmdfm$meta, init.type="Spectral" )