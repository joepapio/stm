library(lda)
library(tm)
library(stm)
library(topicmodels)
library(Matrix)

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
#prepX <- estimateEffect()

plot.STM(xkcdFit, type="perspectives", topics=2)
