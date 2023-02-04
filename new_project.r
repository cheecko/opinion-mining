library(tm)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(fpc)
library(tidyverse)
library(topicmodels)
library(igraph)
library(ldatuning)
library(jsonlite)


wd <- "G:/Uni/Social Networks and Sentiment Analysis/project"
setwd(wd)

columnnames <- list("salary","communication","workLife","oldColleagues", "leadership")
columnnames <-c(columnnames)

# jsonFile <- "exports/deutschebahn_manager.json"
# jsonFile <- "exports/deutschebahn_employee.json"
# jsonFile <- "exports/deutschebahn_student.json"
jsonFile <- "exports/deutschebahn_contractor.json"

# read from json data
jsonData <- fromJSON(jsonFile, flatten=TRUE)
# data <- jsonData[,c("salary","communication","workLife","oldColleagues", "leadership")] 

data <- list()
data <- c(data, jsonData[,c("title")])
data <- c(data, jsonData[,c("positive")])
data <- c(data, jsonData[,c("negative")])
data <- c(data, jsonData[,c("suggestion")])
# data <- c(data, jsonData[,c("atmosphere")])
# data <- c(data, jsonData[,c("image")])
# data <- c(data, jsonData[,c("workLife")])
# data <- c(data, jsonData[,c("career")])
# data <- c(data, jsonData[,c("salary")])
# data <- c(data, jsonData[,c("environment")])
# data <- c(data, jsonData[,c("teamwork")])
# data <- c(data, jsonData[,c("oldColleagues")])
# data <- c(data, jsonData[,c("leadership")])
# data <- c(data, jsonData[,c("workConditions")])
# data <- c(data, jsonData[,c("communication")])
# data <- c(data, jsonData[,c("equality")])
# data <- c(data, jsonData[,c("tasks")])

docs <- VCorpus(VectorSource(data))

##########################################################################################################################################################
# Corpus Preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
for (j in seq(docs)) {
  docs [[j]] <- gsub("/", "", docs[[j]])
  docs [[j]] <- gsub("@", "", docs[[j]])
  docs [[j]] <- gsub("–", "", docs[[j]])
  docs [[j]] <- gsub("’", "", docs[[j]])
  docs [[j]] <- gsub("“", "", docs[[j]])
  docs [[j]] <- gsub("…", "", docs[[j]])
  docs [[j]] <- gsub("‘", "", docs[[j]])
  docs [[j]] <- gsub(")", "", docs[[j]])
  docs [[j]] <- gsub("”", "", docs[[j]])
  docs [[j]] <- gsub("„", "", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, removeWords, stopwords("German"))
docs <- tm_map(docs, removeWords, stopwords("English"))

StW <- read.table("Stopwords.txt") 
docs <- tm_map(docs, removeWords, as.character(StW$V1))
docs <- tm_map(docs, stripWhitespace) 

for (j in seq(docs)) {
  docs [[j]] <- stemDocument(docs[[j]], language = "german")
  docs [[j]] <- stemDocument(docs[[j]], language = "english") 
}

dtm <- DocumentTermMatrix(docs)
rownames(dtm)<-seq(1,length(data))
tdm <- TermDocumentMatrix(docs)

# Remove words that have 3 and less and more than 20 characters and occur only in 2 document
dtmr <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))

# Remove sparse terms from a dtmr
dtmrSparse = removeSparseTerms(dtmr, 0.70) 

write.csv(as.matrix(dtm), file="exports/DTM.csv")
write.csv(as.matrix(dtm), file="exports/DTMR.csv")

# transpose dtm into matrix
dtm <- as.data.frame.matrix(dtm)

freq <- colSums(as.matrix(dtm))
freq_dtmrSparse <- colSums(as.matrix(dtmrSparse))

##########################################################################################################################################################
# Zipf's law / Frequency Histogramm
# Graph dtm frequency
wf = data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq > 5), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=412, hjust=1))

# Graph dtmrSparse frequency
wf_dtmrSparse <- data.frame(word=names(freq_dtmrSparse),freq=freq_dtmrSparse) 
p1 <- ggplot(subset(wf_dtmrSparse, freq > 12), aes(x = reorder(word, -freq), y = freq))
p1 <- p1 + geom_bar(stat="identity")
p1 <- p1 + theme(axis.text.x=element_text(angle=45, hjust=1))

##########################################################################################################################################################
# Wordcloud
# Wordcloud dtm
set.seed(142)
# wordcloud(words = names(freq), freq = freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Wordcloud dtmrSparse
set.seed(142)
# wordcloud(words = names(freq_dtmrSparse), freq = freq_dtmrSparse, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


##########################################################################################################################################################
# Topic Modelling Preparation
# check the presence of rows with a zero's sum
raw.sum = apply(dtm,1,FUN=sum) #sum by raw for each raw of the table

# number of rows with a zero's sum
mmm <- nrow(dtm[raw.sum == 0,])

# if mmm=0, only create dtm2 and NN (number of rows in DTM)
# if mmm>0, delete the rows with zero's sum form corpus

if (mmm == 0) {
  dtm2 <- dtm
  NN <- nrow(dtm)
  NN
} else {
  dtm2 <- dtm[raw.sum != 0,]
  NN <- nrow(dtm2)
}

##########################################################################################################################################################
# Find Topic Recommendation

# result <- FindTopicsNumber(
#   dtm2,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )

# FindTopicsNumber_plot(result)

##########################################################################################################################################################
# LDA Topic Modelling
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 7
ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)

ldaOut.terms <- as.matrix(terms(ldaOut,10)) #topics keywords
topicProbabilities <- as.data.frame(ldaOut@gamma) #topics probability per document
ldaOut.topics <- as.matrix(topics(ldaOut)) #topics by Documents

##########################################################################################################################################################
# Topic Dataframe Building
nrow(ldaOut.topics)
Comment <- seq(1, NN, by = 1)
wfTopics = data.frame(Comment = Comment, Topics = ldaOut.topics)

# Building Sub-Corpus of Topic 1
choosenTopic <- 1 #change this to get data about this topic
topic1 <- wfTopics[wfTopics[2] == choosenTopic,]

#number of comments with Topic 1
topicRow <- nrow(topic1)
dtm2Row <- nrow(dtm2)

list1 <- c()
i = 1
while(i <= dtm2Row) {
  if (wfTopics[i,2] == choosenTopic) {
    list1 <- c(list1, i)}
  i = i + 1
}
length(list1)

wfTopic1 = NULL
for (i in 1:dtm2Row) {
  for (j in 1:topicRow) {
    if (i == list1[j]){
      c <- data.frame(file=as.character(wfTopics[list1[j],1]),document=as.character(docs[[as.numeric(rownames(dtm2)[i])]]))
      wfTopic1 = rbind(wfTopic1,c)
    }
  }
}

topic_docs <- Corpus(VectorSource(as.character(wfTopic1$document)))
mycorpus_dataframe <- data.frame(text=wfTopic1$document, stringsAsFactors=F)

# uncomment code below to write the new csv about a certain topic
# write.csv(mycorpus_dataframe,'sentiments/manager_topic1.csv', row.names=FALSE)

##########################################################################################################################################################
# Sentiment Score
library(syuzhet)
Topic_1 <- read.csv("sentiments/manager_topic7.csv",
  header = TRUE,
  sep = ",",  # or ";"
  strip.white = TRUE,
  fill = TRUE,
  comment.char = "#",
  stringsAsFactors = FALSE
)

# Matrix from Brand Database
Topic_1 = as.data.frame.matrix(Topic_1)
mycorpus_dataframe1<- data.frame(text=Topic_1, stringsAsFactors=F)
mycorpus_dataframe1

# #remove all non graphical characters
usableText = str_replace_all(mycorpus_dataframe1$text,"[^[:graph:]]", " ")

s_v_sentiment <- get_sentiment(usableText)

summary(s_v_sentiment)

# uncomment or call the code below to get histogramm about sentiment
# hist(s_v_sentiment,main="Histogram for the Sentiment by Topic 7",xlab="Scores",ylab="Probability",border="blue",col="green",prob = TRUE,right=TRUE)
# lines(density(s_v_sentiment))


d <- get_nrc_sentiment(usableText)
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

summary(d)

# uncomment or call the code below to get histogramm about nrc sentiment
# qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment) + ggtitle("Opinion sentiments for Topic 7")