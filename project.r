library(tm)
library(ggplot2)
library(wordcloud)
library(cluster)
library(cluster)
library(fpc)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(purrr)
library(topicmodels)
library(igraph)
library(ldatuning)

wd <- "G:/Uni/Social Networks and Sentiment Analysis/project"
setwd(wd)

# csvData <- "deutschebahn_contractor.csv"
# csvData <- "deutschebahn_employee.csv"
# csvData <- "deutschebahn_manager.csv"
csvData <- "deutschebahn_student.csv"

MyData <- read.csv(csvData,
  header = TRUE, #are there column names in 1st row?
  sep = ",", #what separates rows?
  strip.white = TRUE, #strip out extra white space in strings.
  fill = TRUE, #fill in rows that have unequal numbers of columns
  comment.char = "#", #character used for comments that should not be read in
  stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
)
data <- MyData[,c(6, 9, 10, 11)] #title, positive, negative, suggestion

docs <- Corpus(VectorSource(data))

# Corpus Preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
for (j in seq(docs)) {
  docs [[j]] <- gsub("/", "", docs[[j]])
  docs [[j]] <- gsub("@", "", docs[[j]])
  docs [[j]] <- gsub("–", "", docs[[j]])
  docs [[j]] <- gsub("’", "", docs[[j]])
  docs [[j]] <- gsub("“", " ", docs[[j]])
  docs [[j]] <- gsub("…", "", docs[[j]])
  docs [[j]] <- gsub("‘", "", docs[[j]])
  docs [[j]] <- gsub(")", "", docs[[j]])
  docs [[j]] <- gsub("”", "", docs[[j]])
}
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("German"))
docs <- tm_map(docs, removeWords, stopwords("English"))

StW <- read.table("Stopwords.txt") 
docs <- tm_map(docs, removeWords, StW)
docs <- tm_map(docs, stripWhitespace) 

for (j in seq(docs)) {
  docs [[j]] <- stemDocument(docs[[j]], language = "German")
  docs [[j]] <- stemDocument(docs[[j]], language = "English") 
}

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

# Remove words that have 3 and less and more than 20 characters and occur only in 1 document
dtmr <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20),bounds = list(global = c(2,Inf))))

# Remove sparse terms from a dtmr
dtmrSparse = removeSparseTerms(dtmr, 0.50) 

write.csv(as.matrix(dtm), file="exports/DTM.csv")
# write.csv(as.matrix(dtmr), file="exports/DTMR.csv")
# write.csv(as.matrix(dtmr1), file="exports/DTMR1.csv")

# transpose dtm into matrix
dtm <- as.data.frame.matrix(dtm)

filenames <- list.files(getwd(),pattern="*.csv")
filenames <-c(filenames)
rownames(dtm)<-filenames
rownames(dtmrSparse)<-filenames

# transpose matrix DTM into TDM
tdm <- t(dtm)
tf <- as.matrix(tdm)
idf <- log(ncol(tf) / (rowSums(tf != 0)))
tf_idf <- crossprod(tf, diag(idf))
colnames(tf_idf) <- rownames(tf)

# transpose matrix dtmrSparse into tdmrSparse
tdmrSparse <- t(dtmrSparse)
tdmrSparse_tf <- as.matrix(tdmrSparse)
tdmrSparse_idf <- log(ncol(tdmrSparse_tf) / (rowSums(tdmrSparse_tf != 0)))
tdmrSparse_tf_idf <- crossprod(tdmrSparse_tf, diag(tdmrSparse_idf))
colnames(tdmrSparse_tf_idf) <- rownames(tdmrSparse_tf)


# write.csv(as.matrix(tf_idf),file="exports/TFIDF.csv")

freq <- colSums(as.matrix(dtm))
freq_tf_idf <- colSums(as.matrix(tf_idf), na.rm = FALSE)

# Graph dtm frequency
wf = data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq > 10), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))

# Graph tf_idf frequency
wf_tf_idf <- data.frame(word=names(freq_tf_idf),freq=freq_tf_idf) 
p1 <- ggplot(subset(wf_tf_idf, freq > 10), aes(x = reorder(word, -freq), y = freq))
p1 <- p1 + geom_bar(stat="identity")
p1 <- p1 + theme(axis.text.x=element_text(angle=45, hjust=1))

#############################################################################
# Wordcloud
# Wordcloud dtm
set.seed(142)
# wordcloud(words = names(freq), freq = freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Wordcloud tf_idf
set.seed(142)
# wordcloud(words = names(freq_tf_idf), freq = freq_tf_idf, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Wordcloud dtmr if needed

#############################################################################
# Cluster Recommendation
# Elbow
# wss1 <- function(k) {
#   kmeans(tdmrSparse, k, nstart = 10 )$tot.withinss
# }
# k.values <- 1:15
# wss_values1 <- map_dbl(k.values, wss1)

# plot(
#   k.values, wss_values1, type="b", pch = 19, frame = FALSE,
#   xlab="Number of clusters K",
#   ylab="Total within-clusters sum of squares"
# )

# easy function
# set.seed(123)
# fviz_nbclust(tdm, kmeans, method = "wss")
# fviz_nbclust(tdm, kmeans, method = "silhouette")

# set.seed(123)
# gap_stat1 <- clusGap(tdm, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
# print(gap_stat1, method = "firstmax")
# print(fviz_gap_stat(gap_stat1))

# km.res <- kmeans(tdm, 10, nstart = 25)
# print(fviz_cluster(km.res, data = tdm, palette = "jco", ggtheme = theme_minimal()))


#############################################################################
# DOCUMENTS
# Hierarchical Clustering dtm
d1 <- dist(dtm, method="euclidian")
fit <- hclust(d=d1, method="complete")
# plot.new()
# plot(fit, hang=-1, cex=0.5)
# groups <- cutree(fit, k=3) 
# rect.hclust(fit, k=3, border="red")

# Kmeans dtm
# kfit1 <- kmeans(d1, 3)
# clusplot(as.matrix(d1), kfit1$cluster, color=T, shade=T, labels=2, lines=0)

# Hierarchical Clustering tf_idf
d2 <- dist(tf_idf, method="euclidian")
fit2 <- hclust(d=d2, method="complete")
# plot.new()
# plot(fit2, hang=-1, cex=0.5)
# groups <- cutree(fit2, k=3) 
# rect.hclust(fit2, k=3, border="red")

# Kmeans tf_idf
# kfit2 <- kmeans(d2, 3)
# clusplot(as.matrix(d2), kfit2$cluster, color=T, shade=T, labels=2, lines=0)

# Hierarchical Clustering dtmrSparse
d3 <- dist(dtmrSparse, method="euclidian") 
fit3 <- hclust(d=d3, method="complete")
# plot.new()
# plot(fit3, hang=-1, cex=0.5)
# groups <- cutree(fit3, k=2) 
# rect.hclust(fit3, k=2, border="red")

# Kmeans dtmrSparse
# kfit3 <- kmeans(d3, 2)
# clusplot(as.matrix(d3), kfit3$cluster, color=T, shade=T, labels=2, lines=0)

#############################################################################
# TERMS
# Hierarchical Clustering TERMS tdm
d4 <- dist(tdm, method="euclidian")
fit4 <- hclust(d=d4, method="complete")
# plot.new()
# plot(fit4, hang=-1, cex=0.5)
# groups <- cutree(fit4, k=6) 
# rect.hclust(fit4, k=6, border="red")

# Kmeans TERMS tdm
# kfit4 <- kmeans(d4, 2)
# clusplot(as.matrix(d4), kfit4$cluster, color=T, shade=T, labels=2, lines=0)

# Hierarchical Clustering TERMS tf_idf
tf_idf_t = t(tf_idf)
d5 <- dist(tf_idf_t, method="euclidian")
fit5 <- hclust(d=d5, method="complete")
# plot.new()
# plot(fit5, hang=-1, cex=0.5)
# groups <- cutree(fit5, k=6) 
# rect.hclust(fit5, k=6, border="red")

# Kmeans TERMS tf_idf
# kfit5 <- kmeans(d5, 6)
# clusplot(as.matrix(d5), kfit5$cluster, color=T, shade=T, labels=2, lines=0)

# Hierarchical Clustering TERMS tdmrSparse
d6 <- dist(tdmrSparse, method="euclidian") 
fit6 <- hclust(d=d6, method="complete")
# plot.new()
# plot(fit6, hang=-1, cex=0.5)
# groups <- cutree(fit6, k=4) 
# rect.hclust(fit6, k=4, border="red")

# Kmeans TERMS tdmrSparse
# kfit6 <- kmeans(d6, 6)
# clusplot(as.matrix(d6), kfit6$cluster, color=T, shade=T, labels=2, lines=0)

# Hierarchical Clustering TERMS tdmrSparse_tf_idf
tdmrSparse_tf_idf_t = t(tdmrSparse_tf_idf)
d7 <- dist(tdmrSparse_tf_idf_t, method="euclidian")
fit7 <- hclust(d=d7, method="complete")
# plot.new()
# plot(fit7, hang=-1, cex=0.5)
# groups <- cutree(fit7, k=6) 
# rect.hclust(fit7, k=6, border="red")

# Kmeans TERMS tdmrSparse_tf_idf
# kfit7 <- kmeans(d7, 6)
# clusplot(as.matrix(d7), kfit7$cluster, color=T, shade=T, labels=2, lines=0)

#############################################################################
# DOCUMENTS
#cosine similarity dtm
dtm_mm_s = as.matrix(dtm)
dtm_mm <- as.matrix(dtm_mm_s[1:4,])
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
dtm_cs <- cosineSim(dtm_mm)

# cosine hierarchical and k-means dtm
dtm_d_cs <- 1 - dtm_cs
dtm_fit_cs <- hclust(d=dtm_d_cs, method="complete")
# plot.new()
# plot(dtm_fit_cs, hang=-1, cex=0.5)
# groups <- cutree(dtm_fit_cs, k=3) 
# rect.hclust(dtm_fit_cs, k=3, border="red")

# dtm_kfit_cs <- kmeans(dtm_d_cs, 3)
# clusplot(as.matrix(dtm_d_cs), dtm_kfit_cs$cluster, color=T, shade=T, labels=2, lines=0)

#############################################################################
# TERMS
#cosine similarity tdm
tdm_mm_s = as.matrix(tdm)
tdm_mm <- as.matrix(tdm_mm_s)
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
tdm_cs <- cosineSim(tdm_mm)

# cosine hierarchical and k-means tdm
tdm_d_cs <- 1 - tdm_cs
tdm_fit_cs <- hclust(d=tdm_d_cs, method="complete")
# plot.new()
# plot(tdm_fit_cs, hang=-1, cex=0.5)
# groups <- cutree(tdm_fit_cs, k=6) 
# rect.hclust(tdm_fit_cs, k=6, border="red")

# tdm_kfit_cs <- kmeans(tdm_d_cs, 6)
# clusplot(as.matrix(tdm_d_cs), tdm_kfit_cs$cluster, color=T, shade=T, labels=2, lines=0)


#cosine similarity tdmrSparse
tdmrSparse_mm_s = as.matrix(tdmrSparse)
tdmrSparse_mm <- as.matrix(tdmrSparse_mm_s)
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
tdmrSparse_cs <- cosineSim(tdmrSparse_mm)

# cosine hierarchical and k-means tdmrSparse
tdmrSparse_d_cs <- 1 - tdmrSparse_cs
tdmrSparse_fit_cs <- hclust(d=tdmrSparse_d_cs, method="complete")
# plot.new()
# plot(tdmrSparse_fit_cs, hang=-1, cex=0.5)
# groups <- cutree(tdmrSparse_fit_cs, k=6) 
# rect.hclust(tdmrSparse_fit_cs, k=6, border="red")

# tdmrSparse_kfit_cs <- kmeans(tdmrSparse_d_cs, 6)
# clusplot(as.matrix(tdmrSparse_d_cs), tdmrSparse_kfit_cs$cluster, color=T, shade=T, labels=2, lines=0)

#############################################################################
# DOCUMENTS
# ADJACENCY MATRIX
min_cos <- 0.6
dtm_cs[dtm_cs < min_cos] <- 0
dtm_cs <- round(dtm_cs,3)
mm = as.data.frame.matrix(as.matrix(dtm_cs))
rownames(mm)<-filenames
adj_cs <- as.matrix(mm)

g=graph.adjacency(adj_cs,mode="undirected",weighted=TRUE)
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight

# #VERTICES DEGREE CALCULATION
deg <- graph.strength(g, mode="all")
V(g)$size <- deg * 5
hc5 <- terrain.colors(5)
g.max <- max(deg)
vcolors <- 5 - round(4 *(deg / g.max))
vcolors <- hc5[vcolors]

# lay_3 <- layout_in_circle
# plot.igraph(g,layout=lay_3, edge.arrow.size=0.1,edge.width=E(g)$weight*5, vertex.label=V(g)$name, vertex.color=vcolors,vertex.size=V(g)$size,vertex.label.cex=1)

# # COMMUNITY DETECTION USING THE DIFFERENT ALGORITHMS
# ceb <- cluster_edge_betweenness(g)
# plot(ceb, g)
# membership(ceb)

# clp <- cluster_label_prop(g)
# plot(clp , g)
# membership(clp)

# cfg <- cluster_fast_greedy(as.undirected(g))
# plot(cfg, as.undirected(g))
# membership(cfg)

#############################################################################
# TERMS
tdm_min_cos <- 0.6
tdmrSparse_cs[tdmrSparse_cs < tdm_min_cos] <- 0
tdmrSparse_cs <- round(tdmrSparse_cs,3)
tdm_mm = as.data.frame.matrix(as.matrix(tdmrSparse_cs))
tdm_adj_cs <- as.matrix(tdm_mm)

tdm_g=graph.adjacency(tdm_adj_cs,mode="undirected",weighted=TRUE)
list.vertex.attributes(tdm_g)
list.edge.attributes(tdm_g)
V(tdm_g)$name
E(tdm_g)$weight

tdm_deg <- graph.strength(tdm_g, mode="all")
V(tdm_g)$size <- tdm_deg * 0.3
tdm_hc5 <- terrain.colors(5)
tdm_g.max <- max(tdm_deg)
tdm_vcolors <- 5 - round(4 *(tdm_deg / tdm_g.max))
tdm_vcolors <- tdm_hc5[tdm_vcolors]

# tdm_lay_3 <- layout_in_circle
# plot.igraph(tdm_g,layout= tdm_lay_3, edge.arrow.size=0.1,edge.width=E(tdm_g)$weight*0.3,vertex.label=V(tdm_g)$name,
# vertex.color=tdm_vcolors,vertex.size=V(tdm_g)$size,vertex.label.cex=1)

# tdm_ceb <- cluster_edge_betweenness(tdm_g)
# plot(tdm_ceb, tdm_g)
# membership(tdm_ceb)

# tdm_clp <- cluster_label_prop(tdm_g)
# plot(tdm_clp , tdm_g)
# membership(tdm_clp)

# tdm_cfg <- cluster_fast_greedy(as.undirected(tdm_g))
# plot(tdm_cfg, as.undirected(tdm_g))
# membership(tdm_cfg)

#############################################################################
# TOPICS
# Find Topic Recommendation
data("AssociatedPress", package="topicmodels")
dtmTopics <- AssociatedPress[1:10, ]

result <- FindTopicsNumber(
  dtmTopics,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Topics
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)

ldaOut.terms <- as.matrix(terms(ldaOut,6))
topicProbabilities <- as.data.frame(ldaOut@gamma)
ldaOut.topics <- as.matrix(topics(ldaOut))

# write.csv(ldaOut.terms,file="exports/TopicsToTerms.csv")
# write.csv(topicProbabilities,file="exports/TopicProbabilities.csv")
# write.csv(ldaOut.topics,file="exports/DocsToTopics.csv")