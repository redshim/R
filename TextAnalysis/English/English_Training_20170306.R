################################################################################################################################
## 2017. 3. 8
## English text analysis
## Training Text data analysis
## Written By Durability Technology Team
################################################################################################################################


## Delete All Old variables in R Session
rm(list=ls())
gc() # Garbage Collection = Clear memory Usage
options(mc.cores=1) # Use Core 1


# Working Directory Setting
setwd("C://R//TextAnalysis//English")
getwd()

# textmining package install
#install.packages(c("data.table","tm","wordcloud","ggplot2","qgraph","slam","topicmodels","lad","LDAvis","servr","readxl"))
#install.packages(c("wordcloud2","SnowballC"))
library(data.table)
library(tm)
library(wordcloud2)
library(ggplot2)
library(qgraph)
library(SnowballC)
library(readxl)


install.packages("slam")

# import the data from the Source Files
d <- read_excel("YF Enprecis Verbatim-20160602-Engine Not starting.xlsx")
writeLines(as.character(d$contents[[1]]))

# Read data
d = read.csv2('SolarPanel.csv'
             , header=TRUE
#             , fileEncoding='utf8'
             , sep = ','
             , quote = '"'
             , stringsAsFactors = F)
df <- as.data.frame(d)


df$comment


nDocs <- nrow(df)
# Document Count
nDocs

#####################################################################################
# Data Preprocessing
#####################################################################################
df
# Remove Process 가비지 데이터 정제
removeDoc <- function(x) {gsub("@[[:graph:]]*", "", x)}
removeURL <- function(x) { gsub("http://[[:graph:]]*", "", x)}
trim <- function (x) {gsub("^\\s+|\\s+$", "", x)}
removenum <- function (x) {gsub("\\d+", "", x)}

df$comment
df$ptext <- sapply(df$comment, removeURL)
df$ptext <- sapply(df$ptext, removeDoc)
df$ptext <- sapply(df$ptext, trim)
df$ptext <- sapply(df$ptext, removenum)


##################################################################################
#
# Create Corpus Object
#
##################################################################################
library(tm)
# 1.keyword analysis
# tdm (Term Document matrix)

#build corpus 코퍼스 생성
cps <- Corpus(VectorSource(df$ptext))
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, stemDocument)
cps <- tm_map(cps, stripWhitespace)
cps <- tm_map(cps, stemDocument, language = "english")

#remove potentially problematic symbols
#필요없는 문제 제거
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
cps <- tm_map(cps, toSpace, "-")
cps <- tm_map(cps, toSpace, "’")
cps <- tm_map(cps, toSpace, "‘")
cps <- tm_map(cps, toSpace, "•")
cps <- tm_map(cps, toSpace, "\"")
cps <- tm_map(cps, toSpace, "\\n")
cps <- tm_map(cps, toSpace, "�")
cps <- tm_map(cps, toSpace, "▷")
cps <- tm_map(cps, toSpace, "\"")
cps <- tm_map(cps, toSpace, "☆")
cps <- tm_map(cps, toSpace, "ㅠ")
cps <- tm_map(cps, toSpace, "↓")
cps <- tm_map(cps, toSpace, "━")
cps <- tm_map(cps, toSpace, "☏")
cps <- tm_map(cps, toSpace, "℡")
cps <- tm_map(cps, toSpace, "ah인")
cps <- tm_map(cps, toSpace, "ㅇ")
cps <- tm_map(cps, toSpace, "─")


myStopwords <- c(stopwords('english'),"rt","the","it","is","this","would","The",
                  "I","One","of","or","be","and","It")

cps <-tm_map(cps, removeWords, myStopwords)

inspect(cps)
##################################################################################
#
# Create Document Term Matrix with Frequency Count
#
##################################################################################

tdm <- TermDocumentMatrix(cps,
                          control=list(wordLengths=c(2,Inf))) # extract over 2 word
tdm
inspect(tdm)
#Document Term Matrix with TF-IDF Score
dtm <- DocumentTermMatrix(cps,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                         stopwords = TRUE,
                                         removePunctuation=T, # Remove Punctuation
                                         removeNumbers=T, # Remove Number
                                         wordLengths=c(2, Inf)
                          ))
tdm_tf <- t(dtm)


tdm <- removeSparseTerms(tdm, 0.995)
tdm
dim(tdm)
inspect(tdm)


findAssocs(tdm,'brake',0.2)

### Remove Space Character with str_trim
library(stringr)
rownames(tdm) <- str_trim(rownames(tdm))  ##  for windows
rownames(tdm_tf) <- str_trim(rownames(tdm_tf))  ##  for windows


############################################################################################################################
#
# 2. Most Frequency word
#
############################################################################################################################
library(slam)
word.count = as.array(rollup(tdm, 2)) # Rolling up group by the term

# frequently used words
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:50]

# tdm$dimnames$Terms[freq.word]
word <- tdm$dimnames$Terms[freq.word]
freq <- word.count[freq.word]

# Save the Top Frequency Lists to the CSV File
top.freq = cbind(tdm$dimnames$Terms[freq.word],word.count[freq.word])
write.csv(top.freq, file = "1.word_freq.csv", fileEncoding = "UTF-8" )
top.freq

## Bar Graph
library(ggplot2)
library(plyr)
library(reshape2)
windowsFonts(song=windowsFont("MS Song"))

gg1 <-data.frame(term = word, frequency=freq, order(freq, decreasing = TRUE))

Q <- ggplot(gg1, aes(term,frequency), colour = 'red', size = 3)
Q +
  geom_bar(fill="gray", colour="darkgray", stat="identity", width=.8)   +
  coord_flip()   +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=freq),color="black", size=3.5) +
  theme_minimal() +
  ggtitle("Term Frequency Plot") +
  theme(plot.title = element_text(lineheight=.5, face="bold",margin=margin(0,0,0,0)), text=element_text(size=13))


############################################################################################################################
#
# 3. Word Cloud Visualizaion 2
#
############################################################################################################################

library(wordcloud)
m <- as.matrix(tdm)

wordFreq <- sort(rowSums(m),decreasing=TRUE)
head(wordFreq)

wordFreq_F <- wordFreq
wordFreq_F <- subset(wordFreq, wordFreq > 30)
#wordFreq_F
wordFreq_F <- subset(wordFreq_F, wordFreq_F < 272)

length(wordFreq_F)
#wordFreq_F

set.seed(length(wordFreq_F))
pal <- brewer.pal(8,"Dark2")
windowsFonts(song=windowsFont("MS Song"))
wordcloud(words=names(wordFreq_F),freq=wordFreq_F
          ,min.freq=2 ,random.order=F ,rot.per=.1 ,colors=pal ,family="malgun")


# Wordcloud2
library(wordcloud2)
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)[1:100]
d <- data.frame(word = names(v), freq = v)
wordcloud2(d, size=3) # Control Text size with size parameter


## Query Most Frequent Term
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

tdm.mat = as.matrix(tdm)
sort(rowSums(tdm.mat), dec=T)[3:30]



############################################################################################################################
#
# 3. Relation Analysis
#
############################################################################################################################

# extract most top 50 word
most50 = word.order[1:30]
myTdm.mat = as.matrix(tdm[most50,])
myTdm.mat

# Correlation Matrix
cormat = cor(t(myTdm.mat))

cormat

# Visualization
library(qgraph)
library(igraph)
library(showtext)

pdf("Co-Occurence1_tf_idf.pdf")
showtext.begin()
qgraph(cormat, labels=rownames(cormat)
       , diag=F
       , layout='spring'
       , theme = 'Borkulo'
       , label.scale=F
       , label.fill.horizontal = 1
       , legend.cex = .7
)
title("Co-occourance words between the sentence", line = 2.5)
showtext.end()
dev.off()


# Create Rlations Relational Analysis
library(igraph)
library(showtext)
tdm.matrix <- as.matrix(tdm)
word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing=T)
rownames(tdm.matrix)[word.order[1:30]]
freq.words <- tdm.matrix[word.order[1:30], ]
co.matrix <- freq.words %*% t(freq.words)
co.matrix = log(co.matrix)
co.matrix = ifelse(co.matrix > 6, 1, 0)
g = graph.adjacency(co.matrix, diag = F)

inspect(tdm)

# Run Visualization
pdf("Co-Occurence2.pdf")
showtext.begin()                ## turn on showtext
plot(g,
     layout=layout.kamada.kawai,
     vertex.shape='rectangle',
     vertex.label=V(g)$name,
     vertex.size=10,
     asp=F)
showtext.end()                  ## turn off showtext
dev.off()


############################################################################################################################
#
# 4. Topic Modeling
#
############################################################################################################################

library(tm)
library(lda)
library(topicmodels)
dtm = as.DocumentTermMatrix(tdm)
ldaform = dtm2ldaformat(dtm, omit_empty = T)
result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 2,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000,
                                         burnin = 1000,
                                         alpha = 0.01,
                                         eta = 0.01)

# Findn Attributes
attributes(result.lda)

# Topic Keyword
result.lda$assignments
result.lda$topics
top.topic.words(result.lda$topics)[1:10,]
result.lda$topic_sums
result.lda$document_sums


##################################################################################################
# Topic Mapping
##################################################################################################
# Top 4 words of each topic
top.topic.words = top.topic.words(result.lda$topics, num.words = 10)
top.topic.words <- as.data.frame(top.topic.words)
names(top.topic.words) <- c("Topic1", "Topic2")
write.csv(top.topic.words, file = "3.top_topic_words.csv") # Save the Result


# The number of words which is allocated to each topic
count.topic.words <- result.lda$topic_sums
count.topic.words <- as.data.frame(count.topic.words)
rownames(count.topic.words) <- c("Topic1", "Topic2")
names(count.topic.words) <- c("number of word")
write.csv(count.topic.words, file = "4.count_topic_words.csv") # Save the Result

# The number of topics which is allocated to each sentence
doc.topic = result.lda$document_sums
result.lda$document_sums[,1]
doc.topic.trans <- as.data.frame(t(doc.topic))
names(doc.topic.trans) <- c("Topic1", "Topic2")
doc.topic.trans

doc.topic.ratio = scale(doc.topic, center=F, scale=colSums(doc.topic))
doc.topic.ratio.t <- t(doc.topic.ratio)
doc.cnt <- nrow(doc.topic.ratio.t)

# Save the Topic Classification Result
topic.result = c()
for (i in 1:doc.cnt) {
  #print(which.max(doc.topic.ratio[,i]))
  #temp.topic.result <- which.max(doc.topic.ratio[,i])
  temp.topic.result  <-  if (length(which.max(doc.topic.ratio[,i])) > 0 ) { which.max(doc.topic.ratio[,i])} else {999}
  topic.result <- c(topic.result, temp.topic.result)
}


# the ratio of number of topics which is allocated to each sentence
topic.result.value <- paste(round(apply(doc.topic.ratio, 2, function(x) max(x, na.rm = FALSE))*100,0),"%") #<<-

doc.topic.ratio.t <- cbind(as.data.frame(doc.topic.ratio.t),topic.result,topic.result.value)
doc.topic.ratio.t <- as.data.frame(doc.topic.ratio.t)
names(doc.topic.ratio.t) <- c("Topic1", "Topic2")
write.csv(doc.topic.ratio.t, file = "5.doc_topic_ratio.csv")


##################################################################################################
# Topic Mapping Finished
##################################################################################################



##################################################################################################
# Topic Visualization
##################################################################################################

#K <- 20
#G <- 5000
alpha <- 0.02
eta <- 0.02
D <- length(ldaform$documents)  # number of documents (2,000)
W <- length(ldaform$vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(ldaform$documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)

term.table <- table(unlist(word.order))
term.table <- sort(term.table, decreasing = TRUE)[1:length(ldaform$vocab)]
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

theta <- t(apply(result.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result.lda$topics) + eta, 2, function(x) x/sum(x)))

#############################################################################################################

CustComplaints <- list(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = ldaform$vocab,
                       term.frequency = term.frequency)


library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = CustComplaints$phi,
                   theta = CustComplaints$theta,
                   doc.length = CustComplaints$doc.length,
                   vocab = CustComplaints$vocab,
                   term.frequency = CustComplaints$term.frequency,
                   fileEncoding="utf8",
                   encoding = "utf8",
                   family= "Nanumgothic")

#Visualization
serVis(json, out.dir = 'vis_topic3', open.browser = FALSE, encoding = "UTF-8", family= "Nanumgothic")
serVis(json, out.dir = 'vis_topic2', open.browser = interactive(), encoding = "UTF-8", family= "Nanumgothic")

cat(json, file=file('vis4/lda.json', encoding='utf8'))


