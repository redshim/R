################################################################################################################################
## 2017. 2. 20
## chinese text analysis
## Training Text data analysis
## Written By Durability Technology Team
################################################################################################################################

## Delete All Old variables in R Session
rm(list=ls())
gc() # Garbage Collection = Clear memory Usage
options(mc.cores=1) # Use Core 1


# Working Directory Setting
setwd("C://R//TextAnalysis//Chinese - Training")
setwd("C:/R")

getwd()


#install.packages("tmcn", repos="http://R-Forge.R-project.org")
# textmining package install
#install.packages(c("data.table","tm","wordcloud","ggplot2","qgraph","slam","topicmodels","lad","LDAvis","servr","readxl"))
library(data.table)
library(tm)
library(tmcn)
library(wordcloud2)
library(ggplot2)
library(qgraph)
library(SnowballC)
library(readxl)

# locale change
original.locale = Sys.getlocale('LC_CTYPE')
Sys.setlocale(category = "LC_ALL", locale = "Chinese_China.936")



sessionInfo()

Sys.setlocale(category = "LC_ALL", locale = "us")


#options(encoding = "UTF-8")      ## for chines
## use getOption("encoding") to see if things were changed

#loc <- function(os, language = "english") {
#  switch(language,
#         english = ifelse(os == "Windows", "English_United States.1252", "en_US.UTF-8"),
#         chinese = ifelse(os == "Windows", "Chinese", "zh_CN.utf-8"))
#}

## set locale
#Sys.setlocale(category = "LC_ALL", loc(Sys.info()[["sysname"]], "chinese"))



# import the data from the Source Files
d <- read_excel("sail_review_100.xlsx",skip = 1)
writeLines(as.character(d$contents[[1]]))

# Read data
d = read.csv2('sail_review_100_conv.csv'
             , header=TRUE
             , fileEncoding='utf8'
             , sep = ','
             , quote = '"'
             , stringsAsFactors = F)
d <- as.data.frame(d)
# d <- read.csv('data_chinese.csv', header=T, fileEncoding='utf8', sep = ',', quote = '"', stringsAsFactors = F)
View(d)

# Remove none related text
library(stringr)

a <- d$contents
# document split  by [ category ]
tmplists = c()
for(split_doc in a){
  splitlists = str_split(split_doc, '\\【', n=Inf)
  for(alist in splitlists){
    tmplists = c(tmplists,alist)
  }
}
tmplists

tmplists[1]

### you can change the category which you want to grep!!
#  【最不满意的一点】 不知道为什么美系车的内饰永远是这么的奇怪，赛欧的内饰永远是一
#  【空间】 乘坐空间：外形虽小，但内部空间真的大的出乎意料，后排腿部空间有种感觉比
#  【动力】 一二档比较顿卡，但是一脚油门下去提速快的飞起
#  【操控】 方向盘很精准，轻重也合理，刹车不硬也不软，不会有突然急刹的冲击感。
#  【油耗】 还在磨合期。现在油耗在6个油左右，可能还会往高开 懒得去细算油耗，

a_conv1 <- subset(tmplists, (tmplists != "" & grepl("动力】", tmplists)))
a_conv2 <- subset(tmplists, (tmplists != "" & grepl("内饰】", tmplists)))

a_conv <- subset(tmplists, (tmplists != "" & grepl("内饰】", tmplists) & grepl("动力】", tmplists)))


a_conv <- c(a_conv1,a_conv2)

length(a_conv1)

length(a_conv2)
a_conv1[1]

# chinese word segmenter install
#install.packages("rJava")
#install.packages("Rwordseg",repos="http://R-Forge.R-project.org")
# if you can'nt install you can directly download ZIP file from the webpage below
# https://r-forge.r-project.org/R/?group_id=1054

library(Rwordseg)
#################################################################################
# chinese word separation
#################################################################################
Sys.setenv(JAVA_HOME="")  # 충돌 방지

# practice using one sentence
a = segmentCN(a_conv[3], nature = T, nosymbol = T) # word separation

segmentCN(a_conv[2], nature = T, nosymbol = T)

dat = matrix(cbind(a, names(a)), ncol=2) # parts of speech : 명사(Noun), 대명사(Pronoun), 동사(Verb), 형용사(Adjective), 부사(Adverb), 전치사(Preposition), 접속사(Conjunction), 감탄사(Interjecton)
dat = as.data.frame(dat)
names(dat) <- c("token","type")


dat = dat[!dat[1,] == '空间', ]

dat[grep('^[vna]+', dat[,2]),1]  # verb, noun, adjective 만 골라라, 조사 같은 필요없는 것 빼는 명령어


dat$token


# tokenizer
chs_tokenizer = function(doc){
  doc <- as.character(doc)
  a = segmentCN(doc, nature = T, nosymbol = T)
  dat = matrix(cbind(a, names(a)), ncol=2)
  return(dat[grep('^[vna]+', dat[,2]),1])
}

chs_tokenizer(a_conv[2])

##################################################################################
#
# Term Document Matrix
#
##################################################################################
#install.packages(tm)
library(tm)
# 1.keyword analysis
# tdm (Term Document matrix)
cps <- Corpus(VectorSource(a_conv[1:10])) # select 100 items
inspect(cps)



# package install for handling stopword
# https://r-forge.r-project.org/R/?group_id=1571
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, stripWhitespace)
myStopwords <- c("空间")
cps <-tm_map(cps, removeWords, myStopwords) # Remove Stop Ward

#Document Term Matrix with Frequency Count
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=chs_tokenizer
                                       , wordLengths=c(2,Inf) # extract over 2 word
                                       , stopwords= TRUE
                                    )
                                    ) # delet stopwords



as.matrix(sample.TermDocumentMatrix(tdm))

inspect(tdm)
sessionInfo()

debug(TermDocumentMatrix)
debug(chs_tokenizer)
debug(Corpus)

tdm

#Document Term Matrix with TF-IDF Score
dtm <- DocumentTermMatrix(cps,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                         stopwords = TRUE,
                                         tokenize=chs_tokenizer, # Extract Noun
                                         removePunctuation=T, # Remove Punctuation
                                         removeNumbers=T, # Remove Number
                                         wordLengths=c(2, 5)
                          ))
tdm_tf <- t(dtm)

inspect(tdm_tf)

tdm <- removeSparseTerms(tdm, 0.995)
tdm
dim(tdm)
inspect(tdm_tf)


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

word.count

# frequently used words
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:50]
freq.word

# tdm$dimnames$Terms[freq.wor

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

#Q + geom_bar(stat="identity")
#Q + geom_bar(colour="blue", stat="identity") + guides(fill=FALSE)
Q +
  geom_bar(fill="gray", colour="darkgray", stat="identity", width=.8)   +
  coord_flip()   +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=freq),color="black", size=3.5,  family = 'song') +
  theme_minimal() +
  ggtitle("Term Frequency Plot") +
  theme(plot.title = element_text(lineheight=.5, face="bold",margin=margin(0,0,0,0)), text=element_text(size=13, family="song"))


############################################################################################################################
#
# 3. Word Cloud Visualizaion 2
#
############################################################################################################################

library(wordcloud)
m <- as.matrix(tdm)

wordFreq <- sort(rowSums(m),decreasing=TRUE)
head(wordFreq)

wordFreq

wordFreq_F <- subset(wordFreq, wordFreq > 0)
#wordFreq_F
wordFreq_F <- subset(wordFreq_F, wordFreq_F < 272)

length(wordFreq_F)
#wordFreq_F

set.seed(length(wordFreq_F))
pal <- brewer.pal(8,"Dark2")
windowsFonts(song=windowsFont("MS Song"))
wordcloud(words=names(wordFreq_F),freq=wordFreq_F
          ,min.freq=2 ,random.order=F ,rot.per=.2 ,colors=pal ,family="song")


# Wordcloud2
library(wordcloud2)
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)[1:100]
d <- data.frame(word = names(v), freq = v)
wordcloud2(d, size=2) # size로 크기 조절


## Query Most Frequent Term
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

tdm.mat = as.matrix(tdm)
sort(rowSums(tdm.mat), dec=T)[3:30]

getwd()


############################################################################################################################
#
# 3. Relation Analysis
#
############################################################################################################################

# extract most top 50 word
most50 = word.order[1:50]
myTdm.mat = as.matrix(tdm[most50,])
myTdm.mat

# Correlation Matrix
cormat = cor(t(myTdm.mat))

attributes(cormat)

for (i in seq(1,50)){
  for(j in seq(1,50)){
    if (cormat[i,j] <= 0){
      cormat[i,j] = 0
    }
  }
}


graph <- graph.adjacency(cormat>0.5, weighted=TRUE, mode="upper")
E(graph)$weight<-t(cormat)[abs(t(cor_mat))>0.5]
E(graph)[ weight>0.7 ]$color <- "black"
E(graph)[ weight>=0.65 & weight<0.7 ]$color <- "red"
E(graph)[ weight>=0.6 &weight<0.65 ]$color <- "green"
E(graph)[ weight>=0.55 &weight<0.6 ]$color <- "blue"
E(graph)[ weight<0.55  ]$color <- "yellow"
V(graph)$label<- row.names(cormat)#V(graph)$name
graph$layout <- layout.fruchterman.reingold
plot(graph)



# Visualization
library(qgraph)
library(igraph)
library(showtext)
font.add("SimSun", "SimSun.ttf") ## add font
pdf("Co-Occurence1_s.pdf")
showtext.begin()
qgraph(cormat, labels=rownames(cormat)
       , diag=F
       , layout='spring'
       , theme = 'Borkulo'
       , label.scale=F
       , label.fill.horizontal = 1
       , legend.cex = .7
       ,node.width = 1.8
       ,node.height = 1.8
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


# Run Visualization
font.add("SimSun", "SimSun.ttf") ## add font
pdf("Co-Occurence2.pdf")
showtext.begin()                ## turn on showtext
plot(g,
     layout=layout.kamada.kawai,
     vertex.shape='rectangle',
     vertex.label=V(g)$name,
     vertex.size=30,
     asp=F,
     vertex.label.family = 'SimSun')
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
                                         K = 4,
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
names(top.topic.words) <- c("Topic1", "Topic2", "Topic3", "Topic4")
write.csv(top.topic.words, file = "3.top_topic_words.csv") # Save the Result


# The number of words which is allocated to each topic
count.topic.words <- result.lda$topic_sums
count.topic.words <- as.data.frame(count.topic.words)
rownames(count.topic.words) <- c("Topic1", "Topic2", "Topic3", "Topic4")
names(count.topic.words) <- c("number of word")
write.csv(count.topic.words, file = "4.count_topic_words.csv") # Save the Result

# The number of topics which is allocated to each sentence
doc.topic = result.lda$document_sums
result.lda$document_sums[,1]
doc.topic.trans <- as.data.frame(t(doc.topic))
names(doc.topic.trans) <- c("Topic1", "Topic2", "Topic3", "Topic4")
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
names(doc.topic.ratio.t) <- c("Topic1", "Topic2", "Topic3", "Topic4")
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
term.table <- sort(term.table, decreasing = TRUE)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

theta <- t(apply(result.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result.lda$topics) + eta, 2, function(x) x/sum(x)))
#
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
serVis(json, out.dir = 'vis4', open.browser = FALSE, encoding = "UTF-8", family= "Nanumgothic")
cat(json, file=file('vis4/lda.json', encoding='utf8'))


