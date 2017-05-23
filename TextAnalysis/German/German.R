#Setting Current Working Directory
setwd("C://R//TextAnalysis//German")
getwd()

#####################################################################################
#
# 독일어 (Deutch)
#
#####################################################################################

# Deutch locale
Sys.setlocale(category = "LC_ALL", locale = "German")


# Data
dat = read.csv('magic_body_control.csv'
               , header=T
               , fileEncoding = "utf8"
               , sep = ","
               , quote = "\""
               , stringsAsFactors = F)

View(dat)


#### filtering the
library(tidyverse)
library(stringr)

dat <- subset(dat, (Text != ""))
dat <- dat %>%
  filter(str_detect(Text, 'problem|issue'))

#dat$Text[1]
#head(dat)




# coreNLP
Sys.setenv(JAVA_HOME="")  # 충돌 방지
library(coreNLP)


#downloadCoreNLP()
#downloadCoreNLP(type='german')



# 자동 설치에 문제가 생기면 다음 파일을 extdata에 압축 해제
#
# - http://nlp.stanford.edu/software//stanford-corenlp-full-2015-12-09.zip
#
# 위 파일을 압축해제한 폴더에 아래 파일 다운로드
#
# - http://nlp.stanford.edu/software/stanford-german-2016-01-19-models.jar

# Initialize
initCoreNLP(type='german')


# exercise
doc = dat$Text[1]

output = annotateString(doc)

#rm(output)
result = getToken(output)
result
result[grep('ADJA|NN|^V', result$POS), c('token', 'POS')]



# German tokenizer Function
ger_tokenizer = function(doc){
  doc <- as.character(doc)
  output = annotateString(doc)
  result = getToken(output)
  result[grep('ADJA|NN|^V', result$POS),'token']
}
ger_tokenizer(doc)




# German tdm
library(tm)
cps <- Corpus(VectorSource(dat$Text))
cps

cps <- tm_map(cps, content_transformer(tolower)) # Upper Case →  LowerCase
cps <- tm_map(cps, removeWords, stopwords("german")) # 띄어쓰기와 시제 등의 내용 제거
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, stripWhitespace)
myStopwords <- c(stopwords('german'), "rt","like","will","just","the")
cps <-tm_map(cps, removeWords, myStopwords) # 사용자 지정 Stopword 제거




myTdm <- TermDocumentMatrix(cps
                            , control=list(wordLengths=c(3,6)))
myTdm <- removeSparseTerms(myTdm, 0.995)

myTdm



############################################################################################################################
#
# 2. Most Frequency word
#
############################################################################################################################
library(slam)
word.count = as.array(rollup(myTdm, 2)) # 단어가 전체 문서에서 쓰인 개수



# frequently used words
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:30]
freq.word

# tdm$dimnames$Terms[freq.word]
word <- myTdm$dimnames$Terms[freq.word]
freq <- word.count[freq.word]

# csv 파일로 주요 키워드 횟수 저장
top.freq = data.frame(word=myTdm$dimnames$Terms[freq.word], freq=word.count[freq.word])
#write.csv(top.freq, file = "1.word_freq.csv") # 데이터 저장
top.freq


## Bar Graph
library(ggplot2)
library(plyr)
library(reshape2)

gg1 <-data.frame(term = word, frequency=freq, order(freq, decreasing = TRUE))

Q <- ggplot(gg1, aes(term,frequency), colour = 'red', size = 3)

#Q + geom_bar(stat="identity")
#Q + geom_bar(colour="blue", stat="identity") + guides(fill=FALSE)
Q +
  geom_bar(fill="gray", colour="darkgray", stat="identity", width=.8)   +
  coord_flip()   +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=freq),color="black", size=3.5) +
  theme_minimal() +
  ggtitle("Term Frequency Plot") +
  theme(plot.title = element_text(lineheight=.5, face="bold",margin=margin(0,0,0,0)))


#Search Term by the frequency count
findFreqTerms(myTdm, lowfreq=6)

# Term 별 연관 단어 탐색
findAssocs(myTdm,'neue',0.5)





############################################################################################################################
#
# 3. Word Cloud Visualizaion 2
#
############################################################################################################################

library(wordcloud)
m <- as.matrix(myTdm)

wordFreq <- sort(rowSums(m),decreasing=TRUE)
head(wordFreq)

wordFreq_F <- subset(wordFreq, wordFreq > 10)
#wordFreq_F
wordFreq_F <- subset(wordFreq_F, wordFreq_F < 156)

length(wordFreq_F)
#wordFreq_F

set.seed(length(wordFreq_F))
pal <- brewer.pal(8,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(words=names(wordFreq_F),freq=wordFreq_F
          ,min.freq=2 ,random.order=F ,rot.per=.1 ,colors=pal ,family="malgun")


# Wordcloud2
library(wordcloud2)
m1 <- as.matrix(myTdm)
v <- sort(rowSums(m1), decreasing = TRUE)[1:100]
d <- data.frame(word = names(v), freq = v)
wordcloud2(d, size=2) # size로 크기 조절


## Query Most Frequent Term

m1 <- as.matrix(myTdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

tdm.mat = as.matrix(myTdm)
sort(rowSums(tdm.mat), dec=T)[1:10]


############################################################################################################################
#
# 3. Relation Analysis
#
############################################################################################################################

# extract most top 50 word
most50 = word.order[1:80]

myTdm.mat = as.matrix(myTdm[most50,])


# Correlation Matrix
cormat = cor(t(myTdm.mat))



# Visualization
library(qgraph)
qgraph(cormat, labels=rownames(cormat)
       , diag=F
       , layout='spring'
       , theme = 'Borkulo'
       , label.scale=F
       , label.font =1
       , label.fill.horizontal = 1

)
title("Co-occourance words between the sentence", line = 2.5)


############################################################################################################################
#
# 4. Topic Modeling
#
############################################################################################################################

library(tm)
library(lda)
library(topicmodels)
dtm = as.DocumentTermMatrix(myTdm)
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


#fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
#                                   num.iterations = G, alpha = alpha,
#                                   eta = eta, initial = NULL, burnin = 0,
#                                   compute.log.likelihood = TRUE)
#############################################################################################################
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

localeToCharset()
Sys.getlocale()
#iconv(~, "CP949", "UTF-8") #— "CP949" 인코딩 변환


#Visualization

serVis(json, out.dir = 'vis4', open.browser = FALSE, encoding = "UTF-8", family= "Nanumgothic")
cat(json, file=file('vis4/lda.json', encoding='utf8'))


serVis(json, out.dir = 'vis4', open.browser = interactive(),  fileEncoding = "utf8", Encoding ="utf8")



