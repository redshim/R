## 중국어와 독일어 텍스트 분석

setwd("C://R/chinese")
#####################################################################################
#
# 중국어
#
#####################################################################################

# locale 바꾸기

original.locale = Sys.getlocale('LC_CTYPE')
Sys.setlocale(category = "LC_ALL", locale = "Chinese_China.936")


# 중국어 데이터 불러오기

d = read.csv('data_chinese.csv', header=T, fileEncoding='utf8', sep = ',', quote = '"', stringsAsFactors = F)
nrow(d)
View(d)
# 본문이 없는 행 삭제
library(dplyr)
d = d %>% filter(nchar(content) > 0)


# 중국어 word segmenter 설치
install.packages("Rwordseg",repos="http://R-Forge.R-project.org")


# 만약 위의 명령으로 설치가 안될 경우에서 zip 파일을 다운 받아 직접 압축 해체

# - https://r-forge.r-project.org/R/?group_id=1054


# 중국어 단어 분리
Sys.setenv(JAVA_HOME="")  # 충돌 방지
library(Rwordseg)



# 한 문장으로 실습
a = segmentCN(d$content[1], nature = T, nosymbol = T)
dat = matrix(cbind(a, names(a)), ncol=2)
dat[grep('^[vna]+', dat[,2]),1]  # verb, noun, adjective


# tokenizer
chs_tokenizer = function(doc){
  doc <- as.character(doc)
  a = segmentCN(doc, nature = T, nosymbol = T)
  dat = matrix(cbind(a, names(a)), ncol=2)
  return(dat[grep('^[vna]+', dat[,2]),1])
}


# 중국어 불용어 처리를 위한 패키지 설치
#
#  패키지 다운 받아 수동 설치
#
# https://r-forge.r-project.org/R/?group_id=1571
install.packages("tmcn", repos="http://R-Forge.R-project.org")
# tdm
library(tm)
library(tmcn)


options(mc.cores=1)
cps <- Corpus(VectorSource(d$content[1:200]))
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=chs_tokenizer,
                                       wordLengths=c(2,Inf),
                                       stopwords=stopwordsCN()))

tdm

inspect(tdm)
.libPaths()


# wordCloud
library(wordcloud2)
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)[1:100]
d <- data.frame(word = names(v), freq = v)
wordcloud2(d, size=1) # size로 크기 조절



# 연관 단어 그래프
library(igraph)
library(showtext)
tdm.matrix <- as.matrix(tdm)
word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing=T)
rownames(tdm.matrix)[word.order[1:20]]
freq.words <- tdm.matrix[word.order[1:20], ]
co.matrix <- freq.words %*% t(freq.words)
co.matrix = log(co.matrix)
co.matrix = ifelse(co.matrix > 5, 1, 0)
g = graph.adjacency(co.matrix, diag = F)


# 연관 단어 그래프 시각화
font.add("SimSun", "SimSun.ttf") ## add font
pdf("graph.pdf")
showtext.begin()                ## turn on showtext
plot(g,
     layout=layout.kamada.kawai,
     vertex.shape='circle',
     vertex.label=V(g)$name,
     vertex.size=20,
     asp=F,
     vertex.label.family = 'SimSun')
showtext.end()                  ## turn off showtext
dev.off()


# topic modeling
library(tm)
library(lda)
library(topicmodels)
dtm = as.DocumentTermMatrix(tdm)
ldaform = dtm2ldaformat(dtm, omit_empty = T)
result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 30,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000,
                                         burnin = 1000,
                                         alpha = 0.01,
                                         eta = 0.01)

# 토픽별 상위 10개 단어 보기
top.topic.words(result.lda$topics, num.words = 10)
View(top.topic.words(result.lda$topics, num.words = 10))

result.lda$topics
# 각 토픽에 할당된 단어수
result.lda$topic_sums




# 문서별 각 토픽에 할당된 단어수
doc.topic = result.lda$document_sums

# 문서별 각 토픽에 할당된 단어 비율
doc.topic.ratio = scale(doc.topic, center = F, scale = colSums(doc.topic))
View(doc.topic.ratio)



# LDA Viz

library(slam)
word.occur = as.array(rollup(tdm, 2))
View(inspect(tdm))
#word.occur
View(word.order)
#
# 빈도순 정렬
#

word.order = order(word.occur, decreasing = T)
row.names(tdm)[word.order]
typeof(word.order)


alpha <- 0.02
eta <- 0.02
D <- length(ldaform$documents)  # number of documents (2,000)
W <- length(ldaform$vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(ldaform$documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)

term.table <- table(unlist(word.order))
term.table <- sort(term.table, decreasing = TRUE)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

term.frequency


theta <- t(apply(result.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result.lda$topics) + eta, 2, function(x) x/sum(x)))

CustComplaints <- list(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = ldaform$vocab,
                       term.frequency = term.frequency)
CustComplaints

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = CustComplaints$phi,
                   theta = CustComplaints$theta,
                   doc.length = CustComplaints$doc.length,
                   vocab = CustComplaints$vocab,
                   term.frequency = CustComplaints$term.frequency,
                   encoding="UTF-8",
                   family= "Nanumgothic")

localeToCharset()
Sys.getlocale()
#iconv(~, "CP949", "UTF-8") #??? "CP949" 인코딩 변환

#시각화
serVis(json, out.dir = 'vis3', open.browser = FALSE, encoding = "UTF-8", family= "Nanumgothic")



#####################################################################################
#
# 독일어
#
#####################################################################################

setwd("C://R//Chinese")
# 독일어 locale
Sys.setlocale(category = "LC_ALL", locale = "German")


# 독일어 데이터
dat = read.csv('data_german.csv', header=T, fileEncoding = 'utf8', stringsAsFactors = F)
dat$Text[1]


# coreNLP
Sys.setenv(JAVA_HOME="")  # 충돌 방지
library(coreNLP)


downloadCoreNLP()
downloadCoreNLP(type='german')



# 자동 설치에 문제가 생기면 다음 파일을 extdata에 압축 해제
#
# - http://nlp.stanford.edu/software//stanford-corenlp-full-2015-12-09.zip
#
# 위 파일을 압축해제한 폴더에 아래 파일 다운로드
#
# - http://nlp.stanford.edu/software/stanford-german-2016-01-19-models.jar

# 초기화
initCoreNLP(type='german')


# 문장 하나로 실습
doc = dat$Text[1]
output = annotateString(doc)
result = getToken(output)
result[grep('ADJA|NN|^V', result$POS), c('token', 'POS')]



# tokenizer 함수
ger_tokenizer = function(doc){
  doc <- as.character(doc)
  output = annotateString(doc)
  result = getToken(output)
  result[grep('ADJA|NN|^V', result$POS),'token']
}
ger_tokenizer(dat$Text[3])


# 독일어 tdm
library(tm)
cps <- Corpus(VectorSource(dat$Text))
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ger_tokenizer,
                                       wordLengths=c(3,Inf)))


# 다음 경고는 큰 문제는 아닙니다.
#
# FactoredParser: exceeded MAX_ITEMS work limit [200000 items]; aborting.

# 워드클라우드
library(wordcloud2)
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)[1:100]
d <- data.frame(word = names(v), freq = v)
wordcloud2(d, size=2) # size로 크기 조절


## 많이 나왔던 단어 찾아보기

m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

tdm.mat = as.matrix(tdm)
sort(rowSums(tdm.mat), dec=T)[1:10]


## 가장 많이 나왔던 단어 time별로 변화 확인해보기

library(dplyr)
#설명
#vignette("introduction", package = "dplyr")
new.data = cbind(dat, t(tdm.mat))


aus.count = new.data %>%
  group_by(Time) %>%
  summarise(counting = sum(ausgeschlagen))

aus.count
library(ggplot2)
ggplot(aus.count, aes(Time, counting)) +
  geom_bar(stat="identity") + coord_flip()

#library(ggplot2)
#ggplot(delay, aes(dist, delay)) + geom_point(aes(size = count), alpha = 1/2) + geom_smooth() + scale_size_area()

ggplot(aus.count, aes(Time, counting)) + geom_point(aes(size = counting), alpha = 1/2) + geom_smooth() + scale_size_area()
