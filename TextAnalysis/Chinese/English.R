# 영어 with STANFORD NLP
#####################################################################################
#
# 독일어
#
#####################################################################################
original.locale = Sys.getlocale('LC_CTYPE')

setwd("C://R//Chinese")
# 영어 locale
Sys.setlocale(category = "LC_ALL", locale = "English")

# 영어 데이터 LOAD
dat = read.csv2(file = 'data_english1.csv'
                , sep = ","
                , quote = "\""
                , header=T
                , encoding = 'utf-8')
dat <- dat[1:3]


# coreNLP
Sys.setenv(JAVA_HOME="")  # 충돌 방지
library(coreNLP)


#downloadCoreNLP()
#downloadCoreNLP(type='english')

# 초기화
initCoreNLP(type='english')



# 문장 하나로 실습
doc = dat$i_content[1]
doc <- gsub("<em>", "", doc)
doc <- gsub("</em>", "", doc)



sIn <- "Mother died today. Or, maybe, yesterday; I can't be sure."
annoObj <- annotateString(sIn)

 annotateString(sIn)

output <-annotateString(doc)
result = getToken(output)
result[grep('ADJA|NN|^V', result$POS), c('token', 'POS')]

result


# tokenizer 함수
ger_tokenizer = function(doc){
  doc <- as.character(doc)
  doc <- gsub("<em>", "", doc)
  doc <- gsub("</em>", "", doc)
  output = annotateString(doc)
  result = getToken(output)
  result[grep('ADJA|NN|^V', result$POS),'token']
}
ger_tokenizer(dat$i_content[3])




# English Term Document Matrix
library(tm)
cps <- Corpus(VectorSource(dat$i_content))
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ger_tokenizer,
                                       wordLengths=c(3,Inf)))

tdm
# 다음 경고는 큰 문제는 아닙니다.
#
# FactoredParser: exceeded MAX_ITEMS work limit [200000 items]; aborting.

# 워드클라우드
library(wordcloud2)
m1 <- as.matrix(tdm)
v

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

