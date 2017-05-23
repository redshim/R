# 한국어 텍스트마이닝을 위한 패키지 설치
#install.packages("KoNLP")
library(KoNLP)

library(RColorBrewer)
library(wordcloud)

#한글 세종사전 이용
useSejongDic()

# 텍스트 파일 읽음
# working directory 내에 파일이 있다면 파일 이름만 적어주고
# 그렇지 않다면 파일이 저장되어 있는 경로까지 적어줘야함
# f <- file("파일 경로/test.txt", blocking = F)
f <- file("test33.txt", blocking = F, encoding = "UTF-8")
txt <- readLines(f)

# sapply() 리스트 형태가 아닌 행렬 또는 벡터 형태로 결과 값을 반환하는 함수
# 읽어드린 텍스트 파일에서 명사만 추출
nouns <- sapply(txt, extractNoun, USE.NAMES = F)
nouns


# token count
wordcount <- table(unlist(nouns))
wordcount

pal <- brewer.pal(12,"Set3")
pal <- pal[-c(1:2)]
wordcloud(names(wordcount),freq=wordcount,scale=c(6,0.3),min.freq=3,
          random.order=T,rot.per=.1,colors=pal)



# 추출된 명사 상위 30개, 하위 30개 확인
# unlist() 결과를 백터 형식으로 반환하는 함수
head(unlist(nouns), 30)
tail(unlist(nouns), 30)

# 두글자 이상인 명사만 추출
# nouns2 에 임시로 저장하고 다시 nouns에 저장
nouns2 <- unlist(nouns)
nouns <- Filter(function(x) {nchar(x) >= 2}, nouns2)
nouns

# 불필요한 단어 제거 및 변경
# 데이터명 <- gsub("변경할 단어", "변경 후 단어", 데이터명)
# 예) "지금"이라는 단어를 제거
nouns <- gsub("지금", "", nouns)
nouns <- gsub("☞", "", nouns)



require(plyr)
ddply(df, .(color), mutate, count = length(unique(type)))


nouns
# 추출된 명사를 파일로 저장
# write(unlist(nouns), "저장될 폴더/test2.txt")
write(unlist(nouns), "test2.txt" )

# 테이블 형태로 저장
table_data <- read.table("test2.txt")
table_data


aggregate(table_data$V1, by = list(table_data$V1),FUN=sum )


