#GetEmotionalType
getEmotionalType <- function(x,pwords,nwords){
  emotionType <-numeric(0)
  xLen <-length(x)
  emotionType[1:xLen]<- 0
  index <- 1
  while(index <=xLen){
    yLen <-length(x[[index]])
    index2 <- 1
    while(index2<= yLen){
      if(length(pwords[pwords==x[[index]][index2]]) >= 1){
        emotionType[index] <- emotionType[index] + 1
      }else if(length(nwords[nwords==x[[index]][index2]]) >= 1){
        emotionType[index] <- emotionType[index] - 1
      }
      index2<- index2 + 1
    }
    #获取进度
    if(index%%100==0){
      print(round(index/xLen,3))
    }
    index <-index +1
  }
  emotionType
}

negative <-readLines("C:/R/TextAnalysis/YSS/ntusd-negative.txt", encoding="UTF-8")
positive <-readLines("C:/R/TextAnalysis/YSS/ntusd-positive.txt", encoding="UTF-8")

length(negative)
length(positive)

##对评论做分词处理并评级#########################
# 过程类似Part2中讲到的分词处理。然后我自己写了个方法getEmotionalType()，
# 将分词结果与negative表和positive表作对照计算得分。
#################################################
commentTemp <- segmentCN(a_conv)
commentTemp[1:100]

EmotionRank <- getEmotionalType(commentTemp,positive,negative)
EmotionRank[1:200]

commentEmotionalRank <- list(rank=EmotionRank,a_conv)
commentEmotionalRank <- as.data.frame(commentEmotionalRank)
fix(commentEmotionalRank)


write.csv(commentEmotionalRank, file = "8.commentEmotionalRank.csv", fileEncoding = "GB18030" )

