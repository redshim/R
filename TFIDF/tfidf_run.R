#remove(x)
#remove(master)
#remove(mge)
#remove(tt)
#remove(tf_idf_result)
#remove(docid)
#remove(vtype)
#remove(token)
#remove(temp)
tf_idf <- function(data){
  ###########################################
  
  # data는 'docid'와 'token'변수를 가져야 함.
  
  ###########################################
  require(plyr)
  # TF / IDF 값 계산
  data1 <- ddply(data, .(docid), 'nrow')        # 문서 내 단어의 갯수
  data2 <- ddply(data, .(docid, token), 'nrow') # 문서 내 모든 단어의 수(dc_fq)
  data3 <- ddply(data, .(token), summarize, a=length(unique(docid)))        
  # 단어를 포함한 문서의 수(tm_dc_fq)
  data4 <- length(unique(tt$docid))             # 문서 전체 갯수(tot)
  dt <- merge(data,data1, 'docid')
  dt <- merge(dt,data2,c('docid','token'))
  dt <- merge(dt, data3,'token')
  names(dt) <- c('token','docid','tm_fq','dc_fq','tm_dc_fq')
  dt$tot <- data4
  dt$tf <- (dt$dc_fq)/(dt$tm_fq)
  dt$idf <- log((dt$tot)/(dt$tm_dc_fq))
  dt$tfidf <- (dt$tf)*(dt$idf)
  final.dt <- dt[,c('docid','token','tf','idf','tfidf')]
  return(final.dt)
}
args<-commandArgs(TRUE)
qt <- args[1]
qt
#qt <- "EXT01"
filename = paste("C:\\Python27\\NLPK\\stop\\",qt,"_stop.csv", sep="")

x <- read.csv(filename, header=T, dec=".",sep=",")
typeof(x)
names(x)<- c("seq","docid","vtype","vdetail","token")
#x$seq
#x$docid

docid <-x$docid
token <-x$token
#tt<-cbind(docid,token)

tt <- data.frame(docid, token)
#tt

tf_idf_result <-tf_idf(tt)

vtype <-x$vtype
vdetail <- x$vdetail
temp <- data.frame(docid, vtype, vdetail)
master <-unique(temp)

#print(tf_idf_result)
#print(master)
nrow(master)
nrow(tf_idf_result)
#tf_idf_result
mge
mge <- merge(tf_idf_result,master,by="docid")
nrow(mge)


filenamefn = paste("C:\\Python27\\NLPK\\tfidf\\",qt,"_stop.csv", sep="")

write.table(mge, filenamefn, sep=",", row.names=FALSE, col.names=TRUE) 


mge
set.seed(1234)
wordcloud(words = mge$token, freq = mge$tfidf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

library(plyr)
mge_tfidf_mdan <- ddply(mge, c("token"), summarize, tfidf = mean(tfidf))

#word cloud
set.seed(1234)
wordcloud(words = mge_tfidf_mdan$token, freq = mge_tfidf_mdan$tfidf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


mge_tfidf_mdan$token
mge_tfidf_mdan$freq
