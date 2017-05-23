#remove(x)
#remove(master)
#remove(mge)
#remove(tt)
#remove(tf_idf_result)
#remove(docid)
#remove(vtype)
#remove(token)
#remove(temp)

args<-commandArgs(TRUE)
qt <- args[1]
qt
qt <- "EXT01"
filename = paste("C:\\Python27\\NLPK\\stop\\",qt,"_stop.csv", sep="")


x <- read.csv(filename, header=T, dec=".",sep=",")
names(x)<- c("seq","docid","vtype","vdetail","token","wordclass")

typeof(x)
x <- data.frame(x, paste(x$token,x$wordclass,sep='_'))
names(x)<- c("seq","docid","vtype","vdetail","token","wordclass","token_wordclass")

#x$seq
#x$docid
docid <-x$docid
ttoken <-x$token_wordclass

tt <- data.frame(docid, ttoken)
#tt



tf_idf <- function(data){
  ###########################################
  
  # data는 'docid'와 'token'변수를 가져야 함.
  
  ###########################################
  require(plyr)
  # TF / IDF 값 계산
  data1 <- ddply(data, .(docid), 'nrow')        # 문서 내 단어의 갯수
  data2 <- ddply(data, .(docid, ttoken), 'nrow') # 문서 내 모든 단어의 수(dc_fq)
  data3 <- ddply(data, .(ttoken), summarize, a=length(unique(docid)))        
  # 단어를 포함한 문서의 수(tm_dc_fq)
  data4 <- length(unique(tt$docid))             # 문서 전체 갯수(tot)
  dt <- merge(data,data1, 'docid')
  dt <- merge(dt,data2,c('docid','ttoken'))
  dt <- merge(dt, data3,'ttoken')
  names(dt) <- c('ttoken','docid','tm_fq','dc_fq','tm_dc_fq')
  dt$tot <- data4
  dt$tf <- (dt$dc_fq)/(dt$tm_fq)
  dt$idf <- log((dt$tot)/(dt$tm_dc_fq))
  dt$tfidf <- (dt$tf)*(dt$idf)
  final.dt <- dt[,c('docid','ttoken','tf','idf','tfidf')]
  return(final.dt)
}

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

mge <- merge(tf_idf_result,master,by="docid")
nrow(mge)


## Text Split example
## dt$PX = as.character(lapply(strsplit(as.character(dt$PREFIX), split="_"), "[", 1))

mge$token <- as.character(lapply(strsplit(as.character(mge$ttoken), split="_"), "[", 1))
mge$wordclass <- as.character(lapply(strsplit(as.character(mge$ttoken), split="_"), "[", 2))

nrow(mge)

filenamefn = paste("C:\\Python27\\NLPK\\tfidf\\",qt,"_stop.csv", sep="")

write.table(mge, filenamefn, sep=",", row.names=FALSE, col.names=TRUE) 

