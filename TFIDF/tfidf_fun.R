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
typeof(x)
names(x)<- c("seq","docid","vtype","vdetail","token","wordclass")
#x$seq
#x$docid
x
docid <-x$docid
token <-x$token
wordclass <- x$wordclass

#tt<-cbind(docid,token)
docid

tt <- data.frame(docid, token, wordclass)
#tt


tf_idf <- function(data){
  ###########################################
  
  # data�� 'docid'�� 'token'������ ������ ��.
  
  ###########################################
  require(plyr)
  # TF / IDF �� ���
  data1 <- ddply(data, .(docid), 'nrow')        # ���� �� �ܾ��� ����
  data2 <- ddply(data, .(docid, token),'nrow') # ���� �� ��� �ܾ��� ��(dc_fq)
  data3 <- ddply(data, .(token), summarize, a=length(unique(docid)))        
  # �ܾ ������ ������ ��(tm_dc_fq)
  data4 <- length(unique(tt$docid))             # ���� ��ü ����(tot)
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



tf_idf_result <-tf_idf(tt)




vtype <-x$vtype
vdetail <- x$vdetail
temp <- data.frame(docid, vtype, vdetail)
master <-unique(temp)
master
#print(tf_idf_result)
#print(master)
nrow(master)
nrow(tf_idf_result)
#tf_idf_result

mge <- merge(tf_idf_result,master,by="docid")
nrow(mge)


filenamefn = paste("C:\\Python27\\NLPK\\tfidf\\",qt,"_stop.csv", sep="")

write.table(mge, filenamefn, sep=",", row.names=FALSE, col.names=TRUE) 
