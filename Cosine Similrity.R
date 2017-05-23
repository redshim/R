library(tm)
library(proxy)
library(dplyr)

doc <- c( "The sky is blue.", "The sun is bright today.",
          "The sun in the sky is bright.", "We can see the shining sun, the bright sun." )


doc_corpus <- Corpus( VectorSource( doc ) )
control_list <- list( removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE )
tdm <- TermDocumentMatrix( doc_corpus, control = control_list )

# print
( tf <- as.matrix(tdm ) )

# idf
( idf <- log( ncol(tf) / ( 1 + rowSums( tf != 0 ) ) ) )

( idf <- diag(idf) )

tf_idf <- crossprod( tf, idf )
colnames(tf_idf) <- rownames(tf)
tf_idf

# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

doc



# example 
a <- c( 3, 4 )
b <- c( 5, 6 )

# cosine value and corresponding degree
l <- list( numerator = sum( a * b ), denominator = sqrt( sum( a^2 ) ) * sqrt( sum( b^2 ) ) )
list( cosine = l$numerator / l$denominator, 
      degree = acos( l$numerator / l$denominator ) * 180 / pi )


# a slightly larger dataset
setwd("C://R//ML")
news <- read.csv( "news.csv", stringsAsFactors = FALSE )
list( head(news), dim(news) )




# 
# 1. [TFIDF] :
# @vector = pass in a vector of documents  
TFIDF <- function( vector )
{
  # tf 
  news_corpus  <- Corpus( VectorSource(vector) )
  control_list <- list( removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE )
  tf <- TermDocumentMatrix( news_corpus, control = control_list ) %>% as.matrix()
  
  # idf
  idf <- log( ncol(tf) / ( 1 + rowSums( tf != 0 ) ) ) %>% diag()
  
  return( crossprod( tf, idf ) )
}

# tf-idf matrix using news' title 
news_tf_idf <- TFIDF(news$title)

news_tf_idf
news$title


# 2. [Cosine] :
# distance between two vectors
Cosine <- function( x, y )
{
  similarity <- sum( x * y ) / ( sqrt( sum( y^2 ) ) * sqrt( sum( x^2 ) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
  return( acos(similarity) * 180 / pi )
}

# 3. calculate pair-wise distance matrix 
pr_DB$set_entry( FUN = Cosine, names = c("Cosine") )
d1 <- dist( news_tf_idf, method = "Cosine" )
pr_DB$delete_entry( "Cosine" )

length(news_tf_idf)



pr_DB$set_entry


# 4. heirachical clustering 
cluster1 <- hclust( d1, method = "ward.D" )
plot(cluster1)
rect.hclust( cluster1, 5 )

clusterCut <- cutree(cluster1, 5)


width(news_tf_idf)

# distance matrix to vector

str(d1)
m <- data.frame(t(combn(rownames(news_tf_idf),2)), as.numeric(d1))
names(m) <- c("c1", "c2", "distance")
show(m)

write.csv(m, file = "distance.csv")

