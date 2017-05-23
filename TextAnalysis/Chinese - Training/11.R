tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=chs_tokenizer,
                                       wordLengths=c(2,Inf), # extract over 2 word
                                       stopwords= TRUE)
) # delet stopwords
