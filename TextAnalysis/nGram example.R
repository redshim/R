library(ngram)
library(RWeka)

test = "a b c d e f g"

TrigramTokenizer <- function(x) NGramTokenizer(x,
                                               Weka_control(min = 2, max = 2))
TrigramTokenizer(test)
require(RWeka)


tdm <- TermDocumentMatrix(a, control = list(tokenize = TrigramTokenizer))
inspect(tdm)
