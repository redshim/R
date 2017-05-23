##########################################################################################################
#
# Crawling Program R
#
##########################################################################################################
#
# Library Install
#

#install.packages('rvest')
#install.packages('dplyr')

#
# Library Loading
#

library(httr)
library(dplyr)
library(rvest)


##BENZ - ABC (active body control)
##MBD (Magic body control)
#Airmatic
##BMW - Executive drive pro
#Citroen - AFS
#Toyota - active control suspension


# motion body control problem

motortalk = c("http://search.motortalk.net/?area=1&query=motion+body")

mt1 = "http://search.motortalk.net/?area=1&query=Magic+body+control+problem&sortOrder=0"
mt2 = "http://search.motortalk.net/?area=1&query=Magic+body+control+problem&sortOrder=1"

motortalk = c(mt1, mt2)


## Analysis Key word
# airmatic problem, airmatic defekt

#######################################################################################
# you can change the web site address
#######################################################################################
#mt1 = "http://search.motortalk.net/search.html?area=1&sortOrder=0&query=Luftfederung%09Problem+"
#mt2 = "http://search.motortalk.net/search.html?area=1&sortOrder=1&query=Luftfederung%09Problem+"

#motortalk = c(mt1, mt1)

board = read_html(motortalk)

board
#
# View URL
#
posts = board %>%
  html_nodes('.result-inner') %>%
  html_nodes('.title')  %>%
  html_attr('href')
posts


#
# Read Main Contents
#
post = read_html(posts[2])
post %>%
  html_nodes('.bbcode') %>%
  html_text()


#
# 1~2 Page Crawling
#



post_texts = c()
html_addr = c()
for(page in 1:2){
  board = read_html(motortalk[page])
  posts = board %>%
    html_nodes('.result-inner') %>%
    html_nodes('.title')  %>%
    html_attr('href')

  for(post_url in posts){
    r <- GET(post_url, user_agent("myua"))
    if (status_code(r) >= 300){
      print("Fail to Read")
    } else {
      post = read_html(post_url) %>%
        html_nodes('.bbcode') %>%
        html_text()
      post = gsub("?","",post)
      post = gsub("\n", "", post)
      post = gsub("\t", "", post)
      post = gsub(",", "", post)
      post_texts = c(post_texts, post)
      for (post_d in post){
        html_addr = c(html_addr,post_url )
      }
    }
  }
  posts = c()
}

dat = as.data.frame(cbind(as.vector(post_texts),as.vector(html_addr)))

class(html_addr)

names(dat) <- c("Text","url")

View(dat)

#Change output file name here######################################################################################
write.csv(dat, file = "magic_body_control.csv" , fileEncoding = "utf8")
#######################################################################################
