library(httr)
library(dplyr)
library(rvest)



page_ori = 'https://teslamotorsclub.com/tmc/threadloom/search?query=noise'
addr1 = 'https://teslamotorsclub.com/tmc/'

post_texts = c()
for(i in 1:2){
  page = modify_url(page_ori, query=list(page=i))
  board = read_html(page, encoding="UTF-8")

  posts = board %>%
    html_nodes('.threadloom-cardHeader') %>%
    html_nodes('h1 > a')  %>%
    html_attr('href') %>%
    paste(addr1,.,sep = "")
  posts

  for(post_url in posts){
    post = read_html(post_url) %>%
      html_nodes('.messageContent') %>%
      html_nodes('article > blockquote') %>%
      html_text()  %>%
      gsub("\n", "", .) %>%
      gsub("\t", "", .)
      post_texts = c(post_texts, post)
  }

}

post_texts[1]
length(post_texts)

post_df = as.data.frame(post_texts)


board = read_html(page_ori)
posts = board %>%
  html_nodes('.threadloom-cardHeader') %>%
  html_nodes('h1 > a')  %>%
  html_attr('href') %>%
  paste(addr1,"#",.,sep = "")
posts

https://teslamotorsclub.com/tmc/threads/road-noise-19-vs-21-wheels.88286/#post-2031411

