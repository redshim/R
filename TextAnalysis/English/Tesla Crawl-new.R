library(httr)
library(dplyr)
library(rvest)



page = modify_url(page_ori, query=list(page=1))
board = read_html(page, encoding="UTF-8")

posts = board %>%
  html_nodes('.threadloom-cardHeader') %>%
  html_nodes('h1 > a')  %>%
  html_attr('href') %>%
  paste(addr1,.,sep = "")

subjects =  board %>%
  html_nodes('.threadloom-cardHeader') %>%
  html_nodes('h1 > a')  %>%
  html_text()


page_ori = 'https://teslamotorsclub.com/tmc/threadloom/search?query=noise'
addr1 = 'https://teslamotorsclub.com/tmc/'

post_texts = c()
subjects = c()

for(i in 1:10){
  page = modify_url(page_ori, query=list(page=i))
  board = read_html(page, encoding="UTF-8")

  posts = board %>%
    html_nodes('.threadloom-cardHeader') %>%
    html_nodes('h1 > a')  %>%
    html_attr('href') %>%
    paste(addr1,.,sep = "")
  posts

  subject =  board %>%
    html_nodes('.threadloom-cardHeader') %>%
    html_nodes('h1 > a')  %>%
    html_text()

  subjects = c(subjects, subject)

  print(cat("This Step is = ",i))
  Sys.sleep(2)

  for(post_url in posts){
    post = read_html(post_url) %>%
      html_nodes('.messageContent') %>%
      html_nodes('article > blockquote') %>%
      html_text()  %>%
      gsub("\n", "", .) %>%
      gsub("\t", "", .)
    post = post[1]
    post_texts = c(post_texts, post)
  }
}


post_texts
subjects

df <- as.data.frame(cbind(subjects,post_texts))

# Merge Subject and Text

page_ori = 'https://teslamotorsclub.com/tmc/threadloom/search?query=noise'




extract_tesla = function(page_ori){
  post_texts = c()
  for(i in 1:10){
    page = modify_url(page_ori, query=list(page=i))
    board = read_html(page, encoding="UTF-8")

    posts = board %>%
      html_nodes('.threadloom-cardHeader') %>%
      html_nodes('h1 > a')  %>%
      html_attr('href') %>%
      paste(addr1,.,sep = "")
    posts

    print(cat("This Step is = ",i))
    Sys.sleep(2)

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
}

debug(extract_tesla)



extract_tesla(page_ori)

length(post_texts)
