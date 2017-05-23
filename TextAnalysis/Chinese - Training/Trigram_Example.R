
# import the data from the Source Files
d <- read_excel("sail_review_100.xlsx",skip = 1)
writeLines(as.character(d$contents[[1]]))



# Remove none related text
library(stringr)

a <- d$contents
# document split  by [ category ]
tmplists = c()
for(split_doc in a){
  splitlists = str_split(split_doc, '\\【', n=Inf)
  for(alist in splitlists){
    tmplists = c(tmplists,alist)
  }
}
tmplists

### you can change the category which you want to grep!!
#  【最不满意的一点】 不知道为什么美系车的内饰永远是这么的奇怪，赛欧的内饰永远是一
#  【空间】 乘坐空间：外形虽小，但内部空间真的大的出乎意料，后排腿部空间有种感觉比
#  【动力】 一二档比较顿卡，但是一脚油门下去提速快的飞起
#  【操控】 方向盘很精准，轻重也合理，刹车不硬也不软，不会有突然急刹的冲击感。
#  【油耗】 还在磨合期。现在油耗在6个油左右，可能还会往高开 懒得去细算油耗，

a_conv <- subset(tmplists, (tmplists != "" & grepl("动力】", tmplists)))

length(a_conv)


#################################
# Create Chs_Trigram_Tokenizer
#################################

#Example : After Running the Chs_trigram_tokenizer, you can calculate the 3 word combination.
#a_conv[2]
#[1] "动力】 一档起步刚开始在前两个红绿灯熄火两次，后面就没有再熄火，1.3的动力有心理准备，确实比较要有点耐心，一档换二档会有点顿挫感，但是有点耐心，不会太差。3 4 5换挡没有压力，纸币自动车多了一个踩离合和换挡，高速上，60km时速以上，其实跟自动挡没有什么区别。  动力还是比较满意，除了不满意的，都还是可以！提车还有一格，后面加了100块，跑了60km，余下的油，仪表盘显示可以跑256km。 6.5元一升，这个油箱是35L的，所以加满应该不到200块的。省油才是最好的家轿。 加速表现：加速表现5分给4分，一二档比较卡顿，只要舍得给油，还是可以接受，3 4 5很好！ 动力平顺性：平顺性个人觉得比捷达要好，开习惯了就OK了 跑高速：高速无压力，加速还是可以。能听到沙子拍打底盘的声音！ "
#   ==>
#  [1] "动力 起步 开始"   "起步 开始 红绿灯" "开始 红绿灯 熄火" "红绿灯 熄火 没有" "熄火 没有 熄火"
#[6] "没有 熄火 动力"   "熄火 动力 有"     "动力 有 心理"     "有 心理 准备"     "心理 准备 确实"
#[11] "准备 确实 要"     "确实 要 有"       "要 有 耐心"       "有 耐心 换"       "耐心 换 会"
#[16] "换 会 有"         "会 有 挫"         "有 挫 感"         "挫 感 耐心"       "感 耐心 会"
#[21] "耐心 会 差"       "会 差 换挡"       "差 换挡 没有"     "换挡 没有 压力"   "没有 压力 纸币"
#[26] "压力 纸币 车"     "纸币 车 踩"       "车 踩 离"         "踩 离 合"         "离 合 换挡"
#[31] "合 换挡 时速"     "换挡 时速 挡"     "时速 挡 没有"     "挡 没有 区别"     "没有 区别 动力"
#[36] "区别 动力 是"     "动力 是 满意"     "是 满意 满意"     "满意 满意 可以"   "满意 可以 提"
#[41] "可以 提 车"       "提 车 有"         "车 有 加"         "有 加 跑"         "加 跑 余下"
#[46] "跑 余下 油"       "余下 油 仪表盘"   "油 仪表盘 显示"   "仪表盘 显示 可以" "显示 可以 跑"
#[51] "可以 跑 升"       "跑 升 油箱"       "升 油箱 是"       "油箱 是 加"       "是 加 满"
#[56] "加 满 应该"       "满 应该 到"       "应该 到 省"       "到 省 油"         "省 油 是"
#[61] "油 是 好"         "是 好 轿"         "好 轿 加速"       "轿 加速 表现"     "加速 表现 加速"
#[66] "表现 加速 表现"   "加速 表现 卡"     "表现 卡 舍得"     "卡 舍得 油"       "舍得 油 可以"
#[71] "油 可以 接受"     "可以 接受 好"     "接受 好 动力"     "好 动力 平顺"     "动力 平顺 性"
#[76] "平顺 性 平顺"     "性 平顺 性"       "平顺 性 个人"     "性 个人 觉得"     "个人 觉得 捷达"
#[81] "觉得 捷达 要好"   "捷达 要好 开"     "要好 开 习惯"     "开 习惯 跑"       "习惯 跑 无"
#[86] "跑 无 压力"       "无 压力 加速"     "压力 加速 可以"   "加速 可以 能"     "可以 能 听到"
#[91] "能 听到 沙子"     "听到 沙子 拍打"   "沙子 拍打 底盘"   "拍打 底盘 声音"



chs_trigram_tokenizer = function(doc){
  doc <- as.character(doc)
  a = segmentCN(doc, nature = T, nosymbol = T)
  dat = matrix(cbind(a, names(a)), ncol=2)
  dat = dat[grep('^[vna]+', dat[,2]),1]

  i = 1
  token_f = c()

  for (current in dat){

    if(i==1){
      p1_token = current
    }
    if(i==2){
      p2_token = p1_token
      p1_token = current
    }
    if(i >= 3){
      t_token = paste(p2_token, p1_token, current, sep = " ")
      token_f = c(token_f,t_token)
      print(token_f)

      p2_token = p1_token
      p1_token = current
     }
    i = i + 1
  }
  return(token_f)
}


##################################################################################
#
# Term Document Matrix
#
##################################################################################
#install.packages(tm)
library(tm)
# 1.keyword analysis
# tdm (Term Document matrix)
cps <- Corpus(VectorSource(a_conv[1:100])) # select 100 items
inspect(cps)



# package install for handling stopword
# https://r-forge.r-project.org/R/?group_id=1571
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, stripWhitespace)
myStopwords <- c("空间")
cps <-tm_map(cps, removeWords, myStopwords) # Remove Stop Ward

#Document Term Matrix with Frequency Count
tdm_tri <- TermDocumentMatrix(cps,
                          control=list(tokenize=chs_trigram_tokenizer  # changed trigram tokenizer
                                       , wordLengths=c(2,Inf) # extract over 2 word
                                       , stopwords= TRUE
                          )
) # delet stopwords





############################################################################################################################
#
# 2. Most Frequency word
#
############################################################################################################################
library(slam)
word.count = as.array(rollup(tdm_tri, 2)) # Rolling up group by the term

word.count

# frequently used words
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:50]
freq.word

# tdm$dimnames$Terms[freq.wor

word <- tdm_tri$dimnames$Terms[freq.word]
freq <- word.count[freq.word]

# Save the Top Frequency Lists to the CSV File
top.freq = cbind(tdm_tri$dimnames$Terms[freq.word],word.count[freq.word])
#write.csv(top.freq, file = "1.word_freq_trigram.csv", fileEncoding = "UTF-8" )
top.freq

## Bar Graph
library(ggplot2)
library(plyr)
library(reshape2)
windowsFonts(song=windowsFont("MS Song"))

gg1 <-data.frame(term = word, frequency=freq, order(freq, decreasing = TRUE))

Q <- ggplot(gg1, aes(term,frequency), colour = 'red', size = 3)

#Q + geom_bar(stat="identity")
#Q + geom_bar(colour="blue", stat="identity") + guides(fill=FALSE)
Q +
  geom_bar(fill="gray", colour="darkgray", stat="identity", width=.8)   +
  coord_flip()   +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=freq),color="black", size=3.5,  family = 'song') +
  theme_minimal() +
  ggtitle("Term Frequency Plot") +
  theme(plot.title = element_text(lineheight=.5, face="bold",margin=margin(0,0,0,0)), text=element_text(size=13, family="song"))

