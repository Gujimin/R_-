# 필요한 패키지 설치 및 불러오기

if (!requireNamespace("KoNLP", quietly = TRUE)) install.packages("KoNLP")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("wordcloud", quietly = TRUE)) install.packages("wordcloud")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("descr", quietly = TRUE)) install.packages("descr")
if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm")
if (!requireNamespace("topicmodels", quietly = TRUE)) install.packages("topicmodels")
if (!requireNamespace("sna", quietly = TRUE)) install.packages("sna")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")



library(readr)

library(KoNLP)

library(stringr)

library(wordcloud)

library(RColorBrewer)

library(descr)

library(tm)

library(topicmodels)

library(sna)



# 작업 영역 설정 및 데이터 불러오기

setwd("C:/Rstudy")
address <- readLines("p2_address.txt")
address <- address[address != ""] # 벡터내 Null 원소 제거
address <- gsub("[[:punct:]]", "", address) # 구두점 제거



# 각 라인마다 명사 단어들만 남기기

addNouns <- sapply(address, extractNoun, USE.NAMES = FALSE)

addNounsList <- unlist(addNouns)



# 불용어 단어 제거

stopWord <- c("대학", "경북대", "국민", "고려", "광운") # 불용어 단어 등록

addNounsList <- addNounsList[!addNounsList %in% stopWord]



# 2글자 이상의 단어만 필터링

addLastData <- Filter(function(x) {
  
  nchar(x) >= 2
  
}, addNounsList)



# 단어들에 대한 빈도분석

freq_table <- freq(addLastData, plot = FALSE)

freq_table <- as.data.frame(freq_table)

freq_table <- freq_table[order(freq_table$Frequency, decreasing = TRUE),]



# 워드클라우드 시각화

ListWordCount <- table(addLastData)



# 플롯 마진 조정

par(mar = c(1, 1, 1, 1))



wordcloud(
  
  names(ListWordCount),
  
  freq = ListWordCount,
  
  scale = c(5, 0.2),
  
  rot.per = 0.1,
  
  min.freq = 1,
  
  max.words = 100,
  
  random.order = FALSE,
  
  random.color = TRUE,
  
  colors = brewer.pal(11, "Paired")
  
)



# 동시 출현 단어 분석

data <- read.csv("vision.csv", header = TRUE)

text <- as.vector(data$비전)

text <- gsub("[[:punct:]]", "", text)

text <- gsub("[[:digit:]]", "", text)

TermsList <- sapply(text, extractNoun, USE.NAMES = FALSE)



# 코퍼스 작성

corpus <- Corpus(VectorSource(TermsList))



# 불용어 설정

stopWord <- c("대학", "경북대", "국민", "고려", "광운")



# 문서-단어 행렬

dtm <- DocumentTermMatrix(corpus, control = list(
  
  removePunctuation = TRUE,
  
  stopwords = stopWord,
  
  removeNumbers = TRUE,
  
  wordLengths = c(4, 20)
  
))



# 단어-문서 행렬

tdm <- TermDocumentMatrix(corpus, control = list(
  
  removePunctuation = TRUE,
  
  stopwords = stopWord,
  
  removeNumbers = TRUE,
  
  wordLengths = c(4, 20)
  
))



# 중요 단어 산출

tdm.matrix <- as.matrix(tdm)

word.count <- rowSums(tdm.matrix)

word.order <- order(word.count, decreasing = TRUE)

freq.words <- tdm.matrix[word.order[1:30],]

co.matrix <- freq.words %*% t(freq.words)



# 문서-단어 행렬형 변수로 생성

M <- as.matrix(dtm)



# 인접행렬 작성

g <- cor(M)

diag(g) <- 0

g[is.na(g)] <- 0 # NA 값을 0으로 대체

g[g < 0.5] <- 0



# 패키지 로드

library(sna)



# 예제 그래프 데이터 생성 (g를 사용자 정의 데이터로 교체)

set.seed(123)

g <- rgraph(10, tprob=0.3)



# NA 값 확인 및 처리

if (any(is.na(g))) {
  
  g[is.na(g)] <- 0  # NA 값을 0으로 대체 (필요에 따라 다른 값으로 대체)
  
}



# 그래프 시각화

tryCatch({
  
  gplot(g, label = colnames(g), gmode = "graph", label.cex = 0.6, vertex.cex = 1)
  
}, error = function(e) {
  
  cat("에러 발생:", conditionMessage(e), "\n")
  
})



# 네트워크 그래프 작성

gplot(g, label = colnames(g), gmode = "graph", label.cex = 0.6, vertex.cex = 1)



# 토픽 모델링



# readxl 패키지 설치 및 로드

install.packages("readxl")  # 패키지 설치 (한 번만 실행)

library(readxl)



# 엑셀 파일 읽기

data <- read_excel("ck_data.xlsx", sheet = 1)  # sheet 인자를 통해 읽고자 하는 시트를 지정



# 데이터 확인

print(head(data))



text <- as.vector(data$사업단명)

text <- gsub("[[:punct:]]", "", text)

text <- gsub("[[:digit:]]", "", text)

TermsList <- sapply(text, extractNoun, USE.NAMES = FALSE)



# 코퍼스 작성

corpus <- Corpus(VectorSource(TermsList))

stopWord <- c()



# 문서-단어 행렬

dtm <- DocumentTermMatrix(corpus, control = list(
  
  removePunctuation = TRUE,
  
  stopwords = stopWord,
  
  removeNumbers = TRUE,
  
  wordLengths = c(4, 20)
  
))



# 필요한 패키지 로드

library(topicmodels)



# 문서-단어 행렬 예시 데이터 생성

# dtm <- ... (여기서 실제 dtm을 생성해야 합니다)



# 각 행의 합계가 0이 아닌지 확인하여 빈 문서를 제거합니다

row_sums <- rowSums(as.matrix(dtm))

dtm <- dtm[row_sums > 0, ]



# LDA 모델 생성

lda <- LDA(dtm, k = 3)



# 토픽 순위

top_topics <- apply(posterior(lda)$topics, 1, which.max)

print(top_topics)