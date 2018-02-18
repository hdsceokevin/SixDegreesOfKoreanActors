rm(list=ls())
gc()

# 라이브러리 불러오기
library(readxl)
library(reshape2)
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(scales)
library(Hmisc)

options(warn=-1)

getwd()
setwd("/Users/NaSeongho/Jupyter/Movie")

# 영화 데이터 불러오기 
mList <- as.data.frame(read_excel("./Data/Korean movie_20170714.xlsx", sheet=1))
str(mList)
head(mList)

# 영화-연도 데이터 불러오기 
yList <- as.data.frame(read_excel("./Data/Korean movie year_20170714.xlsx", sheet=1))
str(yList)
head(yList)

# 영화-배우 데이터 불러오기
aList <- as.data.frame(read_excel("./Data/Korean movie and actor_20170714.xlsx", sheet=1))
str(aList)
head(aList)

# 불필요한 컬럼 삭제
mList <- mList[,c(-2)]
aList <- aList[,c(-2)]

# 두 데이터프레임 합치기
df <- merge(aList, mList, by="movieCode", all.x=T)
df <- merge(df, yList, by="movieCode", all.x=T)
df <- df[,c("movieCode","movieName","actorCode","actorName","year")]

# 영화코드 기준으로 정렬
df <- df[order(df$movieCode),]
head(df)

# 배우별 출연한 영화수 컬럼 
nMovie <- aggregate(df$movieCode, by=list(df$actorCode), FUN=length)

# 컬럼명 바꾸기
colnames(nMovie) <- c("actorCode", "movieCnt")
head(nMovie)

# 배우-영화 데이터에 병합
df <- merge(df, nMovie, by="actorCode", all.x=T)

# 배우별 영화수 빈도 확인
table(df$movieCnt, useNA="ifany")

# 영화수 최빈도 배우 확인
df[df$movieCnt==max(df$movieCnt),]

# 배우별 출연한 영화수 확인
actors <- df[,c(1,4,6)]
actors <- unique(actors)
nrow(actors)

actors <- actors[order(actors$movieCnt, decreasing=T),]
head(actors,20)

# 5kg 단위로 구간을 만들고, 빈도수를 구합니다.
cuts <- seq(0, 505, by=5)
moviesCuts <- cut2(actors$movieCnt, cuts, minmax=T)
moviesTb <- table(moviesCuts)

# 각 구간별 상대도수를 구한 후, 도수분포표에 추가합니다.
moviesPr <- prop.table(moviesTb)
moviesTb <- rbind(moviesTb, moviesPr)

# 각 구간별 누적상대도수를 구한 후, 도수분포표에 추가합니다.
moviesCm <- cumsum(moviesPr)
moviesTb <- rbind(moviesTb, moviesCm)

# 행별 합계를 구한 후, 도수분포표에 추가합니다.
moviesTb <- addmargins(moviesTb, margin=2)  # 행별 합계

# 행과 열의 이름을 변경합니다.
rownames(moviesTb) <- c("빈도수(명)","상대도수(%)","누적상대도수(%)")
round(moviesTb,2)


# 출연한 영화수가 10편 이상인 배우수
nrow(actors[actors$movieCnt>=10,])

# 출연한 영화수가 10편 이상인 배우만 남기기
df1 <- df[df$movieCnt>=10,]
nrow(df1)

# 영화별 배우수 컬럼 생성
nActor <- aggregate(df1$actorCode, by=list(df1$movieCode), FUN=length)

# 컬럼명 바꾸기
colnames(nActor) <- c("movieCode", "actorCnt")
head(nActor)

# 전체 데이터에 병합
df1 <- merge(df1, nActor, by="movieCode", all.x=T)

# 영화별 배우수 빈도 확인
table(df1$actorCnt, useNA="ifany")

# 배우수 최빈도 영화 확인
# 출연한 영화수가 5편 이상인 배우만 대상으로 했으므로 원래 데이터와 다름!!
df1[df1$actorCnt==max(df1$actorCnt),]

# 출연한 배우수가 2명 이상인 영화만 남기기
df1 <- df1[df1$actorCnt>=2,]
nrow(df1)

# 10편 이상 출연한 배우들이 출연한 영화 편수 (중복 제거)
length(unique(df1$movieCode))

# 컬럼 정렬
df1 <- df1[,c("movieCode","movieName","year","actorCnt","actorCode","actorName","movieCnt")]
head(df1)

# 2000년 이후 영화만 남기기
df2 <- df1[df1$year>=2000,]
nrow(df2)


# 영화*배우 행렬 전처리
maDf <- cbind(df2[,c("movieCode","actorCode")], check=rep(1,nrow(df2)))
head(maDf)

# 영화*배우 행렬 생성
maMat <- acast(maDf, movieCode~actorCode, fill=0)
dim(maMat)


# 배우 간 행렬 만들기 (같은 영화에 출연한 적이 있으면 1, 없으면 0)
actorsMul <- t(maMat) %*% maMat
actorsMul <- ifelse(actorsMul>=2, 1, 0)
diag(actorsMul) <- 0


# 행의 합이 0인 행과 열 삭제하기
zeros <- as.numeric(which(rowSums(actorsMul)==0))
actorsMul <- actorsMul[-zeros,-zeros]
dim(actorsMul)



# 배우 간 거리 계산
# dstActors <- dist(actorsMul)
# dstActors <- as.matrix(dstActors)
# 
# nrow(dstActors)
# distMean <- as.data.frame(rowMeans(dstActors))
# distMean$actorCode <- rownames(distMean)
# rownames(distMean) <- c()
# colnames(distMean) <- c("distMean","actorCode")
# 
# actors1 <- merge(actors, distMean, by="actorCode", all.x=T)
# actors1 <- actors1[is.na(actors1$distMean)==F,]
# actors1 <- actors1[order(actors1$distMean,decreasing=F),]
# head(actors1,20)



## 케빈 베이컨 놀이
# cell value가 1인 컬럼 번호 가져오기 
ones <- list()
for (i in 1:nrow(actorsMul)) {
  ones[[i]] <- as.numeric(which(actorsMul[,i]==1))
}

ones[[1]]
length(ones)

# 행이름과 열이름으로 데이터프레임 생성하기
actorsDf <- data.frame()
for (i in 1:length(ones)) {
  coDf <- data.frame(from=rownames(actorsMul)[i], to=colnames(actorsMul)[ones[[i]]])
  actorsDf <- rbind(actorsDf, coDf)
}

actorsDf <- unique(actorsDf)
dim(actorsDf)

# 최단경로 구하기
graph1 <- graph.data.frame(actorsDf, directed=FALSE)
shortPath <- shortest.paths(graph1)
range(shortPath)

# 연결이 안되는 배우의 경우 99로 치환
# NA로 변경한 뒤, 평균을 구하면 오히려 짧아지는 경향이 있기 때문
shortPath <- ifelse(shortPath==Inf, 99, shortPath)

# 최단경로의 평균 구하기
nrow(shortPath)
pathMean <- as.data.frame(rowMeans(shortPath, na.rm=T))
pathMean$actorCode <- rownames(shortPath)
rownames(pathMean) <- c()
colnames(pathMean) <- c("pathMean","actorCode")

# actors 객체에 병합한 후 정리하기 
actors1 <- merge(actors, pathMean, by="actorCode", all.x=T)
actors1 <- actors1[is.na(actors1$pathMean)==F,]
actors1 <- actors1[order(actors1$pathMean, decreasing=F),]

# 상위 20명 확인
head(actors1,20)
head(actors1[,c(2,4)],20)




# 배우 간 행렬로부터 방향성 없는 무향 네트워크 객체 생성
netActors <- network(actorsMul, directed=F)
netActors

# 각 꼭지점별 중개 중심성(betweenness centrality) 계산
btwness <- betweenness(netActors)
length(btwness)

# degree 계산
degree <- degree(netActors)
length(degree)


# 배우 간 행렬에 betweenness와 degree 컬럼 붙이기
snResult <- data.frame(actorCode=rownames(actorsMul), btwness=btwness, degree=degree)
snResult

actors2 <- merge(actors1, snResult, by="actorCode", all.x=T)
actors2 <- actors2[is.na(actors2$btwness)==F,]

actors2 <- actors2[order(actors2$btwness, decreasing=T),]
head(actors2,20)

actors2 <- actors2[order(actors2$degree, decreasing=T),]
head(actors2,20)





## SNA
# betweenness로 히스토그램 그리기
hist(btwness, breaks=50)

# betweenness가 90 percentile인 값 확인
qant90 <- quantile(btwness, probs=0.9)
qant90

# betweenness가 상위 90%인 노드에 색 입히기
netActors %v% "mode" <- ifelse(btwness > qant90, "Star", "Normal")
nodeColor <- c("Star"="gold", "Normal"="gray80")

# node edge 크기 설정
set.edge.value(netActors, "edgeSize", actorsMul*2)

# 네트워크 맵 그리기
ggnet2(netActors,             # 네트워크 객체
       label=TRUE,            # 노드에 라벨 표현 여부
       label.size=3,          # 레이블 폰트 사이즈
       color="mode",          # 노드 색상 구분 기준
       palette=nodeColor,     # 노드 색상 설정
       size="degree",         # 노드 크기를 연결정도 중심성에 따라 다르게 하기
       edge.size="edgeSize",  # 엣지 굵기를 단어 간 상관계수에 따라 다르게 하기
       mode="circrand",
       family="NanumGothic")




# 키워드만 추출 
keyActors <- c("장광","이한위","김갑수")
keyActors <- actors2$actorCode[actors2$actorName %in% keyActors]
keyActors

# 키워드의 행, 열 제외
actorsMul1 <- actorsMul[, keyActors]
actorsMul1 <- actorsMul1[!rownames(actorsMul1) %in% keyActors,]

# 행의 합이 0 초과인 값만 남기기
actorsMul1 <- actorsMul1[rowSums(actorsMul1)>0,]

# 앞에서 생성한 배우 간 행렬을 이용하여 네트워크 객체를 생성
# 비대칭형 네트워크인 경우, matrix.type="bipartite"를 추가해주어야 함
netActors1 <- network(actorsMul1, directed=F, matrix.type="bipartite")
netActors1

# 각 꼭지점별 중개 중심성(betweenness centrality) 계산
btwness <- betweenness(netActors1)
length(btwness)

# 각 꼭지점별 연결 중심성(degree centrality) 계산
degree <- degree(netActors1)
length(degree)

# 컬러 지정
percent90 <- quantile(betweenness(netActors1), probs=0.9)
netActors1 %v% "color" <- ifelse(betweenness(netActors1)>percent90, "big", "small")
colors <- c("small"="grey", "big"="gold")

set.edge.value(netActors1, "edgeSize", actorsMul1[actorsMul1>0])


# 네트워크 그리기
ggnet2(netActors1,
       label=TRUE,
       label.size=3,
       color="color",
       palette=colors,
       edge.size="edgeSize",
       size=degree(actorsMul1),
       mode="fruchtermanreingold",
       family="NanumGothic")

