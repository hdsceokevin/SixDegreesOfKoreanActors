
# 메모리 초기화
rm(list = ls())
gc()

# 작업폴더 확인
getwd()

# 한글 폰트 지정
par(family = 'NanumGothic')

# 데이터 수집
# 이번 프로젝트를 위해 R로 웹크롤러를 만들어 네X버 영화 사이트의 데이터를 수집함
# 웹크롤러는 'naver movie crawler_v1.R'이며,
# 수집한 데이터는 RDS 형태로 저장하여 GitHub data 폴더에 업로드하였음
# https://github.com/MrKevinNa/SixDegreesOfKoreanActors

# 데이터의 특징 (수집일자 : 2018년 2월 12일 ~ 18일)
# 한국영화 27,121편에 대한 요약정보 수집 (korean_movie_list_20180217.RDS)
# 영화별 출연 배우 리스트를 별도로 수집 (korean_movie_actor_20180217.RDS)

# 데이터 전처리 과정
# 1. 두 개의 RDS 파일을 불러온 후, 불필요한 행 삭제
# 2. 하나의 데이터 프레임(wdf)으로 병합
# 3. 영화별 출연 배우수와 배우별 출연 영화수를 각각 구함
# 4. 간단한 EDA : 상위 20위 영화/배우 확인 및 히스토그램, 도수분포표 생성
# 5. 배우수가 2명 이상인 영화와 출연 영화수가 5편 이상인 배우만 남김
# 6. 영화 * 배우 매트릭스로 변환한 후, 배우 * 배우 매트릭스 생성
# 7. 마지막으로 각 행렬의 합계가 0인 행과 열 위치를 찾아 삭제

# 필요 패키지 불러오기
library(dplyr)
library(ggplot2)
library(reshape2)

# 영화 데이터 불러오기
movieList <- readRDS(file = './data/korean_movie_list_20180217.RDS')
str(movieList)
head(movieList)

# 영화 형식별 빈도수 확인
table(movieList$formt, useNA = 'ifany')

# (멀쩡한) 영화만 선택! (비디오영화와 성인영화는 제외)
# movieList <- movieList[movieList$formt %in% c('', 'TV영화', '단편영화', '옴니버스영화'), ]

# 영화 장르별 빈도수 확인
table(movieList$genre)

# 공연실황 제외
# movieList <- movieList[movieList$genre != '공연실황', ]

# 제작년도별 영화편수
mvYearTbl <- table(movieList$myear, exclude = c(NA, '2018', '2020'))
plot(x = mvYearTbl, type = 'l',
     xlab = '제작년도', ylab = '영화수(편)',
     main = '연도별 한국영화 제작편수')

  
# 영화별 출연 배우 데이터 불러오기
actorList <- readRDS(file = './data/korean_movie_actor_20180217.RDS')
str(actorList)
head(actorList)

# 두 데이터프레임을 하나로 합치기
wdf <- merge(x = movieList[, c('title', 'mcode', 'myear', 'genre', 'formt', 'grade')], 
             y = actorList[, c('mcode', 'aname', 'acode')], 
             by = 'mcode', 
             all.x = T)

# NA를 ''로 치환
# wdf[is.na(wdf) == TRUE] <- ''

# 영화별 출연 배우수 컬럼 만들기
numActors <- wdf %>% 
  select(c('mcode', 'acode')) %>% 
  na.omit() %>% 
  group_by(mcode) %>% 
  summarize(actorCnt = n())
  
# 배우-영화 데이터에 병합
wdf <- merge(x = wdf, y = numActors, by = 'mcode', all.x = T)
rm(numActors)

# 영화별 출연 배우 빈도수 확인
table(wdf$actorCnt, useNA = 'ifany')

# NA 행 삭제
wdf <- wdf[is.na(wdf$actorCnt) == FALSE, ]

# 가장 많은 배우가 출연한 영화 상위 20편 확인
wdf %>% 
  select(c('title', 'actorCnt')) %>% 
  na.omit() %>% 
  unique() %>% 
  arrange(desc(actorCnt)) %>% 
  head(n = 20L)


# 배우별 출연 영화수 컬럼 만들기
numMovies <- wdf %>% 
  select(c('mcode', 'acode')) %>% 
  na.omit() %>% 
  group_by(acode) %>% 
  summarize(movieCnt = n())

# 배우-영화 데이터에 병합
wdf <- merge(x = wdf, y = numMovies, by = 'acode', all.x = T)
rm(numMovies)

# 배우별 출연 영화수 빈도 확인
table(wdf$movieCnt, useNA = 'ifany')

# 가장 많은 영화에 출연한 배우 상위 20명 확인
wdf %>% 
  select(c('aname', 'movieCnt')) %>% 
  na.omit() %>% 
  unique() %>% 
  arrange(desc(movieCnt)) %>% 
  head(n = 20L)


# 히스토그램으로 나타내기
hist(x = wdf$movieCnt,
     breaks = 1:505, col = 'white', border = 'black',
     xlab = '출연한 영화수(편)', ylab = '빈도수(명)',
     main = '한국영화 배우별 출연한 영화수')

# ggplot으로 그리기
# ggplot(data = wdf, aes(x = movieCnt)) +
#   geom_histogram(breaks = 1:505, color = 'black', fill = 'white') +
#   labs(x = '출연한 영화수(편)', y = '빈도수(명)') +
#   ggtitle(label = '한국영화 배우별 출연한 영화수')


# 영화 출연 횟수 데이터로 도수분포표 만들기
# 계급의 크기를 5로 설정한 후, 각 계급별 빈도수를 구합니다.
# [주의] Hmisc의 summarize() 함수가 dplyr의 summarize()와 충돌!! 
cuts <- seq(from = 0, to = 505, by = 5)
cutLabels <- Hmisc::cut2(x = wdf$movieCnt, cuts = cuts, minmax = TRUE)

mvCntTbl <- wdf %>% 
  select(c('acode', 'movieCnt')) %>% 
  na.omit() %>% 
  unique() %>% 
  select('movieCnt') %>% 
  data.matrix() %>% 
  Hmisc::cut2(cuts = cuts, minmax = TRUE) %>% 
  table()

# 각 구간별 상대도수를 구한 후, 도수분포표에 추가합니다.
propTbl <- prop.table(mvCntTbl)
mvCntTbl <- rbind(mvCntTbl, propTbl)

# 각 구간별 누적상대도수를 구한 후, 도수분포표에 추가합니다.
csumTbl <- cumsum(propTbl)
mvCntTbl <- rbind(mvCntTbl, csumTbl)

# 행별 합계를 구한 후, 도수분포표에 추가합니다.
mvCntTbl <- addmargins(A = mvCntTbl, margin = 2)  # 행별 합계

# 소수점 2째자리에서 반올림
mvCntTbl <- round(x = mvCntTbl, digits = 2)

# 행과 열의 이름을 변경합니다.
rownames(mvCntTbl) <- c('빈도수(명)', '상대도수(%)', '누적상대도수(%)')

# 도수분포표 출력하기
# 5편 미만 출연한 영화배우가 전체 26828명 중 83%를 차지!!
print(mvCntTbl)


# 같은 영화에 함께 출연한 배우 데이터를 만들기 위해 DocumentTermMatrix 개념을 활용함!!
# 매트릭스 차원이 크면 매트릭스 연산에 많은 시간이 소요되므로 미리 축소!! 
# 출연한 배우수가 2명 이상인 영화 & 출연한 영화수가 5편 이상인 배우만!!
wdf1 <- wdf[wdf$actorCnt >= 2 & wdf$movieCnt >= 5,
            c('mcode', 'title', 'myear', 'acode', 'aname', 'actorCnt', 'movieCnt')]

# 첫 10줄만 미리보기
head(x = wdf1, n = 10L)

# 영화 * 배우 행렬 전처리 : 한 영화에 출연한 횟수를 1로 지정
wdf1Mat <- wdf1 %>%
  select(c('mcode', 'acode')) %>% 
  mutate(check = 1) %>% 
  acast(formula = mcode ~ acode, fill = 0)

# 차원수 확인
dim(wdf1Mat)

# 미리보기
wdf1Mat[1:10, 1:10]

# 배우 * 배우 행렬 만들기
# 같은 영화에 출연한 횟수를 원소로 갖는 매트릭스 생성
actorsMat <- t(wdf1Mat) %*% wdf1Mat

# 차원수 확인
dim(actorsMat)

# 미리보기
actorsMat[11:20, 11:20]

# 같은 영화에 출연한 적이 있으면 1, 없으면 0을 갖도록 2 이상을 1로 치환
actorsMat <- ifelse(test = (actorsMat >= 2), yes = 1, no = 0)

# 현재 매트릭스는 대칭행렬이며 대각원소는 해당 영화배우의 총 출연횟수이므로,
# 대각원소와 상삼각원소를 0으로 치환하여 하삼각행렬만 값을 갖도록 변환
actorsMat[upper.tri(x = actorsMat, diag = TRUE)] <- 0

# 미리보기
actorsMat[11:20, 11:20]


# 행의 합이 0인 행의 위치 탐색
zeroLocs <- actorsMat %>% 
  purrr::when(rowSums(.) == 0) %>% 
  which() %>% 
  as.numeric()

# 해당 행 삭제
actorsMat <- actorsMat[-zeroLocs, ]

# 차원수 확인
dim(actorsMat)

# 미리보기
actorsMat[11:20, 11:20]


# 케빈 베이컨의 여섯다리!! 
# 1. 배우 * 배우 행렬에서 원소의 값이 1인 행 번호(또는 열 번호)만 수집
# 2. 네트워크 구조로 만들기 위해 행의 배우코드를 'from', 열은 'to'로 지정
# 3. 위 데이터를 graph data frame으로 변환 (방향성 없는 '무향'으로 만듬)
# 4. 배우별 from에서 to에 이르는 가장 짧은 거리를 구함 (연결이 안되면 '99' 강제 할당)
# 5. 배우별 거리 평균을 구함. 이 값이 가장 짧은 배우를 한국판 케빈 베이컨으로 지정!! 

# 필요 패키지 불러오기
library(igraph)
library(network)
library(sna)
library(GGally)
library(scales)

# 매트릭스 원소의 값이 1인 열 번호 가져오기 
ones <- list()

for (i in 1:nrow(actorsMat)) {
  ones[[i]] <- which(actorsMat[i, ] == 1) %>% as.numeric()
}

# 1~10번 행에서 원소의 값이 1인 열 번호 확인!
ones[1:10]

# 행 이름(from)과 열 이름(to)으로 데이터 프레임 생성하기
# 이 데이터 프레임에는 행마다 함께 출연한 적 있는 열 번호가 행으로 저장됨
actorsWdf <- data.frame()

for (i in 1:length(ones)) {
  codf <- data.frame(from = rownames(actorsMat)[i],
                     to = colnames(actorsMat)[ones[[i]]])
  actorsWdf <- rbind(actorsWdf, codf)
}

# 중복 확인
actorsWdf[duplicated(actorsWdf), ]

# 차원수 확인
dim(actorsWdf)


# 방향성이 없는 무향 graph data frame 생성
graphDf <- graph.data.frame(d = actorsWdf, directed = FALSE)
graphDf[1:10]

# 배우별 나머지 3960명과의 최단거리를 구함
shortPath <- shortest.paths(graphDf)

# 차원수 보기
dim(shortPath)

# 최단거리 미리보기
shortPath[1:10, 1:10]

# 최단거리 범위 확인
# 자기 자신은 0, 서로 연결되지 않은 경우 Inf 값을 가짐
range(shortPath)

# 서로 연결되는 않은 배우 간 거리를 99로 치환
# [주의] NA로 변경하면 오히려 평균이 짧아질 수 있으므로 원하는 결과를 얻을 수 없음!
shortPath <- ifelse(test = (shortPath == Inf), yes = 99, no = shortPath)

# 행 기준으로 최단거리의 평균 구하기
pathMean <- data.frame(mean = rowMeans(x = shortPath, na.rm = TRUE))

# 배우코드 열 추가
pathMean$acode <- rownames(pathMean)

# 행 이름 삭제
rownames(pathMean) <- c()

# 배우 정보 병합
actorInfo <- merge(x = pathMean,
                   y = unique(actorList[, c('acode', 'aname')]),
                   by = 'acode', 
                   all.x = TRUE)

# 최단거리 짧은 기준으로 정렬
actorInfo <- actorInfo[order(actorInfo$mean, decreasing = FALSE), ]

# 상위 20명 확인
head(x = actorInfo, n = 20L)


# 네트워크 맵 그리기
plot(graphDf)
plot(graphDf,
     edge.arrow.size = 0.5,
     vertex.color = "gold",
     vertex.size = 15,
     vertex.frame.color = "gray",
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     vertex.label.dist = 2,
     edge.curved = 0.2) 




# 네트워크 그래프 그리기
# 배우 간 행렬로부터 방향성 없는 무향 네트워크 객체 생성
actorsNet <- network(actorsMat, directed = FALSE)
actorsNet

# 각 꼭지점별 중개 중심성(betweenness centrality) 계산
# [주의] igraph의 betweenness()와 충돌!! 
actorsBtw <- sna::betweenness(actorsNet)
length(actorsBtw)

# degree 계산
degree <- degree(netActors)
length(degree)


# 배우 간 행렬에 betweenness와 degree 컬럼 붙이기
snResult <- data.frame(actorCode=rownames(actorsMat), btwness=btwness, degree=degree)
snResult

actors2 <- merge(actors1, snResult, by='actorCode', all.x=T)
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
netActors %v% 'mode' <- ifelse(btwness > qant90, 'Star', 'Normal')
nodeColor <- c('Star'='gold', 'Normal'='gray80')

# node edge 크기 설정
set.edge.value(netActors, 'edgeSize', actorsMat*2)

# 네트워크 맵 그리기
ggnet2(netActors,             # 네트워크 객체
       label=TRUE,            # 노드에 라벨 표현 여부
       label.size=3,          # 레이블 폰트 사이즈
       color='mode',          # 노드 색상 구분 기준
       palette=nodeColor,     # 노드 색상 설정
       size='degree',         # 노드 크기를 연결정도 중심성에 따라 다르게 하기
       edge.size='edgeSize',  # 엣지 굵기를 단어 간 상관계수에 따라 다르게 하기
       mode='circrand',
       family='NanumGothic')




# 키워드만 추출 
keyActors <- c('장광','이한위','김갑수')
keyActors <- actors2$actorCode[actors2$actorName %in% keyActors]
keyActors

# 키워드의 행, 열 제외
actorsMat1 <- actorsMat[, keyActors]
actorsMat1 <- actorsMat1[!rownames(actorsMat1) %in% keyActors,]

# 행의 합이 0 초과인 값만 남기기
actorsMat1 <- actorsMat1[rowSums(actorsMat1)>0,]

# 앞에서 생성한 배우 간 행렬을 이용하여 네트워크 객체를 생성
# 비대칭형 네트워크인 경우, matrix.type='bipartite'를 추가해주어야 함
netActors1 <- network(actorsMat1, directed=F, matrix.type='bipartite')
netActors1

# 각 꼭지점별 중개 중심성(betweenness centrality) 계산
btwness <- betweenness(netActors1)
length(btwness)

# 각 꼭지점별 연결 중심성(degree centrality) 계산
degree <- degree(netActors1)
length(degree)

# 컬러 지정
percent90 <- quantile(betweenness(netActors1), probs=0.9)
netActors1 %v% 'color' <- ifelse(betweenness(netActors1)>percent90, 'big', 'small')
colors <- c('small'='grey', 'big'='gold')

set.edge.value(netActors1, 'edgeSize', actorsMat1[actorsMat1>0])


# 네트워크 그리기
ggnet2(netActors1,
       label=TRUE,
       label.size=3,
       color='color',
       palette=colors,
       edge.size='edgeSize',
       size=degree(actorsMat1),
       mode='fruchtermanreingold',
       family='NanumGothic')

