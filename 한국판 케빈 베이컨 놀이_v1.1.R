
# 메모리 초기화
rm(list = ls())
gc()

# 작업폴더 확인
getwd()

# 한글 폰트 지정
par(family = 'NanumGothic')


# part 1 : 데이터 수집 ----

# 이번 프로젝트를 위해 R로 웹크롤러를 만들어 네이버 영화 사이트의 데이터를 수집
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

# 영화 장르별 빈도수 확인
table(movieList$genre, useNA = 'ifany')

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


# 같은 영화에 함께 출연한 배우 데이터를 만들기 위해 DocumentTermMatrix 개념 활용!
# 매트릭스 차원이 크면 매트릭스 연산에 많은 시간이 소요되므로,
# 출연한 배우수가 2명 이상인 영화 & 출연한 영화수가 5편 이상인 배우만 남긴 후
# 영화 * 배우 행렬로 전처리함. 이 때 각 영화에 출연한 횟수(check)를 1로 지정
# [주의] dplyr의 filter()가 먼저 불려온 stats의 filter()와 충돌!!
wdf2Mat <- wdf %>% 
  select(c('mcode', 'acode', 'actorCnt', 'movieCnt')) %>% 
  na.omit() %>% 
  dplyr::filter(actorCnt >= 2 & movieCnt >= 5) %>% 
  select(c('mcode', 'acode')) %>% 
  mutate(check = 1) %>% 
  acast(formula = mcode ~ acode, fill = 0)

# 차원수 확인
dim(wdf2Mat)

# 미리보기
wdf2Mat[11:20, 11:20]


# 영화배우 코드로는 누구인지 쉽게 알 수 없으므로 영화배우 이름으로 대체!
# 하지만 영화배우 이름은 중복이 꽤 많을 것으로 추정되므로,
# 중복 규모 확인 후, 중복되는 영화배우 이름을 변형함 (이름-1, 이름-2, ...)
actorName <- wdf %>% 
  select(c('acode', 'aname', 'movieCnt')) %>% 
  unique() %>% 
  dplyr::filter(acode %in% colnames(wdf2Mat)) %>% 
  arrange(aname, desc(movieCnt))

# 이름으로 중복되는 규모 확인 : 4482명 중 283명 
actorName %>% 
  select('aname') %>% 
  duplicated() %>% 
  sum()

# 중복 이름 변형
actorName <- transform(`_data` = actorName,
                       anameNew = ifelse(
                         test = (duplicated(aname) | duplicated(aname, fromLast = TRUE)),
                         yes = paste(aname, ave(aname, aname, FUN = seq_along), sep = '-'),
                         no = aname
                         )
                       )

# 배우코드 오름차순으로 정렬
actorName <- actorName[order(actorName$acode, decreasing = FALSE), ]

# 매트릭스의 열 이름을 배우코드에서 배우이름으로 변경하기에 앞서
# 현재 열 이름의 순서와 새로만든 데이터 프레임의 배우코드 순서가 맞는지 확인
setdiff(colnames(wdf2Mat), actorName$acode)

# 열 이름 변경
colnames(wdf2Mat) <- actorName$anameNew


# 배우 * 배우 행렬 만들기
# 같은 영화에 출연한 횟수를 원소로 갖는 매트릭스 생성
actorsMat <- t(wdf2Mat) %*% wdf2Mat

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

# save Rdata
# save.image(file = 'korean_six_degrees_20180220.Rdata')




# part 2. 한국판 케빈 베이컨의 찾기 ----

# 1. 배우 * 배우 행렬에서 원소의 값이 1인 행 번호(또는 열 번호)만 수집
# 2. 네트워크 구조로 만들기 위해 행의 배우코드를 'from', 열은 'to'로 지정
# 3. 위 데이터를 graph 객체로 변환 (방향성 없는 '무향'으로 만듬)
# 4. 배우별 from에서 to에 이르는 가장 짧은 거리를 구함 (연결이 안되면 '99' 강제 할당)
# 5. 배우별 거리 평균을 구함. 이 값이 가장 짧은 배우를 한국판 케빈 베이컨으로 지정!! 

# 필요 패키지 불러오기
library(igraph)

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


# 방향성이 없는 무향 graph 객체 생성
graphObj <- graph_from_data_frame(d = actorsWdf, directed = FALSE)

# graph 객체 출력
print(graphObj)

# 각 배우별 나머지 배우들(3960명)과의 최단거리 계산
shortPath <- shortest.paths(graphObj)

# 차원수 보기
dim(shortPath)

# 최단거리 미리보기
shortPath[1:10, 1:10]

# [에제] 1번 꼭지점인 이휘재와 직접 연결된 배우(들) 추출
vrtis <- shortPath['이휘재', ] %>% 
  purrr::when(. == 1) %>% 
  which() %>% 
  names()

print(vrtis)

# 이휘재와 직접 연결된 배우들 중 전노민과 직접 연결된 배우(들) 확인
vrtis <- shortPath[vrtis, '전노민'] %>% 
  purrr::when(. == 1) %>% 
  which() %>% 
  names()

print(vrtis)


# 이휘재(1번 꼭지점)와 직접 연결된 네트워크 그래프 그려보기
edges <- E(graphObj)[1%--%V(graphObj)]

graphPlt1 <- subgraph.edges(graph = graphObj, eids = edges) %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

plot(x = graphPlt1,
     vertex.color = 'gold',
     vertex.frame.color = 'white',
     vertex.shape = "circle",
     vertex.size = 20,
     #vertex.label = NULL,
     vertex.label.color = "gray20",
     vertex.label.family = 'NanumGothic',
     vertex.label.font = 2,
     vertex.label.cex = 0.7,
     #vertex.label.dist = 1,
     #vertex.label.degree = 0,
     edge.color = "gray50",
     edge.width = 0.8,
     #edge.lty = 1,
     edge.curved = 0.2,
     margin = c(0,0,0,0)
     )


# 이휘재와 전노민 사이에 연결된 꼭지점들로 확대한 네트워크 그래프 그리기
# vrtis <- get.edges(graph = graphObj, es = E(graphObj)[inc(1)])[, 2] %>% c(1)
edges <- E(graphObj)[c('이휘재', vrtis)%--%V(graphObj)]

graphPlt2 <- subgraph.edges(graph = graphObj, eids = edges) %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

plot(x = graphPlt2,
     vertex.color = 'gold',
     vertex.frame.color = 'white',
     vertex.shape = "circle",
     vertex.size = 16,
     #vertex.label = NULL,
     vertex.label.color = "gray20",
     vertex.label.family = 'NanumGothic',
     vertex.label.font = 2,
     vertex.label.cex = 0.7,
     #vertex.label.dist = 1,
     #vertex.label.degree = 0,
     edge.color = "gray50",
     edge.width = 0.8,
     #edge.lty = 1,
     edge.curved = 0.2,
     margin = c(0,0,0,0)
     )


# 최단거리 범위 확인
# 자기 자신은 0, 서로 연결되지 않은 경우 Inf 값을 가짐
range(shortPath)

# 서로 연결되는 않은 배우 간 거리를 99로 치환
# [주의] NA로 변경하면 오히려 평균이 짧아질 수 있으므로 원하는 결과를 얻을 수 없음!
shortPath <- ifelse(test = (shortPath == Inf), yes = 99, no = shortPath)

# 행 기준으로 최단거리의 평균 구하기
pathMean <- data.frame(mean = rowMeans(x = shortPath, na.rm = TRUE))

# 배우코드 열 추가
pathMean$anameNew <- rownames(pathMean)

# 행 이름 삭제
rownames(pathMean) <- c()

# 배우 정보 병합
actorName <- merge(x = actorName,
                   y = pathMean,
                   by = 'anameNew', 
                   all.Y = TRUE)

# 최단거리 짧은 기준으로 정렬
actorName <- actorName[order(actorName$mean, decreasing = FALSE), ]

# 상위 20명 확인
head(x = actorName, n = 20L)

# save Rdata
# save.image(file = 'korean_six_degrees_20180220.Rdata')
