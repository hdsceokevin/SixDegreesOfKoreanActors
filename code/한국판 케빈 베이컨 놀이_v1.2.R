
# 메모리 초기화
rm(list = ls())
gc()

# 작업폴더 확인
getwd()
setwd(dir = '~/Documents/GitHub/SixDegreesOfKoreanActors/data')

# 작업일자 지정
# wdate <- format(x = Sys.Date(), '%Y%m%d')
wdate <- '20200327'

# 한글 폰트 지정
par(family = 'NanumGothic')


# part 1. 데이터 수집 ----

# 이번 프로젝트를 위해 R로 웹크롤러를 만들어 네이버 영화 사이트의
# 데이터를 수집합니다. 웹크롤러는 'naver movie crawler_v1.R'이며,
# 수집한 데이터는 RDS 파일로 GitHub data 폴더에 업로드하였습니다.
# https://github.com/MrKevinNa/SixDegreesOfKoreanActors

# 데이터의 특징 (수집일자 : 2018년 2월 12일 ~ 18일)
# 한국영화 27,121편에 대한 요약정보 수집 
# (korean_movie_list_20200327.RDS)
# 영화별 출연 배우 리스트를 별도로 수집 
# (korean_movie_actor_20200327.RDS)


# part 2. 데이터 전처리 과정 ----

# 1. RDS 파일을 읽어온 후, 하나의 데이터프레임(wdf)으로 병합하고 
#    2010년 이후 제작된 영화만 남김
# 2. 영화별 출연 배우수와 배우별 출연 영화수를 각각 구함
# 3. 간단한 EDA : 상위 20위 영화/배우 확인 및 히스토그램 및 
#    도수분포표 생성
# 4. 배우 2명 이상인 영화와 출연 영화수가 5편 이상인 배우만 남김
# 5. 영화-배우 행렬로 변환한 후, 열이름을 배우 이름으로 변경
# 6. 배우-배우 행렬을 생성한 후, 합계가 0인 행의 위치를 찾아 삭제

# 필요 패키지 불러오기
library(tidyverse)

# 영화 데이터 불러오기
movies <- readRDS(file = 'korean_movie_list_20200327.RDS')
str(object = movies)
head(x = movies)

# 제작년도별 영화편수
table(movies$mYear, useNA = 'ifany')
plot(x = table(movies$mYear, exclude = c('한국', '2020')), 
     type = 'l',
     las = 2,
     xlab = '제작년도', 
     ylab = '영화수(편)',
     main = '연도별 한국영화 제작편수')

  
# 영화별 출연 배우 데이터 불러오기
actors <- readRDS(file = 'korean_movie_actor_20200327.RDS')
str(object = actors)
head(x = actors)

# 두 데이터프레임을 하나로 합치기
wdf <- merge(x = movies, 
             y = actors, 
             by = 'mCode', 
             all.x = TRUE)

# NA를 갖는 행 삭제
wdf <- wdf %>% filter(complete.cases(.))

# 영화별 출연 배우수 컬럼 만들기
numActors <- wdf %>% 
  select(mCode, aCode) %>% 
  group_by(mCode) %>% 
  summarize(actorCnt = n())
  
# 배우-영화 데이터에 병합
wdf <- merge(x = wdf, y = numActors, by = 'mCode', all.x = TRUE)
rm(numActors)

# 영화별 출연 배우 빈도수 확인
table(wdf$actorCnt, useNA = 'ifany')

# 가장 많은 배우가 출연한 영화 상위 20편 확인
wdf %>% 
  select(mName, actorCnt) %>% 
  distinct() %>% 
  arrange(desc(x = actorCnt)) %>% 
  slice(1:20)


# 배우별 출연 영화수 컬럼 만들기
numMovies <- wdf %>% 
  select(mCode, aCode) %>% 
  group_by(aCode) %>% 
  summarize(movieCnt = n())

# 배우-영화 데이터에 병합
wdf <- merge(x = wdf, y = numMovies, by = 'aCode', all.x = TRUE)
rm(numMovies)

# 배우별 출연 영화수 빈도 확인
table(wdf$movieCnt, useNA = 'ifany')

# 가장 많은 영화에 출연한 배우 상위 20명 확인
wdf %>% 
  select(aName, movieCnt) %>% 
  distinct() %>% 
  arrange(desc(x = movieCnt)) %>% 
  slice(1:20)


# 히스토그램으로 나타내기
hist(x = wdf$movieCnt,
     breaks = 1:65, 
     col = 'white', 
     border = 'black',
     xlab = '출연한 영화수(편)', 
     ylab = '빈도수(명)',
     main = '한국영화 배우별 출연한 영화수')

# ggplot으로 그리기
ggplot(data = wdf, 
       mapping = aes(x = movieCnt)) +
  geom_histogram(breaks = 1:65, 
                 color = 'black', 
                 fill = 'white') + 
  labs(title = '한국영화 배우별 출연한 영화수', 
       x = '출연한 영화수(편)', 
       y = '빈도수(명)') + 
  theme_bw() + 
  theme(text = element_text(family = 'NanumGothic'))


# 영화 출연 횟수 데이터로 도수분포표 만들기
# 계급의 크기를 5로 설정한 후, 각 계급별 빈도수를 구합니다.
cuts <- cut(x = wdf$movieCnt, 
            breaks = seq(from = 0, to = 65, by = 5))

# 각 구간별 상대도수를 구한 후, 도수분포표에 추가합니다.
cuts %>% 
  table() %>% 
  prop.table() %>% 
  cumsum() %>% 
  round(digits = 4L) * 100


# 한 영화에 함께 출연한 배우 행렬 생성
# 행렬 차원이 크면 행렬 연산에 많은 시간이 소요되므로,
# 배우 2명 이상인 영화 & 출연한 영화수가 5편 이상인 배우만 남겨
# 영화-배우 행렬로 전처리함. 
# 이 때 각 영화에 출연한 횟수(check)를 1로 지정
# [주의] dplyr의 filter()와 stats의 filter()와 충돌 가능!
wdf2Mat <- wdf %>% 
  filter(actorCnt >= 2 & movieCnt >= 5) %>% 
  select(mCode, aCode) %>% 
  mutate(check = 1) %>% 
  spread(key = aCode, value = check, fill = 0)

rownames(x = wdf2Mat) <- wdf2Mat$mCode
wdf2Mat$mCode <- NULL

# 차원수 확인
dim(x = wdf2Mat)

# 미리보기
wdf2Mat[1:10, 1:10]


# 영화배우 코드 대신 영화배우 이름으로 대체!
# 하지만 영화배우 이름은 중복이 꽤 많을 것으로 추정되므로,
# 중복되는 영화배우 이름을 변형함 (이름-1, 이름-2, ...)
actorName <- wdf %>% 
  select(aCode, aName, movieCnt) %>% 
  distinct() %>% 
  filter(aCode %in% colnames(x = wdf2Mat)) %>% 
  arrange(aName, desc(x = movieCnt))

# 이름으로 중복되는 규모 확인
actorName$aName %>% duplicated() %>% sum()

# 중복 이름 변형
dup1 <- duplicated(x = actorName$aName)
dup2 <- duplicated(x = actorName$aName, fromLast = TRUE)

actorName <- transform(
  `_data` = actorName,
  aNameNew = ifelse(
    test = dup1 | dup2,
    yes = str_c(aName, 
                ave(x = aName, aName, FUN = seq_along), 
                sep = '-'),
    no = aName
  )
)

# 행렬의 열이름을 배우이름으로 변경하기에 앞서 
# 현재 열이름의 순서와 새로 만든 데이터 프레임의 배우코드 순서가
# 서로 맞는지 확인해야 함

# 배우코드 오름차순으로 정렬
actorName <- actorName %>% arrange(aCode)

# 두 벡터가 서로 다른 부분이 있는지 확인
which(x = colnames(x = wdf2Mat) != actorName$aCode)

# 열이름 변경
colnames(x = wdf2Mat) <- actorName$aNameNew


# 배우-배우 행렬 만들기
# 같은 영화에 출연한 횟수를 원소로 갖는 행렬 생성
wdf2Mat <- as.matrix(x = wdf2Mat)
actorsMat <- t(x = wdf2Mat) %*% wdf2Mat

# 차원수 확인
dim(x = actorsMat)

# 미리보기
actorsMat[1:10, 1:10]

# 함께 출연한 적이 있으면 1, 없으면 0을 갖도록 2 이상을 1로 치환
actorsMat <- ifelse(test = (actorsMat >= 2), yes = 1, no = 0)

# 현재 대칭행렬이며 대각원소는 해당 영화배우의 총 출연횟수이므로,
# 대각원소와 상삼각원소를 0으로 치환하여 하삼각행렬만 값을 남김
actorsMat[upper.tri(x = actorsMat, diag = TRUE)] <- 0

# 미리보기
actorsMat[1:10, 1:10]


# 행의 합이 0인 행의 위치 탐색
zeroLocs <- actorsMat %>% 
  when(rowSums(.) == 0) %>% 
  which() %>% 
  as.numeric()

# 해당 행 삭제
actorsMat <- actorsMat[-zeroLocs, ]

# 차원수 확인
dim(x = actorsMat)

# 미리보기
actorsMat[1:10, 1:10]

# save Rdata
fileName <- str_c('korean_six_degrees_', wdate, '.Rdata')
save.image(file = fileName)


# part 3. 한국판 케빈 베이컨의 찾기 ----

# 1. 배우-배우 행렬에서 원소의 값이 1인 열 번호만 수집
# 2. 네트워크 객체로 만들기 위해 행의 배우코드를 'from', 열은 'to'로 지정
# 3. 위 데이터를 graph 객체로 변환 (방향성 없는 '무향'으로 만듬)
# 4. 배우별 from에서 to에 이르는 가장 짧은 거리를 구함 (연결이 안되면 99로 변환)
# 5. 배우별 거리 평균을 구함. 이 값이 가장 작은 배우를 한국판 케빈 베이컨으로 지정!! 

# 필요 패키지 불러오기
library(igraph)

# 결과를 저장할 빈 리스트 객체 생성
ones <- list()

# 행렬 원소의 값이 1인 열 번호 가져오기 
for (i in 1:nrow(actorsMat)) {
  ones[[i]] <- which(actorsMat[i, ] == 1) %>% as.numeric()
}

# 1~10번 행에서 원소의 값이 1인 열 번호 확인!
ones[1:10]


# 결과를 저장할 빈 데이터 프레임 객체 생성
actorsWdf <- data.frame()

# 행이름(from)과 열이름(to)으로 데이터 프레임 생성하기
# 데이터프레임 행은 함께 출연했던 열번호가 행으로 저장됨
for (i in 1:length(ones)) {
  codf <- data.frame(from = rownames(actorsMat)[i],
                     to = colnames(actorsMat)[ones[[i]]])
  actorsWdf <- rbind(actorsWdf, codf)
}

# 중복 확인
actorsWdf %>% duplicated() %>% sum()

# 차원수 확인
dim(x = actorsWdf)


# 방향성이 없는 무향 graph 객체 생성
graphObj <- graph_from_data_frame(d = actorsWdf, 
                                  directed = FALSE)

# graph 객체 출력
print(x = graphObj)

# 각 배우별 나머지 배우들(3960명)과의 최단거리 계산
shortPath <- shortest.paths(graph = graphObj)

# 차원수 보기
dim(x = shortPath)

# 최단거리 미리보기
shortPath[1:10, 1:10]

# 최단거리 범위 확인
# 자기 자신은 0, 서로 연결되지 않은 경우 Inf 값을 가짐
range(shortPath)

# Inf 값을 제외하고 가장 큰 값 확인
range(shortPath, finite = TRUE)

# 서로 연결되는 않은 배우 간 거리를 99로 치환
shortPath <- ifelse(test = (shortPath == Inf), 
                    yes = 99, 
                    no = shortPath)

# 행 기준으로 최단거리의 평균 구하기
pathMean <- rowMeans(x = shortPath, na.rm = TRUE) %>% 
  as.data.frame()

# 배우이름 열 추가
pathMean$aNameNew <- rownames(x = pathMean)

# 평균거리 열이름 추가
colnames(x = pathMean)[1] <- 'pathMean'

# 행이름 삭제
rownames(x = pathMean) <- NULL

# 배우 정보와 병합
resultKoKB <- merge(x = actorName,
                    y = pathMean,
                    by = 'aNameNew', 
                    all.Y = TRUE)

# 최단거리 평균을 기준으로 오름차순 정렬
resultKoKB <- resultKoKB %>% arrange(pathMean)

# 상위 20명 확인
head(x = resultKoKB, n = 20L)


# 최단거리가 가장 짧은 배우와 직접 연결된 네트워크 그래프 그리기
loc <- which(x = rownames(x = shortPath) == '정인기')
print(x = loc)

edges <- E(graph = graphObj)[loc%--%V(graph = graphObj)]

graphPlt1 <- subgraph.edges(graph = graphObj, 
                            eids = edges) %>% 
  simplify(remove.multiple = TRUE, 
           remove.loops = TRUE)

set.seed(seed = 1234)
plot(x = graphPlt1,
     vertex.color = 'gold',
     vertex.frame.color = 'white',
     vertex.shape = 'circle',
     vertex.size = 10,
     # vertex.label = NULL,
     vertex.label.color = 'gray20',
     vertex.label.family = 'NanumGothic',
     vertex.label.font = 2,
     vertex.label.cex = 0.6,
     # vertex.label.dist = 1,
     # vertex.label.degree = 0,
     edge.color = 'gray50',
     edge.width = 0.8,
     # edge.lty = 1,
     edge.curved = 0.2,
     margin = c(0, 0, 0, 0)
     )


# 확대 네트워크 그래프 그리기
vrtis <- shortPath[loc, ] %>% 
  when(. == 1) %>% 
  which()

print(x = vrtis)
colnames(x = shortPath)[vrtis]

# [참고] 모든 from--to 관계 데이터에서 .from(위치) 또는 .to(위치) # 등으로 관심 있는 데이터만 추출할 수 있습니다.
# get.edges(graph = graphObj,
#           es = E(graph = graphObj)[.from(2)])[, 2]

locs <- c(loc, vrtis)
edges <- E(graph = graphObj)[locs%--%V(graph = graphObj)]

graphPlt2 <- subgraph.edges(graph = graphObj, 
                            eids = edges) %>% 
  simplify(remove.multiple = TRUE, 
           remove.loops = TRUE)

set.seed(seed = 1234)
plot(x = graphPlt2,
     vertex.color = 'gold',
     vertex.frame.color = 'white',
     vertex.shape = 'circle',
     vertex.size = 10,
     # vertex.label = NULL,
     vertex.label.color = 'gray20',
     vertex.label.family = 'NanumGothic',
     vertex.label.font = 2,
     vertex.label.cex = 0.6,
     # vertex.label.dist = 1,
     # vertex.label.degree = 0,
     edge.color = 'gray50',
     edge.width = 0.8,
     # edge.lty = 1,
     edge.curved = 0.2,
     margin = c(0, 0, 0, 0)
     )


# save Rdata
save.image(file = fileName)
