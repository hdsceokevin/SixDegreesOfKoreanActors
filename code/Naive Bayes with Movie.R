# 필요 패키지를 불러옵니다.
library(dplyr)
library(ggplot2)
library(reshape2)

# 데이터 저장된 폴더 지정
wdir <- '~/Documents/GitHub/MrKevinNa/SixDegreesOfKoreanActors/data'

# 영화 데이터를 읽습니다.
movieList <- readRDS(file = paste(wdir, 'korean_movie_list_20180217.RDS', sep = '/'))

# 영화별 출연 배우 데이터를 읽습니다.
actorList <- readRDS(file = paste(wdir, 'korean_movie_actor_20180217.RDS', sep = '/'))

# 두 데이터프레임을 하나로 병합합니다.
wdf <- merge(x = movieList[, c('title', 'mcode', 'myear', 'genre', 'formt', 'grade')], 
             y = actorList[, c('mcode', 'aname', 'acode')], 
             by = 'mcode', 
             all.x = T)

# 영화 데이터의 구조를 확인합니다.
str(object = wdf)

# 영화 데이터를 미리보기 합니다.
head(x = wdf, n = 10L)


# 영화별 출연 배우수 벡터를 만듭니다. 
numActors <- wdf %>% 
  select(c('mcode', 'acode')) %>% 
  na.omit() %>% 
  group_by(mcode) %>% 
  summarize(actorCnt = n())

sort(x = numActors$actorCnt, decreasing = TRUE)

hist(x = numActors$actorCnt,
     labels = TRUE,
     xlim = c(0, 30),
     breaks = 30,
     freq = FALSE,
     col = 'gray30',
     border = 'white')

# wdf에 병합하고 벡터는 삭제합니다. 
wdf <- merge(x = wdf, y = numActors, by = 'mcode', all.x = T)
rm(numActors)

# 영화별 출연 배우 빈도수를 확인합니다.
table(wdf$actorCnt, useNA = 'ifany')

# NA 행을 삭제합니다.
# 출연한 배우 데이터가 없는 영화들입니다. 
wdf <- wdf[is.na(wdf$actorCnt) == FALSE, ]


# 배우별 출연 영화수 벡터를 만듭니다.
numMovies <- wdf %>% 
  select(c('mcode', 'acode')) %>% 
  na.omit() %>% 
  group_by(acode) %>% 
  summarize(movieCnt = n())

sort(x = numMovies$movieCnt, decreasing = TRUE)

hist(x = numMovies$movieCnt,
     labels = TRUE,
     xlim = c(0, 40),
     breaks = 100,
     freq = FALSE,
     col = 'gray30',
     border = 'white')

# wdf에 병합하고 벡터는 삭제합니다. 
wdf <- merge(x = wdf, y = numMovies, by = 'acode', all.x = T)
rm(numMovies)

# 배우별 출연 영화수 빈도수를 확인합니다.
table(wdf$movieCnt, useNA = 'ifany')

# 영화 장르 데이터가 있는 영화로 모형을 만들고
# 영화 장르 데이터가 없는 영화는 따로 남기기 (나중에 모형으로 추정!)
wdf4Fit <- wdf[is.na(wdf$genre) == FALSE, ]
wdf4Prd <- wdf[is.na(wdf$genre) == TRUE, ]


# 영화-배우 행렬을 만듭니다.
# 5명 이상 출연한 영화이면서 5편 이상 출연한 배우만 남깁니다. 
# [주의] dplyr의 filter()가 먼저 불려온 stats의 filter()와 충돌합니다!
wdf2Mat <- wdf4Fit %>% 
  select(c('mcode', 'acode', 'actorCnt', 'movieCnt')) %>% 
  na.omit() %>% 
  dplyr::filter(actorCnt >= 11 & movieCnt >= 51) %>% 
  select(c('mcode', 'acode')) %>% 
  mutate(check = 1) %>% 
  acast(formula = mcode ~ acode, fill = 0)

# 행렬의 차원수를 확인합니다.
dim(wdf2Mat)
## [1] 1770  209

# 새로 만든 행렬에 속한 영화배우 이름만 따로 추출합니다.
actorName <- wdf4Fit %>% 
  select(c('acode', 'aname', 'movieCnt')) %>% 
  unique() %>% 
  dplyr::filter(acode %in% colnames(wdf2Mat)) %>% 
  arrange(aname, desc(movieCnt))

# 이름으로 중복되는 규모를 확인합니다.
actorName %>% 
  select('aname') %>% 
  duplicated() %>% 
  sum()
## [1] 0

# 중복된 영화배우 이름을 변형합니다.
actorName <- transform(
  `_data` = actorName,
  anameNew = ifelse(
    test = (duplicated(aname) | duplicated(aname, fromLast = TRUE)),
    yes = paste(aname, ave(aname, aname, FUN = seq_along), sep = '-'),
    no = aname
    )
  )


# 배우코드 기준으로 오름차순 정렬합니다. 
actorName <- actorName[order(actorName$acode, decreasing = FALSE), ]

# 두 벡터가 서로 다른 부분 부분이 있는지 확인합니다. 
identical(colnames(wdf2Mat), actorName$acode)
## [1] TRUE

# 두 벡터가 순서까지 같으므로, 행렬의 열 이름을 변경합니다.
colnames(wdf2Mat) <- actorName$anameNew


# 영화코드에 장르 붙이기
wdf4Trn <- wdf2Mat %>% 
  as.data.frame() %>% 
  mutate(mcode = rownames(.)) %>% 
  merge(y = wdf4Fit[, c('mcode', 'genre')],
        by = 'mcode',
        all.x = TRUE) %>% 
  '['(-1)

str(wdf4Trn)

# 영화 장르별 빈도수를 확인합니다.
table(wdf4Trn$genre, useNA = 'ifany') %>% 
  sort(decreasing = TRUE)

# 2천 행 넘는 장르만 남기기
genre <- table(wdf4Trn$genre, useNA = 'ifany') %>% 
  sort(decreasing = TRUE) %>% 
  purrr::keep(. >= 2000) %>% 
  names()

wdf4Trn <- wdf4Trn[wdf4Trn$genre %in% genre, ]


# 10% 샘플링
set.seed(seed = 123)
N <- nrow(wdf4Trn)
idx <- sample(x = N, size = N * 0.2, replace = FALSE)
trainSet <- wdf4Trn[idx, ]
testSet <- wdf4Trn[-idx, ]

# 행 합계가 10 이상인 영화만 추출
# trainSet <- wdf4Trn[rowSums(wdf4Trn[ , -4367]) >= 10, ]
# set.seed(seed = 123)
# N <- nrow(trainSet)
# trainSet <- trainSet[idx, ]

# ncol(trainSet)
# colSums(trainSet[ , -ncol(trainSet)]) %>% sort() %>% head(n = 50L)
# rowSums(trainSet[ , -ncol(trainSet)]) %>% sort() %>% head(n = 50L)
# 
# trainSet <- trainSet[colSums(trainSet[ , -ncol(trainSet)]) > 0, ]


# 나이브 베이즈 분류모형을 적합합니다. 
library(e1071)
fit <- naiveBayes(genre ~ ., data = trainSet, laplace = 1)
print(fit)

# 분류모형의 추정값(레이블)을 확인합니다. 
pred <- predict(fit, trainSet)
head(x = pred, n = 10L)

# 분류모형의 예측값과 실제값을 비교합니다. 
real <- wdf4Trn$genre
table(pred, real)

# 혼동행렬을 이용하여 분류 성능을 확인합니다. 
library(caret)
confusionMatrix(data = pred, reference = real)


# 분류모형의 추정확률을 확인합니다. 
prob <- predict(fit, wdf4Trn, type = "raw")
head(x = prob, n = 10L)

# ROC 커브를 그려서 분류 성능을 확인하고, 
# AUROC를 계산하는 사용자 정의 함수를 만듭니다. 
library(ROCR)
checkROC <- function(prob, real) {
  
  # ROC 커브를 그려서 분류 성능을 확인합니다. 
  pred <- prediction(prob, real)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf)
  lines(x = c(0, 1), y = c(0, 1), col = 'red', lty = 2)
  
  # AUROC을 계산합니다.
  print(pROC::auc(real, prob))
}

# 결과를 확인합니다. 
checkROC(prob = pred[, 2], real = real)

