
# 네이버 영화 사이트에서 데이터 수집하기
# 웹사이트 url : 'https://movie.naver.com/movie/sdb/browsing/bmovie_nation.nhn'

# 웹크롤링 순서
# 0. 한국영화 개수 확인 
# 1. 영화별 정보 수집
# 2. 영화별 배우 리스트 수집 (주연, 조연)
# 3. 영화별 평점 수집 (선택)
# 4. 영화별 리뷰 수집 (선택)


# 필수 패키지 불러오기
library(tidyverse)
library(httr)
library(rvest)

# 작업경로 확인 후 data 폴더로 변경
getwd()
setwd(dir = './data')


# ------------------------------------------------------------
# part 0. 한국영화 개수 확인
# ------------------------------------------------------------

# HTTP 요청
res <- GET(url = 'https://movie.naver.com', 
           path = '/movie/sdb/browsing/bmovie_nation.nhn')

# 응답 결과 확인
print(res)

# 한국영화 수만 출력
movieCnt <- res %>% 
  read_html(encoding = 'MS949') %>% 
  html_node(css = '#old_content > dl:nth-child(17) > dd > ul > li:nth-child(3)') %>% 
  html_text() %>% 
  str_extract(pattern = '\\d+') %>% 
  as.numeric()

print(x = movieCnt)
## [1] 30161

# 페이지 수 계산
pages <- ceiling(x = movieCnt / 20)
print(pages)
## [1] 1509


# ------------------------------------------------------------
# part 1. 영화별 정보 수집
# ------------------------------------------------------------

# 최종 저장할 데이터프레임 생성
movies <- data.frame()

# 반복문 실행
for (i in 1:pages) {
  
  # 현재 진행상황 출력
  cat('현재', i, '번째 페이지 작업 중!\n')
  
  # HTTP 요청
  res <- GET(url = 'https://movie.naver.com',
             path = '/movie/sdb/browsing/bmovie.nhn',
             query = list('nation' = 'KR',
                          'page' = i))
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # 영화 목록이 담긴 html 요소만 items에 할당
    items <- res %>% 
      read_html(encoding = 'MS949') %>% 
      html_nodes(css = 'ul.directory_list > li')
    
    # 영화 제목 수집
    mName <- items %>% 
      html_node(css = 'a') %>% 
      html_text(trim = TRUE)
    
    # 영화 링크 수집
    mLink <- items %>% 
      html_node(css = 'a') %>% 
      html_attr(name = 'href')
    
    # 영화 코드 수집
    mCode <- mLink %>% str_extract(pattern = '\\d+$')
    
    # 영화 제작년도 수집
    mYear <- items %>% 
      html_node(css = 'ul.detail > li > a') %>% 
      html_text(trim = TRUE)
    
    # 데이터프레임으로 저장
    df <- data.frame(mName, mLink, mCode, mYear)
    movies <- rbind(movies, df)
  }, error = function(e) cat(' ---> 에러가 발생했습니다!\n')
  )
  
  # 1초 간 쉽니다.
  Sys.sleep(time = 1)
}

# 6줄 미리보기
head(x = movies)

# 중복 제거한 행의 수 확인
movies %>% unique() %>% nrow()

# mLink 컬럼에 도메인 추가
movies$mLink <- str_c('https://movie.naver.com', movies$mLink)

# save RDS
today <- Sys.Date() %>% format(format = '%Y%m%d')
fileName <- str_glue('korean_movie_list_{today}.RDS')
# saveRDS(object = movies, file = fileName)

# read RDS
# movies <- readRDS(file = fileName)


# ------------------------------------------------------------
# part 2. 영화별 배우 리스트 수집
# ------------------------------------------------------------

# 연도별 건수 확인
table(movies$mYear, useNA = 'ifany')

# 2010년 이후 영화 수
movies <- movies %>% filter(mYear >= 2010)

# 수집할 영화 개수
N <- movies %>% nrow()
print(x = N)

# 결과 객체 생성
actors <- data.frame()

# for-loop 순환 함수 실행
for (i in 1:N) {
  
  # 현재 진행상황 출력
  cat('현재', i, '번째 영화 작업 중!\n')
  
  # HTTP 요청
  res <- GET(url = 'https://movie.naver.com',
             path = '/movie/bi/mi/detail.nhn',
             query = list('code' = movies$mCode[i]))
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # 배우 정보를 포함하는 html 요소 추출
    items <- res %>% 
      read_html(encoding = 'UTF-8') %>% 
      html_nodes(css = 'ul.lst_people > li')
    
    # 배우 이름
    aName <- items %>% 
      html_node(css = 'p > a') %>% 
      html_attr(name = 'title')
    
    # 배우 코드
    aLink <- items %>% 
      html_node(css = 'p > a') %>% 
      html_attr(name = 'href')
    
    # 배우 코드
    aCode <- aLink %>% str_extract(pattern = '\\d+$')
    
    # 데이터 프레임으로 저장
    df <- data.frame(mCode = movies$mCode[i], 
                     aName = aName, 
                     aLink = aLink, 
                     aCode = aCode)
    
    # 행 기준으로 추가하기
    actors <- rbind(actors, df)
    
  }, error = function(e) cat(' ---> 배우 데이터가 없습니다!\n')
  )
  
  # 1초 간 멈춤
  Sys.sleep(time = 1)
}


# 6줄 미리보기
head(x = actors)

# save RDS
fileName <- str_glue('korean_movie_actor_{today}.RDS')
# saveRDS(object = actors, file = fileName)

# read RDS
# movies <- readRDS(file = fileName)


# ------------------------------------------------------------
# part 3. 영화별 평점 수집 (선택)
# ------------------------------------------------------------

# 결과 객체 생성
points <- data.frame()

# for-loop 순환 함수 실행
for (i in 1:N) {
  
  # 현재 진행상황 출력
  cat('현재', i, '번째 영화 작업 중!\n')
  
  # HTTP 요청
  res <- GET(url = movies$mLink[i])
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # 영화정보를 포함하는 html 요소 추출
    items <- res %>%
      read_html(encoding = 'UTF-8') %>% 
      html_nodes(css = 'div.mv_info_area > div.mv_info > div.main_score > div.score')
    
    # 관람객 평점
    mgspt <- items %>% 
      html_nodes(css = 'a.ntz_score > div > em') %>% 
      html_text(trim = TRUE) %>% 
      str_c(collapse = '')
    
    # 평론가 평점
    crtpt <- items %>% 
      html_nodes(css = 'a.spc > div > em') %>% 
      html_text(trim = TRUE) %>% 
      str_c(collapse = '')
    
    # 네티즌 평점
    ntzpt <- items %>% 
      html_nodes(css = 'a#pointNetizenPersentBasic > em') %>% 
      html_text(trim = TRUE) %>% 
      str_c(collapse = '')
    
    # 결과 저장
    df <- data.frame(mCode = movies$mCode[i], 
                     mgspt = mgspt, 
                     crtpt = crtpt, 
                     ntzpt = ntzpt)
    
    # 행 기준으로 추가하기
    points <- rbind(points, df)
    
  }, error = function(e) cat(' ---> 평점 데이터가 없습니다!\n')
  )
  
  # 1초 간 멈춤
  Sys.sleep(time = 1)
  
}


# 6줄 미리보기
head(x = points)

# save RDS
fileName <- str_glue('korean_movie_score_{today}.RDS')
# saveRDS(object = points, file = fileName)

# read RDS
# movies <- readRDS(file = fileName)


# ------------------------------------------------------------
# part 4. 영화별 리뷰 수집 (선택)
# ------------------------------------------------------------

# 전체 영화의 리뷰를 저장할 빈 데이터프레임 생성
reviews <- data.frame()

# for-loop 순환 함수 실행
for (i in 1:N) {
  
  # 현재 진행상황 출력
  cat('현재', i, '번째 영화 작업 중!\n')
  
  # HTTP 요청
  res <- GET(url = 'https://movie.naver.com',
             path = '/movie/bi/mi/review.nhn',
             query = list('code' = movies$mCode[i]))
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # 리뷰를 포함하는 html 요소 추출
    total <- res %>% 
      read_html() %>% 
      html_node(css = 'span.cnt > em') %>% 
      html_text() %>% 
      as.numeric()
    
    pages <- ceiling(x = total / 10)
    
    if (pages > 0) {
      
      # 영화별 리뷰를 저장할 빈 데이터프레임 생성
      review <- data.frame()
      
      # 반복문 실행
      for (j in 1:pages) {
        
        # 현재 진행상황 출력
        cat('>>', pages, '페이지 중', j, '번째 영화 작업 중!\n')
        
        # HTTP 요청
        res <- GET(url = 'https://movie.naver.com',
                   path = '/movie/bi/mi/review.nhn',
                   query = list('code' = movies$mCode[i],
                                'page' = j))
        
        # 리뷰를 포함하는 html 요소 추출
        items <- res %>% 
          read_html(encoding = 'UTF-8') %>% 
          html_nodes(css = 'ul.rvw_list_area > li')
        
        # 리뷰 제목
        title <- items %>% 
          html_node(css = 'a > strong') %>% 
          html_text(trim = TRUE)
        
        # 리뷰 쓴 User ID
        user <- items %>% 
          html_node(css = 'span.user > a') %>% 
          html_text(trim = TRUE)
        
        # 리뷰 등록일자
        date <- items %>% 
          html_node(css = 'span.user > em:nth-child(2)') %>% 
          html_text(trim = TRUE)
        
        # 리뷰 추천수
        like <- items %>% 
          html_node(css = 'span.user > em:nth-child(3)') %>% 
          html_text(trim = TRUE) %>% 
          str_remove_all(pattern = '추천|[ ,]') %>% 
          as.numeric()
        
        # 데이터 프레임으로 정리
        df <- data.frame(mCode = movies$mCode[i],
                         title = title, 
                         user = user, 
                         date = date, 
                         like = like)
        
        # review 객체에 추가하기
        review <- rbind(review, df)
        
        # 1초 간 멈춤
        Sys.sleep(time = 1)
      }
      
      # reviews 객체에 추가하기
      reviews <- rbind(reviews, review)
      
    }
  }, error = function(e) cat(' ---> 리뷰 데이터가 없습니다!\n')
  )
}


# 6줄 미리보기
head(x = reviews)

# save RDS
fileName <- str_glue('korean_movie_review_{today}.RDS')
# saveRDS(object = reviews, file = fileName)

# read RDS
# movies <- readRDS(file = fileName)


## End of Document
