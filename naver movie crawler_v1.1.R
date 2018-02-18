
# 네이버 영화 사이트에서 데이터 수집하기
# 웹사이트 url : 'http://movie.naver.com/movie/sdb/browsing/bmovie_nation.nhn'

# 웹크롤링 순서
# 0. 한국영화 개수 확인 
# 1. 영화별 정보 수집
# 2. 영화별 배우 리스트 수집 (주연, 조연)
# 3. 영화별 평점 수집 (선택)
# 4. 영화별 리뷰 수집 (선택)


# 필수 패키지 불러오기 ----
library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(magrittr)
library(rlist)


# part 0. 한국영화 개수 확인 ----

# url 지정
main <- 'http://movie.naver.com/'
sub1 <- 'movie/sdb/browsing/bmovie_nation.nhn'
url <- paste0(main, sub1)
cat(url)

# html 요청
resp <- GET(url)

# 응답 결과 확인
print(resp)

# 한국영화 개수
krMovieCnt <- resp %>% 
  read_html(encoding = 'MS949') %>% 
  html_nodes(css = 'dl:nth-child(17) dd ul li:nth-child(3)') %>% 
  html_text() %>% 
  str_extract(pattern = '\\d+') %>% 
  as.numeric()

print(krMovieCnt)
## [1] 27119




# part 1. 영화별 정보 수집 ----

# 리스트를 데이터 프레임으로 전환하는 함수 생성
# FUN 인자값 맨 뒤에 unname()을 추가하면, 마지막 줄을 실행시킬 필요 없음! 
list2df <- function(listObj) {
  newDf <- sapply(X = listObj %>% names() %>% unique(),
                  FUN = function(x) listObj[names(listObj) == x] %>% unlist(),
                  simplify = TRUE) %>% 
    as.data.frame() %>% 
    `rownames<-`(c())
  
  return(newDf)
}

# url 요소 지정
sub2 <- 'movie/sdb/browsing/bmovie.nhn?nation=KR'
url <- paste0(main, sub2, paste0('&page=', 1)); cat(url)

# 페이지 수 계산
pages <- ceiling(krMovieCnt / 20) + 1
print(pages)
## [1] 1357

# 결과 객체 생성
# 리스트 형태로 만든 후 데이터 프레임으로 전환 예정
resultList <- list()

# for-loop 순환 함수 실행
for (i in 1:pages) {
  
  # 현재 진행상황 출력
  cat(i, '/', pages, ': ')
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # url 조립
    url <- paste0(main, sub2, paste0('&page=', i))
    
    # html 요청
    resp <- GET(url)
    
    # 응답 상태코드 출력 (200이면 정상!)
    cat('응답 상태코드', status_code(x = resp), '!\n')
    
    # 영화 목록이 담긴 html만 items에 할당
    items <- resp %>% 
      read_html(encoding = 'MS949') %>% 
      html_node(css = 'ul.directory_list')
    
    # 자식 노드의 수
    n <- xml_length(x = items)
    
    for (j in 1:n) {
      
      # j번째 자식 노드 지정
      item <- items %>% xml_child(search = j)
      
      # 기본 정보 수집 : 영화제목, 영화링크, 영화코드
      title <- item %>% xml_child(search = 1) %>% html_text()
      mlink <- item %>% xml_child(search = 1) %>% html_attr(name = 'href')
      mcode <- mlink %>% str_extract(pattern = '(?<=code\\=)\\d+')
      
      # 세부 정보 html 지정
      info <- item %>% xml_child(search = 2) %>% html_nodes(css = 'a.green')
      
      # 세부 정보 수집 : 제작년도, 개봉년도, 장르, 형식, 등급
      myear <- info[str_detect(string = info, pattern = 'year')] %>% html_text()
      oyear <- info[str_detect(string = info, pattern = 'open=\\d{4}\"')] %>% html_text()
      genre <- info[str_detect(string = info, pattern = 'genre')] %>% html_text()
      formt <- info[str_detect(string = info, pattern = 'form')] %>% html_text()
      grade <- info[str_detect(string = info, pattern = 'grade')] %>% html_text()
      
      # 결과 객체(리스트)에 요소로 추가
      resultList <- list.append(
        resultList, 
        title = title,
        mlink = mlink,
        mcode = mcode,
        myear = ifelse(length(myear) == 0, NA, myear),
        oyear = ifelse(length(oyear) == 0, NA, oyear),
        genre = ifelse(length(genre) == 0, NA, genre),
        formt = ifelse(length(formt) == 0, NA, formt),
        grade = ifelse(length(grade) == 0, NA, grade)
      )
    }
  }, error = function(e) {
    cat('An error occurs at line no', i)
    }
  )
  
  # 3초에 한 번씩 html 요청하도록 설정
  Sys.sleep(time = 3)
}


# 리스트를 데이터 프레임으로 변환
movieList <- list2df(resultList)

# 6줄 미리보기
head(movieList)

# 중복 제거한 행의 수 확인
nrow(unique(movieList))
# movieList[duplicated(movieList), ]
# movieList[movieList$mcode == '93072', ]

# save RDS
# 실수 방지를 위해 스크립트를 주석으로 처리함!
# saveRDS(object = movieList, file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_list_20180217.RDS')

# read RDS
movieList <- readRDS(file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_list_20180217.RDS')




# part 2. 영화별 배우 리스트 수집 ----

# 전체 영화 개수
N <- nrow(movieList)

# 결과 객체 생성
actorList <- data.frame()

# for-loop 순환 함수 실행
for (i in 1:N) {
  
  # 현재 진행상황 출력
  cat(i, '/', N, ': ')
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # url 지정
    url <- paste0(main, movieList$mlink[i])
    
    # 배우/제작진 탭으로 이동
    url <- str_replace(string = url, pattern = 'basic', replacement = 'detail')
    
    # html 요청
    resp <- GET(url = url)
    
    # 응답 상태코드 출력 (200이면 정상!)
    cat('응답 상태코드', status_code(x = resp), '!\n')
    
    # 배우 리스트 html 추출
    actors <- resp %>% 
      read_html(encoding = 'UTF-8') %>% 
      html_node(css = 'ul.lst_people') %>% 
      html_nodes(css = 'div.p_info a.k_name')
    
    # 배우 정보 : 이름, 링크, 코드
    anames <- actors %>% html_text()
    alinks <- actors %>% html_attr(name = 'href')
    acodes <- alinks %>% str_extract(pattern = '(?<=\\=)\\d+')
    
    # 영화 코드 (url에서 추출)
    mcode <- url %>% str_extract(pattern = '(?<=\\=)\\d+')
    
    # 데이터 프레임으로 저장
    df <- data.frame(mcode = mcode, 
                     aname = anames, 
                     alink = alinks, 
                     acode = acodes)
    
    # 행 기준으로 추가하기
    actorList <- rbind(actorList, df)
    
  }, error = function(e) {
    cat('> An error occurs at line no', i, ".\n\n")
    errors <- list.append(errors, movieList$mcode[i])
    }
  )
  
  # 3초에 한 번씩 html 요청하도록 설정
  Sys.sleep(time = 3)
}


# 6줄 미리보기
head(actorList)

# 중복 제거한 행의 수 확인
nrow(unique(actorList))
# actorList[duplicated(actorList), ]
# actorList[actorList$mcode == '89785', ]

actorList <- unique(actorList)

# save RDS
# 실수 방지를 위해 스크립트를 주석으로 처리함!
# saveRDS(object = actorList, file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_actor_20180217.RDS')

# read RDS
actorList <- readRDS(file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_actor_20180217.RDS')



# part 3. 영화별 평점 수집 (선택) ----

# 평점 추출 함수
getScore <- function(node, style) {
  score <- node %>% 
    html_node(css = style) %>% 
    html_text() %>% 
    str_extract(pattern = '\\d{1,2}\\.\\d{1,2}')
  
  return(score)
}

# 샘플 url 지정
main <- 'http://movie.naver.com/'
url <- paste0(main, movieList$mlink[3357]); cat(url)

# 전체 영화 개수
N <- nrow(movieList)

# 결과 객체 생성
resultScore <- list()
errors <- c()

# for-loop 순환 함수 실행
for (i in 10123:N) {
  
  # 현재 진행상황 출력
  cat(i, '/', N, ': ')
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # url 지정
    url <- paste0(main, movieList$mlink[i])
    
    # html 요청
    resp <- GET(url = url)
    
    # 응답 상태코드 출력 (200이면 정상!)
    cat('응답 상태코드', status_code(x = resp), '!\n')
    
    # 영화정보 html 추출
    scores <- resp %>%
      read_html(encoding = 'UTF-8') %>% 
      html_node(css = 'div.mv_info_area div.mv_info div.main_score')
    
    # 관람객 평점
    mgspt <- getScore(scores, 'a.ntz_score div.star_score')
    
    # 평론가 평점
    crtpt <- getScore(scores, 'a.spc div.star_score')
    
    # 네티즌 평점
    ntzpt <- getScore(scores, 'a#pointNetizenPersentBasic')
    
    
    # 결과 저장
    resultScore <- list.append(
      resultScore,
      mcode = movieList$mcode[i],
      mgspt = mgspt,
      crtpt = crtpt,
      ntzpt = ntzpt
    )
    
  }, error = function(e) {
    cat('> An error occurs at line no', i, ".\n\n")
    errors <- list.append(errors, movieList$mcode[i])
  })
  
  # 3초에 한 번씩 html 요청하도록 설정
  Sys.sleep(time = 3)
  
}


# 리스트를 데이터 프레임으로 변환
movieScore <- list2df(resultScore)

# 6줄 미리보기
head(movieScore)

# 중복 제거한 행의 수 확인
nrow(unique(movieScore))

# save RDS
# 실수 방지를 위해 스크립트를 주석으로 처리함!
# saveRDS(object = movieScore, file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_score_20180217.RDS')

# read RDS
movieScore <- readRDS(file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_score_20180217.RDS')




# part 4. 영화별 리뷰 수집 (선택) ----

# 전체 영화 개수
N <- nrow(movieList)

# 결과 객체 생성
reviewList <- data.frame()
errors <- list()

# for-loop 순환 함수 실행
for (i in 1:N) {
  
  # 현재 진행상황 출력
  cat(i, '/', N, ': ')
  
  # 에러 발생에도 계속 수행하도록 설정
  tryCatch({
    
    # url 지정
    url <- paste0(main, movieList$mlink[i])
    
    # 리뷰 탭으로 이동
    url <- str_replace(string = url, pattern = 'basic', replacement = 'point')
    
    # html 요청
    resp <- GET(url = url)
    
    # 응답 상태코드 출력 (200이면 정상!)
    cat('응답 상태코드', status_code(x = resp), '!\n')
    
    # 네티즌 평점 html 추출
    scores <- resp %>% 
      read_html(encoding = 'UTF-8') %>% 
      html_node(css = 'div.score_ntz div#beforePointArea')
    
    # 네티즌 평점 별점 표시 (%)
    starScore <- scores %>% 
      html_node(css = 'div.star_score span.st_on') %>% 
      html_attr(name = 'style') %>% 
      str_extract(pattern = '\\d{1,2}\\.\\d{1,2}')
    
    # 네티증 평점
    ntzScore <- scores %>% 
      html_nodes(css = 'div.star_score em') %>% 
      html_text() %>% 
      paste(collapse = '') %>% 
      as.numeric()
    
    # 리뷰 페이지 수 얻기
    rvCount <- scores %>% 
      html_node(css = 'span.user_count em') %>% 
      html_text() %>% 
      str_extract(pattern = '\\d+') %>% 
      as.numeric()
    
    rvPages <- ceiling(rvCount / 10)
    
    
    # 리뷰 html 추출
    rvUrl <- resp %>% 
      read_html(encoding = 'UTF-8') %>% 
      html_node(css = 'div.ifr_module2 iframe') %>% 
      html_attr(name = 'src')
    
    # 순환 실행
    for (j in 1:rvPages) {
      
      # url 조립
      url <- paste0(main, rvUrl, '&page=', j)
      
      # html 요청
      resp <- GET(url = url)
      
      # 응답 상태코드 출력 (200이면 정상!)
      # cat('응답 상태코드', status_code(x = resp), '!\n')
      
      # 리뷰 html 추출
      reviews <- resp %>% 
        read_html(encoding = 'UTF-8') %>% 
        html_node(css = 'div.score_result')
      
      # 별점
      points <- reviews %>% 
        html_nodes(css = 'div.star_score em') %>% 
        html_text() %>% 
        as.numeric()
      
      # 댓글
      reples <- reviews %>% 
        html_nodes(css = 'div.score_reple p') %>% 
        html_text() %>% 
        str_trim()
      
      # user id
      users <- reviews %>% 
        html_nodes(css = 'div.score_reple a[target="_top"] span') %>% 
        html_text()
      
      # 등록일시
      dates <- reviews %>% 
        html_nodes(css = 'div.score_reple em:nth-child(2)') %>% 
        html_text() %>% 
        str_extract(pattern = '\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}')
      
      # 공감 수
      symCnt <- reviews %>% 
        html_nodes(css = 'div.btn_area strong:nth-child(2) span') %>% 
        html_text() %>% 
        as.numeric()
      
      # 비공감 수
      nonCnt <- reviews %>% 
        html_nodes(css = 'div.btn_area strong:nth-child(4) span') %>% 
        html_text() %>% 
        as.numeric()
      
      # 데이터 프레임으로 정리
      df <- data.frame(
        mcode = movieList$mcode[i],
        scoreAvg = ntzScore,
        reviewCnt = rvCount,
        point = points,
        reple = reples,
        user = users,
        date = dates,
        symCnt = symCnt,
        nonCnt = nonCnt
      )
      
      # 행 기준 
      reviewList <- rbind(reviewList, df)
    }
    
  }, error = function(e) {
    cat('> An error occurs at line no', i, ".\n\n")
    errors <- list.append(errors, movieList$mcode[i])
  })
  
  # 3초에 한 번씩 html 요청하도록 설정
  Sys.sleep(time = 3)
}


# 6줄 미리보기
head(reviewList)

# 중복 제거한 행의 수 확인
nrow(unique(reviewList))
# reviewList[duplicated(reviewList), ]
# reviewList[reviewList$mcode == '39513', ]

reviewList <- unique(reviewList)

# save RDS
# 실수 방지를 위해 스크립트를 주석으로 처리함!
# saveRDS(object = reviewList, file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_review_20180217.RDS')

# read RDS
reviewList <- readRDS(file = '../GitHub/MrKevinNa/SixDegreesOfKoreanActors/data/korean_movie_review_20180217.RDS')
