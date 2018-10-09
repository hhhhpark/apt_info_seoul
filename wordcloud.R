
install.packages("KoNLP")
install.packages("wordcloud2")

library(KoNLP)
library(wordcloud2)
useSejongDic()

#leadlines함수로 한줄씩 읽어옴(배열로 된 벡터 생성)
word<-readLines("14month_info.txt") 

#각 배열(문장)에서 명사를 추출(단어)하여 리스트 형태로 저장
word_noun<-sapply(word,extractNoun,USE.NAMES = FALSE)


#unlist함수를 이용하여 list에 저장된 명사(단어) 다시 벡터로 변환
word_noun_unlist<-unlist(word_noun) 
word_noun_unlist2<-word_noun_unlist #복사본 생성

#table형태로 변환하여 각 단어별 사용 빈도수 확인
word_table<-table(word_noun_unlist)

#gsub함수를 이용하여 필요없는 단어 삭제 (삭제와 저장이 같아야함)
word_noun_unlist2<-gsub("\\d+","",word_noun_unlist2)
word_noun_unlist2<-gsub("개월","",word_noun_unlist2)
word_noun_unlist2<-gsub("14","",word_noun_unlist2)
word_noun_unlist2<-gsub("아기","",word_noun_unlist2)
word_noun_unlist2<-gsub("blog","",word_noun_unlist2)
word_noun_unlist2<-gsub("기","",word_noun_unlist2)
word_noun_unlist2<-gsub("장","",word_noun_unlist2)
word_noun_unlist2<-gsub("더보","",word_noun_unlist2)
word_noun_unlist2<-gsub("이미지","",word_noun_unlist2)
word_noun_unlist2<-gsub("naver","",word_noun_unlist2)
word_noun_unlist2<-gsub("2018","",word_noun_unlist2)
word_noun_unlist2<-gsub("14개월아기","",word_noun_unlist2)
word_noun_unlist2<-gsub("유아","",word_noun_unlist2)
word_noun_unlist2<-gsub("아이","",word_noun_unlist2)
word_noun_unlist2<-gsub("com","",word_noun_unlist2)
word_noun_unlist2<-gsub("식","",word_noun_unlist2)
word_noun_unlist2<-gsub("04.","",word_noun_unlist2)
word_noun_unlist2<-gsub("한","",word_noun_unlist2)
word_noun_unlist2<-gsub("2","",word_noun_unlist2)
word_noun_unlist2<-gsub("내","",word_noun_unlist2)
word_noun_unlist2<-gsub("블로그","",word_noun_unlist2)
word_noun_unlist2<-gsub("2017.","",word_noun_unlist2)
word_noun_unlist2<-gsub("05.","",word_noun_unlist2)
word_noun_unlist2<-gsub("03.","",word_noun_unlist2)
word_noun_unlist2<-gsub("12.","",word_noun_unlist2)
word_noun_unlist2<-gsub("개.","",word_noun_unlist2)
word_noun_unlist2<-gsub("들","",word_noun_unlist2)
word_noun_unlist2<-gsub("개","",word_noun_unlist2)


#단어 글자수가 1개 이상인것 필터링
word_final<-Filter(function(x){nchar(x)>1},word_noun_unlist2)

#최종 정리된 파일 다시 table형태로 반환(최종확인)
word_table2<-table(word_final)

#빈도수 10회 미만인것 지울것~~★

#wordcloud2 함수를 이용하여 최종 결과물 시각화
wc<-wordcloud2(data=word_table2,size=1.5)
wc

