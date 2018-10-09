library(ggplot2)
library(ggmap)
#View(데이터프레임): 엑셀 없이 데이터 쉽게 확인 가능

info<-read.csv("●서울APT실거래가_최종_05월.csv")
map_info<-read.csv("●서울APT위치정보_최종_2월.csv")

#1.전용면적 분리
type1<-info[which(info$전용면적구분=="50이상~60이하"),]
type2<-info[which(info$전용면적구분=="80이상~90이하"),]

#2.건출연한(30년이상or10년미만)별 평균 매매금액 산출함수:avg_area(전용면적,건축연한)
#최근거래건수로 보면 편차 너무 큼(중구: 1년간 30년이상 apt 30평대 3개거래)
avg_area<-function(type,age){
  a<-info[which(info$전용면적구분==type&info$건축년도구분==age),]
  a1<-as.data.frame(tapply(a$거래금액.만원.,a$구,mean))
  a2<-as.data.frame(tapply(a$개수,a$구,sum))
  a1<-cbind(rownames(a1),a1,a2)
  colnames(a1)<-c("구","거래금액.만원.","매매건수")
  return(a1)
}


#2.건출연한(30년이상or10년미만)에따른 함수호출
a1<-avg_area(type = "50이상~60이하",age = "30년이상")
a2<-avg_area(type = "50이상~60이하",age = "10년미만")
a3<-avg_area(type = "80이상~90이하",age = "30년이상")
a4<-avg_area(type = "80이상~90이하",age = "10년미만")


#3.건축 연한에따른 월 매매 추이 함수 
monthly_contract<-function(type){
  cnt_deal1<-aggregate(type$개수,list(as.factor(type$계약년월),type$건축년도구분),sum)
  colnames(cnt_deal1)<-c("계약년월","건축년도구분","월합계")
  avg_deal1<-aggregate(type$거래금액.만원.,list(as.factor(type$계약년월),type$건축년도구분),mean)
  colnames(avg_deal1)<-c("계약년월","건축년도구분","거래금액평균")
  return(cbind(cnt_deal1,avg_deal1))
}

#3.건축 연한에따른 월 매매 추이 함수호출 
b1<-monthly_contract(type = type1)
b2<-monthly_contract(type = type2)

#4. 관련 데이터 시각화(그래프, 지도)
server <- function(input, output,session) {
  output$plot<-renderPlot({
    ggplot(type1,aes(구,거래금액.만원.))+geom_point(aes(color=구))+ylim(0,200000)
  })
  output$plot2<-renderPlot({
    ggplot(type2,aes(구,거래금액.만원.))+geom_point(aes(color=구))+ylim(0,250000)
  })
  
  output$over30_1<-renderPlot({
    ggplot(a1,aes(구,거래금액.만원.))+geom_bar(stat = 'identity',aes(fill=구))+coord_flip()+geom_text(aes(label=round(거래금액.만원.)),color="black",hjust=1)+ylim(0,150000)
    #+geom_vline(aes(xintercept=mean(over30_1),color="red"),size=2)
  })
  output$under10_1<-renderPlot({
    ggplot(a2,aes(구,거래금액.만원.))+geom_bar(stat = 'identity',aes(fill=구))+coord_flip()+geom_text(aes(label=round(거래금액.만원.)),color="black",hjust=1)+ylim(0,150000)
  })
  
  output$over30_2<-renderPlot({
    ggplot(a3,aes(구,거래금액.만원.))+geom_bar(stat = 'identity',aes(fill=구))+coord_flip()+geom_text(aes(label=round(거래금액.만원.)),color="black",hjust=1)+ylim(0,150000)
  })
  output$under10_2<-renderPlot({
    ggplot(a4,aes(구,거래금액.만원.))+geom_bar(stat = 'identity',aes(fill=구))+coord_flip()+geom_text(aes(label=round(거래금액.만원.)),color="black",hjust=1)+ylim(0,150000)
  })
  
  output$percent<-renderPlot({
    ggplot(b1, aes(x="", y=월합계,fill=건축년도구분)) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y")
  })
  
  output$gu_percent<-renderPlot({
        ggplot(type1, aes(x=구, y=개수,fill=건축년도구분)) + geom_bar(stat="identity")+coord_flip()
    #+geom_text(aes(label=개수),color="black",hjust=1) 
  })

  output$percent_2<-renderPlot({
    ggplot(b2, aes(x="", y=월합계,fill=건축년도구분)) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y")
  })
  
  output$gu_percent_2<-renderPlot({
    ggplot(type2, aes(x=구, y=개수,fill=건축년도구분)) + geom_bar(stat="identity")+coord_flip()
    #+geom_text(aes(label=개수),color="black",hjust=1)
  })
  
  output$cnt_deal<-renderPlot({
    ggplot(b1,aes(as.numeric(계약년월),월합계,color=건축년도구분))+geom_line()+scale_x_discrete(name="계약년월",limits=b1$계약년월)+geom_point()+geom_text(aes(label=월합계),size=3.2,color="black",vjust=1)})
  
  output$avg_deal<-renderPlot({
    ggplot(b1,aes(as.numeric(계약년월),거래금액평균,color=건축년도구분))+geom_line()+scale_x_discrete(name="계약년월",limits=b1$계약년월)+geom_point()+geom_text(aes(label=round(거래금액평균)),size=3,color="black",vjust=1)
  })

  output$cnt_deal_2<-renderPlot({
    ggplot(b2,aes(as.numeric(계약년월),월합계,color=건축년도구분))+geom_line()+scale_x_discrete(name="계약년월",limits=b2$계약년월)+geom_point()+geom_text(aes(label=월합계),size=3.2,color="black",vjust=1)})
  
  output$avg_deal_2<-renderPlot({
    ggplot(b2,aes(as.numeric(계약년월),거래금액평균,color=건축년도구분))+geom_line()+scale_x_discrete(name="계약년월",limits=b2$계약년월)+geom_point()+geom_text(aes(label=round(거래금액평균)),size=3,color="black",vjust=1)
  })
  
  observe({
  #getReactiveEnvironment()$currentContext() : UI로부터 입력값을 받아와서 처리해야하는경우 observe({})안에서 사용해야 함
  #You tried to do something that can only be done from inside a reactive expression or observer  

    #1. 아파트명 입력받아와서 검색 (향후 버튼 눌렀을때 검색 하도록 개선 필요)
    a<-subset(info,as.character(info$단지명)==input$search_txt)
    value1<-paste(unique(a$구),unique(a$동),unique(a$단지명))
    a2<-as.data.frame(cbind(a$계약년월,a$층,a$실전용면적,a$거래금액.만원.))
    colnames(a2)<-c("계약년월","층수","실전용면적","거래금액(만원)")
      
    output$age<-renderValueBox({
      valueBox(
        value = as.numeric(unique(a$건축년도)),
        subtitle = paste(value1,"(건축년도)"),
        icon=icon("calendar-alt"),
        color="green"
      )
    })
    
    output$address<-renderValueBox({
      valueBox(
        value = round(mean(a$거래금액.만원.)),
        subtitle = paste("평균 매매금액(만원)"),
        icon=icon("chart-bar"),
        color="orange"
      )
    })
    
    output$count<-renderValueBox({
      valueBox(
        value = sum(a$개수),
        subtitle = "계약건수(2017/01~2018/05)",
        icon=icon("users"),
        color="maroon"
      )
    })
    
    output$avg_apt<-renderPlot({
      ggplot(a,aes(as.character(계약년월),round(거래금액.만원.),color=전용면적구분))+geom_line()+geom_point(size=3)
    })
    
    output$table1<-renderDataTable(a2)
    
    
    #2. 자치구별 매매데이터 지도 표시(마커색깔 구분기준: 건축연한)
    map_gu<-subset(map_info,map_info$gu==input$select1)
    
    get_col<-function(map_gu){
      sapply(map_gu$sep_years,function(sep_years){
        if(sep_years=="30년이상"){
          "green"}
        else if(sep_years=="20~30년"){
          "orange"}
        else if(sep_years=="10~20년"){
          "purple"}
        else if(sep_years=="10년이하")
          "red"
      })
    }
    
    icons<-awesomeIcons(markerColor = get_col(map_gu))
    
    #지도위에 색깔별 범례표시하기 & popup 상세정보 표시하기
    map_gu<-subset(map_info,map_info$gu==input$select1)
    output$map<-renderLeaflet({
      leaflet(data=map_gu)%>%addTiles()%>%
        addAwesomeMarkers(~long,~lat,icon = icons,label = ~as.character(name),
                          popup = paste("아파트명:",map_gu$name,"<br>",
                                        "거래건수:",map_gu$count,"<br>",
                                        "건축년도:",map_gu$years,"<br>",
                                        "건축년도 구분:",map_gu$sep_years))%>%
          addLegend(title="건축년도구분",
                    position = 'bottomright',
                    colors = c("green","orange","purple","red"), 
                    labels = c("30년이상", "20~30년", "10~20년","10년이하"))
      })
    

  })
  
}

