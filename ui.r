library(shiny)
library(shinydashboard)
library(ggmap)
library(leaflet)

info<-read.csv("seoul_apt_info.csv")
map_info<-read.csv("seoul_apt_location_info.csv")

ui <- dashboardPage(
      skin="black",
      dashboardHeader(title = "Seoul APT Info"),
      dashboardSidebar(
        textInput("search_txt",label = "APT name is"),
        selectInput("select1","region",choices = unique(info$구))
    ),
    dashboardBody(
      tabsetPanel(
        tabPanel("51-60m2",
           box(width = 500,title = "서울시 평균매매가 분포(2017/01~2018/05)",status = "warning", 
               solidHeader=TRUE,plotOutput("plot")),
               fluidRow(
                 box("30년이상APT 매매가 평균(2017/01~2018/5)",status = "success",plotOutput("over30_1")),
                 box("10년미만APT 최근 매매가 평균(2017/01~2018/05)",status = "success",plotOutput("under10_1"))
                   ),
               fluidRow(
                 box("건축연한별 APT매매분포(서울시전체2017/01~2018/05)",status = "danger",plotOutput("percent")),
                 box("건축연한에따른 구별 APT매매건수(2017/01~2018/05)",status = "danger",plotOutput("gu_percent")),
                 box("건축연한별 최근 거래건수",status = "danger",plotOutput("cnt_deal")),
                 box("건축연한별 최근 거래금액",status = "danger",plotOutput("avg_deal"))   
               )),
        tabPanel("81-90m2",   
           box(width = 500,title = "서울시 평균매매가 분포(2017/01~2018/05)",status = "warning", 
             solidHeader=TRUE, plotOutput("plot2")),
                 fluidRow(
                   box("30년이상APT 매매가 평균(2017/01~2018/05)",status = "success",plotOutput("over30_2")),
                   box("10년미만APT 매매가 평균(2017/01~2018/05)",status = "success",plotOutput("under10_2"))
                 ),
               fluidRow(
                 box("건축연한별 APT매매분포(서울시전체,2017/01~2018/05)",status = "danger",plotOutput("percent_2")),
                 box("건축연한에따른 구별 APT매매건수(2017/01~2018/05)",status = "danger",plotOutput("gu_percent_2")),
                 box("건축연한별 최근 거래건수",status = "danger",plotOutput("cnt_deal_2")),
                 box("건축연한별 최근 거래금액",status = "danger",plotOutput("avg_deal_2"))
               )),
        tabPanel("Search_APT",   
             h4("원하는 아파트명을 입력(APT Name is)하시면 해당 아파트 매매현황을 보여드립니다"),
             p("Because a choose prompt is present, the selectize version should let
        you clear the selection."), 
             fluidRow(
                valueBoxOutput("age"),
                valueBoxOutput("address"),
                valueBoxOutput("count")
              ),
              box(
                status = "warning",title = "매매현황",width=500,
                plotOutput("avg_apt")
             ),
              box(
               status = "warning", title = "상세 거래내역",width=500,
               dataTableOutput("table1")
             )),
        tabPanel("Map",   
                 h5("사이드패널에 검색조건을 입력하시면 관련 정보를 찾아드립니다"),
                 leafletOutput("map",height = 600)
            ),
        
        tabPanel("wordcloud",   
                 h5("sample keyword: 14개월")
        )
        
        )
    )
)




