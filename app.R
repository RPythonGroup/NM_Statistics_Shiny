#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(httr)
library(rjson)
library(plyr)
library(dplyr)
library(ggplot2)

# UI 구성
ui <- fluidPage(
  titlePanel("Medical Data Chart by Medical Code"),
  sidebarLayout(
    sidebarPanel(
      textInput("st5Cd", "Enter Medical Code (e.g., HK010):", value = "HK010"),
      actionButton("generate", "Generate Chart")
    ),
    mainPanel(
      plotOutput("chart")
    )
  )
)

# Server 로직 구성
server <- function(input, output, session) {
  # 데이터를 저장할 reactiveValues 객체 생성
  rv <- reactiveValues(data = NULL)
  
  observeEvent(input$generate, {
    my_st5Cd <- input$st5Cd
    my_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    base_url <- "http://apis.data.go.kr/B551182/mdlrtActionInfoService"
    call_url <- "getMdlrtActionByClassesStats"
    My_API_Key <- "DRwFWZ/jfi6551teJKgRhkKgC+XhcjobN+ccY2MQdF9yZdx8xL5Kn4IljljMCWGsl2ObEco/rm21r14CN7iG0g=="
    
    data_frame <- data.frame()
    xcondition <- TRUE  
    
    while (xcondition) {
      # API 요청 매개변수 설정
      params <- list(
        serviceKey = URLencode(My_API_Key),
        pageNo = 1,
        numOfRows = 10,
        resultType = "json",
        year = as.character(my_year),
        stdType = "1",
        st5Cd = my_st5Cd
      )
      
      # API 호출
      url <- paste0(base_url, "/", call_url)
      response <- GET(url, query = params)
      
      if (status_code(response) == 200) {
        json_text <- content(response, as = "text", encoding = "UTF-8")
        data <- fromJSON(json_text)
        
        if (!is.null(data$response$body$totalCount)) {
          if (data$response$body$totalCount == 6) {
            data_list <- data$response$body$items$item
            df <- rbind.fill(lapply(data_list, as.data.frame))
            data_frame <- rbind(data_frame, df)
          }
          
          if (data$response$body$totalCount == 5) {
            xcondition <- FALSE
          }
        } else {
          xcondition <- FALSE
        }
      } else {
        xcondition <- FALSE
      }
      
      # 연도를 1 감소
      my_year <- my_year - 1
    }
    
    # 컬럼명 변경
    if (nrow(data_frame) > 0) {
      rv$data <- setNames(data_frame, c("진료금액", "요양기관종별", "환자수", "행위코드", "행위코드명", "총사용량", "연도"))
    } else {
      rv$data <- NULL
    }
  })
  
  output$chart <- renderPlot({
    if (!is.null(rv$data)) {
      ggplot(data = rv$data, aes(x = 연도, y = 총사용량, color = 요양기관종별)) +
        geom_line(size = 1) + 
        geom_point(size = 2) + 
        labs(
          title = paste(input$st5Cd, "연도별 요양기관종별 총사용량"),
          x = "연도",
          y = "총사용량"
        ) +
        theme_minimal() +
        theme(legend.title = element_blank())
    }
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
