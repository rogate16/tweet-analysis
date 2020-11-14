library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  sidebarMenu(
    menuItem("Home", tabName = "h", icon = icon("home")),
    menuItem("Data Analysis", tabName = "e", icon = icon("poll")),
    menuItem("Text Analysis", tabName = "t", icon = icon("book")),
    menuItem("Check Tweets", tabName = "c", icon = icon("search")),
    menuItem("Data", tabName = "d", icon = icon("folder"))
  )
)

body <- dashboardBody(
  custom_theme,
  tabItems(
    tabItem(
      "h",
      HTML("<h3><b>Analyzing Tweets about Omnibus Law</b></h3><br><br>"),
      valueBoxOutput("val1"), valueBoxOutput("val2"), valueBoxOutput("val3"),
      br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("plotly1"), br(),
      HTML("We can see a leap at 03/11/2020 between 10 p.m and 11 p.m. This indicates that the terms Omnibus Law
           was trending on Twitter started from these hours, and hence why the number of tweets grow significantly
           in the next hours")
    ),
    
    tabItem(
      "e",
      box(width = 12, title = HTML("<b>Top Users</b>"),
        dropdownButton(inputId = "set", right = T,
                     radioButtons("by", "Metrics", choiceValues = c("twt", "w_c", "rel"), 
                                  choiceNames = c("Tweets", "Word Count", "Relevance Score"), inline = T),
                     sliderInput("top_n", "Number of Users", 5, 30, 20),
                     circle = TRUE, status = "danger", icon = icon("gear")
        ),
        bsTooltip("set", "Plot Settings"),
        plotlyOutput("top_user")), br(), br(),
      fluidRow(
        box(
          radioButtons("var", "Variable", inline = T, choiceNames = c("Gender", "Polarity", "Relevance Score"),
                       choiceValues = c("gender", "polarity", "relevance")),
          plotlyOutput("data_dist"), 
          title = HTML("<b>Data Distribution</b>"), solidHeader = F, height = "545px"),
        box(
          prettySwitch("outlier", "Remove Outliers", F, "info", F, T),
          div(style="display: inline-block", uiOutput("ui")),
          div(style="display: inline-block", uiOutput("ui2")),
          plotlyOutput("foll_post"), 
          title = HTML("<b>Followers vs Posts</b>"), solidHeader = F, height = "545px"))
    ),
    
    tabItem(
      "t",
      box(HTML("<h2><b>Most Frequent Words</b></h2>"),width = "900px", background = "olive"),
      sliderInput("n_gram", "N-Gram Range", 1, 4, c(1,2)),
      plotlyOutput("word_count") %>% withSpinner(type = 8), br(), br(),
    ),
    
    tabItem(
      "c", 
      box(HTML("<h2><b>Check Tweet Status</b></h2>"), width = "950px", background = "light-blue"),
      textAreaInput("tweets", NULL, height = "200px", width = "920px", placeholder = "Type something..."),
      actionButton("check", "Check"), br(), br(),
      valueBoxOutput("status1"), valueBoxOutput("status2"), valueBoxOutput("status3")
    ),
    
    tabItem(
      "d", DTOutput("dt")
    )
    
  )
)

dashboardPage(header, sidebar, body)