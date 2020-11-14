server <- function(input, output){
  
  output$val1 <- renderValueBox({
    
    valueBox(nrow(omnibus), "tweets", color = "blue", icon = icon("twitter"))
    
  })
  
  output$val2 <- renderValueBox({
    
    valueBox(length(unique(omnibus$screen_name)), "distinct users", color = "blue", icon = icon("address-card"))
    
  })
  
  output$val3 <- renderValueBox({

    valueBox("23", "hours of tweets", color = "blue", icon = icon("clock"))
    
  })
  
  output$plotly1 <- renderPlotly({
    
    omnibus %>% 
      mutate(date = substr(post_date, 1, 10)) %>% 
      mutate(date = ymd(date)) %>% 
      mutate(hour = hour(ymd_hms(post_date))) %>% 
      group_by(date, hour) %>% 
      summarise(length(text_cleaned)) -> ts
    ts$date_time <- ymd_h(paste(ts$date, ts$hour))
    colnames(ts)[3] <- "tweets"
    
    p1 <- ggplot(ts, aes(date_time, tweets, text=paste("Post Time :",ts$date_time,"<br>Total Tweets :", ts$tweets),
                        group = 1)) + 
      geom_line(color="skyblue", size = 1) + theme_minimal() + chart_theme + 
      labs(title = "Tweets per Hour", x = "Post Time", y = "Number of Tweets")
    
    ggplotly(p1, tooltip="text")
    
  })
  
  output$top_user <- renderPlotly({
    
    if(input$by=="twt"){
      
      top.user <- as.data.frame(table(omnibus$screen_name))
      top.user <- top.user[order(top.user$Freq, decreasing = T),]
      top.user <- head(top.user,input$top_n)
      
      p2 <- ggplot(top.user, aes(reorder(Var1, Freq), Freq, text = paste("@",Var1,"<br>",Freq," Tweets", sep = ""))) + 
        geom_col(fill="skyblue") + theme_minimal() + coord_flip() + chart_theme + 
        labs(title = paste(input$top_n, "Users with Most Tweets"), x = "Username", y = "Total Tweets")
      
      ggplotly(p2, tooltip="text")
      
    } else if(input$by=="w_c"){
      
      top.user <- omnibus %>% group_by(screen_name) %>% summarise(words = sum(tweet_length))
      top.user <- top.user[order(top.user$words, decreasing = T),]
      top.user <- head(top.user,input$top_n)
      
      p2 <- ggplot(top.user, aes(reorder(screen_name, words), words, text = paste("@",screen_name,"<br>",words," Words", sep = ""))) + 
        geom_col(fill="skyblue") + theme_minimal() + coord_flip() + chart_theme + 
        labs(title = paste(input$top_n, "Users with Highest Word Count"), x = "Username", y = "Total Words")
      
      ggplotly(p2, tooltip="text")
    } else {
      
      top.user <- omnibus %>% group_by(screen_name) %>% summarise(total_relevance = sum(relevance_score))
      top.user <- top.user[order(top.user$total_relevance, decreasing = T),]
      top.user <- head(top.user, input$top_n)
      
      p2 <- ggplot(top.user, aes(reorder(screen_name, total_relevance), total_relevance, text = paste("@",screen_name,"<br>",total_relevance," Relevance Score", sep = ""))) + 
        geom_col(fill="skyblue") + theme_minimal() + coord_flip() + chart_theme + 
        labs(title = paste(input$top_n, "Users with Highest Relevance Score"), x = "Username", y = "Relevance Score")
      
      ggplotly(p2, tooltip="text")
      
    }
    
  })
  
  output$data_dist <- renderPlotly({
    
    var.choice <- input$var
    
    if(var.choice=="relevance"){
      p3 <- ggplot(omnibus, aes(relevance_score)) + geom_density(fill = "skyblue") + 
        theme_minimal() + chart_theme + 
        labs(title = "Relevance Distribution", x = "Relevance Score", y = "Density")
      ggplotly(p3, tooltip = "x")
    } else {
      data.dist <- as.data.frame(table(omnibus[var.choice]))
      data.dist <- data.dist[data.dist$Var1!="",]
      data.dist <- data.dist[order(data.dist$Freq, decreasing = T),]
      data.dist$Prop <- round(data.dist$Freq/sum(data.dist$Freq),2)*100
      
      p3 <- ggplot(head(data.dist,5), aes(Var1, Freq, text=paste(stringr::str_to_title(Var1), " : ", Freq, " Tweets<br>(", 
                                                                 Prop, "%)", sep=""))) + 
        geom_col(fill="skyblue") + theme_minimal() + chart_theme + 
        labs(title = paste(stringr::str_to_title(var.choice),"Distribution"), 
             x = stringr::str_to_title(var.choice), y = "Total Tweets")
      ggplotly(p3, tooltip="text")
    }
    
  })
  
  output$ui <- renderUI({
    
    if(input$outlier==T){
      
      numericInput("num", NULL, 5, 1, 50, width = "100px")
      
    }
    
  })
  
  output$ui2 <- renderUI({
    
    if(input$outlier==T){
      
      actionButton("apply", "Apply", width = "150px")
      
    }
    
  })
  
  remove.outliers <- eventReactive(input$apply, {
    
    omnibus %>% 
      group_by(screen_name) %>% 
      summarise(posts = length(text_cleaned), followers = max(user_followers_count)) -> foll.post
    
    foll.post <- foll.post[order(foll.post$followers),]
    foll.post <- head(foll.post, -input$num)
    p4 <- ggplot(foll.post, aes(followers, posts, 
                                 text = paste("@",screen_name,"<br>Followers : ", followers, 
                                              "<br>Posts : ",posts, sep = ""))) +
      geom_point(color="skyblue") + theme_minimal() + scale_x_continuous(labels = scales::comma) + 
      chart_theme + labs(title = "Followers vs Posts", x = "Total Followers", y = "Total Posts")
    
    ggplotly(p4, tooltip="text")
    
  })
  
  output$foll_post <- renderPlotly({
    
    omnibus %>% 
      group_by(screen_name) %>% 
      summarise(posts = length(text_cleaned), followers = max(user_followers_count)) -> foll.post
    
    foll.post <- foll.post[order(foll.post$followers),]
    
    if(input$outlier==F){
      
      p4 <- ggplot(foll.post, aes(followers, posts, 
                                  text = paste("@",screen_name,"<br>Followers : ", followers, 
                                               "<br>Posts : ",posts, sep = ""))) +
        geom_point(color="skyblue") + theme_minimal() + scale_x_continuous(labels = scales::comma) + 
        chart_theme + labs(title = "Followers vs Posts", x = "Total Followers", y = "Total Posts")
      
      ggplotly(p4, tooltip="text")
      
    } else {
      
        remove.outliers()
      
    }
    
  })
  
  output$word_count <- renderPlotly({
    
    Tokenizer <- function(x){
      unlist(lapply(ngrams(words(x), input$n_gram[1]:input$n_gram[2]), paste, collapse = " "), use.names = FALSE)
    }
    
    dtm <- as.matrix(DocumentTermMatrix(VCorpus(VectorSource(omnibus$text_cleaned)), control = list(tokenize = Tokenizer)))
    
    phrases <- data.frame(Terms = colnames(dtm), Freq = colSums(dtm), row.names = NULL)
    phrases <- phrases[order(phrases$Freq, decreasing = T),]
    
    p5 <- ggplot(head(phrases, 20), aes(reorder(Terms, Freq), Freq, text = Freq)) + 
      geom_col(fill="skyblue") + theme_minimal() + coord_flip() + chart_theme + 
      labs(title = paste("Top 20 Most Frequent Phrases (",input$n_gram[1],"-",input$n_gram[2]," words )", sep = ""), 
           x = "Terms", y = "Frequency")
    
    ggplotly(p5, tooltip = "y")
    
  })
  
  check.words <- eventReactive(input$check, {
    
    new_length <- lengths(strsplit(input$tweets, " "))
    valueBox(new_length, "Words", icon("book"), "light-blue")
    
  })
  
  output$status1 <- renderValueBox({
    
    check.words()
    
  })
  
  tweet <- reactiveValues(text = c())
  
  check.polarity <- eventReactive(input$check, {
    
    if(input$check==0){
      # pass
    } else {
      new_tweet <- data.frame(text = c("placeholder",input$tweets))
      new_tweet <- VCorpus(VectorSource(new_tweet$text))
      new_tweet <- tm_map(new_tweet, content_transformer(tolower))
      new_tweet <- tm_map(new_tweet, removeNumbers)
      new_tweet <- tm_map(new_tweet, removePunctuation)
      new_tweet <- tm_map(new_tweet, removeWords, stopwords.id$V1)
      
      tweet$text <- new_tweet[[2]]$content
      
      new_dtm <- DocumentTermMatrix(new_tweet)
      new_matrix <- apply(as.matrix(new_dtm), MARGIN=2, toBinary)
      score <- predict(model, new_matrix, type="raw")

      if(score[2,2]>score[2,1]){
        valueBox("Positive", "Tweet", color = "green", icon = icon("twitter"))
      } else {
        valueBox("Negative", "Tweet", color = "red", icon = icon("twitter"))
      }
    }
  })
  
  output$status2 <- renderValueBox({
    
    check.polarity()
    
  })
  
  check.relevance <- eventReactive(input$check, {
    
    new_relevance <- relevance(tweet$text)
    valueBox(new_relevance, "Relevance Score", icon("star"), "blue")
    
  })
  
  output$status3 <- renderValueBox({
    
    check.relevance()
    
  })
  
  output$dt <- renderDataTable({
    
    omnibus$post_date <- lubridate::ymd_hms(omnibus$post_date)
    datatable(omnibus,
              colnames = c("Username", "Gender", "Followers", "Post Date", "Tweet", "Cleaned Tweet", 
                           "Polarity", "Word Count", "Relevance Score"),
              options = list(scrollY=T, autoWidth=T, scrollX=T, lengthChange=F), filter = "top")
    
  })
  
}