
function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  cands1 <- reactive({
    input$selection1
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    par(mar = rep(0, 4))
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Spectral"))
  })
  
  output$selection2 <- renderUI({
    cands <- input$selection1
    if (length(cands) == 0 || length(cands) == 2){
      if(length(cands) == 0){
        cands <- c("JoeBiden", "BernieSanders")
      }
      cand1 <- getCandidateTopics(cands[1])
      cand2 <- getCandidateTopics(cands[2])
      names(cand1)[1] <- "Topic"
      names(cand2)[1] <- "Topic"
      topics <- inner_join(cand1, cand2, by = "Sub.Topic",
                           suffix=c(sprintf(".%s", cands[1]), sprintf(".%s", cands[2])))
      topics <- subset(topics, select = c(sprintf("Topic.%s", cands[1]), 
                                          "Sub.Topic", 
                                          sprintf("Response.%s", cands[1]), 
                                          sprintf("Response.%s", cands[2])))
      colnames(topics)[colnames(topics)==sprintf("Topic.%s", cands[1])] <- "Topic"
      vals <- topics$Topic
    } else {
      cand1 <- getCandidateTopics(cands[1])
      names(cand1)[1] <- "Topic"
      vals <- cand1$Topic
    }
    
    selectizeInput("selection5", "Select Topics",
                                   choices = vals, multiple = TRUE,
                                   options = list(placeholder = 'Select Topic(s)'))
  })
  
  output$table <- DT::renderDataTable({
    cands <- input$selection1 
    if(length(cands) == 2 || length(cands) == 0){
      if(length(cands) == 0){
        cands <- c("JoeBiden", "BernieSanders")
      }
      cand1 <- getCandidateTopics(cands[1])
      cand2 <- getCandidateTopics(cands[2])
      names(cand1)[1] <- "Topic"
      names(cand2)[1] <- "Topic"
      topics <- inner_join(cand1, cand2, by = "Sub.Topic", 
                           suffix=c(sprintf(".%s", cands[1]), sprintf(".%s", cands[2])))
      topics <- subset(topics, select = c(sprintf("Topic.%s", cands[1]), 
                                          "Sub.Topic", 
                                          sprintf("Response.%s", cands[1]), 
                                          sprintf("Response.%s", cands[2])))
      topics[,3] <- tolower(topics[,3])
      topics[,4] <- tolower(topics[,4])
      colnames(topics)[colnames(topics)==sprintf("Topic.%s", cands[1])] <- "Topic"
      
      if(input$selection3 == "All"){
        topics = topics
      } else if(input$selection3 == "Agreements"){
        for(row in nrow(topics):1){
          if(topics[row,3] != topics[row,4]){
            topics <- topics[-row,]
          }
        }
      } else{
        for(row in nrow(topics):1){
          if(topics[row,3] == topics[row,4]){
            topics <- topics[-row,]
          }
        }
      }
    }else{
      cand1 <- getCandidateTopics(cands[1])
      topics <- cand1
      colnames(topics)[1] <- "Topic"
    }
    #based on input number of levels, could be all
    topics <- getNumLevels(input$selection5,topics)
    DT::datatable(data = topics, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$stream <- renderStreamgraph({
    cats <- getStreamCategories()
    dates <- input$streamgraph
    cats <- cats[cats$year >= dates[1] & cats$year<=dates[2],]
    streamgraph(cats, "Issue", "value", "year", 
                      height="500px", width="950px") %>%
      sg_axis_y(0) %>%
      sg_axis_x(1, "date", "%b-%d") %>%
      sg_fill_manual(c("#833471", "#C4E538", "#EE5A24", "#12CBC4", "#0652DD", "#FDA7DF", "#009432", "#EA2027","#9980FA", "#FFC312")) %>%
      sg_legend(show=TRUE, label="Issues: ") %>%
      sg_colors(axis_color = "black") %>%
      sg_add_marker(x = "2019-06-26", label = "Debate")  %>%
      sg_add_marker(x = "2019-07-30", label = "Debate")  %>%
      sg_add_marker(x = "2019-09-12", label = "Debate") %>%
      sg_add_marker(x = "2019-10-15", label = "Debate") %>%
      sg_add_marker(x = "2019-11-20", label = "Debate")  %>%
      sg_add_marker(x = "2019-12-19", label = "Debate") %>%
      sg_add_marker(x = "2020-01-14", label = "Debate") %>%
      sg_add_marker(x = "2020-02-07") %>%
      sg_add_marker(x = "2020-02-19") %>%
      sg_add_marker(x = "2020-02-25") %>%
      sg_add_marker(x = "2020-03-15", label = "Debate")
    
  })
  output$LDAPlot <- renderPlot({
    df_top_terms <- read.csv("Top10Terms_All.csv", header = T, sep = ',')
    
    df_top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered()
  }, height = 550)
  
  output$LDADataset <- DT::renderDataTable({
    ldaTopics <- read.csv("ClassifiedTweets.csv", header=T, sep=',')
    nums <- input$selection4
    
    if(length(nums) > 0){
      for(val in nrow(ldaTopics):1){
        if(!(ldaTopics$topic[val] %in% nums)){
          ldaTopics <- ldaTopics[-val,]
        }
      }
    }
    
    DT::datatable(data = ldaTopics, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
}





