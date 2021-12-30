library(tm)
library(wordcloud)
library(memoise)
library(devtools)
library(shinythemes)
library(caret)
library(dplyr)
library(pracma)
library(devtools)
library(streamgraph)
library(htmlwidgets)
library(reshape)
library(tidytext)



candidate <<- list("Joe Biden" = "JoeBiden",
               "Bernie Sanders" = "BernieSanders", 
               "Amy Klobuchar" = "AmyKlobuchar", 
               "Andrew Yang" = "AndrewYang", 
               "Elizabeth Warren" = "ElizabethWarren", 
               "Mike Bloomberg" = "MikeBloomberg", 
               "Pete Buttigieg" = "PeteButtigieg")

candidate2 <<- list("Joe Biden" = "JoeBiden",
                    "Bernie Sanders" = "BernieSanders")

cLevel <<- list("Criminal Justice", "Cybersecurity", "Economy",
                "Education", "Elections", 
                "Energy, Environment & Climate Change", 
                "Food & Agriculture", "Gun Control", 
                "Health Care", "Immigration", 
                "Infrastructure", 
                "Marijuana & Cannabis Legalization", 
                "Military", "Taxes", "Technology",
                "Trade")

topicsNumbers <<- list("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4, "Five" = 5, 
                       "Six" = 6, "Seven" = 7, "Eight" = 8, "Nine" = 9, "Ten" = 10)
sg_add_marker <- function(sg, x, label="", stroke_width=1, stroke="#7f7f7f", space=5,
                          y=0, color="#7f7f7f", size=12, anchor="start") {
  
  if (inherits(x, "Date")) { x <- format(x, "%Y-%m-%d") }
  
  mark <- data.frame(x=x, y=y, label=label, color=color, stroke_width=stroke_width, stroke=stroke,
                     space=space, size=size, anchor=anchor, stringsAsFactors=FALSE)
  
  if (is.null(sg$x$markers)) {
    sg$x$markers <- mark
  } else {
    sg$x$markers <- bind_rows(mark, sg$x$markers)
  }
  
  sg
}

getTermMatrix <- memoise(function(candidate) {
  if (!(candidate %in% candidate))
    stop("Unknown candidate")
  
  text <- read.csv(sprintf("%s.csv", candidate), header=T, sep=',')
  require(tm)
  gsub("`|\\'", "", iconv(text$text, to="ASCII//TRANSLIT")) 
  iconv(text$text, to="UTF-8")
  myCorpus = Corpus(DataframeSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

getCandidateTopics <- memoise(function(candidate){
  if (!(candidate %in% candidate))
    stop("Unknown candidate")
  cand <- read.csv(sprintf("%s_ViewsOnIssues_Politico.csv", candidate), header=T, sep=',', stringsAsFactors = FALSE)
})

getNumLevels <- memoise(function(inSelect,topics){
  #check to see if var is in this join as a level
  if(length(inSelect) > 0){
    trial <- topics[which(topics$Topic %in% inSelect),]
  }else{
    trial <- topics
  }
  trial
})

getStreamCategories <- memoise(function(){
  cats <- read.csv("CatsByDate.csv", header=T, sep=',', stringsAsFactors = FALSE)
})



