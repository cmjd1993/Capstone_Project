library(shiny)
library(tm)
library(stringr)
library(datasets)

# Load data 

load("Uni_Data.RData")
load("Bi_Data.RData")
load("Tri_Data.RData")
load("Quad_Data.RData")

# This function is used to clean the corpus before inputting it into the algorithm
Clean <- function(String)
{
  library(tm)
  # clean input
  Words <- iconv(String, "latin1", "ASCII", sub="")
  Words <-  gsub("[^[:alpha:][:space:][:punct:]]", "", Words);
  
  Corpus <- Corpus(VectorSource(Words))
  # removes numbers
  Corpus <- tm_map(Corpus, removeNumbers)
  # transforms all text to lower case
  Corpus <- tm_map(Corpus, content_transformer(tolower)) 
  # remove punctuation
  Corpus <- tm_map(Corpus, removePunctuation)
  # strips white space
  Corpus <- tm_map(Corpus, stripWhitespace)
  
  clean <- as.character(Corpus[[1]])
  
  return(clean)
}

# Word Prediction Algorithm using Back-off
WordPrediction <- function(String)
{
  library(tm)
  library(stringr)
  
  # clean word using CleanWord
  cleanWord <- Clean(String)
  
  # split word out 
  word_split <- unlist(strsplit(cleanWord, split=" "))
  
  # get word length
  length_split <- length(word_split)
  
  # condition to assist with back-off
  WordFound <- FALSE
  
  NextWord <- as.character(NULL)
  
  # search for word in quadgram usng if statement where if words are found it return next word, else
  # it will move to next ngram
  
  if(length_split >= 3 & !WordFound)
  {
    # take last 3 words of string to search
    term <- paste(word_split[(length_split-2):length_split], collapse=" ")
    
    # search for words
    termSearch <- paste("^", term, sep = "")
    Quad <- Quad_Data[grep(termSearch, Quad_Data$Words),]
    
  
    if (length(Quad[,1]) > 1)
    {
      # split the strings found and return last word in string as next word prediciton
      Quad_split <- data.frame(str_split_fixed(Quad$Words, " ", 4))
      NextWord <- Quad_split[1,4]
      WordFound <- TRUE
      # apply a message to advise what ngram was used to predict string
      message <- "predicted using 4-Gram"
    }
    Quad <- NULL
  }
  
  # search for word in trigram usng if statement where if words are found it return next word, else
  # it will move to next ngram
  
  if(length_split >= 2 & !WordFound)
  {
    # take last 2 words of string to search
    term <- paste(word_split[(length_split-1):length_split], collapse=" ")
    
    # search for words
    termSearch <- paste("^", term, " ", sep = "")
    Tri <- Tri_Data[grep(termSearch, Tri_Data$Words),]
    
    if (length(Tri[,1]) > 1)
    {
      # split the strings found and return last word in string as next word prediciton
      Tri_split <- data.frame(str_split_fixed(Tri$Words, " ", 3))
      NextWord <- Tri_split[1,3]
      WordFound <- TRUE
      # apply a message to advise what ngram was used to predict string
      message <- "predicted using 3-Gram"
    }
    Tri <- NULL
  }
  
  # search for word in bigram usng if statement where if words are found it return next word, else
  # it will move to next ngram
  
  if(length_split >= 1 & !WordFound)
  {
    # take last word of string to search
    term <- paste(word_split[(length_split):length_split], collapse=" ")
    
    # search for word
    termSearch <- paste("^", term, sep = "")
    Bi <- Bi_Data[grep(termSearch, Bi_Data$Words),]
    
    if (length(Bi[,1]) > 1)
    {
      # split the strings found and return last word in string as next word prediciton
      Bi_split <- data.frame(str_split_fixed(Bi$Words, " ", 2))
      NextWord <- Bi_split[1,2]
      WordFound <- TRUE
      # apply a message to advise what ngram was used to predict string
      message <- "predicted using 2-Gram"
    }
    Bi <- NULL
  }
  
  # if word is not found in previous ngrams, we will use unigram to return the most frequent word
  
  if (!WordFound & length_split > 0)
  {
    # no need to split here as there is only one word
    NextWord <- Uni_Data$Word[1:3]
    # apply a message to advise what ngram was used to predict string
    message <- "unable to find next word. The output returns the most frequent words as the next word"
  }
  
  Next <- word(NextWord, -1)
  
  if(length_split > 0)
  {
    data <- data.frame(NextWord, message)
    return(data)
  } else {
    NextWord <- "NA"
    message <- "NA"
    
    data <- data.frame(NextWord, message)
    
    return(data)
  }
  
}


# Define server logic to return Word Prediction Algorithm output, I decided to
# use text output so the app is neat and doesn't look to code heavy as that can 
# deter users from using the app

shinyServer(function(input, output) {
   
  output$text1 <- renderText({
    string <- Clean(input$String)
    paste("'", WordPrediction(string)[1,1], "'")
  })
  
  output$text2 <- renderText({
    paste("Input Sentence: ", "'", input$String, "'")})
  
  output$text3 <- renderText({
    string <- Clean(input$String)
    string_data <- WordPrediction(string)
     paste("This output was", as.character(string_data[1,2]))
   
  })
}
)