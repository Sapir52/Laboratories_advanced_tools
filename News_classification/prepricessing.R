#preprocessing
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(tm)
attach(mtcars)
#------------------------------
# Get data
get_data <- function(name_csv){
  data <- read.csv(name_csv)
  return(data)
}
get_corpus <- function(data_text){
  return(Corpus(VectorSource(data_text)))
}

# Preprocessing into one column
preprocessing <- function(marge_data,data_col){
# Remove row if cell is empty
  marge_data[marge_data==""]<-NA
  marge_data<-marge_data[complete.cases(marge_data),]
  # Remove chars
  data_col<-gsub("’","",as.character(data_col))
  data_col<-gsub("‘","",as.character(data_col))
  data_col<-gsub("”","",as.character(data_col))
  data_col<-gsub("“","",as.character(data_col))
  # Convert dataframe to corpus
  corpus <-get_corpus(data_col)
  # Remove numbers 
  corpus <- tm_map(corpus, removeNumbers) 
  #Remove punctuation
  corpus <- tm_map(corpus, removePunctuation) 
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  #Lowercase
  corpus <- tm_map(corpus,content_transformer(tolower))
  # Remove Stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Stemming
  corpus <- tm_map(corpus,stemDocument)
  return(corpus) #type list
}
# Convert data to dataframe
convert_to_dataframe <- function(data,corpus){
  for(i in 1:length(data)) {
    data[[i]] <-as.character(corpus[[i]])
  }
  return(data)
}

# Preprocessing for all cols
run_preprocessing<- function(marge_data,data_col){
  corpus <-preprocessing(marge_data, data_col)
  return(convert_to_dataframe(data_col, corpus))
}

# Create new col that contine title and text
marge_title_and_text <- function(marge_data){
  return(paste(marge_data$title,marge_data$text))
}

# Write to csv
write_to_csv <- function(df,name_file){
  write.csv(df,name_file, row.names = FALSE)
}

main <-function(){
  # Get news data
  data_fake <-get_data("Data/Fake.csv")
  data_true<-get_data("Data/True.csv")
  # Marge 2 dataframe
  marge_data <- rbind(data_fake,data_true)
  # Run preprocessing
  marge_data$title<- run_preprocessing(marge_data, marge_data$title)
  marge_data$text<- run_preprocessing(marge_data, marge_data$text)
  #  Merge title and text columns
  marge_data$title_text <- marge_title_and_text(marge_data)
  # Sort marge_data by title
  newdata <- marge_data[order(as.character(marge_data$title)),]
  #  Write data to a file
  write_to_csv(newdata,"Preprocessing.csv")
}