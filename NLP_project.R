#Load libraries
library(readr) #Read the datasets
library(tm) #Text mining
library(textstem) #Lemmatization
library(stopwords) #Words to remove
library(syuzhet) #Sentiment analysis
library(ggplot2) #Make the plots
library(scales) #Make percent in the pie charts
library(wordcloud) #Make the wordclouds

#Load data
source_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(source_dir)
Republican = read_csv("Republican.csv")
Democratic = read_csv("Democratic.csv")

##Load corpus
#Republican
corpus_rep = VectorSource(Republican$Text)
corpus_rep = Corpus(corpus_rep)

#Democratic
corpus_dem = VectorSource(Democratic$Text)
corpus_dem = Corpus(corpus_dem)


##### CLEANING #####

#Change contractions to whitespace
corpus_rep = tm_map(corpus_rep, gsub, pattern="'", replacement=" ")
corpus_rep = tm_map(corpus_rep, gsub, pattern="'", replacement=" ")

#Change to lowercase
corpus_rep = tm_map(corpus_rep,tolower)
corpus_dem = tm_map(corpus_dem,tolower)

#Remove numbers
corpus_rep = tm_map(corpus_rep,removeNumbers)
corpus_dem = tm_map(corpus_dem,removeNumbers)

#Remove punctuation symbols
corpus_rep = tm_map(corpus_rep,removePunctuation)
corpus_dem = tm_map(corpus_dem,removePunctuation)

#Lemmatization
corpus_rep = tm_map(corpus_rep, lemmatize_strings)
corpus_dem = tm_map(corpus_dem, lemmatize_strings)

#Remove stopwords (SMART) and some other especific words for this domain
extra_words = c("make", "year", "don", "didn", "hes", "ive", "million")
mystopwords = c(stopwords(source="smart"),extra_words)
corpus_rep = tm_map(corpus_rep,removeWords,mystopwords)
corpus_dem = tm_map(corpus_dem,removeWords,mystopwords)

#Remove extra whitespace
corpus_rep = tm_map(corpus_rep,stripWhitespace)
corpus_dem = tm_map(corpus_dem,stripWhitespace)


##### ANALYSIS 1: MORE USED WORDS #####

#Term Document Matrix
tdm_rep = TermDocumentMatrix(corpus_rep)
tdm_dem = TermDocumentMatrix(corpus_dem)

#### WORDS FREQUENCY

#Frequency of each word
freq_rep = rowSums(as.matrix(tdm_rep))
freq_dem = rowSums(as.matrix(tdm_dem))

#Top 10 of the most frequent words
MF_rep = tail(sort(freq_rep), n=10)
MF_dem = tail(sort(freq_dem), n=10)

##Wordcloud
#Republican
pal=brewer.pal(8,"Reds")
word.cloud=wordcloud(words=names(freq_rep), freq=freq_rep,
                     min.freq=50, random.order=F, colors=pal)

#Democratic
pale=brewer.pal(8,"Blues")
word.cloud=wordcloud(words=names(freq_dem), freq=freq_dem,
                     min.freq=30, random.order=F, colors=pale)

#### WORDS USED IN MORE SPEECHES, TOP 10

#Number of speeches in which each word appears
ndoc_rep = rowSums(as.matrix(tdm_rep) != 0)
ndoc_dem = rowSums(as.matrix(tdm_dem) != 0)

#Top 10 of the most used words
MU_rep = tail(sort(ndoc_rep), n=10)
MU_dem = tail(sort(ndoc_dem), n=10)

###Barplot
##Create data frame to make the barplot
#Republican
MU_df_rep = as.data.frame(MU_rep)
MU_df_rep$words = rownames(MU_df_rep) 

#Democratic
MU_df_dem = as.data.frame(MU_dem)
MU_df_dem$words = rownames(MU_df_dem) 

##Plot
#Republican
ggplot(MU_df_rep, aes(reorder(words, MU_rep), MU_rep)) +
  geom_bar(stat="identity",fill='red', width=0.5) + coord_flip() + 
  xlab("Words") + ylab("Number of speeches") +
  ggtitle("Most Used Words Republican")

#Democratic
ggplot(MU_df_dem, aes(reorder(words, MU_dem), MU_dem)) +
  geom_bar(stat="identity",fill='blue', width=0.5) + coord_flip() + 
  xlab("Words") + ylab("Number of speeches") +
  ggtitle("Most Used Words Democratic")


##### ANALYSIS 2: SENTIMENTS ANALYSIS #####

#Get sentiments
Sent_rep = get_nrc_sentiment(corpus_rep$content)
Sent_dem = get_nrc_sentiment(corpus_dem$content)

####Positive / Negative Speeches

##Number of speeches positive and negative
#Republican
pos_neg_rep = Sent_rep$positive-Sent_rep$negative
pos_rep = sum(pos_neg_rep > 0)
neg_rep = sum(pos_neg_rep < 0)

#Democratic
pos_neg_dem = Sent_dem$positive-Sent_dem$negative
pos_dem = sum(pos_neg_dem > 0)
neg_dem = sum(pos_neg_dem < 0)

###Pie chart
##Create dataframe to make the pie chart 
#Republican
Sentiment = c('Positive','Negative')
value_rep = c(pos_rep,neg_rep)
Pie_rep = data.frame(Sentiment,value_rep)

#Democratic
Sentiment = c('Positive','Negative')
value_dem = c(pos_dem,neg_dem)
Pie_dem = data.frame(Sentiment,value_dem)

##Plot the pie chart
#Republican
ggplot(Pie_rep, aes(x="", y=value_rep, fill=Sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("Negative" = "red", "Positive" = '#8B0000')) +
  theme_void() +
  ggtitle("Positive / Negative  Republican")+
  geom_text(aes(y = value_rep/2 + c(0, cumsum(value_rep)[-length(value_rep)]),
                label = percent(value_rep/30) ), size=6, color='White')

#Democratic
ggplot(Pie_dem, aes(x="", y=value_dem, fill=Sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("Negative" = "blue", "Positive" = '#00008B')) +
  theme_void() +
  ggtitle("Positive / Negative  Democratic")+
  geom_text(aes(y = value_dem/2 + c(0, cumsum(value_dem)[-length(value_dem)]),
                label = percent(value_dem/21, accuracy =0.1) ), size=5, color='White')

####Sentiments Analysis 
####Anger, anticipation, disgust, fear, joy, sadness, surprise and trust

###Some transformations
#1º transpose
tSent_rep = data.frame(t(Sent_rep))
tSent_dem = data.frame(t(Sent_dem))

#2º computes column sums across rows for each sentiment
tSent_rep_total = data.frame(rowSums(tSent_rep))
tSent_dem_total = data.frame(rowSums(tSent_dem))

##3º Rename the column and create a new one from the index
#Republican
names(tSent_rep_total)[1] = "count"
tSent_rep_total = cbind("sentiment" = rownames(tSent_rep_total), tSent_rep_total)
rownames(tSent_rep_total) = NULL

#Democratic
names(tSent_dem_total)[1] = "count"
tSent_dem_total = cbind("sentiment" = rownames(tSent_dem_total), tSent_dem_total)
rownames(tSent_dem_total) = NULL

###Barplot of count of words associated with each sentiment
#Take the selected sentiments, discard positive and negative
tSent_rep_plot = tSent_rep_total[1:8,]
tSent_dem_plot = tSent_dem_total[1:8,]

##Plot
#Republican
quickplot(sentiment, data=tSent_rep_plot, weight=count, geom="bar", fill=sentiment, ylab="count") +
  ggtitle("Sentiments Analysis Republican")

#Democratic
quickplot(sentiment, data=tSent_dem_plot, weight=count, geom="bar", fill=sentiment, ylab="count") +
  ggtitle("Sentiments Analysis Democratic")