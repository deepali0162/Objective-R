# Convert the text to lower case
x1 <- tm_map(x, tolower)
inspect(x1[1])
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)

# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
x1 <- tm_map(x1, removeWords) 
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])
# Text stemming
x1<-lemmatize_words(x1)
#x1 <- tm_map(x1, stemDocument)

# Term document matrix 
# converting unstructured data to structured format using TDM
dtm <- DocumentTermMatrix(x1)
dtm <- as.matrix(dtm)
dtm[1:2,1:2]
#tdm.tfidf <- tm::weightTfIdf(tdm)
#tfidf.matrix <- as.matrix(tdm.tfidf) 
###Cluster algorithm building
km <- kmeans(dtm,2) 
km$cluster
clust<-data.frame("cluster"=km$cluster,dtm)