#1. Use the "pdftools" package to read the five PDF files in R:

library(pdftools)

# Set the working directory to the folder where the PDF files are stored
setwd("/Users/arpan/Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Data/MDS503P1")

# Read the five PDF files and store them in a list
pdf_files <- list.files(pattern = "*.pdf")
pdf_text <- lapply(pdf_files, pdf_text)

pdf_files

#2. Create a "corpus" with the "tm" package:

library(tm)

# Create a corpus from the pdf_text list
corpus <- Corpus(VectorSource(unlist(pdf_text)))

str(corpus)

# Inspect the corpus to ensure it has been created correctly
#inspect(corpus)
inspect(corpus[1:1])


#3. Perform pre-processing of the corpus:

# Convert all text to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove numbers, punctuation, and whitespace
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#removing unwanted html links and "\n" new line in the Corpus
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http[^[:space:]]*","", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\n*","", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("–","", x)))
inspect(corpus) #inspect the results in corpus

str(corpus)

# Inspect the corpus to ensure pre-processing has been done correctly
inspect(corpus[1:1])


#4. Get term-document matrix to show the most frequent terms:

# Create the term-document matrix
tdm <- TermDocumentMatrix(corpus, control = list(wordLength=c(1,Inf)))

# Get the most frequent terms cosidering lower frequency = 20
freq_terms <- findFreqTerms(tdm, lowfreq = 30)

# Inspect the most frequent terms
freq_terms

#Converting the TDM to an matrix form
m <- as.matrix(tdm)
freq_Count <- sort(rowSums(m),decreasing = T)  #counting the term frequency 
head(freq_Count,20)   #first 20  frequency counts


#5. Create a word cloud with and without color:
library(wordcloud)
par(mar = c(2, 3, 2, 3)) #(bottom, left, top, right margins)

#Without color
wordcloud(words = names(freq_Count), freq = freq_Count, min.freq = 5, random.order = FALSE)


#With color
wordcloud(words = names(freq_Count), freq = freq_Count, min.freq = 5, 
          random.order = FALSE, colors = rainbow(length(freq_Count)))


#6. Create a network graph:

library(graph)
library(Rgraphviz)
plot(tdm, term = freq_terms, corThreshold = 0.5)
  
#7. Topic modeling 
library(tm)
library(topicmodels)
set.seed(07)

dtm <- as.DocumentTermMatrix(t(tdm), weighting = weightTf)
lda_Model <- LDA(dtm, k=4)


#getting the terms in the topic model
terms(lda_Model, 3)

  
     