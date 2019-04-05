library(twitteR)
library(ROAuth)
require(RCurl)

api_key = "x46q4lDVSlQxU0zgdH2CbNRLR"
api_secret<-"CInQ4UoElDLMn0yiYWybrbqg3xVTOdreetMn9243J8QxlkDrEs"
access_token<-"174116669-3lIfvG6rbIGWgGEP5j7NEuUYwMsZVvLPoemiQxuo"
access_token_secret<-"N1dvKKJsdrfJI6zWWeEVa1ANiKbGoxMPEx8FTH34zgt69"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
Trudeau<-searchTwitter("Trudeau", n=15000)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(wordcloud)
df1 <- twListToDF(Trudeau)
myCorpus <- Corpus(VectorSource(df1$text))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- Corpus(VectorSource(myCorpus))

wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"and", "when", "what", "to", "this","the","that","so","of","it","is","in","at","a","be","by","for","have","on","our","are","i","will","with","you")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
library(wordcloud)
wordcloud(myCorpus ,max.words =150,min.freq=3,scale=c(4,.5),colors=palette())
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm
(freq.terms <- findFreqTerms(tdm, lowfreq = 50))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat="identity") +xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))
df <- twListToDF(Trudeau)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste("Trudeau", '_stack.csv'))==FALSE) write.csv(df, file=paste("bitcoin", '_stack.csv'), row.names=F) 
stack <- read.csv(file=paste("Trudeau", '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste("Trudeau", '_stack.csv'), row.names=F)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('C:\\Users\\prade\\OneDrive\\Documents\\R Project\\positive-words.txt', what='character', comment.char=';')
Neg <- scan('C:\\Users\\prade\\OneDrive\\Documents\\R Project\\negative-words.txt', what='character', comment.char=';')

Dataset <- stack
Dataset$text <- as.factor(Dataset$text)
Dataset$text<- str_replace_all(Dataset$text,"í ½í²¸í ½í²°' "," ")
scores <- score.sentiment(Dataset$text, pos, Neg, .progress='text')


write.csv(scores, file=paste("Trudeau", '_scores.csv'), row.names=TRUE)
stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))


by.tweet <- group_by(stat, tweet, created)


by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste("Trudeau", '_opin.csv'), row.names=TRUE)


ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  
  ggtitle(Trudeau)
