#install.packages('wordcloud')

pos <- scan('positive-words.txt',what='character',comment.char=';')
neg <- scan('negative-words.txt',what='character',comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


#scores = score.sentiment(df.new$FullText, pos, neg, .progress='text')

scores = score.sentiment(df.texts.clean[,1], pos, neg, .progress='text')
scores$very.pos = as.numeric(scores$score >= 1)
scores$very.neg = as.numeric(scores$score <= -1)
scores$Month = df.texts.clean$Month
scores$BankID = df.texts.clean$BankID

scores$text = lapply(scores$text, as.character)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# create a df with only negative sentences

#Use the lines below to create dataframes from the different mediatypes
#df.negative.twitter = scores[(scores$very.neg == 1 & scores$mediatype == "twitter"),c(2,5)]
#df.negative.facebook = scores[(scores$very.neg == 1 & scores$mediatype == "facebook"),c(2,5)]
df.negative = scores[scores$very.neg == 1, c(2,5,6)]
df.positive = scores[scores$very.pos == 1, c(2,5,6)]
df.negative$text = lapply(df.negative$text, as.character)
df.positive$text = lapply(df.positive$text, as.character)

#----------------------------------------------------------------------------------------
#The variables below will have the number of positive and negative posts with their respective relative percentages
#BANKA NUMBERS
bankA.positive.counter = sum(scores[scores$BankID == "BankA", 3])
bankA.negative.counter = sum(scores[scores$BankID == "BankA", 4])
bankA.positive.percentage = (bankA.positive.counter*100) / (bankA.positive.counter + bankA.negative.counter)
bankA.negative.percentage = (bankA.negative.counter*100) / (bankA.positive.counter + bankA.negative.counter)

#BANKB NUMBERS
bankB.positive.counter = sum(scores[scores$BankID == "BankB", 3])
bankB.negative.counter = sum(scores[scores$BankID == "BankB", 4])
bankB.positive.percentage = (bankB.positive.counter*100) / (bankB.positive.counter + bankB.negative.counter)
bankB.negative.percentage = (bankB.negative.counter*100) / (bankB.positive.counter + bankB.negative.counter)

#BANKC NUMBERS
bankC.positive.counter = sum(scores[scores$BankID == "BankC", 3])
bankC.negative.counter = sum(scores[scores$BankID == "BankC", 4])
bankC.positive.percentage = (bankC.positive.counter*100) / (bankC.positive.counter + bankC.negative.counter)
bankC.negative.percentage = (bankC.negative.counter*100) / (bankC.positive.counter + bankC.negative.counter)

#BANKD NUMBERS
bankD.positive.counter = sum(scores[scores$BankID == "BankD", 3])
bankD.negative.counter = sum(scores[scores$BankID == "BankD", 4])
bankD.positive.percentage = (bankD.positive.counter*100) / (bankD.positive.counter + bankD.negative.counter)
bankD.negative.percentage = (bankD.negative.counter*100) / (bankD.positive.counter + bankD.negative.counter)

#BANKe NUMBERS
banke.positive.counter = sum(scores[scores$BankID == "Banke", 3])
banke.negative.counter = sum(scores[scores$BankID == "Banke", 4])
banke.positive.percentage = (banke.positive.counter*100) / (banke.positive.counter + banke.negative.counter)
banke.negative.percentage = (banke.negative.counter*100) / (banke.positive.counter + banke.negative.counter)


#Total Positives
total.positives = sum(scores$very.pos)

#Creates the dataframe with the positive posts
dat.positive<-data.frame(
  banks = factor(c("BankA","BankB","BankC","BankD", "Banke"), levels=c("BankA","BankB", "BankC", "BankD", "Banke")),
  positive = c((bankA.positive.counter*100)/total.positives, (bankB.positive.counter*100)/total.positives, 
               (bankC.positive.counter*100)/total.positives, (bankD.positive.counter*100)/total.positives, (banke.positive.counter*100)/total.positives)
)
(p <- qplot(banks, positive, data=dat.positive, position="dodge", geom="bar", 
            stat="identity"))

#TotalNegatives
total.negative = sum(scores$very.neg)

#Creates the dataframe with the negative posts
dat.negative<-data.frame(
  banks = factor(c("BankA","BankB","BankC","BankD", "Banke"), levels=c("BankA","BankB", "BankC", "BankD", "Banke")),
  positive = c((bankA.negative.counter*100)/total.positives, (bankB.negative.counter*100)/total.negative, 
               (bankC.negative.counter*100)/total.positives, (bankD.negative.counter*100)/total.negative, (banke.negative.counter*100)/total.positives)
)

(p <- qplot(banks, positive, data=dat.negative, position="dodge", geom="bar", 
            stat="identity"))



#----------------------------------------------------------------------------------------
#Creating the dataFrames for each bank and sentiment

#BankA
df.bankA.negative = scores[(scores$BankID == "BankA" & scores$very.neg == 1), c(2, 5)]
df.bankA.positive = scores[(scores$BankID == "BankA" & scores$very.pos == 1), c(2, 5)]
write.table(df.bankB.positive[,1], file = "positiveCorpusB.txt")
#BankB
df.bankB.negative = scores[(scores$BankID == "BankB" & scores$very.neg == 1), c(2, 5)]
df.bankB.positive = scores[(scores$BankID == "BankB" & scores$very.pos == 1), c(2, 5)]

#BankC
df.bankC.negative = scores[(scores$BankID == "BankC" & scores$very.neg == 1), c(2, 5)]
df.bankC.positive = scores[(scores$BankID == "BankC" & scores$very.pos == 1), c(2, 5)]
#BankD
df.bankD.negative = scores[(scores$BankID == "BankD" & scores$very.neg == 1), c(2, 5)]
df.bankD.positive = scores[(scores$BankID == "BankD" & scores$very.pos == 1), c(2, 5)]
#Banke
df.banke.negative = scores[(scores$BankID == "Banke" & scores$very.neg == 1), c(2, 5)]
df.banke.positive = scores[(scores$BankID == "Banke" & scores$very.pos == 1), c(2, 5)]

#----------------------------------------------------------------------------------------#GGPLOT2 BAR GRAPHIC OF NEGATIVE AND POSITIVE POSTS
library(ggplot2)

colors = c("#990000", "#000099", "#990000", "#000099", "#990000", "#000099", "#990000", "#000099")

df <- data.frame(x=c("bankA","bankA","bankB","bankB","bankC","bankC","bankD","bankD"), 
                 y=c(bankA.negative.counter, bankA.positive.counter,
                     bankB.negative.counter, bankB.positive.counter,
                     bankC.negative.counter, bankC.positive.counter,
                     bankD.negative.counter, bankD.positive.counter), g = rep(1:2, 4))
(p <- qplot(x, y, data=df, group=g, position="dodge", geom="bar", 
            stat="identity") + scale_fill_manual(values = colors[order(df$x)]))

#----------------------------------------------------------------------------------------
#Create the files for the wordcloud
library(wordcloud)
#write.table(df.negative[(df.negative$BankID == "BankA"),1], file = "negativeCorpusA.txt")
words.remove = c(stopwords("english"),"twit_hndl", "ADDRESS", "twit", "hndl", "twit_hndl_banka", "banka","name")
words.remove

#Function to generate a wordcloud and a txt file with the frequent words ranked using a dataframe
createWordCloud = function(dataset){
  #Create a corpus using a dataframe
  #corpus <- Corpus(DataframeSource(dataset))
  corpus <- VCorpus(VectorSource(dataset$text))
  
  #clean the corpus text
  corpus <- tm_map(corpus, content_transformer(tolower))  
  corpus <- tm_map(corpus, removeWords, c("name", "bank", "twit_hndl", "AD", "twit_hnld_bankb_help","ADRESS", "twit","twithndlbankc",
                                          "twit_hndl_banka", "twit_hndl_bankb", "twit_hndl_bankd", "twit_hndl_banke",
                                          "name_resp", "internet", "hndl", "twit_hndl_banka","twit_hndl_bankc", 
                                          "banka", "bankb", "feedbankb", "bankc", "bankd", "banke"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  #Creates a Document Term Matrix to be used in the wordcloud
  corpusText <- tm_map(corpus, PlainTextDocument)
  dtm <- DocumentTermMatrix(corpusText)
  
  tdm <- TermDocumentMatrix(corpus)
  #Get the frequency of the words
  freq <- colSums(as.matrix(dtm))   
  length(freq)   
  ord <- order(freq)   
  m <- as.matrix(dtm)   
  dim(m)   
  
  #Sort the words by frequency rank
  freq<- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  #Create a txt file with the ranked words
  write.table(freq, file = "input.txt")
  
  dtms <- removeSparseTerms(dtm, 0.25) # Prepare the data (max 15% empty space)   
  freq <- colSums(as.matrix(dtm)) # Find word frequencies   
  dark2 <- brewer.pal(6, "Dark2")   
  #Create the wordcloud
  wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  
}

#Call the function to generate different wordclouds
#Creating clouds to see the diffence between what banks A, B, C and D have different than the other banks
createWordCloud(scores[(scores$BankID != "Banke" & scores$very.pos == 1),c(2,3)])
createWordCloud(scores[(scores$BankID == "Banke" & scores$very.pos == 1),c(2,3)])

createWordCloud(scores[(scores$BankID != "Banke" & scores$very.neg == 1),c(2,3)])
createWordCloud(scores[(scores$BankID == "Banke" & scores$very.neg == 1),c(2,3)])

#Create clouds to see the difference between the banks A, B, C and D
#createWordCloud(scores[(scores$BankID == "Banke"),c(2,3)])
#Run the line below for all the negative posts in a single wordcloud
createWordCloud(scores[(scores$very.neg == 1),c(2,3)])
#Run the line below for all the positive posts in a single wordcloud
createWordCloud(scores[(scores$very.pos == 1),c(2,3)])

#Run the line below for a full dataset analysis
#createWordCloud(scores[,c(2,3)])
createWordCloud(df.bankA.positive)
createWordCloud(df.bankA.negative)
createWordCloud(df.bankB.negative)
createWordCloud(df.bankB.positive)
createWordCloud(df.bankC.negative)
createWordCloud(df.bankC.positive)
createWordCloud(df.bankD.negative)
createWordCloud(df.bankD.positive)
createWordCloud(df.banke.negative)
createWordCloud(df.banke.positive)

#----------------------------------------------------------------------------------------------------------------
#Calculate the percentages removing the posts from bank employees
bankA.staff.indx = which(sapply(df.bankA.positive$text,function(x) grepl("Name_Resp",x)))
bankB.staff.indx = which(sapply(df.bankB.positive$text,function(x) grepl("Name_Resp",x)))
bankC.staff.indx = which(sapply(df.bankC.positive$text,function(x) grepl("Name_Resp",x)))
bankD.staff.indx = which(sapply(df.bankD.positive$text,function(x) grepl("Name_Resp",x)))
banke.staff.indx = which(sapply(df.banke.positive$text,function(x) grepl("Name_Resp",x)))

#BANKA NUMBERS
bankA.positive.counter = bankA.positive.counter - length(bankA.staff.indx)
bankA.positive.percentage = (bankA.positive.counter*100) / (bankA.positive.counter + bankA.negative.counter)
#BANKB NUMBERS
bankB.positive.counter = bankB.positive.counter - length(bankB.staff.indx)
bankB.positive.percentage = (bankB.positive.counter*100) / (bankB.positive.counter + bankB.negative.counter)
#BANKC NUMBERS
bankC.positive.counter = bankC.positive.counter - length(bankC.staff.indx)
bankC.positive.percentage = (bankC.positive.counter*100) / (bankC.positive.counter + bankC.negative.counter)

#BANKD NUMBERS
bankD.positive.counter = bankD.positive.counter - length(bankD.staff.indx)
bankD.positive.percentage = (bankD.positive.counter*100) / (bankD.positive.counter + bankD.negative.counter)

#Banke NUMBERS
banke.positive.counter = banke.positive.counter - length(banke.staff.indx)
banke.positive.percentage = (banke.positive.counter*100) / (banke.positive.counter + banke.negative.counter)

#Creates the dataframe with the positive posts
dat.positive<-data.frame(
  banks = factor(c("BankA","BankB","BankC","BankD", "Banke"), levels=c("BankA","BankB", "BankC", "BankD", "Banke")),
  positive = c(bankA.positive.percentage, bankB.positive.percentage, 
                bankC.positive.percentage, bankD.positive.percentage, banke.positive.percentage)
)
(p <- qplot(banks, positive, data=dat.positive, position="dodge", geom="bar", 
            stat="identity"))
