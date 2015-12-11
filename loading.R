df = read.table('corpus.txt',sep="|",header=T)

# If you want to test on just 1000 records using df.1000 created below
#idx.1000 = sample(1:nrow(df),1000)
#df.1000 = df[idx.1000,]

# Grab just the texts removing the empty rows, so you can load them in the Corpus
df.texts = as.data.frame(df[df$FullText != "",c(4, 6)])
colnames(df.texts) = c('Months','FullText')

# Remove non-ascii characters
df.texts.clean = as.data.frame(iconv(df.texts$FullText, "latin1", "ASCII", sub=""))
colnames(df.texts.clean) = 'FullText'
df.texts.clean$Months = df.texts$Months

# Load using the tm library
library(tm) 
docs <- Corpus(DataframeSource(df.texts.clean))   

# Strip extra whitespace
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("twit_hndl", "twit", "hndl", "ADDRESS"))
docs <- tm_map(docs, removeWords, stopwords("english"))

bankA.idx = which(sapply(df.texts.clean$FullText,function(x) grepl("BankA",x)))
bankB.idx = which(sapply(df.texts.clean$FullText,function(x) grepl("BankB",x)))
bankC.idx = which(sapply(df.texts.clean$FullText,function(x) grepl("BankC",x)))
bankD.idx = which(sapply(df.texts.clean$FullText,function(x) grepl("BankD",x)))

df.texts.clean$BankID = vector(mode ="numeric", length=nrow(df.texts))
df.texts.clean$BankID[bankA.idx] = "BankA"
df.texts.clean$BankID[bankB.idx] = "BankB"
df.texts.clean$BankID[bankC.idx] = "BankC"
df.texts.clean$BankID[bankD.idx] = "BankD"
df.texts.clean$BankID[c(-bankA.idx,-bankB.idx,-bankC.idx,-bankD.idx)] = "Banke"

#bankA.docs = docs[bankA.idx]
#bankB.docs = docs[bankB.idx]
#bankC.docs = docs[bankC.idx]
#bankD.docs = docs[bankD.idx]

#summary(docs)