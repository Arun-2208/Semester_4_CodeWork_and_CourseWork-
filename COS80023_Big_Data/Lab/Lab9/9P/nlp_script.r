################ Pass level task #############################

#loading the libraries we will need
library("caret")
library("quanteda")
library(ggplot2)

#In the file open dialog, find the file sms_spam.csv
data.raw <- read.csv(file.choose(), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
nrow(data.raw)
View(data.raw)

# Makes the type field (ham or spam) a factor:
data.raw$type <- as.factor(data.raw$type)
# Shows the proportions of the values of the type attribute:
prop.table(table(data.raw$type))

#Number of characters of each sms is stored in a TextLength variable. There are as many TextLength variables as rows.
data.raw$TextLength <- nchar(data.raw$text)
summary(data.raw$TextLength)

#find the statistics for each type (ham and spam) separately:
indexes <- which(data.raw$type=="ham")
ham <- data.raw[indexes,]
spam <- data.raw[-indexes,]
spam$TextLength <- nchar(spam$text)
ham$TextLength <- nchar(ham$text)
summary(ham$TextLength)
summary(spam$TextLength)

#Plot the histogram
ggplot(data.raw, aes(x = TextLength, fill = type)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")

#separate into training and testing data
set.seed(32984)
indexes <- createDataPartition(data.raw$type, times = 1,
                               p = 0.7, list = FALSE)

train.raw <- data.raw[indexes,]
test.raw <- data.raw[-indexes,]

# Tokenize SMS text messages. Symbols and other special elements can be removed at the same time. ngrams is also an option, default is 1; ngrams = 2:3 gives all combinations of 2 and 3 words
train.tokens <- tokens(train.raw$text, what = "word", remove_numbers = TRUE, 
                       remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, ngrams = 1)
#Change all words to lower case:
train.tokens <- tokens_tolower(train.tokens)

#Stopwords are words like 'in', 'as', etc that carry no meaning and are therefore clutter.
#Stopword lists depend on the domain - some domains may need words that others find meaningless
#Quanteda's list of stop words is
stopwords()

#Remove all stopwords by using quanteda's build-in stopword list
train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")

#Stem the tokens
train.tokens <- tokens_wordstem(train.tokens, language = "english")

#View the content (can be done after each step) Change number of rows and columns if desired
train.tokens[1:20, 1:100]



#Create the document frequency matrices (aka bag of words) for both sets; lower case has already been done
#Look at all the options for dfm()
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)

#to be able to add the label (annotation), we have to change the matrix to a frame:
train.tokens.df <- convert(train.tokens.dfm, to = "data.frame")
#add the label
train.tokens.df <- cbind(type = train.raw$type, train.tokens.df)
View(train.tokens.df[1:20, 1:100])

# Cleanup column names (removes invalid tokens like . at the start of the word)
names(train.tokens.df) <- make.names(names(train.tokens.df))
train.tokens.df <- train.tokens.df[, !duplicated(colnames(train.tokens.df))]

#settings for training, using stratified samples as before
set.seed(48743)
folds <- createMultiFolds(train.tokens.df$type, k = 10, times = 1)
traincntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, index = folds)

#this is a large dataset, so we use a simple decision tree as classifier
rpart_model <- train(type ~., data = train.tokens.df, method = "rpart", trControl = traincntrl, tuneLength = 7)

#Prepare the test set as you prepared the training set:
test.tokens <- tokens(test.raw$text, what = "word", remove_numbers = TRUE, 
                       remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, ngrams = 1)

test.tokens <- tokens_tolower(test.tokens)
test.tokens <- tokens_select(test.tokens, stopwords(), selection = "remove")
test.tokens <- tokens_wordstem(test.tokens, language = "english")
test.tokens.dfm <- dfm(test.tokens, tolower = FALSE)
ncol(test.tokens.dfm)
test.tokens.dfm = dfm_match(test.tokens.dfm, featnames(train.tokens.dfm))
ncol(test.tokens.dfm)
test.tokens.df <- convert(test.tokens.dfm, to = "data.frame")
test.tokens.df <- cbind(type = test.raw$type, test.tokens.df)

names(test.tokens.df) <- make.names(names(test.tokens.df))
test.tokens.df <- test.tokens.df[, !duplicated(colnames(test.tokens.df))]

# apply the model to the testing set
testresult <- predict(rpart_model, newdata=test.tokens.df)

# apply the model to the training set
trainresult <- predict(rpart_model, newdata=train.tokens.df)

#show the results for both
confusionMatrix(table(testresult, test.tokens.df$type))
confusionMatrix(table(trainresult, train.tokens.df$type))



