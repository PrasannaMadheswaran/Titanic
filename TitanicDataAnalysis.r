/* read Train and Test CSV files*/
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

test[,]
#add Survived column with "none" to test by creating a new DF
test.survived <- data.frame(Survived = rep("none", nrow(test)), test[,])

#combine test.survived and train

data.combined <- rbind(train, test.survived)

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)

table(data.combined$Pclass)

library(ggplot2)
library(stringr)


# Bar plot of count of suvivors vs PClass
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar() + xlab("Plcass") + ylab("Total Count") + labs(fill = "Survived")

# duplicate name investigation
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names), ]

#Investigation of no. of Passengers with Title Miss, Mrs and Male
misses <- data.combined[which(str_detect(data.combined$Name, "Miss. ")), ]
misses[1:5,]

mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs. ")), ]
mrs[1:5,]

males <- data.combined[which(data.combined$Sex == "male"), ]
males[1:5,]

#function to extract the Title from Name

extractTitle <- function(name){
  if (str_detect(name, "Miss. ") == TRUE) {
    return ("Miss. ")
  }
  else if (str_detect(name, "Mr. ") == TRUE) {
    return ("Mr. ")
  }
  else if (str_detect(name, "Mrs. ") == TRUE) {
    return ("Mrs. ")
  }
  else if (str_detect(name, "Master. ") == TRUE) {
    return ("Master. ")
  }
  else {
    return ("Other")
  }
}  

#Assignment of Titles
titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  
}

data.combined$title <- as.factor(titles)


#Plot of Title with Survivors count by Class
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + 
   geom_bar() + 
   facet_wrap(~Pclass) + 
   ggtitle("Pclass") + 
   xlab("title") + 
   ylab("Total Count") + 
   labs(fill = "Survived")