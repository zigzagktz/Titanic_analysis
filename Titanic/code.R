t<- read.csv("train.csv",sep=",",header=TRUE)
te <- read.csv("test.csv",sep=",",header=TRUE)
te$Survived <- NA
df<-rbind(t,te)
summary(df)
head(df)
tail(df)

##to many missing values, removing cabin, not important
df <- df[,-11]

for (i in c(1:11)) { m[i]<- sum(is.na(df[,i])) }
m

##column 6 only have 263 NAs
##we see not much difference between age median and age mean. assume normally distributed.
df[which(is.na(df$Age)),6] <- mean(df$Age,na.rm=TRUE)



##impute NAs in empty spacE
which(is.na(df$Fare))
df$Fare[1044] <- median(df$Fare,na.rm=TRUE)

##mostly embarked from S so imnpute "S"
table(df$Embarked)
which(df$Embarked=="")
df$Embarked[c(62,830)] <- "S"
df$Embarked <- droplevels(df$Embarked)
table(df$Embarked)


##for (i in c(1:12)) {
##for (j in c(1:1309)){ 
##if(df[j,i]=="") {df[j,i]<- "NA"} }  }

summary(df)

##we see not much difference between age median and age mean. assume normally distributed.
df[which(is.na(df$Age)),6] <- mean(df$Age,na.rm=TRUE)


##no na in fares
sum(is.na(df$Fare))
summary(df$Fare)

##drop pasenger id
df <- df[,-1]

##we have cleared the data, now we can split it
test <- df[892: 1309,]
df <- df[1:891,]

##most number of 3rd class passengers, almost half
aggregate(df$Survived,by=list(df$Pclass),FUN=mean) ##variying survival rate depending upon the class
ggplot(df,aes(x=Pclass,fill=factor(Survived))) + geom_histogram(stat="count")


##seprating title out of names in training
df$Name <- as.character(df$Name)
lastname <- strsplit(df$Name,",")
for (i in c(1:nrow(df))) { a[i] <- lastname[[i]][2]}
b <- strsplit(a[1:nrow(df)],". ")
for (i in c(1:nrow(df))) { title[i] <- b[[i]][1]}
title <- trimws(title)
df <- cbind(df,title)
df$title <- as.character(df$title)

##seprating title out of names in testing
test$Name <- as.character(test$Name)
lastname <- strsplit(test$Name,",")
for (i in c(1:nrow(test))) { g[i] <- lastname[[i]][2]}
g<- as.character(g)
h <- strsplit(g[1:nrow(test)],". ")
for (i in c(1:nrow(test))) { m[i] <- h[[i]][1]}
title <- trimws(m)
test <- cbind(test,title)
test$title <- as.character(test$title)

##cange factor to character
df$Embarked <- as.character(df$Embarked)

##
ggplot(df,aes(x=SibSp,fill=factor(Survived))) + geom_bar(binwidth = .5)

##SURVIAL RATE DECREASSED AS THE RESPONSIBILITY INCREASED.
percent <- df %>% group_by(SibSp) %>% summarise(lived=sum(Survived==1),died= sum(Survived==0))
survival_rate <- (percent$lived)*100/(percent$lived + percent$died )
percent <- cbind(percent,survival_rate)
percent
ggplot(percent,aes(x=SibSp,y=survival_rate,col=I("red"))) + geom_line()

#having a parent or chind does not seem to efect the survial.
ggplot(df,aes(x=Parch,fill=factor(Survived))) + geom_bar(binwidth = .5)
percent2 <- df %>% group_by(Parch) %>% summarise(lived=sum(Survived==1),died= sum(Survived==0))
survival_rate2 <- (percent2$lived)*100/(percent2$lived + percent2$died )
percent2 <- cbind(percent2,survival_rate2)
percent2
ggplot(percent2,aes(x=Parch,y=survival_rate2,col=I("red"))) + geom_line()

##but combining them will increase the accuracy a little bit.
family_size <- df$SibSp + df$Parch + 1
df <- cbind(df,family)
ggplot(df,aes(x=family,fill=factor(Survived))) + geom_histogram(stat = "count")
percent3 <- df %>% group_by(family) %>% summarise(lived=sum(Survived==1),died= sum(Survived==0))
survival_rate3 <- (percent3$lived)*100/(percent3$lived + percent3$died )
percent3 <- cbind(percent3,survival_rate3)
percent3
ggplot(percent3,aes(x=family,y=survival_rate3,col=I("red"))) + geom_line()

##combining 3 of them
dummy <- cbind(percent[,c(1,4)],percent2[,c(1,4)], percent3[,c(1,4)])
g <- ggplot(dummy,aes(x=family,y=survival_rate3,color=I("red"),group=2)) + geom_line() 
g <- g + geom_line(aes(x=family,y=survival_rate2,color="green")) + geom_point(shape=16)
g <- g + geom_line(aes(x=family,y=survival_rate,color="blue")) + geom_line() 
g
##all three seems to follow a pattern, so we create a new feature called family to be used for prediciton


#we see that people with high class had better chance of survival.
ggplot(df,aes(x=Pclass,fill=factor(Survived))) + geom_bar()
percent_survive_by_class <- df %>% group_by(Pclass) %>% summarise(survival_rate=sum(Survived==1)*100/ (survival_rate= sum(Survived ==1)+sum(Survived==0)) )
percent_survive_by_class

#sruvival vs fare

barplot(df$Fare)
#I see a few outliers
fare <- sort(df$Fare,decreasing = T)
head(fare)
top_fare <- tail(order(df$Fare),3)
df[top_fare,]
#all 3 with highest fare survived.


fare <- fare[-c(1,2,3)]
head(fare)
summary(fare)
##now the mean has moved down

##mostly fare are from 5 to 15
ggplot(df,aes(Fare,color=I("red"),fill=I("green"),alpha=I(0.3))) + geom_histogram(binwidth = 1) + xlim(NA,50)


##we see that higher the fair higher the survival
ggplot(df,aes(x=Fare,fill=factor(Survived))) + geom_histogram()+ xlim(0,100)

##~~~~

## survival vs title
table(df$title) ##we can merge some of them 

survival_by_title <- df %>% group_by(title) %>% summarise(value=mean(Survived)*100)
##married women were saved most
##only 15% of men survived with mr title
##unmarried women with miss title also survived 70%
##master also survived
##mean women and upper title were given preferences

df$title[df$title!="Master" & df$title != "Miss" & df$title!= "Mr" & df$title!= "Mrs"] <- "Other"
table(df$title)
ggplot(df,aes(x=title,fill=factor(Survived))) + geom_bar()
##~~~~~~~~~

## survival vs embarked
ggplot(df,aes(x=Embarked,fill=factor(Survived))) + geom_histogram(stat="count")
aggregate(df$Survived,by=list(df$Embarked),FUN=mean)
## there is sufficient difference in percentage, so we can use this also for prediction

## survival vs age
ggplot(df,aes(x=Age)) + geom_density(col="green",fill="red",alpha=.4,size=1)
## see they are mostly middle age people
## as the age increased the survival also increased
## this can also be used for prediction


##sex vs survival 
ggplot(df,aes(x=Sex,fill=factor(Survived))) + geom_histogram(stat="count")
## Women had higeher survival rate

##normalize continous variables

##change eveything to factor

