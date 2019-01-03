df <- read.csv("train.csv",header=TRUE,sep=",")
head(df)
for (i in c(1:12)) { m[i]<- sum(is.na(df[,i])) }
m
##column 6 only have 177 NAs
df[df$Cabin=="",11] <- "NA"
for (i in c(1:12)) {
  for (j in c(1:891)){ 
    if(df[j,i]=="") {df[j,i]<- "NA"} }  }
summary(df)

##we see not much difference between age median and age mean. assume normally distributed.
df[which(is.na(df$Age)),6] <- mean(df$Age)

##mostly embarked from S so imnpute "S"
table(df$Embarked)
df[df$Embarked=="",12] <- "S"

##no na in fares
sum(is.na(df$Fare))
summary(df$Fare)

##most number of 3rd class passengers, almost half
ggplot(df,aes(x=Pclass)) + geom_histogram(binwidth = 1)

##seprating title our of names
lastname <- strsplit(df$Name,",")
for (i in c(1:nrow(df))) { a[i] <- lastname[[i]][2]}
b <- strsplit(a[1:nrow(df)],". ")
for (i in c(1:nrow(df))) { title[i] <- b[[i]][1]}

##mostly fare are from 5 to 15
ggplot(df,aes(Fare,color=I("red"),fill=I("green"),alpha=I(0.3))) + geom_histogram(binwidth = 1) + xlim(NA,50)

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

##we see that higher the fair higher the survival
ggplot(df,aes(x=Fare,fill=factor(Survived))) + geom_histogram()+ xlim(0,100)

fare <- fare[-c(1,2,3)]
head(fare)
summary(fare)
##now the mean has moved down
##~~~~

## survival vs title
df<- cbind(df,title)
table(df$title) ##we can merge some of them 

survival_by_title <- df %>% group_by(title) %>% summarise(value=mean(Survived)*100)
##married women were saved most
##only 15% of men survived with mr title
##unmarried women with miss title also survived 70%
##master also survived
##mean women and upper title were given preferences

df$title <- gsub(" ","",df$title, fixed = TRUE) ## removing whitespace
df$title[df$title!="Master" & df$title != "Miss" & df$title!= "Mr" & df$title!= "Mrs"] <- "Other"
table(df$title)
ggplot(df,aes(x=title,fill=factor(Survived))) + geom_bar()
##~~~~~~~~~

##
