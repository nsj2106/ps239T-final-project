rm(list=ls()) 
setwd("/Users/nsj2106/Documents/Spring 2016/Friday Pradeep Class")
install.packages('foreign', repos = "http://cran.us.r-project.org")
library('foreign')

data_04=as.data.frame(read.spss('NES2004.sav')) 

install.packages('ggplot2', repos = "http://cran.us.r-project.org")
library(ggplot2)

#a package that helps ggplot; turning data from wide to long. 
install.packages('reshape2', repos = "http://cran.us.r-project.org")
library(reshape2)


#packages for standard errors, t-tests etc.
install.packages('sandwich', repos = "http://cran.us.r-project.org")
library('sandwich')
install.packages('lmtest', repos = "http://cran.us.r-project.org")
library('lmtest')

#output tables for regressions etc.
install.packages("stargazer")
library(stargazer)


#27189 individuals in this dataset 
summary(data_04$b3)

#14779 men and 12410 women; women are the focus of this analysis
women=subset(data_04, data_04$b3=='Female')
men=subset(data_04, data_04$b3=='Male')

#How many women participate in vote mobilization activities? Row percentages (appendix).
prop.table(table(data_04$b3, data_04$q7a),1)
prop.table(table(data_04$b3, data_04$q7b),1)
prop.table(table(data_04$b3, data_04$q7c),1)
prop.table(table(data_04$b3, data_04$q7e),1)

#absolute numbers.
table(data_04$b3, data_04$q7a)
table(data_04$b3, data_04$q7b)
table(data_04$b3, data_04$q7c)
table(data_04$b3, data_04$q7e)


##For the most part, around 1 percent of the female population participate in these
#activities. What predicts women's engagement in such public activities, especially in patriarchal societies like India? 

#Let's check out all the Indian states and N covered in this dataset. 
table(data_04$v1)

#Let's sort the data to see if there is any regional variation in how women participate.
#I will be sorting it by women who identified with a certain state and who said yes to attending 
#meetings; in decreasing order. Let's view the top six states for each variable. 
test <- data.frame(table(women$v1, women$q7a))
Region_Yes_A=subset(test, test$Var2=='Yes')
head(Region_Yes_A[ order(Region_Yes_A$Freq, decreasing=TRUE), ])

test2 <- data.frame(table(women$v1, women$q7b))
Region_Yes_B=subset(test2, test2$Var2=='Yes')
head(Region_Yes_B[ order(Region_Yes_B$Freq, decreasing=TRUE), ])

test3 <- data.frame(table(women$v1, women$q7c))
Region_Yes_C=subset(test3, test3$Var2=='Yes')
head(Region_Yes_C[ order(Region_Yes_C$Freq, decreasing=TRUE), ])

test4 <- data.frame(table(women$v1, women$q7d))
Region_Yes_D=subset(test4, test4$Var2=='Yes')
head(Region_Yes_D[ order(Region_Yes_D$Freq, decreasing=TRUE), ])

test5 <- data.frame(table(women$v1, women$q7e))
Region_Yes_E=subset(test5, test5$Var2=='Yes')
head(Region_Yes_E[ order(Region_Yes_E$Freq, decreasing=TRUE), ])


#We now know that something is going on in the Northeast. Now let's make separate
#regional identifiers to run some tests.
North_East=subset(data_04, (data_04$v1=='2: Arunachal Pradesh' | data_04$v1=='3: Assam' | data_04$v1=='14: Manipur' |
                              data_04$v1=='15: Meghalaya' | data_04$v1=='16: Mizoram' | data_04$v1=='17: Nagaland'|
                              data_04$v1=='23: Tripura' | data_04$v1=='21: Sikkim'))


South_India=subset(data_04, (data_04$v1=='1: Andhra Pradesh' | data_04$v1=='10: Karnataka' | data_04$v1=='11: Kerala' |
                              data_04$v1=='22: Tamil Nadu' | data_04$v1=='32: Pondicherry'))


East_India=subset(data_04, (data_04$v1=='25: West Bengal' | data_04$v1=='4: Bihar' | data_04$v1=='33: Jharkhand' |
                              data_04$v1=='18: Orissa' | data_04$v1=='34: Chhattisgarh'))


North_India=subset(data_04, (data_04$v1=='19: Punjab' | data_04$v1=='9: Jammu & Kashmir' | data_04$v1=='7: Haryana' |
                              data_04$v1=='8: Himachal Pradesh'| data_04$v1=='24: Uttar Pradesh'|
                               data_04$v1=='35: Uttaranchal' | data_04$v1=='30: Delhi' | data_04$v1=='27: Chandigarh'| 
                               data_04$v1=='12: Madhya Pradesh'))

West_India=subset(data_04, (data_04$v1=='5: Goa' | data_04$v1=='6: Gujarat' | data_04$v1=='13: Maharashtra' |
                               data_04$v1=='20: Rajasthan'))


#I'd also like to subset women by each region above. 
North_East_Women=subset(North_East, North_East$b3=='Female')
North_East_Men=subset(North_East, North_East$b3=='Male')

North_Women=subset(North_India, North_India$b3=='Female')
North_Men=subset(North_India, North_India$b3=='Male')

East_Women=subset(East_India, East_India$b3=='Female')
East_Men=subset(East_India, East_India$b3=='Male')

South_Women=subset(South_India, South_India$b3=='Female')
South_Men=subset(South_India, South_India$b3=='Male')

West_Women=subset(West_India, West_India$b3=='Female')
West_Men=subset(West_India, West_India$b3=='Male')


#Basic crosstabulations suggest that more women are participating in such activities than other regions.Column percentages.
prop.table(table(North_East$b3, North_East$q7a),2)
prop.table(table(North_East$b3, North_East$q7b),2)
prop.table(table(North_East$b3, North_East$q7c),2)
prop.table(table(North_East$b3, North_East$q7e),2)

prop.table(table(North_India$b3, North_India$q7a),2)
prop.table(table(North_India$b3, North_India$q7b),2)
prop.table(table(North_India$b3, North_India$q7c),2)
prop.table(table(North_India$b3, North_India$q7e),2)

prop.table(table(East_India$b3, East_India$q7a),2)
prop.table(table(East_India$b3, East_India$q7b),2)
prop.table(table(East_India$b3, East_India$q7c),2)
prop.table(table(East_India$b3, East_India$q7e),2)

prop.table(table(West_India$b3, West_India$q7a),2)
prop.table(table(West_India$b3, West_India$q7b),2)
prop.table(table(West_India$b3, West_India$q7c),2)
prop.table(table(West_India$b3, West_India$q7e),2)

prop.table(table(South_India$b3, South_India$q7a),2)
prop.table(table(South_India$b3, South_India$q7b),2)
prop.table(table(South_India$b3, South_India$q7c),2)
prop.table(table(South_India$b3, South_India$q7e),2)


#Using column percentages above, I want a graph that is a percentage of men and women who participated in 
#campainging based on India's five regions. 
testing <- data.frame(prop.table(table(South_India$b3, South_India$q7a),2))
testing
Region_1=subset(testing, testing$Var2=='Yes')
Region_1

#But this is taking too damn long. I may as well just write it manually. 
Election_Meetings = c(67.9, 32, 79.35, 20.64, 81.4, 18.56, 78.47, 21.5, 74.2, 25.79)
Rallies = c(67.95, 32.0, 78.94, 21.0, 79.17, 20.8, 81.60, 18.39, 75.47, 24.5)
Canvassing = c(69.30, 30.6, 77.17, 22.82, 76.65, 23.34, 78.17, 21.28, 73.70, 26.29)
Leaflets = c(72.67, 27.30, 75.93, 24.1, 75, 24.95, 80.5, 19.49, 75.16, 24.83)

#Basic R Plot
d222 <- cbind(Election_Meetings, Rallies, Canvassing, Leaflets )
barplot(d222, main = "Red: Northeast India (N=2266), Yellow: North India (N=3425),
        Green: East India (N=2163), Blue: West India (N=2114), Gray: South India (N=2442)", 
        xlab="Percentage of Male & Female Campaigners By Region (Women Shown in Dark Colors)", 
        col=c("red", "darkred", "yellow1", "yellow4", "green", "darkgreen", "skyblue", "skyblue4", "gray66", "gray37"), beside = TRUE, 
        names.arg=c("Meetings", "Rallies", "Canvassing", "Leaflets"))


#This looks tacky and way too colorful. Will use ggplot2 instead.


#Putting variables above into a data.frame because that what ggplot2 uses.
dtest <- data.frame(Election_Meetings, Rallies, Canvassing, Leaflets)
dtest

#Now using reshape2 to switch it from wide to long format. This is better for variables comparison. 
dtestm <- melt(dtest)
dtestm

#adding a new column called 'rowid' as an identifying variable. 
dtestm$rowid <- 1:40

#adding new column where I label women and men for each region one numerical value. This will help me color in ggplot2.
dtestm$wtf <- c(1,1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5,1,1,2,2,3,3,4,4,5,5)
dtestm

#https://rpubs.com/escott8908/RGC_Ch3_Gar_Graphs
figure_1 <- ggplot(dtestm, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South"), values=c("brown3", "grey69", "grey59", "grey49", "grey29")) + 
  xlab("") + ylab("") + ggtitle("Percentage of Male & Female Campaigners By Region (Women Shown in Alternate Columns)")
figure_1


#Now, let's look specifically at the gender gap, with and without the Indian state of Arunachal Pradesh. Graph 2 
#will contain similar information as graph 1, but will remove AP to show that there does appear to be a 
#Northeast regional effect. I am removing AP because it is an outliter in terms of literacy rates.

North_East1=subset(data_04, (data_04$v1=='3: Assam' | data_04$v1=='14: Manipur' |
                               data_04$v1=='15: Meghalaya' | data_04$v1=='16: Mizoram' | data_04$v1=='17: Nagaland'|
                               data_04$v1=='23: Tripura' | data_04$v1=='21: Sikkim'))


prop.table(table(North_East1$b3, North_East1$q7a),2)
prop.table(table(North_East1$b3, North_East1$q7b),2)
prop.table(table(North_East1$b3, North_East1$q7c),2)
prop.table(table(North_East1$b3, North_East1$q7e),2)

#With Arunachal Pradesh
Gap_Elec_Mtngs = c(35.9, 58.7, 62.87, 56.95, 48.41)
Gap_Rallies = c(35.95, 57.94, 58.2, 53.3, 50.95)
Gap_Canvassing = c(38.78, 54.37, 53.31, 57.43, 47.41)
Gap_Leaflets = c(45.39, 51.93, 50.05, 61.01, 50.33)


#Without Arunachal Pradesh
Gap_Elec_Mtngs = c(29.039, 58.7, 62.87, 56.95, 48.41)
Gap_Rallies = c(30.31, 57.94, 58.2, 53.3, 50.95)
Gap_Canvassing = c(34.47, 54.37, 53.31, 57.43, 47.41)
Gap_Leaflets = c(40.93, 51.93, 50.05, 61.01, 50.33)

#Basic R Plot of regional variation without Arunachal Pradesh. 
x333 <- cbind(Gap_Elec_Mtngs, Gap_Rallies, Gap_Canvassing, Gap_Leaflets)
barplot(x333, main = "Column1: Northeast India (N=2266), Column2: North India (N=3425),
        Column3: East India (N=2163), Column4: West India (N=2114), Column5: South India (N=2442)", 
        xlab="Gap Between Men & Women Political Campaigners By Region (Excluding Arunachal Pradesh)", 
        col=c("red", "grey60", "grey60"
              , "grey70", "grey80"), beside = TRUE, 
        names.arg=c("Meetings", "Rallies", "Canvassing", "Leaflets"))


#Now let's use ggplot2 again, because this will be more elegant. 
#once again, let's put what we placed earlier in cbind into a dataframe. 
dtest2 <- data.frame(Gap_Elec_Mtngs, Gap_Rallies, Gap_Canvassing, Gap_Leaflets)
dtest2

#Now using reshape2 to switch it from wide to long format. Ggplot2 like this, don't ask why. 
dtest2 <- melt(dtest2)
dtest2

#adding a new column called rowid as an identifying variable. 
dtest2$rowid <- 1:20
dtest2

#adding new column where I label women and men for each region one numerical value. This will help me color in ggplot2.
dtest2$wtf <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
dtest2

figure_2 <- ggplot(dtest2, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South"), values=c("brown3", "grey69", "grey59", "grey49", "grey29")) + 
  xlab("") + ylab("") + ggtitle("Gap Between Men and Women Campaigners By Region (Excluding Arunachal Pradesh")
figure_2


##Now let's look at other variables like an ASSOCIATION. 
#All India
prop.table(table(data_04$b3, data_04$q19),1)

#Regional Breakdown of participation in civic associations. Row percentages.
prop.table(table(North_East$b3, North_East$q19),1)
prop.table(table(North_India$b3, North_India$q19),1)
prop.table(table(East_India$b3, East_India$q19),1)
prop.table(table(West_India$b3, West_India$q19),1)
prop.table(table(South_India$b3, South_India$q19),1)


Women_Associations= c(22.8, 5.4, 6.6, 7.6, 36.15)
Men_Associations= c(30.6, 11.6, 13.87, 17.57, 36.15)
Assoc <- cbind (Men_Associations, Women_Associations)

barplot(Assoc, main = "Red: Northeast India (N=2266), Black: North India (N=3425),
        Green: East India (N=2163), Blue: West India (N=2114), Gray: South India (N=2442)", 
        xlab="Percentage of Men & Women in Associations Based on Region", 
        col=c("red","black", "darkgreen", "blue", "gray", "red","black", "darkgreen", "blue", "gray"), 
        beside = TRUE)

#ggplot2 instead.
#once again, let's put what we placed earlier in a cbind into a dataframe. 
dtest3 <- data.frame(Men_Associations, Women_Associations)
dtest3

#Now using reshape2 to switch it from wide to long format. Ggplot2 like this, don't ask why. 
dtest3 <- melt(dtest3)
dtest3

#adding a new column called rowid as an identifying variable. 
dtest3$rowid <- 1:10
dtest3

#adding new column where I label women and men for each region one numerical value. This will help me color in 
#ggplot2.
dtest3$wtf <- c(1,2,3,4,5,1,2,3,4,5)
dtest3

figure_3 <- ggplot(dtest3, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South"), values=c("brown3", "grey69", "grey59", "grey49", "grey29")) + 
  xlab("") + ylab("") + ggtitle("% of Men & Women in Associations By Region")
figure_3



#What about LITERACY?
#NE is known for having above average levels of literacy, especially for women.
#Let's compare literacy, especially with progressive states in the South.
#Northeast Illiteracy: 27.4, North India: 48.3, East India: 47.3, West: 47.7, South: 38.5.

prop.table(table(North_East$b3, North_East$b4),1)
prop.table(table(North_India$b3, North_India$b4),1)
prop.table(table(East_India$b3, East_India$b4),1)
prop.table(table(West_India$b3, West_India$b4),1)
prop.table(table(South_India$b3, South_India$b4),1)

Women_Illiteracy= c(27.4, 48.3, 47.3, 47.7, 38.5)
Men_Illiteracy= c(15.46, 22, 23.3, 20.6, 25.74)
Illiteracy <- cbind(Women_Illiteracy, Men_Illiteracy)
barplot(Illiteracy, main = "Red: Northeast India (N=2266), Black: North India (N=3425),
        Green: East India (N=2163), Blue: West India (N=2114), Gray: South India (N=2442)", 
        xlab="Percentage of Illiteracy", 
        col=c("red","black", "darkgreen", "blue", "gray", "red", "black", "darkgreen",
              "blue", "gray"), beside = TRUE)

#once again, let's put what we placed earlier in a cbind into a dataframe. 
dtest4 <- data.frame(Men_Illiteracy, Women_Illiteracy)
dtest4

#Now using reshape2 to switch it from wide to long format. Ggplot2 like this, don't ask why. 
dtest4 <- melt(dtest4)
dtest4

#adding a new column called 'rowid' as an identifying variable. 
dtest4$rowid <- 1:10
dtest4

#adding new column where I label women and men for each region one numerical value. This will help me color 
#in ggplot2.
dtest4$wtf <- c(1,2,3,4,5,1,2,3,4,5)
dtest4

figure_4 <- ggplot(dtest4, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South"), values=c("brown3", "grey69", "grey59", "grey49", "grey29")) + 
  xlab("") + ylab("") + ggtitle("% Illiteracy By Region")
figure_4


##Important that the levels of women and men illiteracy is low, and the gap is small, compared
#to other regions of India, even South India. 


#What about the variable LEVEL OF INDEPENDENCE 
#I measure that with a question in the survey that asks, 'whose opinion mattered to you while voting'?
prop.table(table(North_East$b3, North_East$q9),1)
prop.table(table(North_India$b3, North_India$q9),1)
prop.table(table(East_India$b3, East_India$q9),1)
prop.table(table(West_India$b3, West_India$q9),1)
prop.table(table(South_India$b3, South_India$q9),1)
prop.table(table(data_04$b3, data_04$q9),1)


Independence_Level= c(63, 46.1, 53.4, 39.2, 62.8, 52.6)
barplot(Independence_Level, main = "Red: Northeast India (N=2266), Black: North India (N=3425),
        Green: East India (N=2163), Blue: West India (N=2114), Gray: South India (N=2442),
        All-India (12,410)", 
        xlab="Women's Level of Political Independence (Percentage of Women Who Said 'Voted Own')", 
        col=c("red","black", "darkgreen", "blue", "gray", "purple"), beside = TRUE)


#once again, let's put what we placed earlier in a cbind into a dataframe. 
dtest5 <- data.frame(Independence_Level)
dtest5

#Now using reshape2 to switch it from wide to long format. 
dtest5 <- melt(dtest5)
dtest5

#adding a new column called rowid as an identifying variable. 
dtest5$rowid <- 1:6
dtest5

#adding new column where I label women and men for each region one numerical value. This will help me color 
#in ggplot2. All the digits that are numbered the same in 'wtf' will be in the same color.
dtest5$wtf <- c(1,2,3,4,5,6)
dtest5

figure_5 <- ggplot(dtest5, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South", "All-India Level"), values=c("brown3", "grey69", "grey59", "grey49", "grey29", "navajowhite2")) + 
  xlab("") + ylab("") + ggtitle("Women's Level of Political Independence (% Female Who Said 'Voted Own')")
figure_5


#so we see that South Indian/NE women are quite 'independent', but does that translate into politics.
#not necessarily, just because women are more independent doesn't imply that they will campaign more..

#Variable 'LEVEL OF INTEREST IN POLITICS'
#I now subset and combine data where women who have "some interest" and a "great deal" of interest in 
#politics are combined so I am left with only two responses "Not interested" and "interested" 

NE_question_14 = North_East$q14
levels(NE_question_14)
levels(NE_question_14)[1] = 'Not Interested'
levels(NE_question_14)
levels(NE_question_14)[2]='Interested' 
levels(NE_question_14)
levels(NE_question_14)[3]='Interested' 
levels(NE_question_14)


N_question_14 = North_India$q14
levels(N_question_14)
levels(N_question_14)[1] = 'Not Interested'
levels(N_question_14)
levels(N_question_14)[2]='Interested' 
levels(N_question_14)
levels(N_question_14)[3]='Interested' 
levels(N_question_14)

E_question_14 = East_India$q14
levels(E_question_14)
levels(E_question_14)[1] = 'Not Interested'
levels(E_question_14)
levels(E_question_14)[2]='Interested' 
levels(E_question_14)
levels(E_question_14)[3]='Interested' 
levels(E_question_14)


W_question_14 = West_India$q14
levels(W_question_14)
levels(W_question_14)[1] = 'Not Interested'
levels(W_question_14)
levels(W_question_14)[2]='Interested' 
levels(W_question_14)
levels(W_question_14)[3]='Interested' 
levels(W_question_14)

S_question_14 = South_India$q14
levels(S_question_14)
levels(S_question_14)[1] = 'Not Interested'
levels(S_question_14)
levels(S_question_14)[2]='Interested' 
levels(S_question_14)
levels(S_question_14)[3]='Interested' 
levels(S_question_14)


prop.table(table(North_East$b3, NE_question_14),1)
prop.table(table(North_India$b3, N_question_14),1)
prop.table(table(East_India$b3, E_question_14),1)
prop.table(table(West_India$b3, W_question_14),1)
prop.table(table(South_India$b3, S_question_14),1)


Women_Interest_Politics = c(43.2, 28.3, 32.0, 15.9, 28.09)
Men_Interest_Politics = c(60.56, 52.77, 50.41, 38.61, 44.86)

d6 <- cbind(Men_Interest_Politics, Women_Interest_Politics)
barplot(d6, main = "Red: Northeast India, Black: North India,
        Green: East India, Blue: West India, Gray: South India", 
        xlab="Men and Women Interest in Politics By Region", 
        col=c("red","black", "darkgreen", "blue", "gray"), beside = TRUE)



#once again, let's put what we placed earlier in a cbind into a dataframe. 
dtest6 <- data.frame(Men_Interest_Politics, Women_Interest_Politics)
dtest6

#Now using reshape2 to switch it from wide to long format. 
dtest6 <- melt(dtest6)
dtest6

#adding a new column called rowid as an identifying variable. 
dtest6$rowid <- 1:10
dtest6

#adding new column where I label women and men for each region one numerical value. 
dtest6$wtf <- c(1,2,3,4,5,1,2,3,4,5)
dtest6

figure_6 <- ggplot(dtest6, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.7)) + 
  scale_fill_manual(name="", labels = c("Northeast","North","East","West","South"), values=c("brown3", "grey69", "grey59", "grey49", "grey29")) + 
  xlab("") + ylab("") + ggtitle("Men & Women Interest in Politics by Region)")
figure_6

##Women in the South, may be more liberated and independent but not that interested in politcs.
#regional effect- men and women are more interested in politics in the NE. But the fact that
#women can be so engaged is interesting. Now let's look at male attitudes towards women
#in politics and society. High levels of indepdenence corroborated by NFHS survey by the way (decisionmaking).
#Not as high in the south. 

##Table of attitudes- men and women on range of issues. Men more willing to encourage women in 
#politics; in addition, while cannot coontrol for APSA, can understand their views on regional loyalty and army.
#Men have somewhat more favorable attitudes towards women in politics in NE. 

#First let's combine into Disagrees and Agrees. I'm not interested in the "somewhat disagrees" etc.
NE_question_36e = North_East$q36e
levels(NE_question_36e)
levels(NE_question_36e)[1] = 'Disagree'
levels(NE_question_36e)
levels(NE_question_36e)[2]='Disagree' 
levels(NE_question_36e)
levels(NE_question_36e)[3]='Agree' 
levels(NE_question_36e)
levels(NE_question_36e)[2]='Agree' 
levels(NE_question_36e)

N_question_36e = North_India$q36e
levels(N_question_36e)
levels(N_question_36e)[1] = 'Disagree'
levels(N_question_36e)
levels(N_question_36e)[2]='Disagree' 
levels(N_question_36e)
levels(N_question_36e)[3]='Agree' 
levels(N_question_36e)
levels(N_question_36e)[2]='Agree' 
levels(N_question_36e)

E_question_36e = East_India$q36e
levels(E_question_36e)
levels(E_question_36e)[1] = 'Disagree'
levels(E_question_36e)
levels(E_question_36e)[2]='Disagree' 
levels(E_question_36e)
levels(E_question_36e)[3]='Agree' 
levels(E_question_36e)
levels(E_question_36e)[2]='Agree' 
levels(E_question_36e)

W_question_36e = West_India$q36e
levels(W_question_36e)
levels(W_question_36e)[1] = 'Disagree'
levels(W_question_36e)
levels(W_question_36e)[2]='Disagree' 
levels(W_question_36e)
levels(W_question_36e)[3]='Agree' 
levels(W_question_36e)
levels(W_question_36e)[2]='Agree' 
levels(W_question_36e)

S_question_36e = South_India$q36e
levels(S_question_36e)
levels(S_question_36e)[1] = 'Disagree'
levels(S_question_36e)
levels(S_question_36e)[2]='Disagree' 
levels(S_question_36e)
levels(S_question_36e)[3]='Agree' 
levels(S_question_36e)
levels(S_question_36e)[2]='Agree' 
levels(S_question_36e)

NE_question_36g = North_East$q36g
levels(NE_question_36g)
levels(NE_question_36g)[1] = 'Disagree'
levels(NE_question_36g)
levels(NE_question_36g)[2]='Disagree' 
levels(NE_question_36g)
levels(NE_question_36g)[3]='Agree' 
levels(NE_question_36g)
levels(NE_question_36g)[2]='Agree' 
levels(NE_question_36g)

N_question_36g = North_India$q36g
levels(N_question_36g)
levels(N_question_36g)[1] = 'Disagree'
levels(N_question_36g)
levels(N_question_36g)[2]='Disagree' 
levels(N_question_36g)
levels(N_question_36g)[3]='Agree' 
levels(N_question_36g)
levels(N_question_36g)[2]='Agree' 
levels(N_question_36g)

E_question_36g = East_India$q36g
levels(E_question_36g)
levels(E_question_36g)[1] = 'Disagree'
levels(E_question_36g)
levels(E_question_36g)[2]='Disagree' 
levels(E_question_36g)
levels(E_question_36g)[3]='Agree' 
levels(E_question_36g)
levels(E_question_36g)[2]='Agree' 
levels(E_question_36g)

W_question_36g = West_India$q36g
levels(W_question_36g)
levels(W_question_36g)[1] = 'Disagree'
levels(W_question_36g)
levels(W_question_36g)[2]='Disagree' 
levels(W_question_36g)
levels(W_question_36g)[3]='Agree' 
levels(W_question_36g)
levels(W_question_36g)[2]='Agree' 
levels(W_question_36g)

S_question_36g = South_India$q36g
levels(S_question_36g)
levels(S_question_36g)[1] = 'Disagree'
levels(S_question_36g)
levels(S_question_36g)[2]='Disagree' 
levels(S_question_36g)
levels(S_question_36g)[3]='Agree' 
levels(S_question_36g)
levels(S_question_36g)[2]='Agree' 
levels(S_question_36g)


NE_question_36j = North_East$q36j
levels(NE_question_36j)
levels(NE_question_36j)[1] = 'Disagree'
levels(NE_question_36j)
levels(NE_question_36j)[2]='Disagree' 
levels(NE_question_36j)
levels(NE_question_36j)[3]='Agree' 
levels(NE_question_36j)
levels(NE_question_36j)[2]='Agree' 
levels(NE_question_36j)

N_question_36j = North_India$q36j
levels(N_question_36j)
levels(N_question_36j)[1] = 'Disagree'
levels(N_question_36j)
levels(N_question_36j)[2]='Disagree' 
levels(N_question_36j)
levels(N_question_36j)[3]='Agree' 
levels(N_question_36j)
levels(N_question_36j)[2]='Agree' 
levels(N_question_36j)

E_question_36j = East_India$q36j
levels(E_question_36j)
levels(E_question_36j)[1] = 'Disagree'
levels(E_question_36j)
levels(E_question_36j)[2]='Disagree' 
levels(E_question_36j)
levels(E_question_36j)[3]='Agree' 
levels(E_question_36j)
levels(E_question_36j)[2]='Agree' 
levels(E_question_36j)

W_question_36j = West_India$q36j
levels(W_question_36j)
levels(W_question_36j)[1] = 'Disagree'
levels(W_question_36j)
levels(W_question_36j)[2]='Disagree' 
levels(W_question_36j)
levels(W_question_36j)[3]='Agree' 
levels(W_question_36j)
levels(W_question_36j)[2]='Agree' 
levels(W_question_36j)

S_question_36j = South_India$q36j
levels(S_question_36j)
levels(S_question_36j)[1] = 'Disagree'
levels(S_question_36j)
levels(S_question_36j)[2]='Disagree' 
levels(S_question_36j)
levels(S_question_36j)[3]='Agree' 
levels(S_question_36j)
levels(S_question_36j)[2]='Agree' 
levels(S_question_36j)

#Therefore, while tribal elite sixth schedule- this does not seem
#to translate to the men in the region. 
prop.table(table(North_East$b3, NE_question_36e),1)
prop.table(table(North_India$b3, N_question_36e),1)
prop.table(table(East_India$b3, E_question_36e),1)
prop.table(table(West_India$b3, W_question_36e),1)
prop.table(table(South_India$b3, S_question_36e),1)

prop.table(table(North_East$b3, NE_question_36j),1)
prop.table(table(North_India$b3, N_question_36j),1)
prop.table(table(East_India$b3, E_question_36j),1)
prop.table(table(West_India$b3, W_question_36j),1)
prop.table(table(South_India$b3, S_question_36j),1)


#What about the variable 'loytalty to region'?
#Perhaps one factor that encourages women's particpation in politics is the role of APSA. However, survey
#does not ask any questions- but does on loyalty to region, and funding for army. We see that in all categories,
#higher percentage loyal to region. 


#Army appears least popular in the Northeast compared to other regions. Interesting. 
prop.table(table(North_East$b3, North_East$q24i),1)
prop.table(table(North_India$b3, North_India$q24i),1)
prop.table(table(East_India$b3, East_India$q24i),1)
prop.table(table(West_India$b3, West_India$q24i),1)
prop.table(table(South_India$b3, South_India$q24i),1)



NE_question_24a = North_East$q24a
levels(NE_question_24a)
levels(NE_question_24a)[1] = 'Disagree'
levels(NE_question_24a)
levels(NE_question_24a)[2]='Disagree' 
levels(NE_question_24a)
levels(NE_question_24a)[3]='Agree' 
levels(NE_question_24a)
levels(NE_question_24a)[2]='Agree' 
levels(NE_question_24a)

N_question_24a = North_India$q24a
levels(N_question_24a)
levels(N_question_24a)[1] = 'Disagree'
levels(N_question_24a)
levels(N_question_24a)[2]='Disagree' 
levels(N_question_24a)
levels(N_question_24a)[3]='Agree' 
levels(N_question_24a)
levels(N_question_24a)[2]='Agree' 
levels(N_question_24a)

E_question_24a = East_India$q24a
levels(E_question_24a)
levels(E_question_24a)[1] = 'Disagree'
levels(E_question_24a)
levels(E_question_24a)[2]='Disagree' 
levels(E_question_24a)
levels(E_question_24a)[3]='Agree' 
levels(E_question_24a)
levels(E_question_24a)[2]='Agree' 
levels(E_question_24a)


W_question_24a = West_India$q24a
levels(W_question_24a)
levels(W_question_24a)[1] = 'Disagree'
levels(W_question_24a)
levels(W_question_24a)[2]='Disagree' 
levels(W_question_24a)
levels(W_question_24a)[3]='Agree' 
levels(W_question_24a)
levels(W_question_24a)[2]='Agree' 
levels(W_question_24a)

S_question_24a = South_India$q24a
levels(S_question_24a)
levels(S_question_24a)[1] = 'Disagree'
levels(S_question_24a)
levels(S_question_24a)[2]='Disagree' 
levels(S_question_24a)
levels(S_question_24a)[3]='Agree' 
levels(S_question_24a)
levels(S_question_24a)[2]='Agree' 
levels(S_question_24a)


prop.table(table(North_East$b3, NE_question_24a),1)
prop.table(table(North_India$b3, N_question_24a),1)
prop.table(table(East_India$b3, E_question_24a),1)
prop.table(table(West_India$b3, W_question_24a),1)
prop.table(table(South_India$b3, S_question_24a),1)


##24H

NE_question_24h = North_East$q24h
levels(NE_question_24h)
levels(NE_question_24h)[1] = 'Disagree'
levels(NE_question_24h)
levels(NE_question_24h)[2]='Disagree' 
levels(NE_question_24h)
levels(NE_question_24h)[3]='Agree' 
levels(NE_question_24h)
levels(NE_question_24h)[2]='Agree' 
levels(NE_question_24h)

N_question_24h = North_India$q24h
levels(N_question_24h)
levels(N_question_24h)[1] = 'Disagree'
levels(N_question_24h)
levels(N_question_24h)[2]='Disagree' 
levels(N_question_24h)
levels(N_question_24h)[3]='Agree' 
levels(N_question_24h)
levels(N_question_24h)[2]='Agree' 
levels(N_question_24h)

E_question_24h = East_India$q24h
levels(E_question_24h)
levels(E_question_24h)[1] = 'Disagree'
levels(E_question_24h)
levels(E_question_24h)[2]='Disagree' 
levels(E_question_24h)
levels(E_question_24h)[3]='Agree' 
levels(E_question_24h)
levels(E_question_24h)[2]='Agree' 
levels(E_question_24h)


W_question_24h = West_India$q24h
levels(W_question_24h)
levels(W_question_24h)[1] = 'Disagree'
levels(W_question_24h)
levels(W_question_24h)[2]='Disagree' 
levels(W_question_24h)
levels(W_question_24h)[3]='Agree' 
levels(W_question_24h)
levels(W_question_24h)[2]='Agree' 
levels(W_question_24h)

S_question_24h = South_India$q24h
levels(S_question_24h)
levels(S_question_24h)[1] = 'Disagree'
levels(S_question_24h)
levels(S_question_24h)[2]='Disagree' 
levels(S_question_24h)
levels(S_question_24h)[3]='Agree' 
levels(S_question_24h)
levels(S_question_24h)[2]='Agree' 
levels(S_question_24h)

#What about the variable 'minority rights'? Are women particularly concerned with fighting for those?
prop.table(table(North_East$b3, NE_question_24h),1)
prop.table(table(North_India$b3, N_question_24h),1)
prop.table(table(East_India$b3, E_question_24h),1)
prop.table(table(West_India$b3, W_question_24h),1)
prop.table(table(South_India$b3, S_question_24h),1)



##24I (ARMY)

NE_question_24i = North_East$q24i
levels(NE_question_24i)
levels(NE_question_24i)[1] = 'Disagree'
levels(NE_question_24i)
levels(NE_question_24i)[2]='Disagree' 
levels(NE_question_24i)
levels(NE_question_24i)[3]='Agree' 
levels(NE_question_24i)
levels(NE_question_24i)[2]='Agree' 
levels(NE_question_24i)

N_question_24i = North_India$q24i
levels(N_question_24i)
levels(N_question_24i)[1] = 'Disagree'
levels(N_question_24i)
levels(N_question_24i)[2]='Disagree' 
levels(N_question_24i)
levels(N_question_24i)[3]='Agree' 
levels(N_question_24i)
levels(N_question_24i)[2]='Agree' 
levels(N_question_24i)

E_question_24i = East_India$q24i
levels(E_question_24i)
levels(E_question_24i)[1] = 'Disagree'
levels(E_question_24i)
levels(E_question_24i)[2]='Disagree' 
levels(E_question_24i)
levels(E_question_24i)[3]='Agree' 
levels(E_question_24i)
levels(E_question_24i)[2]='Agree' 
levels(E_question_24i)


W_question_24i = West_India$q24i
levels(W_question_24i)
levels(W_question_24i)[1] = 'Disagree'
levels(W_question_24i)
levels(W_question_24i)[2]='Disagree' 
levels(W_question_24i)
levels(W_question_24i)[3]='Agree' 
levels(W_question_24i)
levels(W_question_24i)[2]='Agree' 
levels(W_question_24i)

S_question_24i = South_India$q24i
levels(S_question_24i)
levels(S_question_24i)[1] = 'Disagree'
levels(S_question_24i)
levels(S_question_24i)[2]='Disagree' 
levels(S_question_24i)
levels(S_question_24i)[3]='Agree' 
levels(S_question_24i)
levels(S_question_24i)[2]='Agree' 
levels(S_question_24i)

#AGAINST ARMY (most women in that region are against the army than other parts of the country)
prop.table(table(North_East$b3, NE_question_24i),1)
prop.table(table(North_India$b3, N_question_24i),1)
prop.table(table(East_India$b3, E_question_24i),1)
prop.table(table(West_India$b3, W_question_24i),1)
prop.table(table(South_India$b3, S_question_24i),1)

#What about religion? 
summary(North_East_Women$b7)
summary(North_Women$b7)
summary(East_Women$b7)
summary(West_Women$b7)
summary(South_Women$b7)

prop.table(table(North_East_Women$b7, North_East_Women$q7a),2)


#Let's run a few regressions. Religion doesn't matter, but associations do. 
#Employment matters- most women in the dataset are housewives. This confirms that for such actiivyt- need to 
#have autonomy from the household and be involvd in public space. These are highly public activities. 

##I create an index by combining all four variables. Critics may point out that two of the variables (attending meetings and 
#participating in rallies) are different from the other two (door to door canvassing and distributing leaflets). 
#For instance, one may assume that the latter two are more "individual" acts whereas the former function as a 
#collective. Nevertheless, for the purpose of this analysis and because all four variables are correlated, I
#equally weight all four in my composite index. For every three women on aveage at least one of them
#particpated in one of the activities (see table with summary statistics). 

#DV
political_campaigning <- (women$q7a=="Yes")+(women$q7b=="Yes")+(women$q7c=="Yes")+(women$q7e=="Yes")
round(mean(political_campaigning),4)
round(sd(political_campaigning),4)

#IV (need to subset and combine data before the regressions)
summary(women$b4)
Literacy <- ifelse(women$b4=='Non literate', 0, 1)
round(mean(Literacy),4)
round(sd(Literacy),4)

summary(women$q6)
Campaign_Interest <- ifelse(women$q6=='Somewhat'|women$q6=='Great deal', 1, 0)
round(mean(Campaign_Interest),4)
round(sd(Campaign_Interest),4)

summary(women$q9)
Independence <- ifelse(women$q9=='Voted own', 1, 0)
round(mean(Independence),4)
round(sd(Independence),4)

summary(women$q19)
Associations <- ifelse(women$q19=='Yes', 1, 0)
round(mean(Associations),4)
round(sd(Associations),4)

#dummy variable for Northeast
North_East_Region=ifelse(women$v1=='2: Arunachal Pradesh' | women$v1=='3: Assam' | women$v1=='14: Manipur' |
                                 women$v1=='15: Meghalaya' | women$v1=='16: Mizoram' | women$v1=='17: Nagaland'|
                                 women$v1=='23: Tripura' | women$v1=='21: Sikkim', 1, 0)
round(mean(North_East_Region),4)
round(sd(North_East_Region),4)


South_India_Region=ifelse(women$v1=='1: Andhra Pradesh' | women$v1=='10: Karnataka' | women$v1=='11: Kerala' |
                               women$v1=='22: Tamil Nadu' | women$v1=='32: Pondicherry', 1, 0)
round(mean(South_India_Region),4)
round(sd(South_India_Region),4)

summary(women$q14)
Politics_Interest <- ifelse(women$q14=='Some interest'|women$q14=='Great deal', 1, 0)
round(mean(Politics_Interest),4)
round(sd(Politics_Interest),4)

summary(women$b7)
Hinduism <- ifelse(women$b7=='Hindu', 1, 0)
round(mean(Hinduism),4)
round(sd(Hinduism),4)

summary(women$b7)
Islam <- ifelse(women$b7=='Muslim', 1, 0)
round(mean(Islam),4)
round(sd(Islam),4)

summary(women$b7)
Christianity <- ifelse(women$b7=='Christian', 1, 0)
round(mean(Christianity),4)
round(sd(Christianity),4)

summary(women$b6a)
Scheduled_Caste <- ifelse(women$b6a=='SC', 1, 0)
round(mean(Scheduled_Caste),4)
round(sd(Scheduled_Caste),4)

summary(women$b6a)
Scheduled_Tribe <- ifelse(women$b6a=='ST', 1, 0)
round(mean(Scheduled_Tribe),4)
round(sd(Scheduled_Tribe),4)

summary(women$b6a)
OBC <- ifelse(women$b6a=='OBC', 1, 0)
round(mean(OBC),4)
round(sd(OBC),4)

summary(women$q24a)
Subnationalism <- ifelse(women$q24a=='Somewhat agree'|women$q24a=='Fully agree', 1, 0)
round(mean(Subnationalism),4)
round(sd(Subnationalism),4)

summary(women$q24i)
Army_Spending <- ifelse(women$q24i=='Somewhat agree'|women$q24i=='Fully agree', 1, 0)
round(mean(Army_Spending),4)
round(sd(Army_Spending),4)

summary(women$q36c)
Reservation <- ifelse(women$q36c=='Somewhat agree'|women$q36c=='Fully agree', 1, 0)
round(mean(Reservation),4)
round(sd(Reservation),4)

summary(women$b5)
Employment <- ifelse(women$b5=='HOUSE-WIFE/HUSBANDS'|women$b5==' N.A.', 0, 1)
round(mean(Employment),4)
round(sd(Employment),4)

summary(women$b10)
Rural <- ifelse(women$b10=='Village', 1, 0)
round(mean(Rural),4)
round(sd(Rural),4)

summary(women$b19)
Low_Income <- ifelse(women$b19=='Up to Rs. 1000'|women$b19=='Rs. 1001- Rs. 2000'|women$b19==' Rs. 2001- Rs. 3000'
                     |women$b19=='Rs. 4001- Rs. 5000', 1, 0)
round(mean(Low_Income),4)
round(sd(Low_Income),4)

summary(women$b19)
Moderate_High_Income <- ifelse(women$b19=='Rs. 5001- Rs. 10,000'|women$b19=='Rs. 10,001- Rs. 20,000', 1, 0)
round(mean(Moderate_High_Income),4)
round(sd(Moderate_High_Income),4)

###############
regression1 = lm(political_campaigning~Literacy+Associations+
                   North_East_Region+North_East_Region*Scheduled_Tribe+South_India_Region+Hinduism+Islam+Christianity+Scheduled_Caste
                 +Scheduled_Tribe+OBC+Subnationalism+Army_Spending+Reservation
                 +Employment+Rural+Low_Income+Moderate_High_Income, data=women)
summary(regression1)
regression1$NEW_SE = vcovHC(regression1, type="HC1")
coeftest(regression1, regression1$NEW_SE)

#output
stargazer(regression1, type="text",
          out="regression1.txt")

################
#second regression of just Northeastern women. See below.
#DV
political_campaigning2 <- (North_East_Women$q7a=="Yes")+(North_East_Women$q7b=="Yes")+(North_East_Women$q7c=="Yes")+(North_East_Women$q7e=="Yes")
round(mean(political_campaigning2),4)
round(sd(political_campaigning2),4)

#IV
summary(North_East_Women$b4)
Literacy2 <- ifelse(North_East_Women$b4=='Non literate', 0, 1)
round(mean(Literacy2),4)
round(sd(Literacy2),4)

summary(North_East_Women$q6)
Campaign_Interest2 <- ifelse(North_East_Women$q6=='Somewhat'|North_East_Women$q6=='Great deal', 1, 0)
round(mean(Campaign_Interest2),4)
round(sd(Campaign_Interest2),4)

summary(North_East_Women$q9)
Independence2 <- ifelse(North_East_Women$q9=='Voted own', 1, 0)
round(mean(Independence2),4)
round(sd(Independence2),4)

summary(North_East_Women$q19)
Associations2 <- ifelse(North_East_Women$q19=='Yes', 1, 0)
round(mean(Associations2),4)
round(sd(Associations2),4)

summary(North_East_Women$q14)
Politics_Interest2 <- ifelse(North_East_Women$q14=='Some interest'|North_East_Women$q14=='Great deal', 1, 0)
round(mean(Politics_Interest2),4)
round(sd(Politics_Interest2),4)

summary(North_East_Women$b7)
Hinduism2 <- ifelse(North_East_Women$b7=='Hindu', 1, 0)
round(mean(Hinduism2),4)
round(sd(Hinduism2),4)

summary(North_East_Women$b7)
Islam2 <- ifelse(North_East_Women$b7=='Muslim', 1, 0)
round(mean(Islam2),4)
round(sd(Islam2),4)

summary(North_East_Women$b7)
Christianity2 <- ifelse(North_East_Women$b7=='Christian', 1, 0)
round(mean(Christianity2),4)
round(sd(Christianity2),4)

summary(North_East_Women$b6a)
Scheduled_Caste2 <- ifelse(North_East_Women$b6a=='SC', 1, 0)
round(mean(Scheduled_Caste2),4)
round(sd(Scheduled_Caste2),4)

summary(North_East_Women$b6a)
Scheduled_Tribe2 <- ifelse(North_East_Women$b6a=='ST', 1, 0)
round(mean(Scheduled_Tribe2),4)
round(sd(Scheduled_Tribe2),4)

summary(North_East_Women$b6a)
OBC2 <- ifelse(North_East_Women$b6a=='OBC', 1, 0)
round(mean(OBC2),4)
round(sd(OBC2),4)

summary(North_East_Women$q24a)
Subnationalism2 <- ifelse(North_East_Women$q24a=='Somewhat agree'|North_East_Women$q24a=='Fully agree', 1, 0)
round(mean(Subnationalism2),4)
round(sd(Subnationalism2),4)

summary(North_East_Women$q24h)
Minority_Rights2 <- ifelse(North_East_Women$q24h=='Somewhat agree'|North_East_Women$q24h=='Fully agree', 1, 0)
round(mean(Minority_Rights2),4)
round(sd(Minority_Rights2),4)

summary(North_East_Women$q24i)
Army_Spending2 <- ifelse(North_East_Women$q24i=='Somewhat agree'|North_East_Women$q24i=='Fully agree', 1, 0)
round(mean(Army_Spending2),4)
round(sd(Army_Spending2),4)

summary(North_East_Women$q36c)
Reservation2 <- ifelse(North_East_Women$q36c=='Somewhat agree'|North_East_Women$q36c=='Fully agree', 1, 0)
round(mean(Reservation2),4)
round(sd(Reservation2),4)

summary(North_East_Women$b5)
Employment2 <- ifelse(North_East_Women$b5=='HOUSE-WIFE/HUSBANDS'|North_East_Women$b5==' N.A.', 0, 1)
round(mean(Employment2),4)
round(sd(Employment2),4)

summary(North_East_Women$b10)
Rural2 <- ifelse(North_East_Women$b10=='Village', 1, 0)
round(mean(Rural2),4)
round(sd(Rural2),4)

summary(North_East_Women$b19)
Low_Income2 <- ifelse(North_East_Women$b19=='Up to Rs. 1000'|North_East_Women$b19=='Rs. 1001- Rs. 2000'|North_East_Women$b19==' Rs. 2001- Rs. 3000'
                     |North_East_Women$b19=='Rs. 4001- Rs. 5000', 1, 0)
round(mean(Low_Income2),4)
round(sd(Low_Income2),4)

summary(North_East_Women$b19)
Moderate_High_Income2 <- ifelse(North_East_Women$b19=='Rs. 5001- Rs. 10,000'|North_East_Women$b19=='Rs. 10,001- Rs. 20,000', 1, 0)
round(mean(Moderate_High_Income2),4)
round(sd(Moderate_High_Income2),4)

summary(North_East_Women$b5)
Employment2 <- ifelse(North_East_Women$b5=='HOUSE-WIFE/HUSBANDS'|North_East_Women$b5==' N.A.', 0, 1)
round(mean(Employment2),4)
round(sd(Employment2),4)


########
regression2 = lm(political_campaigning2~Literacy2+Associations2+
                   Hinduism2+Islam2+Christianity2+Scheduled_Caste2
                 +Scheduled_Tribe2+OBC2+Subnationalism2+Army_Spending2+Reservation2
                 +Employment2+Rural2+Low_Income2+Moderate_High_Income2+Employment2, data=North_East_Women)
summary(regression2)
regression2$NEW_SE = vcovHC(regression2, type="HC1")
coeftest(regression2, regression2$NEW_SE)

#output
stargazer(regression2, type="text",
           out="regression2.txt")
