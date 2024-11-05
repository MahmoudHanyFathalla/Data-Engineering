setwd("C:/rere/R_Data")
wbcd <- read.csv("wisc_bc_data.csv",stringsAsFactors = FALSE)
str(wbcd)
View (wbcd)
table(wbcd$diagnosis)
wbcd <-wbcd[-1]
#many  R machine learning classifiers requires the target feature to be coded as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis,levels= c("B","M"),
                         labels =c( "bengin","Malignant"))
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
wbcd_train <- wbcd_n[1:469, ] #80% mn el rows w kol el col. b3mlha trainig
wbcd_test <- wbcd_n[470:569, ] #20% b3mlhom testing
wbcd_train_labels <- wbcd[1:469,1] #b3ml train le column el diagnosis bs
wbcd_test_labels <-wbcd[470:569,1] #b3ml test le column el diagnosis bs
wbcd_test_labels
install.packages("class")
library(class)
wbcd_test_pred <- knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels, k=21) #ana hena
#b3ml predictions results ashan akarenha bl results el ha2e2ia ely el dactra tl3olha w akaren el accuracy bt3t my predictions
wbcd_test_pred
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)



data()
install.packages("tidyverse")
library(tidyverse)
View(starwars)
#filter height to be bigger than 150 and the mass smaller than 200
#put a new col "height in meters" and divide it by 100
#select only the height in meter and the mass
#sort  the mass from small to bigb values
starwars %>% 
  filter(height>150 & mass <200) %>%  #filtering
  mutate(height_in_meters=height/100) %>% #new column
  select(height_in_meters,mass) %>% 
  arrange(-mass) %>%  #hyrateb mn soghyar ll kbeer laken law ktbt -mass hy3ml mn kber lel soghyr
  view()



getwd() 
setwd("C:/rere/R_Data")
getwd()
wbcd <- read.csv("wisc_bc_data.csv",stringsAsFactors = FALSE)
str(wbcd) #structure and dataframe and summary
wbcd <-wbcd[-1] #3ashan ashel awel column bs
str(wbcd)
head(wbcd) #byshed awel 6 values ka default
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c("B","M"),labels = c("bengin","malignant")) #keda hawlt kol b le benign w kol m le malignant
table(wbcd$diagnosis) #bageeb 3adad kol benign w kol malignant
normalize <- function(x){
 return((x-min(x)) / (max(x)-min(x)))
} #function ashan akhali kol elarkam msh akbar mn 1 w msh asghar mn 0
normalize(c(1,2,3,4,5))
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize)) #ayza ahawel el table kolo lel normalized phase khabta whda msh col col
View(wbcd_n)



install.packages("readr",include_dependencies = TRUE)
install.packages("dplyr",include_dependencies = TRUE)
library(readr)
library(dplyr)
getwd()
setwd("C:/rere/R_Data")
df <- read.csv("airline_2m.csv",stringsAsFactors = FALSE)
View(df)
df <- df[1:500000,]
df <- subset(df,select = c('Year',
                           'Month',
                           'Reporting_Airline',
                           'OriginCityName',
                           'DestCityName','ArrDelayMinutes',
                           'AirTime','DepDelayMinutes','Distance'))
colnames(df)
df$AirTime
#write.csv(df,"C:\\SAMPLEAIRLINE.csv")
mean(df$Year)
mean(df$DepDelayMinutes,na.rm = T) #E7seb el mean aknk msh shayef el null value
avgDelay <- tapply(df$DepDelayMinutes, df$Reporting_Airline,mean, na.rm = T)
avgDelay
delay <-data.frame(airlines = names(avgDelay), average = avgDelay)
View(delay)
df%>%
  filter(Year == 2010) %>%
  select(Year,DestCityName , AirTime)
df%>%
  group_by(Reporting_Airline)%>%
  summarize(DepDelay = mean (DepDelayMinutes,na.rm=T) ,ArrDelay = mean(ArrDelayMinutes, na.rm = T))%>%
  arrange(desc(DepDelay),desc(ArrDelay))
df%>%
  filter(Month == 1 | Month == 2 & AirTime >0) %>%
  mutate(speed = Distance/AirTime) %>%
  select (Month , Distance , AirTime ) %>%
  arrange(speed)%>%
  head(3)



#filtering data 
#%o%
library(dplyr)
data(mtcars)
xx<- filter(mtcars, mpg>20)
yy <-select(xx,mpg,hp)  #in 2 individual steps
result_2_usual <-select(filter(mtcars,mpg>20),mpg,hp) #in one line
result_pipe <- mtcars%>% #using pipe
  filter(mpg>20) %>%
  select(mpg,hp)
#summarizing data
library(dplyr)
data("mtcars")
xx <-group_by(mtcars,cyl)
result_2a_usual <- summarize(xx,avg_mpg=mean(mpg))
#el grouping ashan law ayza a3ml function ala kaza cell be nafs el value
#in one line
result_2a_usual<-summarise(group_by(mtcars,cyl),avg_mpg=mean(mpg))
#using pipe operator
result_pipe <- mtcars %>% 
  group_by(cyl) %>% 
  summarize(avg_mpg=mean(mpg))




#creating new columns
library(dplyr)
data(mtcars)
result_1_usual <-mutate(mtcars,weight_kg=wt*2)
result_pipe <- mtcars %>% 
  mutate(weight_kg=wt*2)
#arrange and sort
result_pipe <-mtcars %>% 
  arrange(desc(hp)) %>% 
  head(10)

result_1_pipe <-mtcars %>% 
  arrange(hp) %>% 
  head(10)
#law mktbtsh desc htratbha ascending as a default
result_usual <-head(arrange(mtcars,desc(hp),10) #without pipe in one line
#combining multiple functions
result_6_pipe<-mtcars %>% 
  filter(cyl==6) %>% 
  summarize(avg_hp=mean(hp),avg_mpg=mean(mpg))
#without pipe
result_6_usual <- summarize(filter(mtcars,cyl==6),avg_hp=mean(hp),avg_mpg=mean(hp))
#lapply #(bt apply list)
#sapply #(nt apply vector)


num_list <- list(a=1:5,b=6:10 , c=11:15)
num_list
mean_function <- function(x)mean(x)
result_lapply<-lapply(num_list,mean_function)  #hy7seb el mean le column column
result_lapply
result_sapply<-sapply(num_list,mean_function)
result_sapply



getwd()
setwd("E:/study/Big data")
getwd()
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)
head(usedcars)
mean(usedcars$price)
range(usedcars$price)
median(c(1,2,3,4))#(2+3)/2
#quantiles are values that split sorted data into equal parts.
#quantiles (4 equal parts)
quantile(usedcars$price)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])




Name <- 1
name<-0
my_number <- 1
my_number <- 999
ls()
die<- 1:6
die -1
die/2
die + 1:4 # + 1 2 3 4 1 2 recycling bec.1:4 longer object length is not a multiple of shorter object length
die * die
die %*% die # sum(die*die)
die %o% die #outer mul.
mean(1:6)
factorial(3)
mean(die)
round(mean(die))
args(round)
round(pi, digits = 3)
sample(die, size=2) #get 2 random numbers from die
sample(die, size=2,replace=TRUE)#with replacement where the 2 number could be the same


setwd("e:/R_Data")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
View(wbcd)
setwd("e:/R_Data")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
View(wbcd)
str(wbcd)
View(wbcd)
table(wbcd$diagnosis)
View(wbcd)
wbcd <-wbcd[-1]
View(wbcd)
wbcd$diagnosis <-factor(wbcd$diagnosis,levels = c("B", "M"),
labels ="Benign", "Malignant")
table(wbcd$diagnosis)
wbcd$diagnosis <-factor(wbcd$diagnosis,levels = c("B", "M"),
labels =c("Benign", "Malignant"))
table(wbcd$diagnosis)
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
table(wbcd$diagnosis)
wbcd <-wbcd[-1]
wbcd$diagnosis <-factor(wbcd$diagnosis,levels = c("B", "M"),
labels =c("Benign", "Malignant"))
table(wbcd$diagnosis)
View(wbcd)
normalize <-function(x){
return((x - min(x))/(max(x)- min(x)))
}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
wbcd_n1<-lapply(wbcd[2:31], normalize)
wbcd_n1
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
View(wbcd_n)
View(wbcd_n)
wbcd_train <-wbcd_n[1:469, ]
wbcd_train <-wbcd_n[470:569, ]
wbcd_train <-wbcd_n[1:469, ]
wbcd_test <-wbcd_n[470:569, ]
View(wbcd)
View(wbcd_n)
wbcd_train_labels <-wbcd[1:469, 1]
wbcd_train_labels
wbcd_test_labels <-wbcd[470:569, 1]
wbcd_test_labels
install.packages("class")
library(class)
wbcd_test_pred <-knn(train=wbcd_train, test=wbcd_test,
cl=wbcd_train_labels, k=21)
wbcd_test_pred
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)
data()
install.packages("tidtverse")
library(tidyverse)
install.packages("tidyverse")
library(tidyverse)
view(starwars)
starwars %>%
filter(height>150 & mass<200) %>%
mutate(height_in_meters=height/100) %>%
select(height_in_meters, mass) %>%
arrange(mass) %>%
view()


setwd("e:/R_Data")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
table(wbcd$diagnosis)
wbcd <-wbcd[-1]
wbcd$diagnosis <-factor(wbcd$diagnosis,levels = c("B", "M"), 
                        labels =c("Benign", "Malignant"))
table(wbcd$diagnosis)
normalize <-function(x){
  return((x - min(x))/(max(x)- min(x)))
}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
wbcd_n1<-lapply(wbcd[2:31], normalize)
wbcd_n1
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))

wbcd_train <-wbcd_n[1:469, ]
wbcd_test <-wbcd_n[470:569, ]

wbcd_train_labels <-wbcd[1:469, 1]
wbcd_train_labels
wbcd_test_labels <-wbcd[470:569, 1]
wbcd_test_labels

install.packages("class")
library(class)
wbcd_test_pred <-knn(train=wbcd_train, test=wbcd_test, 
                     cl=wbcd_train_labels, k=21)
wbcd_test_pred

install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)



#Matrices
m<- matrix(c(1,2,3,4, 5, 6), nrow = 2)
m
n<- matrix(c(1,2,3,4, 5, 6), ncol = 2)
n
x<- matrix(c(1,2,3,4, 5, 6), ncol = 2, byrow = TRUE)
x

#Data Frames
subject_name <- c("ahmed", "Mona", "Mostafa")
temprature <- c(37.1, 37.6, 39.4)
flu_status <- c(FALSE, FALSE, TRUE)
gender <- c("Male", "Female", "Male")
blood <- factor(c("O", "AB", "A"),
                levels=c("A", "B", "AB", "O") )
blood
symptoms <- factor(c("MILD", "SEVERE", "MODERATE"),
                  levels=c("MILD","MODERATE", "SEVERE" ),
                  ordered = TRUE)
pt_data <- data.frame(subject_name,temprature, flu_status, gender,blood, symptoms)
pt_data
pt_data$subject_name
pt_data[c("temprature", "flu_status")]
pt_data[c(2:3)]