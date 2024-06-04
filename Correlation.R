#correlation analysis

library(readxl)

data=read_xlsx("/Users/Desktop/Data.xlsx", sheet = "2020")

#BASELINE
#HbA1c vs BMI
data1<-data.frame(data$HbA1c, data$BMI)
model <- lm(data1$data.BMI ~ data1$data.HbA1c, data = data1)
plot(data1$data.HbA1c, data1$data.BMI, main = "HbA1c vs BMI", #scatterplot
     xlab = "BMI",ylab = "HbA1c", abline(model,lty = 2, col = "red",colors(distinct = TRUE)))
summary(model)

#1 YEAR
data=read_xlsx("/Users/Desktop/Data.xlsx", sheet = "2021-22")

#HbA1c vs BMI
data1<-data.frame(data$HbA1c, data$BMI)
model <- lm(data1$data.BMI ~ data1$data.HbA1c, data = data1)
plot(data1$data.HbA1c, data1$data.BMI, main = "HbA1c vs BMI", #scatterplot
     xlab = "BMI",ylab = "HbA1c", abline(model,lty = 2, col = "red",colors(distinct = TRUE)))
summary(model)

#2 YEAR

data=read_xlsx("/Users/Desktop/Data.xlsx", sheet = "2022-23")

#HbA1c vs BMI
data1<-data.frame(data$HbA1c, data$BMI)
model <- lm(data1$data.BMI ~ data1$data.HbA1c, data = data1)
plot(data1$data.HbA1c, data1$data.BMI, main = "HbA1c vs BMI", #scatterplot
     xlab = "BMI",ylab = "HbA1c", abline(model,lty = 2, col = "red",colors(distinct = TRUE)))
summary(model)

#3 YEAR

data=read_xlsx("/Users/Desktop/Data.xlsx", sheet = "2023-24")

#HbA1c vs BMI
data1<-data.frame(data$HbA1c, data$BMI)
model <- lm(data1$data.BMI ~ data1$data.HbA1c, data = data1)
plot(data1$data.HbA1c, data1$data.BMI, main = "HbA1c vs BMI", #scatterplot
     xlab = "BMI",ylab = "HbA1c", abline(model,lty = 2, col = "red",colors(distinct = TRUE)))
summary(model)
