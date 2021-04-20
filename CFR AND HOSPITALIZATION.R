
hos_UK <- read.csv("C:\\Users\\Laplace\\Documents\\R work in CityU\\data\\hospital data for UK\\data_2021-Apr-11.csv", header = T)

hos_Australia <- read.table("C:\\Users\\Laplace\\Documents\\R work in CityU\\data\\hospital data for Austrila\\hos_Austrila.txt", header = T)

hos_South_Africa <- read.csv("C:\\Users\\Laplace\\Documents\\R work in CityU\\data\\hospital data for South Africa\\covid-19_hospital.csv", header = T)

hos_Australia$Date <- as.Date(paste(rep("2020/",188), as.character(hos_Australia$Date)), format = "%Y/ %d/%m")
hos_South_Africa$Date <- as.Date(as.character(hos_South_Africa$date),  format = "%d-%m-%Y")
library(lubridate)
library(dplyr)
library(ggplot2)
hos_UK$Date <- ymd(as.character(hos_UK$date))

mydata <- read.csv("C:\\Users\\Laplace\\Documents\\R work in CityU\\SIR epidemics\\WHO-COVID-19-global-data.csv", sep = ",")
names(mydata)[1] <- "Date_reported"
mydata$Date_reported <- as.Date(as.character(mydata$Date_reported), format = "%Y/%m/%d")

UK <- mydata %>%
  filter(Date_reported >= as.Date("20200327", format = "%Y%m%d") & Date_reported <= as.Date("20210331", format = "%Y%m%d")) %>%
  filter(Country == "The United Kingdom")
CFR_UK <- data.frame(Date_reported = UK$Date_reported, CFR = UK$New_deaths/UK$New_cases)
hos_num <- hos_UK %>%
  filter(Date >= as.Date("20200327", format = "%Y%m%d") & Date <= as.Date("20210331", format = "%Y%m%d"))

hos_CFR_UK <- data.frame(Date = hos_num$Date, Cases_in_hospital = hos_num$hospitalCases, CFR = CFR_UK$CFR)

#write.csv(hos_CFR_UK, "C:\\Users\\Laplace\\Desktop\\hos_CFR_UK.csv")

Australia <- mydata %>%
  filter(Date_reported >= as.Date("20200407", format = "%Y%m%d") & Date_reported <= as.Date("20201011", format = "%Y%m%d")) %>%
  filter(Country == "Australia")
CFR_Australia <- data.frame(Date_reported = Australia$Date_reported, CFR = Australia$New_deaths/Australia$New_cases)
hos_num_Australia <- hos_Australia %>%
  filter(Date >= as.Date("20200407", format = "%Y%m%d") & Date <= as.Date("20201011", format = "%Y%m%d"))

hos_CFR_Australia <- data.frame(Date = hos_num_Australia$Date, Cases_in_hospital = hos_num_Australia$number, CFR = CFR_Australia$CFR)

#write.csv(hos_CFR_Australia, "C:\\Users\\Laplace\\Desktop\\hos_CFR_Australia.csv")

library(smooth)
library(Mcomp)
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

hos_CFR_UK$moving_average <- mav(hos_CFR_UK$CFR,5)

hos_CFR_Australia$CFR[which(hos_CFR_Australia$CFR == "NaN")] <- 0
hos_CFR_Australia$moving_average <- mav(hos_CFR_Australia$CFR,5)

#write.csv(hos_CFR_UK, "C:\\Users\\Laplace\\Desktop\\hos_CFR_UK1.csv")
#write.csv(hos_CFR_Australia, "C:\\Users\\Laplace\\Desktop\\hos_CFR_Australia1.csv")


South_Africa <- mydata %>%
  filter(Date_reported >= as.Date("20200524", format = "%Y%m%d") & Date_reported <= as.Date("20200912", format = "%Y%m%d")) %>%
  filter(Country == "South Africa")
CFR_South_Africa <- data.frame(Date_reported = South_Africa$Date_reported, CFR = South_Africa$New_deaths/South_Africa$New_cases)
CFR_South_Africa$moving_average <- mav(CFR_South_Africa$CFR,5)
write.csv(CFR_South_Africa, "C:\\Users\\Laplace\\Desktop\\CFR_South_Africa.csv")
write.csv(hos_South_Africa, "C:\\Users\\Laplace\\Desktop\\hos_South_Africa.csv")
