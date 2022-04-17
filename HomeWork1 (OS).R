
install.packages("readxl")
install.packages("xlsx")
install.packages('openxlsx')
install.packages('ggplot2')
install.packages("waterfalls")
library(waterfalls)
library(ggplot2)
library(scales)



require(openxlsx)
require(ggplot2)
require(data.table)

EVDS=read_excel("EVDS(HW).xlsx")
str(EVDS)

EVDS$Tarih <- as.Date(EVDS$Tarih, format = '%Y-%m-%d')
EVDS$Month=month(EVDS$Tarih)
EVDS$Year=year(EVDS$Tarih)

summary(EVDS)
str(EVDS)


# Change in Dollar against Turkish Lira

ggplot(EVDS,aes(x=Year,y=USDTL/12))+ geom_bar(stat="identity",fill="#0FEC4951")+
  xlab("Period") + ylab("Dollar Exhange Rate")+ ggtitle("Total Full Year Dollar Exchange Rate in Turkey between 2018 & 2021")



# Food Price Change Over Time (2018-2021)

install.packages("quantmod")
install.packages("dplyr")
library(quantmod)
library(dplyr)
library(ggplot2)
library(scales)



ggplot(EVDS, aes(x = Tarih, y = GIDA)) +
  geom_line(color = "indianred3", size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '3 years', 
               labels = scales::date_format('%Y-%m-%d')) +
  labs(title = "Food Price Index Change",
       subtitle = "From 2018 to 2021",
       x = "",
       y = "Food Price Index") +
  theme_minimal()



#Price Index Increase on Basic Necessities & Living Cost Increase

cols <- c("GIDA","ENERJI","ISTANBUL")

ggplot(EVDS)+
  geom_line(aes(x=Tarih, y=GIDA, color="Food Price Index"))+
  geom_line(aes(x=Tarih, y=ENERJI ,color="Household Energy Price Index"))+
  geom_line(aes(x=Tarih, y=ISTANBUL , color= "Real Estate Price Index"))+

    labs(title = "Price Index Increase on Basic Necessities",
         subtitle = "From 2018 to 2021",
         x = "Date",
         y = "Food - Energy and Household Price Index") +
      theme_minimal()


# Sales vs Price in New Residence and Real Estate 

ggplot(data=EVDS, aes(x=KONUTSATIS,y=factor(ISTANBUL))) + geom_point(aes(color=Year)) +
  xlab("New Residence Sales Figure") + ylab("Real Estate Price Index for Istanbul")+ ggtitle("New Residence Sales and Change in Real Estate Price between 2018 & 2021")


install.packages("readxl")
install.packages("xlsx")

library(readxl)
library(xlsx)
require(ggplot2)
require(data.table)



#Monthly Distribution of Turkey Tourist Arrivals between 2018-2021

VISIT=read_excel("VISIT.xlsx")
str(VISIT)

VISIT$Tarih <- as.Date(VISIT$Tarih, format = '%Y-%m-%d')
VISIT$Month=month(VISIT$Tarih)
VISIT$Year=year(VISIT$Tarih)
str(VISIT)


theme_set(theme_classic())

g <- ggplot(VISIT, aes(x=factor(month(Tarih)),y=TOPLAM))
g + geom_boxplot(aes(fill=factor(month(Tarih)))) + 
  labs(title="Turkey Tourist Arrivals between 2018-2021", 
       subtitle="Monthly Distribution",
       caption="Source: mpg",
       x="Number Of Arrivals",
       y="Month")

library(ggplot2)
theme_set(theme_classic())



# Density Comparison Between Europe and Total Arrivals in Turkey

install.packages("viridis")
install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)


ggplot(VISIT)+
  geom_density(aes(TOPLAM, fill = "TOPLAM"), alpha = .4)+
  geom_density(aes(EUROPE, fill = "Europe"),alpha = .4)+
  
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="right"
  ) +
  ylab("") +
  xlab("Assigned Probability (%)")
