
#adding necessary libraries

library(readxl)
library(ggplot2)
library(data.table)

EVDS=read_xlsx("/Users/okansayar/Desktop/R/RStudio Directory/EVDS(HW).xlsx")
str(EVDS)

#Format Date
EVDS$Tarih <- as.Date(EVDS$Tarih, format = '%Y-%m-%d')
EVDS$Month=month(EVDS$Tarih)
EVDS$Year=year(EVDS$Tarih)

summary(EVDS)
str(EVDS)


# Change in Dollar against Turkish Lira
# Average currency change for USD/TL in every year between 2018-2021

ggplot(EVDS,aes(x=Year,y=USDTL/12))+ geom_bar(stat="identity",fill="#0FEC4951")+
  xlab("Period") + ylab("Dollar Exhange Rate")+ ggtitle("Total Full Year Dollar Exchange Rate in Turkey between 2018 & 2021")



# Food Price Change Over Time (2018-2021)

#The Food Index showing a clear increase in food prices in Turkey. 
#Real data has been showed with red line. Time period as a scaled 3 years.
#Smooth line also added the see the increase as well

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
#This graph indicate the Food, Energy and Real Estate Price increase in a place. For calculating the price increase on basic needs.

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
# The different colored point change with the year effect. And position of point in y colon shows the Price index.
# This graph combine the new residence sales figures wit price change over years form 2018 to 2021

ggplot(data=EVDS, aes(x=KONUTSATIS,y=factor(ISTANBUL))) + geom_point(aes(color=Year)) +
  xlab("New Residence Sales Figure") + ylab("Real Estate Price Index for Istanbul")+ ggtitle("New Residence Sales and Change in Real Estate Price between 2018 & 2021")


# Monthly Distribution of Turkey Tourist Arrivals between 2018-2021
# The VISIT data file includes General tourist arrival numbers for a 3 years period. So, ı would like to demonstrate the month distrubution of arrivals.
# Clearly summer season has a a peak number and also August is the most popular month among tourists.

VISIT=read_excel("/Users/okansayar/Desktop/R/RStudio Directory/VISIT.xlsx")
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


# Density Comparison Between Europe and Total Arrivals in Turkey
# Comparison between total visit of Turkey against Density of European visitors.
# Graph demonstrate European visitors, have a big ratio in total tourist visitors.


ggplot(VISIT, aes(x=TOPLAM))+
geom_histogram(aes(y=..density..), colour="black", fill="white")+
geom_density(fill="red", alpha=0.4)+
  
  theme(
    legend.position="right"
  ) +
  ylab("") +
  xlab("Assigned Probability (%)")
