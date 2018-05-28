library(dplyr)
library(lubridate)
View(economics)

eco <- economics
eco$rate <- (eco$unemploy / eco$pop) * 100 
View(eco)
brks <- eco$date[seq(1,length(eco$date),12)]
lbls <- lubridate::year(brks)
chart <- ggplot(eco,aes(x=date,y=rate)) + geom_line()
chart+scale_x_date(labels = lbls,breaks = brks) + theme(axis.text.x = element_text(angle = 90)) 

data <- EuStockMarkets
View(data)

str(data)

plot.ts(DAX)
DAX <- data[,1]
plot(DAX,main="DAX Daily Closing Value",col="Blue")
install.packages("ISLR")
library(ISLR)
View(Hitters)
install.packages("quantmod")
library(quantmod)
library(plotly)
# to connect to yahoo finance and to get the apple stocks from the begining to previous day data
getSymbols("AAPL",src = "yahoo")

df <- data.frame(Date=index(AAPL),coredata(AAPL))
View(df1)
df1 <- tail(df,30)
brks <- df1$Date[seq(1,length(df1$Date),12)]
lbls <- lubridate::year(brks)
str(df1)
ggplot(df1,aes(x=Date)) +
  geom_line(aes(y=AAPL.Open),col="Black",size =1.2) +
  geom_line(aes(y=AAPL.High),col="Green",size =1.2) +
  geom_line(aes(y=AAPL.Low),col="Red",size =1.2) +
  geom_line(aes(y=AAPL.Close),col="Orange",size =1.2) 

# creating a candlestick chart using plotly by different colors
p <- plot_ly(data = df1,x = ~Date,type = "candlestick",open = ~AAPL.Open,close=~AAPL.Close,
             high=~AAPL.High,low=~AAPL.Low) %>% layout(title="Basic Candlestick Chart")


cost <- read.csv("E:/Term 2/Data Visualization/Data Set/Cost per event and cost per athlete in the Olympics .csv")

# to identity the cost per event in the olympics category(type) wise

View(cost)

data_final <- cost %>% group_by(Type) %>% summarise(Total_cost = sum(Cost.per.event..mio..USD))       

pie = plot_ly(data_final,type = 'pie',labels = ~Type,values=~Total_cost,
              textposition = "inside",textinfo="label+percent",showlegend=F,
              hoverinfo ="text",text=~paste('$',Total_cost,'millions')) %>% 
  layout(title="Expense on Olympics")
