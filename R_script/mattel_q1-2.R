rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(forecast)

df<- read_csv("data.csv")
names(df)
str(df)

median_Sale_price_2006 <- 
  df %>%
  filter(YrSold==2006) %>%
  filter(GrLivArea>=2000) %>%
  group_by(Neighborhood) %>%
  summarise(median_sale_price= median(SalePrice))

bbplot<- 
  df %>%
  filter(YrSold==2006) %>%
  filter(GrLivArea>=2000) %>%
  ggplot( aes(x=Neighborhood, y= SalePrice, fill=Neighborhood ))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2)+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45)) 
  
  
ggplotly(bbplot)


############################################
############################################

seasonality <-
  df %>%
  mutate(Sold_date = make_datetime(YrSold, MoSold)) %>%
  group_by(Sold_date ) %>%
  summarize(`Number of house sold`= n())


p<- seasonality %>% 
  ggplot(aes(x=Sold_date, y= `Number of house sold`)) + 
  geom_line(color = 'rosybrown',linetype = "dashed" , size= 0.3) +
  geom_point(color = "rosybrown2", size = 1) +
  scale_x_datetime(labels = date_format("%Y-%m"), 
                   breaks = date_breaks("3 months")) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Number of houses sold") +
  xlab("")

ggplotly(p)


myts <- ts(seasonality$`Number of house sold`, frequency=12, start = c(2006, 01), end = c(2010, 07))

myds_month <- decompose(myts)
myds_month
plot(myds_month)


#Then we can create a model using tslm
# We can model using trend, season and random

my_df_ts <- data.frame(Houses_sold = myts, as.numeric(time(myts)))
names(my_df_ts) <- c("Houses_sold", "time")

mymodel <- tslm(Houses_sold~season+trend,my_df_ts)

# And forecast using this same model
# We are going to predict the next 10 years
# We can see the ascending trend
my_fc <- forecast(mymodel)
my_fc
autoplot(my_fc)+
  theme_minimal()

