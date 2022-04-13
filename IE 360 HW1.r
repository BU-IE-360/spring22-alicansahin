setwd("C:/Users/alica/OneDrive/Masaüstü/ali belgeler/BOUN/3.Sınıf/2.DÖNEM/IE 360/HW1")
#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("corrplot")
library(ggplot2)
library(data.table)
#install.packages("https://cran.r-project.org/src/contrib/Archive/lubridate/lubridate_1.7.4.tar.gz",repos=NULL)

library(lubridate)
sold_house_data = read.csv("toplam_konut_satış.csv")
setDT(sold_house_data)
sold_house_data[,year_of_date:=year(ym(tarih))]
sold_house_data[,month_of_date:=month(ym(tarih))]
sold_house_data
yearly_sold = sold_house_data[,list(total = sum(toplam_satış)),list(year_of_date)]
yearly_sold


ggplot(yearly_sold,aes(x=year_of_date,y= total)) + 
geom_line() + ylab("Total Sold House") + xlab("Years") + ggtitle("Total Sold House in Spesific Years")


ggplot(sold_house_data,aes(x=factor(year_of_date),y=toplam_satış)) + geom_boxplot(aes(fill = factor(year_of_date)))+ labs(fill="Years")+
xlab("Years") + ylab("Total Sold House") + ggtitle("Boxplot of Total Sold House vs Years")
ggplot(sold_house_data,aes(x=month_of_date,y=toplam_satış)) +
geom_line() + facet_wrap(~year_of_date) + scale_x_discrete(limits = c(1:12))+xlab("Months") + ylab("Total Sold House")


konut_kredi_faiz = read.csv("konut_kredi_faiz.csv")
setDT(konut_kredi_faiz )
konut_kredi_faiz[,year_of_date := year(ym(tarih))]
konut_kredi_faiz[,month_of_date := month(ym(tarih))]
konut_kredi_faiz

ggplot(konut_kredi_faiz, aes(x=factor(year_of_date),y = faiz_oran)) + geom_boxplot(aes(fill = factor(year_of_date)))+
labs(x="Years",y="Interest Rates",title="Boxplot of interest rates in years") +theme(legend.position = "none")
ggplot(konut_kredi_faiz,aes(month_of_date,faiz_oran,fill=factor(year_of_date))) + geom_bar(stat = 'identity') +labs(fill="Years") + facet_wrap(~year_of_date)+scale_x_discrete(limits = c(1:12))+ylab("Interest Rates") + xlab("Months")+
ggtitle("Monthly interest rates in different years")

gecinme_index = read.csv("gecinme_index.csv")
setDT(gecinme_index)
gecinme_index[,year_of_date := year(ym(tarih))]
gecinme_index[,month_of_date := month(ym(tarih))]
gecinme_index



ggplot(gecinme_index,aes(month_of_date,gecinme_index,color=factor(year_of_date))) +
geom_point() + scale_x_discrete(limits = c(1:12)) + ylab("Cost of Living Index") + xlab("Months") + ggtitle("Cost of Living Index in Months")


ggplot(gecinme_index,aes(x=month_of_date,y=gecinme_index)) + geom_line(aes(color=factor(year_of_date))) + facet_wrap(~year_of_date) +
scale_x_discrete(limits = c(1:12)) + ylab("Cost of Living Index") + xlab("Months") + ggtitle("Monthly Index in Different Years")

house_search = read.csv("satılık_arama.csv")
setDT(house_search)

house_search[,year_of_date := year(ym(tarih))]
house_search[,month_of_date := month(ym(tarih))]
house_search

ggplot(house_search,aes(x=month_of_date,y=arama)) +geom_line(aes(color=factor(year_of_date))) + facet_wrap(~year_of_date)+ylab("Search") +xlab("Months")+scale_x_discrete(limits = c(1:12))+
ggtitle("Search Volumes in Months")


ggplot(house_search,aes(month_of_date,arama,color=factor(year_of_date))) + geom_line() + scale_x_discrete(limits = c(1:12))+
ylab("Search") +xlab("Months") + ggtitle("Search Volumes in Months")

library(corrplot)
data_table_summary = data.table(date=sold_house_data$tarih,sold_house=sold_house_data$toplam_satış,interest_rate=konut_kredi_faiz$faiz_oran,cost_of_living_index = gecinme_index$gecinme_index,search_volume = house_search$arama)
data_table_summary[,month_of_date := as.factor(month(ym(date)))]
data_table_summary[,year_of_date := as.factor(month(ym(date)))]
data_table_summary


corrplot(cor(data_table_summary[,c(2:7)]),method="number",title = "Corrplot of different variables",mar=c(0,0,1,0)) 


