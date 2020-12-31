library(dplyr)
library(ggplot2)
library(cowplot)


day_len<-21
sample_size<-140
dates<-Sys.Date()-400+seq(0,day_len)

x1<-rbinom(n=day_len+1,size=sample_size,prob =0.25)
n1<-rep(sample_size,day_len+1)

x2<-rbinom(n=day_len+1,size=sample_size,prob =0.35)
n2<-rep(sample_size,day_len+1)


data1_sales<-data.frame("date"=dates,
                       "x"=x1,
                        "n"=n1)

data1_sales$group<-"Control"


data2_sales<-data.frame("date"=dates,
                       "x"=x2,
                       "n"=n2)

data2_sales$group<-"Variant 1"
data_sales<-rbind(data1_sales,data2_sales)


#########################
posterior_beta_dist(alpha=3,beta=3,x=50,n=100)


data_sales%>%
  filter(group=='Control')%>%
  select(date,x,n)%>%
  plot_posterior(alpha=1,beta=1)

data_sales%>%
  plot_bayesian_trend(col_groups =c("Control"="blue","Variant 1"="green"))


a<-data_sales%>%
  save_bayesian_test() 

 