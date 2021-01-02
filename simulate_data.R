day_len<-500
sample_size<-1
dates<-Sys.Date()-400+seq(0,day_len)

x1<-rbinom(n=day_len+1,size=sample_size,prob =0.5)
n1<-rep(sample_size,day_len+1)

x2<-rbinom(n=day_len+1,size=sample_size,prob =0.55)
n2<-rep(sample_size,day_len+1)

x3<-rbinom(n=day_len+1,size=sample_size,prob =0.45)
n3<-rep(sample_size,day_len+1)


data1_coin<-data.frame("date"=dates,
                        "x"=x1,
                        "n"=n1)

data1_coin$group<-"Control"


data2_coin<-data.frame("date"=dates,
                        "x"=x2,
                        "n"=n2)


data2_coin$group<-"Variant 1"
data_coin<-rbind(data1_coin,data2_coin)


data3_coin<-data.frame("date"=dates,
                       "x"=x3,
                       "n"=n3)


data3_coin$group<-"Variant 2"
data_coin<-rbind(data_coin,data3_coin)
data_three_coins<-data_coin
save(data_three_coins,file ="data/data_three_coins.rda")





