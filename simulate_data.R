day_len<-75
sample_size<-1
dates<-Sys.Date()-400+seq(0,day_len)

x1<-rbinom(n=day_len+1,size=sample_size,prob =0.5)
n1<-rep(sample_size,day_len+1)

x2<-rbinom(n=day_len+1,size=sample_size,prob =0.6)
n2<-rep(sample_size,day_len+1)


data1_coin<-data.frame("date"=dates,
                        "x"=x1,
                        "n"=n1)

data1_coin$group<-"Control"


data2_coin<-data.frame("date"=dates,
                        "x"=x2,
                        "n"=n2)


data2_coin$group<-"Variant 1"
data_coin<-rbind(data1_coin,data2_coin)


saveRDS(data_coin,file ="data_coin.rds")





