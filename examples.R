library(dplyr)
library(ggplot2)
library(cowplot)




#########################
posterior_beta_dist(alpha=3,beta=3,x=50,n=100)


data_sales%>%
  filter(group=='Control')%>%
  select(date,x,n)%>%
  plot_posterior(alpha=1,beta=1)




data_sales%>%
  plot_bayesian_trend(col_groups =c("Control"="blue","Variant 1"="red"))


mu<-.3
var<-mu*(1-mu)*0.05

alpha<-((1-mu)/var-1/mu)*mu**2
beta<-alpha*(1/mu-1)
c(alpha,beta)

beta_moments_to_parameters(mu,mu*(1-mu)*0.05)



a<-data_coin%>%
  save_bayesian_test(alpha =1,bet=1) 
a
