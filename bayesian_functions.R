posterior_beta_dist <- function(alpha, beta, x, n, conf_interval = 0.95) {
  p_low <- (1 - conf_interval) / 2
  p_high <- conf_interval + p_low

  results <- list()

  results$prior_alpha <- alpha
  results$prior_beta <- beta


  results$prior_conf_interval <- c(
    qbeta(p_low, alpha, beta),
    qbeta(p_high, alpha, beta)
  )

  results$posterior_alpha <- x + alpha
  results$posterior_beta <- n - x + beta


  results$conf_interval <- c(
    qbeta(p_low, results$posterior_alph, results$posterior_beta),
    qbeta(p_high, results$posterior_alph, results$posterior_beta)
  )

  return(results)
}




plot_posterior <- function(data_input, alpha, beta, conf_interval = 0.95) {
  n <- dim(data_input)[1]
  plot_data <- data.frame("date" = 0:n)
  plot_data$conf_interval1 <- rep(0, 1 + n)
  plot_data$conf_interval2 <- rep(0, 1 + n)

  posterior_beta <- posterior_beta_dist(alpha, beta,
    x = data_input$x[1],
    n = data_input$n[1], conf_interval = 0.95
  )


  plot_data[1, "conf_interval1"] <- posterior_beta$prior_conf_interval[1]
  plot_data[1, "conf_interval2"] <- posterior_beta$prior_conf_interval[2]

  plot_data[2, "conf_interval1"] <- posterior_beta$conf_interval[1]
  plot_data[2, "conf_interval2"] <- posterior_beta$conf_interval[2]

   

  for (i in 2:n) {
     posterior_beta <- posterior_beta_dist(alpha, beta,
      x = data_input$x[i],
      n = data_input$n[i], conf_interval = 0.95
    )
     

    plot_data[i + 1, "conf_interval1"] <- posterior_beta$conf_interval[1]
    plot_data[i + 1, "conf_interval2"] <- posterior_beta$conf_interval[2]
  }

  return(plot_data)
}
  

plot_bayesian_trend<-function(data_input,
                              col_groups=c('blue','red'),
                              alpha=1,
                              beta=1){
  
  names(data_input)[1:4]<-c('date','x','n','group')
  
  all_groups<-unique(data_input$group)
  m<-length(all_groups)
  trends_plot<-list()
  for(i in 1:m){

         
    trends_plot[[i]]<-data_input%>%
      filter(group==all_groups[i])%>%
      select(date,x,n)%>%
      plot_posterior(alpha=alpha,beta=beta)
     n<-dim(trends_plot[[i]])[1]
    
     
    x_values<-c(trends_plot[[i]][1,'date'],
                trends_plot[[i]][,'date'],
                trends_plot[[i]][n,'date'],
                trends_plot[[i]][n:1,'date']
                )
     
    y_values<-c(trends_plot[[i]][1,'conf_interval1'],
                trends_plot[[i]][,'conf_interval2'],
                trends_plot[[i]][n,'conf_interval1'],
                trends_plot[[i]][n:1,'conf_interval1'])
    

        
  data_temp<-data.frame("x"=x_values,"y"=y_values,"group"=all_groups[i])
  
  
  if(i==1){
    data_output<-data_temp
  }else{
    data_output<-rbind(data_output,data_temp)
  }
  
  }
  
  
  plot_output<-data_output%>%
    ggplot()+
    geom_polygon(aes(
      x=x,
      y=100*y,
      fill=group,
     ),
    alpha=0.3,
    col="black")+xlab("")+ylab("")+
    scale_fill_manual(values=col_groups,name="")+
    theme_bottom
  
  return(plot_output)
}
          
          

proportion_bayesian_test<-function(data_input,groups=c("Control","Variant 1"),
                                   n_simulations=100,alternative="two.sided",
                                   alpha=1,beta=1,col_groups=c("blue","red")){
  
  
  
  data_output<-list()
  
  if(dim(data_input)[2]>=4){
    names(data_input)[2:4]<-c("x","n","group")
  }else{
    names(data_input)[1:3]<-c("x","n","group")
  }
  
  
  data1_summary<-data_input%>%
    filter(group==groups[1])%>%
    summarise(x=sum(x),n=sum(n))%>%
    select(x,n)
  
  data2_summary<-data_input%>%
    filter(group==groups[2])%>%
    summarise(x=sum(x),n=sum(n))%>%
    select(x,n)
  
  
  beta_dist1<-posterior_beta_dist(alpha=alpha,
                                  beta=beta,
                                  x=data1_summary$x,
                                  n=data1_summary$n)
  
  beta_dist2<-posterior_beta_dist(alpha=alpha,
                                  beta=beta,
                                  x=data2_summary$x,
                                  n=data2_summary$n)
  
  
  simul1<-rbeta(n=n_simulations,
                shape1=beta_dist1$posterior_alpha,
                shape2=beta_dist1$posterior_beta)
 
  
  simul2<-rbeta(n=n_simulations,
                shape1=beta_dist2$posterior_alpha,
                shape2=beta_dist2$posterior_beta) 
  
  
  data_output$simulations1<-simul1
  data_output$simulations2<-simul2
  
  
  if(alternative=='less'){
    data_output$p_value<-sum(simul1<=simul2)/n_simulations
  }
  if(alternative=='greater'){
    data_output$p_value<-sum(simul1>=simul2)/n_simulations
  }
  if(alternative=='two.sided'){
    data_output$p_value<-1-max(sum(simul1<=simul2)/n_simulations,sum(simul1>=simul2)/n_simulations)
  }
  
  
  data_output$plot<-ggplot()+
    geom_density(aes(x=100*simul1,fill=groups[1]),alpha=0.3)+
    geom_density(aes(x=100*simul2,fill=groups[2]),alpha=0.3)+
    xlab("")+
    ylab("")+
    scale_fill_manual(values=col_groups,name="")+
    theme(axis.text.y=element_text(size=0))+
    theme_bottom
  

  return(data_output)
  
}   



save_bayesian_test<-function(data_input,
                   alpha=1,
                   beta=1,
                   title_label=NULL,
                   file_name=NULL){
  
  

  names(data_input)[1:4]<-c("date","x","n","group")
  
  

  plot_p1_temp<-data_input%>%
    group_by(group)%>%
    summarise(x=sum(x),
              n=sum(n))%>%
    select(x,n,group)%>%
    proportion_bayesian_test(
      n_simulations =500,
      alpha = alpha,
      beta=beta,
      col_groups = c("Control"="blue","Variant 1"="red")
    )
   plot_p1<-plot_p1_temp$plot+
    ggtitle(title_label)+
    theme(legend.position = "none")
  
  plot_p2<-data_input%>%
    plot_bayesian_trend(alpha=alpha,
                        beta=beta)+xlab("Time")+
    theme_bottom
  
  
  final_temp<-plot_grid(plot_p1,plot_p2,
                        ncol=1)
  
return(final_temp)
}

          
      