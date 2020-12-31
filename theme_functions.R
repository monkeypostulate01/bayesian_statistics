theme_bottom<-theme_bw()+
  theme(
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title  = element_text(size=15,face="bold"),
    axis.title.x  = element_text(color="gray",size=25,face="bold"),
    axis.title.y  = element_text(color="gray",size=25,face="bold"),
    plot.title=element_text(size=20,hjust=.5),
    
    legend.text=element_text(size=18),
    legend.position ="bottom"
        )