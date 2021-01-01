# ###############################################
# Title: ggplot theme
# Author: Abel Camacho Guardian
# Date: 31/12/2019
#
# ##############################################


theme_bottom<-theme_bw()+
  theme(
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    axis.title  = element_text(size=12,face="bold"),
    axis.title.x  = element_text(color="gray",size=12,face="bold"),
    axis.title.y  = element_text(color="gray",size=12,face="bold"),
    plot.title=element_text(size=20,hjust=.5),
    
    legend.text=element_text(size=16),
    legend.position ="bottom"
        )
