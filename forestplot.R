forestplot <- function(CI_Data)
{
  
  addgap <- ifelse ((max(CI_Data[, 3:4])- min(CI_Data[, 3:4])) >10 ,  2.5,  0.15)
  
  p <- ggplot(CI_Data,aes(factor(ID)))+ labs(x=NULL, y=NULL)
  p <- p + geom_text(data=CI_Data,aes(x= factor(ID),y = min(CI_Data[1:10, 3:4]-addgap), label=CI_Data[,2],vjust=0.5, hjust=0.25, size=2))  # gene names
  p <- p + geom_hline(aes(yintercept=0),linetype=2, size=0.1) #vertical line at 1
  p <- p + geom_errorbar(data=CI_Data,aes(x=factor(ID),ymin =CI_lower, ymax=CI_higher), color = CI_Data$boxcolor,  width=0.1) #errorbars
  p <- p + geom_point(data=CI_Data,aes(x=factor(ID), y=PointEstimate),shape=22,size=5,fill=CI_Data$boxcolor, colour=CI_Data$boxcolor)   #HR
  p <- p + scale_x_discrete(labels=CI_Data$ID) 
  p <- p + coord_flip()
  p <- p + geom_point(aes(0.5,max(CI_Data[, 3:4])),shape=3,alpha=0)   
  p <- p + geom_segment(aes(x = 0.5, y = min(CI_Data[, 3:4]), xend = 0.5, yend = max(CI_Data[, 3:4]))) # horizontal line   
  p <- p + geom_segment(aes(x = 0.5, y = min(CI_Data[, 3:4]), xend = 0.5, yend = max(CI_Data[, 3:4])) , arrow=arrow(length = unit(0.02, "npc")),linetype=1,size=1) # right arrow 
  p <- p + geom_segment(aes(x = 0.5, y = min(CI_Data[, 3:4]), xend = 0.5, yend = min(CI_Data[, 3:4])-addgap),arrow=arrow(length = unit(0.02, "npc")),linetype=1,size=1) #left arrow
  
  p <- p + theme(axis.line = element_blank(), 
                 #axis.text.x = element_blank(), 
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 axis.ticks.length = unit(0.0001, "mm"),
                 #axis.text = element_text(margin=margin(0,0,-2,-2,"lines")), 
                 legend.position = "none", 
                 panel.background = element_rect(fill = "transparent"),  
                 panel.border = element_blank(),
                 #panel.spacing = unit(c(-0.1,-0.1,-0.5,-0.5), "mm"),
                 #plot.margin = unit(c(10,-2.5,0,10), "mm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank())
  
  plot(p)
  
}
