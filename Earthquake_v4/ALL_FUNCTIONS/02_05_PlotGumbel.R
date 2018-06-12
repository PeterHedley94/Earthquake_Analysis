require(ggplot2)


plot <- ggplot(aes(y = flow, x = year,color = flow,fill=flow),data = dataf)
plot + geom_bar(stat='identity')
hist(dataf$flow)

T = c(2,10,30,100)


plot <- ggplot(aes(y = dataranked$flow, x = dataranked$yT,color = dataranked$flow),data = dataf)+geom_smooth(method = "lm")
plot + geom_point(stat='identity') + theme_light() +  ggtitle("Peak Flow (m3/s) vs Return Period (years) River Ryton") +
  guides(fill=FALSE) + scale_colour_gradient(low = "green", high = "red")+ labs(x="Return Period (years)",y="Peak Flow (m3/s)") + theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=10, hjust=0)) #scale_x_log10(breaks = c(-5,-1,0,1,5)) +