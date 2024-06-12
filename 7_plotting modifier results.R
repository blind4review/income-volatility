#load libraries from main file

plot<-read_csv(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/plot_data.csv")

order(as.factor(plot$level))

plot$variable_f = factor(plot$variable, levels=c("Poverty status","Average wealth","\u2265$2,000 in savings","Home ownership","Years of education","Income class in 1990","Income class of average income"))

#plot$level_n <- ordered(plot$level_n,
                    # levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                     #labels = xaxislabs)
symbol<-"\u2265"

table(plot$level_n)
#breaks<-c("Always","Never","Sometimes",">2,000","≤$2,000",">12 years","≤12 years","At least once","Never","High","Low","Middle","High","Low","Middle","At least once","Never")
modplot<-ggplot(plot, aes(x=as.factor(level_n), y=beta))+
  #geom_hline(yintercept=0.30, color="red", linetype="dashed") +
  geom_point(size=1.7, color="gray60") + #ylim(0, 0.6) + 
  geom_errorbar(aes(x = as.factor(level_n), ymin = lci, ymax = uci),color="gray60",width=0.1,size=0.3)+
  facet_grid(.~variable_f, scales="free",space="free_x") +
  labs(y = "Income volatility beta and 95% confidence intervals \n ", x = " ") +
  scale_x_discrete(breaks = c(01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17), labels = c("Never\n(N=1622)","At least once\n(N=1021)","\u2265$2,000\n(N=2428)","<$2,000\n(N=207)","Always\n(N=318)","Sometimes\n(N=1625)","Never\n(N=700)","At least once\n(N=2217)","Never\n(N=426)",">12 years\n(1157)","\u226412 years\n(N=1486)","High\n(N=160)","Midde\n(N=1403)","Low\n(N=1080)","High\n(N=277)","Middle\n(N=1581)","Low\n(N=785)"))+
  theme_light()+theme(
    legend.position="bottom", 
    axis.text.x = element_text(size=7), 
    axis.text.y = element_text(size=7), 
    axis.title.x = element_text(size=10), 
    axis.title.y = element_text(size=10), 
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 7.75, face="bold",color="gray10"),
    strip.background = element_rect(color="gray10", fill="white", linetype=0),
    panel.grid.minor = element_line(color="gray93"),
    panel.grid.major = element_line(color="gray93"))

modplot

library(tidyverse)

quartz(type = 'pdf', file = "/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/modifiers_plot.pdf", width=11, height=4)
modplot
dev.off()

#pdf(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/modifiers_plot.pdf",
#    width = 11, # The width of the plot in inches
#    height = 4) # The height of the plot in inches
#modplot
#dev.off()
