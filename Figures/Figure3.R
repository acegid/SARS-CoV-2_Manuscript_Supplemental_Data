library(ggplot2)
library(cowplot)
suppressMessages(library(colorspace, quietly = T))
library(tidyverse)
library(ggpubr)
library(dplyr)
library(magick)
library(rsvg)
library(ggstream)
library(reshape2)
library(lubridate)
library(MetBrewer)





um<-c("#fb8500", "#219ebc", "#ffb703", "#ced4da")
names(um)<-c("North\nAmerica", "Europe" ,"Africa", "East Asia")




tdf<-read.csv("Markov_jumps_bin_normalized_month.csv")

tdf$startLocation<-gsub("ern ", "\n", tdf$startLocation)

rdf<-data.frame(cbind("2020-06", NA, NA, NA))
names(rdf)<-c("year_month","startLocation", "count","mean")
tdf<-rbind(rdf, tdf)

rdf<-data.frame(cbind("2021-09", NA, NA, NA))
names(rdf)<-c("year_month","startLocation", "count","mean")
tdf<-rbind(rdf, tdf)


tdf$mean<-as.numeric(as.character(tdf$mean))


B<-ggplot() +
  geom_col(data=tdf,
           aes(x = year_month, y=mean, fill=startLocation, group=startLocation),
           color="black")+
  theme_pubr()+
  scale_fill_manual(values=um)+
  ylab("Mean number of introductions by month")+
  xlab("")+
  theme(legend.title = element_blank(),
        axis.text.x =element_text(size=11, angle=90, family="sans", 
                                  hjust = -0.15, vjust=0.5),
        axis.text.y =element_text(size=10, family="sans"),
        axis.title.y =element_text(size=10, family="sans"),
        legend.key.size = unit(0.25,"line"),
        strip.background = element_blank(),
        plot.margin = unit(c(-0.05, 0.2, 0, -0.05), "cm"),
        
        legend.text = element_text(size=12, family="sans"),
        panel.border = element_rect(fill=NA, color="black"))+
  guides(fill="none")




# 
# 
mjdf<-read.csv("Markov_jumps_overall.csv") 

mjdf$startLocation<-gsub("Northern America", "North\nAmerica", mjdf$startLocation)

mjdf$startLocation<-factor(mjdf$startLocation, 
                           levels=c( "North\nAmerica", "Europe"))


A<-ggplot(data=mjdf, aes(x=startLocation, y=mean, fill=startLocation))+
  geom_col(position="dodge", color="black")+
  theme_pubr()+
  ylab("Mean number of\nintroductions overall")+
  xlab("")+
  theme(legend.position = c(0.85, 0.5),
        legend.title = element_blank(),
        axis.text.x =element_text(size=8),
        axis.text.y =element_text(size=8),
        axis.title.y = element_text(size=6),
        axis.title.x = element_text(size=8),
        plot.title = element_text(face="bold", hjust = -0.15, vjust=0),
        legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=8),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        
        strip.background = element_blank(),
        panel.border = element_rect(fill=NA))+
  scale_fill_manual(values=um)+
  coord_flip()+guides(color="none",fill="none")




In<-ggdraw(B) +
  draw_plot(A, .089, .6, .35, .35) 





tdf<-read.csv("Markov_jumps_EXPORT_bin_normalized_month.csv")

tdf$endLocation<-gsub("ern ", "\n", tdf$endLocation)

B<-ggplot() +
  geom_col(data=tdf,
           aes(x = year_month, y=mean, fill=endLocation, group=endLocation),
           color="black")+
  theme_pubr()+
  scale_fill_manual(values=um)+
  ylab("Mean number of exports by month")+
  xlab("")+
  theme(legend.position = c(0.85, 0.6), legend.title = element_blank(),
        axis.text.x =element_text(size=10, angle=90, family="sans", 
                                  hjust = -0.15, vjust=0.5),
        axis.text.y =element_text(size=10, family="sans"),
        axis.title.y =element_text(size=10, family="sans"),
        legend.key.size = unit(1,"line"),
        strip.background = element_blank(),
        plot.margin = unit(c(0.5, 0.2, 0, -0.4), "cm"),
        
        legend.text = element_text(size=12, family="sans"),
        panel.border = element_rect(fill=NA, color="black"))+
  guides(fill="none")+
  scale_y_continuous(breaks=c(seq(0,150,25)))



# 
# 
mjdf<-read.csv("Markov_jumps_EXPORT_overall.csv") 
mjdf$endLocation<-gsub("Northern America", "North\nAmerica", mjdf$endLocation)
mjdf$endLocation<-gsub("Eastern Asia", "East\nAsia", mjdf$endLocation)

mjdf$endLocation<-factor(mjdf$endLocation, levels=c( "East\nAsia", 
                                                     "North\nAmerica", 
                                                     "Africa", "Europe"))

A<-ggplot(data=mjdf, aes(x=endLocation, y=mean, fill=endLocation))+
  geom_col(position="dodge", color="black")+
  theme_pubr()+
  ylab("Mean number of\nexports overall")+
  xlab("")+
  theme(
    legend.title = element_blank(),
    axis.text.x =element_text(size=8),
    axis.text.y =element_text(size=6),
    axis.title.y = element_text(size=6),
    axis.title.x = element_text(size=8),
    plot.title = element_text(face="bold", hjust = -0.15, vjust=0),
    legend.key.size = unit(0.5, 'cm'),
    legend.text=element_text(size=8),
    strip.background = element_blank(),
    panel.border = element_rect(fill=NA))+
  scale_fill_manual(values=um)+
  coord_flip()+guides(color="none",fill="none")




Uit<-ggdraw(B) +
  draw_plot(A, .091, .53, .4, .4) 




Jumps<-plot_grid(Uit, In, ncol=1, labels=c("B", "E"), hjust=1)



library(scales)



cv<-c()
cv <- c(cv, "Europe"="#219ebc" )
cv <- c(cv, "East\nAsia"="#ced4da")
cv <- c(cv, "Middle East"="#8ecae6")
cv <- c(cv, "North\nAmerica"="#fb8500")
cv <- c(cv, "South\nAsia"="#343a40")
cv <- c(cv, "Africa"="#ffb703" )
cv <- c(cv, "Central\nAmerica"="#8d99ae")
cv <- c(cv, "Oceania"="#adb5bd")
cv <- c(cv, "South\nAmerica"="#495057")
cv <- c(cv, "South-east Asia"="#6c757d")
cv <- c(cv, "Carribean"="#2b2d42")
cv <- c(cv, "Central\nAsia"="#3d405b")





show_col(cv)

fdf<-read.csv("fold_increase_in_travel_outgoing.csv")
fdf$fold_increasee<-fdf$fold_increasee*3000
fdf<-fdf[!is.na(fdf$month),]



mdf<-read.csv("Travel_outgoing_byregion.csv")
mdf<-mdf[order(c(mdf$year, mdf$month)),]
mdf<-mdf[!is.na(mdf$month),]

mdf$region<-gsub("ern ", "\n", mdf$region)


m<-c()
for (x in mdf$month) {
  if (x %in% seq(1,9,1)) {
    m<-c(m, paste("0", x, sep=""))  
  }
  else {
    m<-c(m, as.character(x))   
  }
  
}

mdf$month<-m


lev<-c("2020-05" , "2020-06",  "2020-07",  "2020-08" , "2020-09" ,
       "2020-10", "2020-11", "2020-12", "2021-01" , "2021-02" , 
       "2021-03" , "2021-04" )

mdf$my<-paste(mdf$year, mdf$month, sep="-")
mdf$my<-factor(mdf$my, levels=lev)

fdf<-fdf[order(c(fdf$year, fdf$month)),]
fdf$my<-paste(fdf$year, fdf$month, sep="-")

fdf$my<-factor(fdf$my, levels=lev)


A<-ggplot()+
  geom_col(data=mdf, aes(x=as.factor(my), 
                         y=totalVol, fill=region), color="black")+
  geom_line(data=fdf, aes(x=as.factor(my), 
                          y=fold_increasee, group=1), color="black")+
  
  scale_y_continuous( name="Total passenger volume",
                      sec.axis = sec_axis( ~./3000,   
                                           name="Fold increase in travel\nfrom May 2020", 
                                           breaks=c(seq(0, 30, 5))))+
  
  xlab("")+ theme_pubr()+
  theme(
    legend.title = element_blank(),
    axis.ticks.x= element_blank(),
    axis.text.x = element_blank(),
    axis.text.y =element_text(size=7, family="sans"),
    axis.title.y =element_text(size=9, family="sans"),
    legend.key.size = unit(1,"line"),
    legend.text = element_text(size=8, family="sans"),
    legend.key = element_rect(fill=NA),
    strip.background = element_blank(),
    plot.margin = unit(c(0.3, 0.5, -0.3, 0.5), "cm"),
    panel.border = element_rect(fill=NA, color="black"))+
  scale_fill_manual(values=cv)+ 
  guides(fill=F)+
  theme(legend.box.margin=margin(-10,-10,30,-10),
        legend.margin=margin(0,0,5,0))+
  scale_x_discrete(breaks = levels(mdf$my), limits=levels(mdf$my))


aa<-mdf %>% 
  dplyr::group_by(region) %>% 
  summarise(ss = sum(totalVol))
aa$ss<-(aa$ss/sum(aa$ss))*100


fmedf<-mdf[mdf$my %in% c("2020-07",  "2020-08" , "2020-09" ,
                         "2020-10", "2020-11", "2020-12", "2021-01" , 
                         "2021-02" , "2021-03" , "2021-04"),] %>% 
  dplyr::group_by(region) %>% 
  summarise(ss = sum(totalVol))
fmedf$ss<-(fmedf$ss/sum(fmedf$ss))*100


fmedf2<-mdf[mdf$my %in% c("2020-05", "2020-06", "2020-07",  "2020-08" , "2020-09"),] %>% 
  dplyr::group_by(region) %>% 
  summarise(ss = sum(totalVol))
fmedf2$ss<-(fmedf2$ss/sum(fmedf2$ss))*100

medf<-mdf[mdf$my %in% c("2020-12", "2021-01" , "2021-02" , "2021-03" , "2021-04"),] %>% 
  dplyr::group_by(region) %>% 
  summarise(ss = sum(totalVol))
medf$ss<-(medf$ss/sum(medf$ss))*100

medf2<-mdf[!mdf$my %in% c("2020-10", "2020-11", "2020-12", "2021-01" , 
                          "2021-02" , "2021-03" , "2021-04"),] %>% 
  dplyr::group_by(region) %>% 
  summarise(ss = sum(totalVol))
medf2$ss<-(medf2$ss/sum(medf2$ss))*100


pdf<-read.csv("Ourgoing_air_percentage.csv")
pdf<-pdf[order(c(pdf$year, pdf$month)),]
pdf<-pdf[!is.na(pdf$month),]




m<-c()
for (x in pdf$month) {
  if (x %in% seq(1,9,1)) {
    m<-c(m, paste("0", x, sep=""))  
  }
  else {
    m<-c(m, as.character(x))   
  }
  
}

pdf$month<-m




pdf$my<-paste(pdf$year, pdf$month, sep="-")

pdf$my<-factor(pdf$my, levels=c("2020-05" , "2020-06",  "2020-07", 
                                "2020-08" , "2020-09" ,
                                "2020-10", "2020-11", "2020-12", 
                                "2021-01" , "2021-02" , "2021-03" , "2021-04"))


pdf$region<-gsub("ern ","\n", pdf$region)

B<-ggplot()+
  geom_col(data=pdf, aes(x=my, y=Percentage.of.monthly.volume, fill=region), color="black")+
  
  scale_y_continuous( name = "Percentage of monthly travel", 
                      breaks=c(seq(0, 100, 10)))+
  
  xlab("")+
  theme_pubr()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.text.x =element_text(size=9, family="sans", angle=90, 
                                  hjust = -0.15, vjust=0.5),
        axis.text.y =element_text(size=7, family="sans"),
        axis.title.y =element_text(size=8, family="sans"),
        legend.key.size = unit(0.5,"line"),
        legend.text = element_text(size=8, family="sans"),
        legend.key = element_rect(fill=NA, color="black"),
        strip.background = element_blank(),
        plot.margin = unit(c(0, 1.2, -0.1, 0.9), "cm"),
        panel.border = element_rect(fill=NA, color="black"))+
  scale_fill_manual(values=cv, drop=T)+
  guides(fill="none")
# +
#   theme(legend.box.margin=margin(-10,-10,30,-10),legend.margin=margin(0,0,5,0))+guides(fill="none")

# scale_x_discrete(labels=c("May-2020" , "June-2020",  "July-2020",  "Aug-2020" , "Sep-2020" ,
#                           "Oct-2020", "Nov-2020", "Dec-2020", "Jan-2021" , "Feb-2021" , "Mar-2021" , "Apr-2021" ))


travel<-plot_grid(A, B, ncol=1, labels=c("C", "D"), rel_heights = c(1,1))






subdf<-read.csv("estimated-export-index_byregion_collapsed.csv")
subdf$date<-as.Date(subdf$date)

subdf$region<-gsub("ern ","\n", subdf$region)



EII<-ggplot(data=subdf) +
  geom_area(aes(x=date, y=num_intros, group=region, fill = region), 
            size=0.2, color="black") + theme_pubr()+
  ylab("Daily Export Intensity Index")+xlab("")+
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.text=element_text(size=9), strip.background = element_blank(),
        strip.text = element_text(size=12),
        axis.text.y = element_text(size=10), 
        legend.key.size = unit(1,"line"),
        legend.key = element_rect(fill=NA, color="black"),
        plot.margin=unit(c(0,0.5,-1,0), "cm"),
        axis.text.x =element_text(size=9, family="sans", angle=90, 
                                  hjust = -0.15, vjust=0.5),
        
        axis.title.y = element_text(size=10),
        panel.background = element_rect(fill = NA, color = "black"))+
  scale_fill_manual(values=cv,
                    breaks=c("Europe", "Africa", "North\nAmerica", 
                             "Middle East",
                             "Carribean", "Central\nAmerica","Central\nAsia",
                             "East\nAsia",
                             "Oceania","South\nAmerica","South-east\nAsia",
                             "South\nAsia"))+
  theme(legend.box.margin=margin(-10,-10,30,-10),
        legend.margin=margin(0,0,5,0))+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")





sf<-read.csv("sampling_fractions.csv")
sf<-sf[sf$my %in% c("2020-10", "2020-11",
                    "2020-12", "2021-01", "2021-02", "2021-03"),]


ggplot()+
  geom_tile(data=sf[sf$region=="Europe",], 
            aes(x=my, y=country, fill=sf))




image = image_read("B1525_DTA.region.history_FULL_tree.png")




tree_plot<-image_ggplot(image, interpolate = FALSE)+
  theme(plot.margin = unit(c(0, 0, 0, -5.5), "cm"))


right<-plot_grid(travel, EII, ncol=1, labels=c("" ,"F"))
right_full<-plot_grid(Jumps, right, ncol=2)+
  theme(plot.margin = unit(c(0, 0, 0, -5.5), "cm"))


plot_grid(tree_plot, right_full, ncol=2, labels=c("A", ""))

# # 
# # 
ggsave(filename = "/Users/eparker/Dropbox (Scripps Research)/Projects/Nigeria/Figures/B1525.pdf",
       height = 8,
       width = 15, bg="white", device="pdf")
