#Nigeria data

alldata <- read.csv("nga.data.csv", header=T)
alldata$date <- as.Date(alldata$date)
alldata$epiweek <- epiweek(alldata$date)-8
write.csv(alldata, "nga.data.csv")

epi <- read.csv("epiweek.csv", header=T)
library("lubridate")
library("ggplot2")
library("cowplot")
epi$sample_date <- as.Date(epi$sample_date)
epi$epi_week <- epiweek(epi$sample_date)
for (x in epi$epi_week(epi$epi_week)+53) {
  if (epi$sample_date==2021)
}

write.csv(epi, "chunkydate.csv")

updated_epi <- read.csv("chunkydate.csv", header=T)
plot(updated_epi$epi_week)

finalcurve <- read.csv("naija_epidata1.csv", header = TRUE)
finalcurve$date <- as.Date(finalcurve$date)

A <- ggplot(finalcurve, aes(x=date)) + scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  geom_col(aes(y=new_cases,fill="New_cases"), width=5) + 
  geom_line(aes(y=genomes, color="Genomes"), size=1, group=1) + scale_y_log10() + ylab("") + xlab("Sample collection date")+
  scale_fill_manual(name="", values=c("New_cases"="skyblue")) +
  scale_color_manual(name="", values=c("Genomes"="red")) + theme_bw(14) + ggtitle("A") + theme(plot.title= element_text(size=14, face="bold"), legend.position = "bottom")


B <- ggplot(finalcurve, aes(x=date)) + scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  geom_area(aes(y=fraction,fill="Sampling_fraction"), color="brown") +
  ylab("Sampling fraction") + 
  scale_colour_manual(name="", values = c("Sampling_fraction"="orange"))+ theme_bw(14) + ggtitle("B") + theme(plot.title = element_text(size=14, face="bold"), legend.position = "none")

plot_grid(A,B, nrow=2)                                  
facet_wrap(A,B)

#UK data

ukcases <- read.csv("gbr_newcases.csv", header=T)
ukcases$date <- as.Date(ukcases$date)
ukcases$epiweek <- epiweek(ukcases$date)-8
write.csv(ukcases, "gbr_newcases.csv")

#Epicurve comparison
combined <- read.csv("combined_epidata2.csv", header=T)
combined$date <- as.Date(combined$date)
ggplot(combined, aes(x=date)) + scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  geom_line(aes(y=nga_newcases, color="Nigeria"),size=1.5)+
  geom_line(aes(y=uk_newcases, color="United Kingdom"), size=1.5)+
  geom_line(aes(y=usa_newcases, color="United States"), size=1.5)+
  geom_line(aes(y=sa_newcases, color="South Africa"), size=1.5)+
  geom_line(aes(y=uae_newcases, color="United Arab Emirates"), size=1.5)+
  geom_line(aes(y=gh_newcases, color="Ghana"), size=1.5) + scale_y_log10(labels=trans_format("log10",math_format(10^.x))) +
  theme_bw() + ylab("New cases") +
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.x = element_text(size = 16), axis.title.y = element_text(size=16))+
  scale_color_manual(name="Country",values = c("Ghana"="blue3", "Nigeria"="green3", "South Africa"="pink3", "United Arab Emirates"="red3", "United Kingdom"="skyblue2", "United States"="gold2"))
