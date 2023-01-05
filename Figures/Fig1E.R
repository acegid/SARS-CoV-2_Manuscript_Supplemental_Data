library('ggplot2')
mobil <- read.csv("nga_combined.csv")
mobil$date <- as.Date(mobil$date)
names(mobil)[names(mobil) == 'retail_and_recreation_percent_change_from_baseline'] <- 'retail_and_recreation'
names(mobil)[names(mobil) == 'grocery_and_pharmacy_percent_change_from_baseline'] <- 'grocery_and_pharmacy'
names(mobil)[names(mobil) == 'parks_percent_change_from_baseline'] <- 'parks'
names(mobil)[names(mobil) == 'transit_stations_percent_change_from_baseline'] <- 'transit_stations'
names(mobil)[names(mobil) == 'workplaces_percent_change_from_baseline'] <- 'workplaces'
names(mobil)[names(mobil) == 'residential_percent_change_from_baseline'] <- 'residential'


ggplot(mobil, aes(x=date)) +
  geom_line(aes(y=retail_and_recreation, colour="retail_and_recreation"), size=1.0, alpha=0.8) +
  geom_line(aes(y=grocery_and_pharmacy, colour="grocery_and_pharmacy"), size=1.0, alpha=0.8) +
  geom_line(aes(y=parks, colour="parks"), size=1.0, alpha=0.8) +
  geom_line(aes(y=transit_stations, colour="transit_stations"), size=1.0, alpha=0.8) +
  geom_line(aes(y=workplaces, colour="workplaces"), size=1.0, alpha=0.8) +
  geom_line(aes(y=residential, colour="residential"), size=1.0, alpha=0.8) +
  scale_color_manual(name="Location", values = c("retail_and_recreation"="red", "grocery_and_pharmacy"="blue2","parks"="mediumpurple", "transit_stations"="green3", "workplaces"="gold2","residential"="hotpink3")) +
  scale_x_date(date_label = "%b", date_breaks = "month") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12,hjust=1, angle=90), axis.text.y = element_text(size=12), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16)) + 
  ylab("Percentage change from baseline") + xlab("Date") +
  geom_hline(yintercept = 0, linetype="dashed", color="black")
  
