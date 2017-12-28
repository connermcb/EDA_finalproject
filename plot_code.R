## J Hopkins EDA Final Project
# load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# merge datasets
full <- merge(NEI, SCC, by='SCC')

# packages
library(dplyr)
library(ggplot2)

## Question 1
# open png plotting device
png("question1.png")

# subset, group, summarize data
pm2.5 <- NEI%>%
  group_by(year)%>%
  summarize(total_emissions=sum(Emissions))

# plot summaries
plot(pm2.5$year, pm2.5$total_emissions, type = "l", col="blue", lwd=2, 
     main="Total PM2.5 Emissions by Year", 
     xlab="Year", ylab="Tons of PM2.5")
points(pm2.5$year, pm2.5$total_emissions, pch=20, col="blue", cex=1.5)

# turn png device off
dev.off()

## Question 2
# turn on png plot device
png("question2.png")

# group, summarize data
pm2.5 <- NEI[NEI$fips==24510,]%>%
  group_by(year)%>%
  summarize(total_emissions=sum(Emissions))

# plot summaries
plot(pm2.5$year, pm2.5$total_emissions, type = "l", col="blue", lwd=2, 
     main="Baltimore PM2.5 Emissions by Year", 
     xlab="Year", ylab="Emissions")
points(pm2.5$year, pm2.5$total_emissions, pch=20, col="blue", cex=1.5)

# turn png device off
dev.off()


## Question 3
ems_type <- NEI[NEI$fips==24510,]%>%
  group_by(year, type)%>%
  summarise(tot=sum(Emissions))

ggplot(ems_type)+
  geom_line(aes(x=year, y=tot, color=as.factor(type)))+
  labs(title="PM2.5 Emissions by Type", 
       x="Year", y="Tons of PM2.5 Emissions",
       color="Source Type")+
  theme(plot.title = element_text(hjust=0.5))

## Question 4
coal <- full[grepl("Coal", full$Short.Name),]
coal_sums <- coal%>%
  group_by(year)%>%
  summarize(tot=sum(Emissions))

plot(coal_sums$year, coal_sums$tot, type = "l")

## Question 5
cars_balt <- full[grepl("Gasoline", full$Short.Name) & full$fips=="24510",]
cars_balt <- cars_balt%>%
  group_by(year)%>%
  summarize(tot=sum(Emissions))
plot(cars_balt$year, cars_balt$tot, type="l")

## Question 6
cars_la <- full[grepl("Gasoline", full$Short.Name) & 
                  full$fips %in% c("24510", "06037"),]
cars_la <- cars_la%>%
  group_by(fips, year)%>%
  summarize(tot=sum(Emissions))
ggplot(cars_la)+
  geom_line(aes(year, tot, color=fips), show.legend = FALSE)+
  geom_point(aes(year, tot, color=fips), show.legend = FALSE)+
  geom_text(aes(year, tot, label=round(tot, 1)), vjust=-0.6, data = cars_la)+
  scale_y_continuous(limits = c(0,3050))+
  theme_minimal()

plot(cars_la$year, cars_la$tot, type="l", col="blue")
plot(cars_balt$year, cars_balt$tot, type="l", col="red")
cars_la
