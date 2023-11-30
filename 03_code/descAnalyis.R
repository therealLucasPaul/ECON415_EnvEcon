### Read in packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(vars)

### Read in data
recycling <- read.csv("../02_data/municipal-waste-recycling-rate.csv") %>%
  rename(Rate = Variable...Recycling...MUNW)%>%
  mutate(Year=as.Date(paste0(Year, '-01-01')))

generation <- read.csv("../02_data/municipal-waste-generation.csv")
gen_percapita <- generation %>%
  filter(MEASURE == "KG_CAP") %>%
  mutate(TIME=as.Date(paste0(TIME, '-01-01'))) %>%
  filter(TIME >= as.Date("1995/01/01")) %>%
  filter(!(LOCATION %in% c("EU27_2020","OECD","OECDE"))) %>%
  rename("Country"="LOCATION")

translation <- read.csv("../02_data/translationtable.csv") %>%
  dplyr::select(alpha.3, name, region, sub.region)

gen_percapita <- left_join(gen_percapita, translation, by=c("Country"="alpha.3"))

#### Plots
### Recycling
g <- ggplot2::ggplot(recycling, aes(x = Year, y = Rate, color=Code)) + 
  geom_line()+
  scale_y_continuous(breaks = seq(0, 100, by=10))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  labs(y="Recycling Rate in %",x="Year", title="Recycling Rates of Municipial Waste")+
  theme(panel.grid.minor = element_blank())+
  theme_minimal()
g

tmp <- recycling %>%
  filter(Year == "2015-01-01") %>%
  filter(!(Entity %in% c("OECD - Europe","OECD - Total")))

hist <- ggplot(tmp, aes(Rate))+
  geom_histogram(binwidth = 5, color="black", fill="grey", alpha=0.5)+
  labs(x="Recycling Rate in %",y="Frequency",title="Distribution of OECD Recycling Rates of Municipal Waste (2015)", subtitle=paste0("Mean: ",round(mean(tmp$Rate),2),"%"))+
  scale_x_continuous(breaks = seq(0,100,by=5))+
  theme_classic()
hist
ggsave(hist, file="../04_figures/hist_recyclingrates.png" , width=7, height=5)

### Generation
gen_pc_EUROPE <- ggplot2::ggplot(filter(gen_percapita,region=="Europe"), aes(x = TIME, y = Value, color=Country)) + 
  geom_line()+
  scale_y_continuous(breaks = seq(0, 1000, by=100))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  labs(y="Municipal Waster Generation in Kilogramms/Capita",x="Year", title="Municipal Waster Generation in Kilogramms/Capita from 1995 to 2021", subtitle = "Subsample: Countries from Europe")+
  theme(panel.grid.minor = element_blank())+
  theme_minimal()
gen_pc_EUROPE
ggsave(gen_pc_EUROPE, file="../04_figures/wastegeneration_Europe.png" , width=7, height=5)

gen_pc_NONEUROPE <- ggplot2::ggplot(filter(gen_percapita,region!="Europe"), aes(x = TIME, y = Value, color=Country)) + 
  geom_line()+
  scale_y_continuous(breaks = seq(0, 1000, by=100))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  labs(y="Municipal Waster Generation in Kilogramms/Capita",x="Year", title="Municipal Waster Generation in Kilogramms/Capita from 1995 to 2021", subtitle = "Subsample: Non-European Countries")+
  theme(panel.grid.minor = element_blank())+
  theme_minimal()
gen_pc_NONEUROPE
ggsave(gen_pc_NONEUROPE, file="../04_figures/wastegeneration_NonEurope.png" , width=7, height=5)

avg_pc <- gen_percapita %>%
  dplyr::select(TIME, Value, region)%>%
  group_by(TIME) %>%
  mutate(num_countries = length(Value))%>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Subsample="All")

avg_pc_Europe <- gen_percapita %>%
  dplyr::select(TIME, Value, region)%>%
  filter(region=="Europe")%>%
  group_by(TIME) %>%
  mutate(num_countries = length(Value))%>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Subsample="Europe")

avg_pc_NonEurope <- gen_percapita %>%
  dplyr::select(TIME, Value, region)%>%
  filter(region!="Europe")%>%
  group_by(TIME) %>%
  mutate(num_countries = length(Value))%>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(Subsample="Non-Europe")

avg_pc_total <- rbind(avg_pc,avg_pc_Europe,avg_pc_NonEurope)


avg_pc_plot <- ggplot2::ggplot(avg_pc_total, aes(x = TIME, y = Value, color=Subsample)) + 
  geom_line(linetype=1, linewidth=1)+
  scale_y_continuous(breaks = seq(300, 550, by=25), limits=c(300,550))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y", limits=c(as.Date("1995/01/01"), as.Date("2022/01/01")))+
  labs(y="Average Municipal Waster Generation in kg/Capita",x="Year", 
       title="Average Municipal Waste Generation per Year", 
       subtitle = "Sample: All available countries, Numbers above refer to number of countries in sample")+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual(values=c("#ED0000","#004688","#42B540"))+
  geom_text(aes(label = num_countries), vjust = -1) +
  theme_minimal()
avg_pc_plot
ggsave(avg_pc_plot, file="../04_figures/AvgGen_pc.png" , width=7, height=4)

growth <- gen_percapita %>%
  dplyr::select(Country,TIME,Value)%>%
  group_by(Country) %>%
  mutate(g = (Value-lag(Value))/lag(Value)*100)

tmp <- growth %>%
  group_by(Country) %>%
  summarise_all(mean,na.rm=TRUE) %>%
  rename("Rate" = "g") %>%
  filter(!is.na(Rate))

hist_growthrates <- ggplot(tmp, aes(Rate))+
  geom_histogram(binwidth = 1, color="black", fill="grey", alpha=0.5)+
  labs(x="Average Growth Rate in %",y="Frequency",title="Distribution of Average YoY Growth Rates of per capita Municipal Waste between 1995 and 2021", subtitle=paste0("Mean: ",round(mean(tmp$Rate, na.rm=TRUE),2),"%"))+
  scale_x_continuous(breaks = seq(-5,100,by=5), limits=c(-5,18))+
  theme_classic()
hist_growthrates
ggsave(hist_growthrates, file="../04_figures/hist_growthrates.png" , width=7, height=5)

### ETS
ts <- ts(data$ETS_Price_EUR, start = c(2008,3,25), frequency=365)
plot(ts)

ETS_return <- na.omit(diff(log(ts)))*100
plot(ETS_return, main="Daily return of the ETS price")

g <- ggplot2::ggplot(data, aes(x = Date, y = ETS_Price_EUR)) + 
  geom_line()+
  geom_line( color="darkred", size=0.5, alpha=1, linetype=1) +
  scale_y_continuous(breaks = seq(-50, 100, by=10))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  labs(y="ETS Price",x="Year", title="EU ETS Price")+
  theme(panel.grid.minor = element_blank())+
  theme_minimal()
g
ggsave(g, file="../04_figures/timeseries_ETS.png" , width=7, height=4)

### OIL
ts <- ts(data$OilPriceUSD, start = c(2008,3,25), frequency=365)
plot(ts)

ETS_return <- na.omit(diff(log(ts)))*100
plot(ETS_return, main="Daily return of the ETS price")

g <- ggplot2::ggplot(data, aes(x = Date, y = OilPriceUSD)) + 
  geom_line()+
  geom_line( color="darkred", size=0.5, alpha=1, linetype=1) +
  scale_y_continuous(breaks = seq(-50, 150, by=25))+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  labs(y="ETS Price",x="Year", title="EU ETS Price")+
  theme(panel.grid.minor = element_blank())+
  theme_minimal()
g
ggsave(g, file="../04_figures/timeseries_ETS.png" , width=7, height=4)


#### Econometric Analysis
var1 <- VAR(na.omit(data[,c("GrowthRateOIL","GrowthRateETS")]))
summary(var1)
plot(irf(var1))
