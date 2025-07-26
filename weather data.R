rm(list=ls())

setwd("C:/Users/kalea/OneDrive/Documents/Ratajczak_lab/2023_R_code")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(emmeans)
library(cowplot)
library(plotly)

## loading in weather and precipitation data and making sure they are all numeric
weather = read.csv("AWE011.csv")
precip = read.csv("APT011.csv")

weather$RHUM <- as.numeric(weather$RHUM)
weather$TAIR <- as.numeric(weather$TAIR)
weather$WSPEED <- as.numeric(weather$WSPEED)
weather$WMAX <- as.numeric(weather$WMAX)
weather$RECDAY <- as.factor(weather$RECDAY)
weather$RECHOUR <- as.numeric(weather$RECHOUR)
weather$RECMONTH <- as.numeric(weather$RECMONTH)

precip$month <- as.numeric(precip$month)
precip$date <- as.numeric(precip$date)
precip$year <- as.numeric(precip$year)
precip$ppt  <- as.numeric(precip$ppt)

# Weather data ------------------------------------------------------------

#### data grouped by day and filtered for april 2021 ####
w_hist <- weather %>% 
  group_by(RECYEAR, RECMONTH, RECDAY ) %>%
  filter(RHUM != ".", TAIR != ".", WSPEED != ".", WMAX != ".") 

apr_stat = filter(w_hist, RECYEAR == "2021", RECMONTH == "4", RECDAY == "3" | RECDAY == "12", between(RECHOUR, left = 900, right = 2000))


summary(apr_stat)

theme_set(theme_bw()+ theme(text = element_text(size=15),axis.text.x = element_text(size=12), 
                            axis.text.y = element_text(size=14), axis.title.y = element_text(size=14),
                            axis.title.x = element_text(size=14), 
                            #legend.position = "none",
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank()))

## graphs for humidity, temperature, and wind speed

hum <- ggplot(data = apr_stat, aes(x = RECHOUR, linetype = RECDAY)) +
  geom_line(aes(y=RHUM), color = "#3D65A5")+
  geom_line(aes(y = RHUM), color = "#3D65A5")+
  xlab("Hour")+
  scale_y_continuous(name = "Relative Humidity (%)")

temp <- ggplot(data = apr_stat, aes(linetype = RECDAY)) +
  geom_line(aes(x=RECHOUR, y=TAIR), color = "#CC79A7")+
  geom_line(aes(x = RECHOUR, y = TAIR), color = "#CC79A7")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y.right = element_text(margin = margin(l = 10)))+
  scale_y_continuous(name = "Temperature ( C)")

wspeed <- ggplot(data = apr_stat, aes(linetype = RECDAY, color = RECDAY)) +
  geom_line(aes(x=RECHOUR, y=WSPEED), color = "#009E73")+
  geom_line(aes(x = RECHOUR, y = WSPEED), color = "#009E73")+
  #theme(axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.title.y.right = element_text(margin = margin(l = 10)))+
  scale_y_continuous(name = "Wind Speed (m/s)")+
  xlab("Hour")

print(wspeed)

final_plot <- plot_grid(temp, wspeed, hum, ncol = 2, align = "v", axis = "lr",
                        rel_heights = c(4, 4, 5))

print(final_plot)


# same graphs w/ alt color scheme to match other plots --------------------

hum <- ggplot(data = apr_stat, aes(x = RECHOUR, linetype = RECDAY, color = RECDAY)) +
  geom_line(aes(y=RHUM))+
  geom_line(aes(y = RHUM))+
  xlab("Hour")+
  scale_y_continuous(name = "Relative Humidity (%)")+
  scale_color_manual("RECDAY", values=c("#D69C4E", "#046C9A"))

temp <- ggplot(data = apr_stat, aes(linetype = RECDAY, color = RECDAY)) +
  geom_line(aes(x=RECHOUR, y=TAIR))+
  geom_line(aes(x = RECHOUR, y = TAIR))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y.right = element_text(margin = margin(l = 10)))+
  scale_y_continuous(name = "Temperature ( C)")+
  scale_color_manual("RECDAY", values=c("#D69C4E", "#046C9A"))

wspeed <- ggplot(data = apr_stat, aes(linetype = RECDAY, color = RECDAY)) +
  geom_line(aes(x=RECHOUR, y=WSPEED))+
  geom_line(aes(x = RECHOUR, y = WSPEED))+
  #theme(axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.title.y.right = element_text(margin = margin(l = 10)))+
  scale_y_continuous(name = "Wind Speed (m/s)")+
  scale_color_manual("RECDAY", values=c("#D69C4E", "#046C9A"))+
  xlab("Hour")

print(wspeed)
final_plot <- plot_grid(temp, wspeed, hum, ncol = 1, align = "v", axis = "lr",
                        rel_heights = c(3, 3, 3))

print(final_plot)


# stats to see if differences in abiotics ---------------------------------

m1 <- aov(TAIR ~ RECDAY, data = apr_stat)
summary(m1)

m2 <- aov(WSPEED ~ RECDAY, data = apr_stat)
summary(m2)

m3 <- aov(RHUM ~ RECDAY, data = apr_stat)
summary(m3)

###Monthly data for all years of historical data and filtered to show april ####
w_hist2 <- w_hist %>% 
  group_by(RECYEAR, RECMONTH) %>%
  #filter(RHUM != ".", TAIR != ".", WSPEED != ".") %>% 
  summarise(m_hum = mean(d_hum), m_temp = mean(d_temp), m_wind = mean(d_wind))

apr_hist = filter(w_hist2, RECMONTH == "4")

ggplot(apr_hist, aes(x=RECYEAR, y=m_hum))+
  geom_point()+
  geom_path()+
  geom_smooth(method = "lm")

ggplot(apr_hist, aes(x=RECYEAR, y=m_temp))+
  geom_point()+
  geom_path()

ggplot(apr_hist, aes(x=RECYEAR, y=m_wind))+
  geom_point()+
  geom_path()

#data grouped by year ####
w_hist3 <- w_hist2 %>% 
  group_by(RECYEAR) %>% 
  summarise(y_hum = mean(m_hum), y_temp = mean(m_temp), y_wind = mean(m_wind))

ggplot(w_hist3, aes(x=RECYEAR, y=y_wind))+
  geom_point()+
  geom_path()+
  geom_smooth(method="lm")

## monthly and yearly avg
apr_avg = mean("m_hum", data=apr_hist)



# Precip data -------------------------------------------------------------
p_hist <- precip %>% 
  group_by(year) %>%
  filter(ppt != ".") %>% 
  summarise(avg_ppt = mean(ppt), ann_prcp = sum(ppt))

ggplot(p_hist, aes(x=year, y=ann_prcp))+
  geom_point()+
  geom_path()+
  xlim(2010, 2025)

#filtering out data we don't want
pcp_filt = filter(precip, year>1993, ppt !=".")
unique(pcp_filt$year)

mean(pcp_filt$ppt)

#dplyr loop to summarize the data into means and percentiles
tseries_dat = pcp_filt %>% group_by(.dots="year") %>%
  filter(ppt != ".") %>% 
  summarize(low_ci_ppt = quantile(ppt, 0.05), 
            up_ci_ppt = quantile(ppt, 0.95),
            mean_ppt = mean(ppt),
            ann_ppt= sum(ppt)
  )

## mean precip/year
ggplot(tseries_dat, aes(x=year, y=mean_ppt))+
  geom_vline(xintercept=2020.5, alpha=0.5, size=3, colour="#FF0000")+
  geom_line(size=1)+
  geom_point(size=2.5)+
  xlim(2005,2023)

## total precip/year
ggplot(tseries_dat, aes(x=year, y=ann_ppt))+
  geom_vline(xintercept=2020.5, alpha=0.5, size=3, colour="#FF0000")+
  geom_line(size=1)+
  geom_point(size=2.5)+
  xlim(2000,2023)

mean(tseries_dat$ann_ppt)

#filtering to only the years pre and post fire
mini_dat = filter(precip, year>2018)
mini_dat$year = as.factor(mini_dat$year)

# box plot for the years
ggplot(mini_dat, aes(x=year, y=ppt))+
  geom_boxplot()

#quick anova to see if there's anything
aov1 = lm(ppt~year, data=mini_dat, method="REML")
aov1
anova(aov1)
test_tab = summary(aov1)
em1 = emmeans(aov1, list(pairwise ~ year), adjust = "tukey")
em1



