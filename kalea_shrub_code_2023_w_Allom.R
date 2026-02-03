### Allometry code and shrub density from 2020 - 2023 ###
### Authors: Kalea R. Nippert-Churchman and Zak Ratajczak ###
### Last updated: 02/03/2026 ###
### code used to convert stem diameters into woody plant allometric equations 
### to look at changes in stem density, height, leaf area, etc. before and after an extreme fire in 2021 ###


#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(segmented)
library(nls2)
library(strucchange)
library(AICcmodavg)
library(emmeans)
library(ggpubr)
library(multcomp)
setwd("~/Ratajczak_lab/2023_R_code")

#bringing in shrub allometrics
stem_allo = read.csv("woody_allometry_master.csv", stringsAsFactors = FALSE)
head(stem_allo)

stem_allo$basal_area = (pi*stem_allo$Diam_at_0pt5^2)*0.0001
stem_allo$log_height = log(stem_allo$Height_m)
stem_allo$log_diam = log(stem_allo$Diam_at_0pt5)
stem_allo$log_leaf_area = log(stem_allo$Leaf_area_cm2)
stem_allo$total_biomass = stem_allo$Leaf_mass_g + stem_allo$Stem_mass_g
stem_allo$log_biomass = log(stem_allo$total_biomass)
stem_allo$log_leaf_biomass = log(stem_allo$Leaf_mass_g)

#creating fits for all the different species
codr_fit = coefficients(lm(log_leaf_area~log_diam, data=filter(stem_allo, Species=="CODR")))
pram_fit = coefficients(lm(log_leaf_area~log_diam, data=filter(stem_allo, Species=="PRAM")))
rhar_fit = coefficients(lm(log_leaf_area~log_diam, data=filter(stem_allo, Species=="RHAR")))
rhgl_fit = coefficients(lm(Leaf_area_cm2~Diam_at_0pt5_cm, data=filter(stem_allo, Species=="RHGL")))
rupe_fit = coefficients(lm(log_leaf_area~log_diam, data=filter(stem_allo, Species=="RUPE")))
zaam_fit = coefficients(lm(log_leaf_area~log_diam, data=filter(stem_allo, Species=="ZAAM")))

codr_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="CODR")))
pram_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="PRAM")))
rhgl_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="RHGL")))
rhar_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="RHAR")))
rupe_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="RUPE")))
zaam_fit_ht = coefficients(lm(log_height~log_diam, data=filter(stem_allo, Species=="ZAAM")))

codr_fit2 = coefficients(lm(log_biomass~log_diam, data=filter(stem_allo, Species=="PRAM")))
pram_fit2 = coefficients(lm(log_biomass~log_diam, data=filter(stem_allo, Species=="PRAM")))
rhar_fit2 = coefficients(lm(log_biomass~log_diam, data=filter(stem_allo, Species=="RHAR")))
rhgl_fit2 = coefficients(lm(total_biomass~Diam_at_0pt5_cm, data=filter(stem_allo, Species=="RHGL")))
rupe_fit2 = coefficients(lm(total_biomass~Diam_at_0pt5_cm, data=filter(stem_allo, Species=="RUPE")))
zaam_fit2 = coefficients(lm(log_biomass~log_diam, data=filter(stem_allo, Species=="ZAAM")))

codr_fit3 = coefficients(lm(log_leaf_biomass~log_diam, data=filter(stem_allo, Species=="PRAM")))
pram_fit3 = coefficients(lm(log_leaf_biomass~log_diam, data=filter(stem_allo, Species=="PRAM")))
rhar_fit3 = coefficients(lm(log_leaf_biomass~log_diam, data=filter(stem_allo, Species=="RHAR")))
rhgl_fit3 = coefficients(lm(Leaf_mass_g~Diam_at_0pt5_cm, data=filter(stem_allo, Species=="RHGL")))
rupe_fit3 = coefficients(lm(Leaf_mass_g~log_diam, data=filter(stem_allo, Species=="RUPE")))
zaam_fit3 = coefficients(lm(log_leaf_biomass~log_diam, data=filter(stem_allo, Species=="ZAAM")))

#bringing in shrub counts and making some basic manipulations
shrub_counts <- read_csv("shrub_stem_counts_2020.csv", col_types = cols(Diam_cm = col_number(), Year = col_number()))
# reminder, this "stringsAsFactor" seems to fix a lot of the more annoying tibble/dply errors with conditionals!!!
shrub_counts_2022_kalea <- read_csv("shrub_counts_2022.csv", col_types = cols(Diam_cm = col_number(), Year = col_number()))
shrub_counts_2023_kalea <- read_csv("shrub_counts_2023.csv", col_types = cols(Diam_cm = col_number(), Year = col_number()))

#combining data sets
shrub_counts = rbind(shrub_counts, shrub_counts_2022_kalea, shrub_counts_2023_kalea)

#filter to K4A and 4B - 2020 has multiple watersheds, 2022 and 2023 data only have k4a and 4b data
# select first wants to know what data-set to work on, then logical criteria to keep/toss data
shrub_counts = filter(shrub_counts, Watershed == "K4A" | Watershed =="4B")
unique(shrub_counts$Watershed)

#shrub_counts_test = filter(shrub_counts, Watershed == "K4A" | Watershed =="4B")
#unique(shrub_counts_test$Watershed)

shrub_counts$unq_plot = paste(shrub_counts$Watershed, shrub_counts$Plot_ID, sep="_")
shrub_counts$unq_plot2 = paste(shrub_counts$Watershed, shrub_counts$Plot_ID, 
                               shrub_counts$Transect_ID, shrub_counts$Transect_position, shrub_counts$Watershed, sep="_")
colnames(shrub_counts)[4]="Sp_code"
colnames(shrub_counts)[5]="Diam"
unique(shrub_counts$Sp_code)

head(shrub_counts)

#fixing mis-spellings
shrub_counts$Sp_code[shrub_counts$Sp_code=="RGHL"]="RHGL"
shrub_counts$Sp_code[shrub_counts$Sp_code=="XAAM"]="ZAAM"
shrub_counts$Sp_code[shrub_counts$Sp_code=="PRAN"]="PRAM"
shrub_counts$Sp_code[shrub_counts$Sp_code=="ROAR"]="RHAR"
shrub_counts$Sp_code[shrub_counts$Sp_code=="GLTR (honey locust)"]="GLTR"
unique(shrub_counts$Sp_code)
shrub_counts$basal_area = (pi*(shrub_counts$Diam/2)^2)*0.0001
shrub_counts$log_diam = log(shrub_counts$Diam)

#using allometric equations to bring in leaf area and biomass
shrub_counts$leaf_area = NA
shrub_counts$tot_biomass = NA
shrub_counts$leaf_biomass = NA
shrub_counts$shrub_ht = NA
#shrub_counts$basal_area= NA

for(i in 1:dim(shrub_counts)[1]){
  if(shrub_counts$Sp_code[i]=="CODR") {
    shrub_counts$leaf_area[i]= exp(codr_fit[1]+shrub_counts$log_diam[i]*codr_fit[2])
    shrub_counts$tot_biomass[i]= exp(codr_fit2[1]+shrub_counts$log_diam[i]*codr_fit2[2])
    shrub_counts$leaf_biomass[i]= exp(codr_fit3[1]+shrub_counts$log_diam[i]*codr_fit3[2])
    shrub_counts$shrub_ht[i]= exp(codr_fit_ht[1]+shrub_counts$log_diam[i]*codr_fit_ht[2])
  }
  
  if(shrub_counts$Sp_code[i]=="PRAM") {
    shrub_counts$leaf_area[i]= exp(pram_fit[1]+shrub_counts$log_diam[i]*pram_fit[2])
    shrub_counts$tot_biomass[i]= exp(pram_fit2[1]+shrub_counts$log_diam[i]*pram_fit2[2])
    shrub_counts$leaf_biomass[i]= exp(pram_fit3[1]+shrub_counts$log_diam[i]*pram_fit3[2])
    shrub_counts$shrub_ht[i]= exp(pram_fit_ht[1]+shrub_counts$log_diam[i]*pram_fit_ht[2])
    
  }
  
  if(shrub_counts$Sp_code[i]=="RHAR") {
    shrub_counts$leaf_area[i]= exp(rhar_fit[1]+shrub_counts$log_diam[i]*rhar_fit[2])
    shrub_counts$tot_biomass[i]= exp(rhar_fit2[1]+shrub_counts$log_diam[i]*rhar_fit2[2])
    shrub_counts$leaf_biomass[i]= exp(rhar_fit3[1]+shrub_counts$log_diam[i]*rhar_fit3[2])
    shrub_counts$shrub_ht[i]= exp(rhar_fit_ht[1]+shrub_counts$log_diam[i]*rhar_fit_ht[2])
    
  }
  
  if(shrub_counts$Sp_code[i]=="RHGL") {
    shrub_counts$leaf_area[i]= rhgl_fit[1]+shrub_counts$Diam[i]*rhgl_fit[2]
    shrub_counts$tot_biomass[i]= rhgl_fit2[1]+shrub_counts$Diam[i]*rhgl_fit2[2]
    shrub_counts$leaf_biomass[i]= rhgl_fit3[1]+shrub_counts$Diam[i]*rhgl_fit3[2]
    shrub_counts$shrub_ht[i]= exp(rhgl_fit_ht[1]+shrub_counts$log_diam[i]*rhgl_fit_ht[2])
  }
  
  #  if(shrub_counts$Sp_code[i]=="RUPE") {
  #    shrub_counts$leaf_area[i]= exp(rupe_fit[1]+shrub_counts$log_diam[i]*rupe_fit[2])
  #    shrub_counts$tot_biomass[i]= rupe_fit2[1]+shrub_counts$Diam[i]*rhar_fit2[2]
  #    shrub_counts$leaf_biomass[i]= rhgl_fit3[1]+shrub_counts$Diam[i]*rhgl_fit3[2]
  #  }
  
  if(shrub_counts$Sp_code[i]=="ZAAM") {
    shrub_counts$leaf_area[i]= exp(zaam_fit[1]+shrub_counts$log_diam[i]*zaam_fit[2])
    shrub_counts$tot_biomass[i]= exp(zaam_fit2[1]+shrub_counts$log_diam[i]*zaam_fit2[2])
    shrub_counts$leaf_biomass[i]= exp(zaam_fit3[1]+shrub_counts$log_diam[i]*zaam_fit3[2])
    shrub_counts$leaf_biomass[i]= exp(zaam_fit_ht[1]+shrub_counts$log_diam[i]*zaam_fit_ht[2])
    
  }
}

# converting to m2
shrub_counts$leaf_area = shrub_counts$leaf_area/10000
hist(shrub_counts$leaf_area)

ggplot(shrub_counts, aes(x=Diam, y=leaf_area, group=Sp_code, colour=Sp_code))+
  geom_point()+
  geom_smooth(method="lm", se=F)
ggplot(shrub_counts, aes(x=leaf_area, group=Sp_code, colour=Sp_code))+
  geom_histogram()+
  facet_grid(.~Sp_code)
ggplot(shrub_counts, aes(x=Year, y=basal_area, group=Watershed, colour=Watershed))+
  geom_boxplot()+
  facet_grid(~Year)

hist(shrub_counts$leaf_area, breaks=20)

#shrub_counts = filter(shrub_counts, Sp_code!="RUPE")
# some filtering for better 1:1 comparisons, might undo later
shrub_counts3 = shrub_counts
shrub_counts = filter(shrub_counts, Sp_code !="RUPE")


#already filtered out so shrub_counts only has 4B and K4A
unique(shrub_counts$Watershed)


#bringing in the meta data and doign some manipulations
meta_dat = read.csv("woody_veg_meta_data_2020.csv", stringsAsFactors = FALSE)
head(meta_dat)
meta_dat$unq_plot = paste(meta_dat$Watershed, meta_dat$Plot.number, sep="_")
meta_dat$unq_plot2 = paste(meta_dat$Watershed, meta_dat$Plot.ID, 
                           meta_dat$Transect, meta_dat$Transect.position..2.to.22.m., meta_dat$Watershed, sep="_")
meta_dat = filter(meta_dat, !is.na(Plot.number))

head(meta_dat)


#creating meta-data factors for later
meta_dat$fire = ifelse(meta_dat$Watershed=="1D", 1, NA)
meta_dat$fire[meta_dat$Watershed=="2C"] = 2
meta_dat$fire[meta_dat$Watershed=="2D"] = 2
meta_dat$fire[meta_dat$Watershed=="K2A"] = 2
meta_dat$fire[meta_dat$Watershed=="4B"] = 3.5
meta_dat$fire[meta_dat$Watershed=="4F"] = 3
meta_dat$fire[meta_dat$Watershed=="K4A"] = 3.5
meta_dat$fire[meta_dat$Watershed=="N1B"] = 1
meta_dat$fire[meta_dat$Watershed=="N1A"] = 1
meta_dat$fire[meta_dat$Watershed=="N2A"] = 2
meta_dat$fire[meta_dat$Watershed=="N2B"] = 2
meta_dat$fire[meta_dat$Watershed=="N4D"] = 3.5
meta_dat$grz = ifelse(meta_dat$Watershed=="1D", "ug", NA)
meta_dat$grz[meta_dat$Watershed=="K1B"] = "ug"
meta_dat$grz[meta_dat$Watershed=="2C"] = "ug"
meta_dat$grz[meta_dat$Watershed=="2D"] = "ug"
meta_dat$grz[meta_dat$Watershed=="K2A"] = "ug"
meta_dat$grz[meta_dat$Watershed=="4B"] = "ug"
meta_dat$grz[meta_dat$Watershed=="4F"] = "ug"
meta_dat$grz[meta_dat$Watershed=="K4A"] = "ug"
meta_dat$grz[meta_dat$Watershed=="N1B"] = "ng"
meta_dat$grz[meta_dat$Watershed=="N1A"] = "ng"
meta_dat$grz[meta_dat$Watershed=="N2A"] = "ng"
meta_dat$grz[meta_dat$Watershed=="N2B"] = "ng"
meta_dat$grz[meta_dat$Watershed=="N4D"] = "ng"


#bringing in historical fire data and joining to meta data
knz_fire_hist = read.csv("konz_fire_hist.csv", stringsAsFactors = FALSE)
meta_dat = left_join(meta_dat, knz_fire_hist, by="Watershed")
#creating a second "watershed" for plots 13 to 24 in N2B and N4D
meta_dat$Watershed[meta_dat$Watershed=="N4D" & meta_dat$Plot.number>12] = "N4D_2"
meta_dat$Watershed[meta_dat$Watershed=="N2B" & meta_dat$Plot.number>12] = "N2B_2"
meta_dat$Watershed[meta_dat$Watershed=="N1B" & meta_dat$Plot.number>12] = "N1B_2"
meta_dat = filter(meta_dat, Watershed!="N1B_2")
meta_dat2 = filter(meta_dat, Watershed!="K2A")
meta_dat2 = filter(meta_dat2, Watershed!="N2A")


write.csv(shrub_counts, "shrub_counts.csv")

#setting the plot theme
theme_set(theme_bw()+ theme(text = element_text(size=15),axis.text.x = element_text(size=12), 
                            axis.text.y = element_text(size=14), axis.title.y = element_text(size=14),
                            axis.title.x = element_text(size=14), panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank()))


########sp codes w/out allometry added in, need to filter out#############
##shrub_counts = filter(shrub_counts, Sp_code != RIMI)

ggplot(shrub_counts, aes(x=Year, y=BA, group=Watershed, colour=Watershed))+
  geom_boxplot()+
  facet_grid(.~Year)

#creating plot_sum - grouping all data at the plot level
plot_sum = shrub_counts %>% 
  group_by(Watershed, Year, unq_plot)%>%
  summarize(stem_count = sum(ifelse(Diam>0, 1, 0), na.rm=T)/8.25, BA = sum(basal_area, na.rm=T)/8.25, 
            leaf_area=sum(leaf_area, na.rm=T)/8.25, tot_C_biomass = sum(tot_biomass, na.rm=T)/8.25,
            tot_C_leaf = sum(leaf_biomass, na.rm=T)/8.25, shrub_ht_max = max(shrub_ht, na.rm=T), avg_ht = mean(shrub_ht, na.rm=T))
plot_sum = data.frame(plot_sum)
plot_sum = plot_sum %>% add_row(Watershed="K4A", "Year"=2020, unq_plot="11", stem_count= 0, BA=0, leaf_area=0, tot_C_biomass=0, tot_C_leaf=0, shrub_ht_max=0, avg_ht=0)
colnames(plot_sum)

############## XX WATERSHED ANALYSIS FIGURES XX #########################
ggplot(plot_sum, aes(x=Year, y=stem_count, group=Watershed, colour=Watershed)) +
  geom_boxplot()+
  facet_grid(.~Year)

ggplot(plot_sum, aes(x=Year, y=shrub_ht_max, group=Watershed, colour=Watershed)) +
  geom_boxplot()+
  facet_grid(.~Year)

ggplot(plot_sum, aes(x=Year, y=BA, group=Watershed, colour=Watershed))+
  geom_boxplot()+
  facet_grid(.~Year)

ggplot(plot_sum, aes(x=Year, y=leaf_area, group=Watershed, colour=Watershed))+
  geom_boxplot()+
  facet_grid(.~Year)

ggplot(plot_sum, aes(x=Year, y=tot_C_biomass, group=Watershed, colour=Watershed))+
  geom_boxplot()+
  facet_grid(.~Year)

ggplot(plot_sum, aes(x=Year, y=tot_C_leaf, group=Watershed, colour=Watershed)) +
  geom_boxplot()+
  facet_grid(.~Year)

### creation of plotsum5 and turning Year and Watershed into factors#####
plotsum5 = plot_sum%>%mutate(Watershed=as.factor(Watershed), Year=as.factor(Year))

###Stem count ANOVA and Tukey ############3
aov1 = lmer(stem_count ~ Watershed*Year + (1 + Watershed | unq_plot), data = plotsum5)
aov2 = anova(aov1)
print(aov2)

resid = residuals(aov1)
plotNormalHistogram(resid)
plot(fitted(aov1), residuals(aov1))
hist(resid)

em1 = emmeans(aov1, list(pairwise ~ Watershed+Year+Watershed*Year), adjust = "tukey")
em1

em1 = TukeyHSD(aov1)

########### BA anova and Tukey ############
#aov4 = lm(BA~Watershed*Year, data=plotsum5)
#aov4
#aov6 = aov(BA~Watershed*Year, data=plotsum5)
#anova(aov4)
##aov5 = anova(aov4)
#aov5
#summary(aov4)
#print(summary(aov6))

#resid2 = residuals(aov4)
#plotNormalHistogram(resid)
#plot(fitted(aov4), residuals(aov4))
#hist(resid)

#em2 = emmeans(aov6, list(pairwise ~ Watershed+Year+Watershed*Year), adjust="tukey")
#em2

#### height ANOVA and Tukey ######
aov3 = lmer(avg_ht ~ Watershed*Year + (1 + Watershed | unq_plot), data = plotsum5)
aov4 = anova(aov3)
print(aov4)

resid = residuals(aov3)
plotNormalHistogram(resid)
plot(fitted(aov3), residuals(aov3))
hist(resid)

em2 = emmeans(aov3, list(pairwise ~ Watershed+Year+Watershed*Year), adjust = "tukey")
em2

em2 = TukeyHSD(aov1)

##### leaf area ANOVA and Tukey #############

aov5 = lmer(leaf_area ~ Watershed*Year + (1 + Watershed | unq_plot), data=plotsum5)
aov5
aov6 = anova(aov5)

summary(aov5)


resid4 = residuals(aov5)
plotNormalHistogram(resid)
plot(fitted(aov5), residuals(aov5))
hist(resid4)

em3 = emmeans(aov5, list(pairwise ~ Watershed+Year+Watershed*Year), adjust="tukey")
em3

#Bonferroni corrections########
### stem count #####
bon1 = emmeans(aov1, list(pairwise ~ Watershed+Year+Watershed*Year), adjust = "bonferroni")
bon1

###### basal area ##########
#bon2 = emmeans(aov, list(pairwise~Watershed+Year+Watershed*Year), adjust = "bonferroni")
#bon2

##### height ######
bon3 = emmeans(aov3, list(pairwise~Watershed+Year+Watershed*Year), adjust = "bonferroni")
bon3

#### leaf area ###########
bon4 = emmeans(aov5, list(pairwise~Watershed+Year+Watershed*Year), adjust = "bonferroni")
bon4

## checking 2020 - 2022 data ##########
#plotsum6 = filter(plotsum5, Year!=2023)

#aov1 = lm(stem_count~Watershed*Year, data=plotsum6)
#aov1
#aov3 = aov(stem_count~Watershed*Year, data=plotsum6)
#anova(aov1)
#aov2 = anova(aov1)
#aov2
#summary(aov1)
#print(summary(aov3))

#resid = residuals(aov1)
#plotNormalHistogram(resid)
#plot(fitted(aov1), residuals(aov1))
#hist(resid)

#em1 = emmeans(aov3, list(pairwise ~ Watershed+Year+Watershed*Year), adjust = "tukey")
#em1

#em1 = TukeyHSD(aov3)

### creation of plot sum 2 and more ggplots #####################
plot_sum2 = group_by(plot_sum, Watershed, Year)%>%
  summarize(mean_avg_ht=mean(avg_ht), sd_avg_ht=sd(avg_ht), 
            upper_avg_ht=mean_avg_ht+sd_avg_ht/12^0.5, 
            lower_avg_ht=mean_avg_ht-sd_avg_ht/12^0.5, 
            mean_BA=mean(BA), sd_BA = sd(BA), upper_BA=mean_BA+sd_BA/12^0.5,
            lower_BA=mean_BA-sd_BA/12^0.5,
            mean_stem_count=mean(stem_count), sd_stem_count=sd(stem_count), 
            upper_stem_count=mean_stem_count+sd_stem_count/12^0.5,
            lower_stem_count=mean_stem_count-sd_stem_count/12^0.5,
            mean_leaf_area=mean(leaf_area), sd_leaf_area=sd(leaf_area),
            upper_leaf_area=mean_leaf_area+sd_leaf_area/12^0.5,
            lower_leaf_area=mean_leaf_area-sd_leaf_area/12^0.5,
            mean_bio=mean(tot_C_biomass), sd_bio=sd(tot_C_biomass),
            upper_bio=mean_bio+sd_bio/12^0.5, lower_bio=mean_bio-sd_bio/12^0.5)

plot_sum3= group_by(plot_sum, Watershed, Year)%>%
  summarize(mean_avg_stem=mean(stem_count))

plot_sum2 = plot_sum2%>%mutate(x_lab=paste(Year, Watershed, sep="_"))

plot_sum3 = plot_sum3%>%mutate(x_lab=paste(Year, Watershed, sep="_"))

##Factor correcting plot_sum2##########
plotsum7 = plot_sum2%>%mutate(Watershed=as.factor(Watershed), Year=as.factor(Year))

###ggplots#####
ggplot(plotsum7, aes(x=x_lab, y=mean_stem_count, group=x_lab, colour=Watershed, shape=Watershed))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=lower_stem_count, ymax=upper_stem_count), width=0.4, size=1)+
  xlab(NULL)+
  ylim(4.5,13)+
  ylab("Stem density across watershed (# m )")+
  scale_color_manual("Watershed", values=c("#D69C4E", "#046C9A"))+
  scale_x_discrete(labels=c("2020", "2020", "2022", "2022", "2023", "2023"))+
  theme(legend.position = c(2.4))
ggsave("kalea_stem_density_2023.pdf", width=6, height=6)

ggplot(plotsum7, aes(x=x_lab, y=mean_BA, group=x_lab, colour=Watershed, shape=Watershed))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=lower_BA, ymax=upper_BA), width=0.4, size=1)+
  xlab(NULL)+
  ylim(0, 0.0025)+
  ylab("Basal area across watershed (m )")+
  scale_color_manual("Watershed", values=c("#D69C4E", "#046C9A"))+
  scale_x_discrete(labels=c("2020 \n extreme", "2020 \n prescribed", "2022 \n extreme", "2022 \n prescribed", "2023 \n extreme", "2023 \n prescribed"))+
  theme(legend.position = c(2.4))
ggsave("kalea_BA_2023.pdf", width=6, height=6)

ggplot(plotsum7, aes(x=x_lab, y=mean_avg_ht, group=x_lab, colour=Watershed, shape=Watershed))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=lower_avg_ht, ymax=upper_avg_ht), width=0.4, size=1)+
  xlab(NULL)+
  ylim(1.0, 2.4)+
  ylab("Mean height across watershed (m)")+
  scale_color_manual("Watershed", values=c("#D69C4E", "#046C9A"))+
  scale_x_discrete(labels=c("2020", "2020", "2022", "2022", "2023", "2023"))+
  theme(legend.position = c(2.4))
ggsave("kalea_avg_ht_2023.pdf", width=6, height=6)


ggplot(plotsum7, aes(x=x_lab, y=mean_BA, group=x_lab, colour=Watershed, shape=Watershed))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=lower_BA, ymax=upper_BA), width=0.4, size=1)+
  xlab(NULL)+
  ylim(0, 0.0025)+
  ylab("Basal area across watershed (m )")+
  scale_color_manual("Watershed", values=c("#D69C4E", "#046C9A"))+
  scale_x_discrete(labels=c("2020 \n extreme", "2020 \n prescribed", "2022 \n extreme", "2022 \n prescribed", "2023 \n extreme", "2023 \n prescribed"))+
  theme(legend.position = c(2.4))
ggsave("kalea_BA_2023.pdf", width=6, height=6)

ggplot(plotsum7, aes(x=x_lab, y=mean_leaf_area, group=x_lab, colour=Watershed, shape=Watershed))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=lower_leaf_area, ymax=upper_leaf_area), width=0.4, size=1)+
  xlab(NULL)+
  ylim(1, 7.0)+
  ylab("Leaf area across watershed (m )")+
  scale_color_manual("Watershed", values=c("#D69C4E", "#046C9A"))+
  scale_x_discrete(labels=c("2020", "2020", "2022", "2022", "2023", "2023"))+
  theme(legend.position = c(2.4))
ggsave("kalea_leaf_area_2023.pdf", width=6, height=6)


########## just messing around and trying to do some species analysis##############

plot_sum = shrub_counts %>%
  group_by(Sp_code,Watershed, Year, unq_plot) %>%
  summarize(stem_count = sum(ifelse(Diam>0, 1, 0))/4, BA = sum(basal_area)/4, 
            leaf_area=sum(leaf_area)/4, tot_C_biomass = sum(tot_biomass)/4,
            tot_C_leaf = sum(leaf_biomass)/4, shrub_ht_max = max(shrub_ht))
colnames(plot_sum)
plot_sum = data.frame(plot_sum)
plot_sum = plot_sum %>% add_row(Watershed="K4A", "Year"=2020, unq_plot="11", stem_count= 0, BA=0, leaf_area=0, tot_C_biomass=0, tot_C_leaf=0, shrub_ht_max=0)

plot_sum2 = left_join(meta_dat, plot_sum, by="unq_plot")
colnames(plot_sum2)[1]="Watershed"
plot_sum2 = filter(plot_sum2, Watershed=="4B" | Watershed=="K4A")
plot_sum2 = filter(plot_sum2, Sp_code=="CODR" | Sp_code=="RHGL")

plot_sum2$stem_count[is.na(plot_sum2$stem_count)]=0
plot_sum2$BA[is.na(plot_sum2$BA)]=0
plot_sum2$leaf_area[is.na(plot_sum2$leaf_area)]=0
plot_sum2$shrub_ht_max[is.na(plot_sum2$shrub_ht_max)]=0

plot_sum11 = plotsum7 = plot_sum2%>%mutate(Sp_code=as.factor(Sp_code), Watershed=as.factor(Watershed), Year=as.factor(Year))

ggplot(plot_sum11, aes(x=Watershed, y=BA, colour=Watershed))+
  geom_boxplot()+
  facet_grid(Sp_code~Year)+
  ylim(0, 0.01)
labs(x="Watershed", y="Basal area (m2/ha)")

ggplot(plot_sum11, aes(x=Watershed, y=stem_count, colour=Watershed))+
  geom_boxplot()+
  facet_grid(Sp_code~Year)+
  labs(x="Watershed", y="Stem Count")

ggplot(plot_sum11, aes(x=Watershed, y=shrub_ht_max, colour=Watershed))+
  geom_boxplot()+
  facet_grid(Sp_code~Year)+
  labs(x="Watershed", y="Max Shrub Height")

ggplot(plot_sum11, aes(x=Watershed, y=leaf_area, colour=Watershed))+
  geom_boxplot()+
  facet_grid(Sp_code~Year)+
  labs(x="Watershed", y= "avg leaf area")

############ species anovas to see if theres actually anything going on #############
aovD = lm(shrub_ht_max~Sp_code*Watershed*Year, data=plot_sum11)
aovD
aovG = aov(shrub_ht_max~Sp_code*Watershed*Year, data=plot_sum11)
anova(aovD)
aovF = anova(aovD)
aovF
summary(aovD)
print(summary(aovG))

resid3 = residuals(aovD)
plotNormalHistogram(resid)
plot(fitted(aovD), residuals(aovD))
hist(resid)

em3 = emmeans(aovG, list(pairwise ~ Watershed+Year+Watershed*Year), adjust="tukey")
em3
