### Population density - analysis

#Load packages
library("readxl")
library("ggplot2")
library("glmmTMB")
library("sjPlot")
library("DHARMa")
library("ggpubr")
library("grid")


################################################################################
################################## Summary Plots ###############################

## Seed summary
seed <- read_excel('Population_density.xlsx', sheet = "Seed_summary")
seed$Seed_Type <- factor(seed$Seed_Type, levels = c("Unpollinated_seeds", "Mature_seeds", "Predated_seeds", "Pistil_number", "Pollinated_seeds_number"))

#Set facet names
seed_names <- list(
  'Unpollinated_seeds'="Unpollinated seeds",
  'Predated_seeds'="Predated seeds",
  'Mature_seeds'="Mature seeds",
  'Pistil_number'="Total seeds",
  'Pollinated_seeds_number'= "Pollinated seeds")

seed_labeller <- function(variable,value){
  return(seed_names[value])
}

#Plot seed summary
ggplot(seed, aes(x = Pop_density, y = Seed_count, color = Population)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)  +
  facet_wrap(facets = vars(Seed_Type), labeller = seed_labeller)+
  scale_color_manual(labels = c("Leysin", "Rochers de Naye", "Solalex"), 
                     values = c("#CC6677", "#44AA99", "#DDCC77"))+
  labs(x = "Population density", y = "Number of seeds", 
       title = "Seed type for different population densities", 
       color = "Population") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())

#ggsave("Summary_plot_seeds.jpg",width = 30, height = 20, units = "cm")



## Pupa summary
pupa <- read_excel('Population_density.xlsx', sheet = "Pupa_summary")
pupa$Pupa <- factor(pupa$Pupa, levels = c("Tot_pupa", "Parasited_pupa"))

#Set facet names
pupa_names <- list(
  'Tot_pupa'="Total pupa",
  'Parasited_pupa'="Parasited pupa")

pupa_labeller <- function(variable,value){
  return(pupa_names[value])
}

#Plot pupa summary
ggplot(pupa, aes(x = Pop_density, y = Pupa_count, color = Population)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)  +
  facet_wrap(facets =  vars(Pupa), labeller = pupa_labeller )+
  scale_color_manual(labels = c("Leysin", "Rochers de Naye", "Solalex"), 
                     values = c("#CC6677", "#44AA99", "#DDCC77"))+
  labs(x = 'Population density', y = 'Number of pupa', 
       title = "Number of pupa for different population densities", 
       color = "Population") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())
        
#ggsave("Summary_plot_pupa.jpg",width = 30, height = 20, units = "cm")



## Rates summary
rate <- read_excel('Population_density.xlsx', sheet = "Rates_summary")
rate$Rate <- factor(rate$Rate , levels = c("Pollination_rate", "Predation_rate","Parasitic_rate"))

#Set facet names
rate_names <- list(
  'Pollination_rate'="Pollination rate",
  'Predation_rate'="Predation rate",
  'Parasitic_rate'="Parasitic rate")

rate_labeller <- function(variable,value){
  return(rate_names[value])
}

#Plot rate summary
ggplot(rate, aes(x = Pop_density, y = Count, color = Population)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE)  +
  facet_wrap(facets =  vars(Rate), labeller = rate_labeller)+
  scale_color_manual(labels = c("Leysin", "Rochers de Naye", "Solalex"), 
                     values = c("#CC6677", "#44AA99", "#DDCC77"))+
  labs(x = "Population density", y = "Rate", 
       title = "Rates for different population densities",
       color = "Population") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())

#ggsave("Summary_plot_rates.jpg",width = 30, height = 20, units = "cm")



################################################################################
############################## 1 - Seed predation ##############################

predation <- read_excel('Population_density.xlsx', sheet = "Flowers")
data_cols <- c("Flower_ID", "Population", "Pop_density", "Subpopulation")
predation[predation_cols] <- lapply(predation[predation_cols], factor) 


#Summary of predation rate
ggplot(predation, aes(x = Subpopulation, y = Predation_rate, color = Population)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)  +
  scale_color_manual(labels = c("Leysin", "Rochers de Naye", "Solalex"), 
                     values = c("#CC6677", "#44AA99", "#DDCC77"))+
  labs(x = "Subpopulation", y = "Predation rate", 
       title = "Predation rate across Subpopulations", 
       color = "Population") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())

#ggsave("Predation.jpg",width = 30, height = 20, units = "cm")


##################################### Models ###################################

pred <-glmmTMB(Predation_rate~Pop_density*Population+(1|Flower_ID), 
        ziformula = ~Pop_density + Population,
        family = betabinomial, 
        weights = Pollinated_seeds_number,data=predation)

#check the model assumptions
darmapred <- simulateResiduals(fittedModel = pred, n=2000)
plot(darmapred)
testDispersion(darmapred)
testZeroInflation(darmapred)

summary(pred)

drop1(pred,test="Chisq")
drop1(update(pred,.~.-Pop_density:Population),test="Chisq")

plot_model(pred,type = "eff",terms=c("Pop_density"))
plot_model(pred,type = "eff",terms=c("Population"))

predplot<-plot_model(pred,type = "eff",terms=c("Pop_density","Population"))+
  scale_color_manual(values =  c("#CC6677", "#44AA99", "#DDCC77"), 
                     labels = c("Leysin", "Rochers de Naye", "Solalex"))+
  labs(x = "Population density", y = "Predation rate", 
       title = "Prediced probabilities of Predation rate")+
  theme_bw() + 
  theme(axis.text.x = element_text(size=18, hjust = 1), 
        axis.text.y = element_text(size=18),
        axis.title = element_text(size=22),
        title = element_blank(),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18),
        legend.position = c(0.3, 0.9),
        strip.text.x = element_text(size = 18),
        axis.ticks = element_blank(),
        aspect.ratio = 1.5/1)
predplot
ggsave("pred_model.jpg",width = 35, height = 20, units = "cm")


################################################################################
################################ 2 - Parasitism ################################
parasitism <- read_excel('Population_density.xlsx', sheet = "Flowers_pupa")
parasitism_cols <- c("Flower_ID", "Population", "Pop_density", "Subpopulation")
parasitism[parasitism_cols] <- lapply(parasitism[parasitism_cols], factor) 

#Summary of parasitic rate
ggplot(parasitism, aes(x = Subpopulation, y = Parasitic_rate, color = Population)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE)+
  scale_color_manual(labels = c("Leysin", "Rochers de Naye", "Solalex"), 
                     values = c("#CC6677", "#44AA99", "#DDCC77"))+
  labs(x = 'Subpopulation', y = 'Parasitic rate', 
       title = "Parasitism rate across Subpopulations", 
       color = "Population") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())

#ggsave("Parasitism_boxplot.jpg",width = 30, height = 20, units = "cm")



################################################################################
############################# 3 - Previous years S1 ############################

S1 <- read_excel('Population_density.xlsx', sheet = "S1_19-22")
S1_cols <- c("Year", "Flower_ID")
S1[S1_cols] <- lapply(S1[S1_cols], factor) 


#Plot predation rate across years
ggplot(S1, aes(x = Year, y = Predation_rate, color = Year)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)  +
  labs(x = "Year", y = "Predation rate", color = "Predation") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust = 1), 
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=18),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())

#ggsave("Predation_time_bx.jpg",width = 30, height = 20, units = "cm")


##################################### Models ###################################
pred_t <- glmmTMB(sqrt(Predation_rate)~Year+(1|Flower_ID),data=S1, zi=~Year,
                  weights = S1$Pollinated_seed_number,
                  family = betabinomial)

#Check model assumptions
darmapred_t <- simulateResiduals(fittedModel = pred_t,n=2000)
plot(darmapred_t)
testDispersion(darmapred_t)
testZeroInflation(darmapred_t)

summary(pred_t)
drop1(pred_t, test = "Chisq")

year_plot <-plot_model(pred_t, type = "eff",terms=c("Year"))+
  labs(x = "Year", y = "Predation rate")+
  theme_bw() + 
  theme(axis.text.x = element_text(size=18, hjust = 1), 
        axis.text.y = element_text(size=18),
        axis.title = element_text(size=22),
        title = element_blank(),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18),
        legend.position = c(0.3, 0.9),
        strip.text.x = element_text(size = 18),
        axis.ticks = element_blank(),
        aspect.ratio = 1.5/1)


#years <-ggsave("dens.jpg",width = 40, height = 20, units = "cm")

#post hoc to see if 2022 diff from others
pairwise.wilcox.test(S1$Predation_rate, S1$Year, p.adjust.method = "BH")


################################################################################
################################### Final plot #################################

pop_density <- ggarrange(predplot, year_plot + rremove("ylab"), 
          labels = c("(A)", "(B)"), font.label=list(color="black",size=18),
          ncol = 2, nrow = 1) 

ggsave("Population_density_final.jpg",width = 30, height = 20, units = "cm")


