### Volatile Organic Compounds - analysis

#Load packages
library("readxl")
library("ggplot2")
library("emmeans")
library("ggpubr")
library("grid")
library("vegan")
library("glue")
library("pairwiseAdonis")
library("cowplot")




################################################################################
################################# 1 - Flowers ##################################

flowers <- read_excel('Volatiles.xlsx', sheet = "Flowers")
flowers <- flowers[-c(141:200),] # remove HF 8 and 9 because there was a problem during sampling, remove HF10 because outlier 

cols <- c("Treatment", "Sample", "Type")
flowers[cols] <- lapply(flowers[cols], factor) 
flowers$Compounds <- factor(flowers$Compounds, levels = c( "Butyl_acetate", "(E)-2-Octenal", "3-Hexen-1-ol", 
                                                           "Butyl_propionate", "(+)-3-Carene", "a-Fenchene", "Benzaldehyde",
                                                           "Octanal", "(3E)-3-hexenyl_acetate", "2-Ethylhexanol", "D-Limonene", 
                                                           "Eucalyptol", "b-Ocimene", "b-cis-Ocimene", "Linalool", "Nonanal",
                                                           "Carveol", "Decanal", "Caryophyllene", "Palmitic_acid"))



comp_fl <- c( "Butyl_acetate", "(E)-2-Octenal", "3-Hexen-1-ol", 
              "Butyl_propionate", "(+)-3-Carene", "a-Fenchene", "Benzaldehyde",
              "Octanal", "(3E)-3-hexenyl_acetate", "2-Ethylhexanol", "D-Limonene", 
              "Eucalyptol", "b-Ocimene", "b-cis-Ocimene", "Linalool", "Nonanal",
              "Carveol", "Decanal", "Caryophyllene", "Palmitic_acid")

comp_fl_type<- c("aliphatic", "benzenoid", "terpenoid")


# Basic dot plot
ggplot(flowers, aes(x=Compounds, y=Abundance, fill = Treatment)) + 
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=2, binwidth = 1.5)+
  scale_fill_manual(labels = c("Female-stage \nhermaphrodite ", "Male-stage \nhermaphrodite", "Male"), 
                    values=c("HF" = "brown3", "HM" = "lightgoldenrod1", "M"="lightskyblue2" )) +
  labs(x = 'Compounds', y = 'Abundance (ng)', 
       title = "Abundance of compounds found in flowers", 
       fill = "Flower type") +
  guides(fill = guide_legend(byrow = TRUE))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=12, vjust=1, hjust = 1), 
        axis.text.y = element_text(size=12), 
        axis.title = element_text(size=15),
        title = element_text(size =18),
        legend.text = element_text(size =15),
        legend.title = element_text(size =15),
        legend.spacing.y = unit(0.3, 'cm'),
        axis.ticks = element_blank()) 

#ggsave("Flower_dotplot.jpg",width = 30, height = 20, units = "cm")

############################### Compound abundance #############################
##Separate compounds at a certain abundance threshold to plot them better
#compounds with abundance >50
high_ab_fl <- subset(flowers, Compounds %in% c("3-Hexen-1-ol", "b-cis-Ocimene","(3E)-3-hexenyl_acetate"))
#compounds with abundance <50
low_ab_fl  <- subset(flowers, !Compounds %in% c("3-Hexen-1-ol", "b-cis-Ocimene","(3E)-3-hexenyl_acetate"))

#Plot high abundance compounds
high_fl <- ggplot(high_ab_fl, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "darkolivegreen3", "orchid3"), 
                    labels = c("Aliphatic", "Benzenoid", "Terpenoid"),drop = F )+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Female-stage hermaphrodite ", "Male-stage hermaphrodite", "Male"))+
  labs(x = "Compounds", y = "Abundance (ng)", alpha= "Flower type", fill = "Compound type") +
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=18, vjust=1, hjust = 1), 
        axis.text.y = element_text( size=18), 
        axis.title = element_text(size=22),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.box.spacing = unit(0.1, "pt"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 2/1)

#Plot low abundance compounds
low_fl <- ggplot(low_ab_fl, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "darkolivegreen3", "orchid3"), 
                    labels = c("Aliphatic", "Benzenoid", "Terpenoid"), drop = F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Female-stage hermaphrodite ", "Male-stage hermaphrodite", "Male"))+
  labs(x = "Compounds", y = "Abundance (ng)", alpha= "Flower type", fill = "Compound type") +
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=18, vjust=1, hjust = 1), 
        axis.text.y = element_text( size=18),
        axis.title = element_text(size=22),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.box.spacing = unit(0.1, "pt"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 1/2) 

## Saving the plot  
flower_comp <- ggarrange(high_fl + rremove("ylab") + rremove("xlab"), 
                         low_fl + rremove("ylab") + rremove("xlab"), 
                         ncol = 2, nrow = 1, common.legend = T, legend = "right",widths = c(1,2.9))
flower_comp <-annotate_figure(flower_comp, 
                              left = textGrob("Abundance (ng)", rot = 90, vjust = 1, gp = gpar(cex = 1.3, fontsize = 20)))
flower_comp

#ggsave("Flower_compounds.jpg",width = 35, height = 20, units = "cm")


## Significant differences in abundances between flower sex (for each compound)
source("function_vocs.R")
stat_compound(flowers, comp_fl)


### Plot only the compounds that are significantly different between flower sex
significant_comp_fl <- subset(flowers, Compounds %in% c("(+)-3-Carene","Carveol", "Caryophyllene"))

ggplot(significant_comp_fl, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge", width = 0.4) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.4) +
  scale_fill_manual(values = c("lightblue3", "darkolivegreen3", "orchid3"), 
                    labels = c("Aliphatic", "Benzenoid", "Terpenoid"), drop = F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Female-stage hermaphrodite ", "Male-stage hermaphrodite", "Male"))+
  labs(x = 'Compounds', y = 'Abundance (ng)',
       title = "Compounds varying between flower types", 
       alpha= "Flower type", fill = "Compound type") +
  theme_bw()+
  theme(axis.text = element_text(size=15), 
        axis.title.y = element_text(size=15),
        axis.ticks = element_blank(),
        title = element_text(size =18),
        legend.title = element_text(size=15), 
        legend.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.border = element_blank()) 

#ggsave("Flowers_significant_compounds.jpg",width = 30, height = 20, units = "cm")


################## Total amount of VOCs emitted per treatment ##################
#Get the total emission for each sample
tot_fl_treat <- aggregate(flowers$Abundance, by = list(flowers$Sample, flowers$Treatment), FUN = sum)
colnames(tot_fl_treat) <- c("Sample", "Treatment", "Abundance")

#Test
kruskal.test(Abundance ~ Treatment, data = tot_fl_treat)
pairwise.wilcox.test(tot_fl_treat$Abundance, tot_fl_treat$Treatment, p.adjust.method = "BH")


################################ Compound type #################################
#Average compound type per treatment
comp_type_fl <- ggplot(flowers, aes(Type, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "darkolivegreen3", "orchid3"), 
                    labels = c("Aliphatic", "Benzenoid", "Terpenoid"), drop = F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Female-stage hermaphrodite ", "Male-stage hermaphrodite", "Male"))+
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  labs(x = 'Compound Type', y = 'Abundance (ng)',  alpha = "Flower type") +
  theme_bw()+
  theme(axis.text.x = element_text(size=15, angle = 45, vjust = 0.6),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=15),
        legend.title = element_text(size=15), 
        legend.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())
        
#ggsave("Flower_compound_type.jpg",width = 30, height = 20, units = "cm")


#Significant differences in abundance according to compound type?
kruskal.test(Abundance ~ Type, data = flowers)
pairwise.wilcox.test(flowers$Abundance, flowers$Type, p.adjust.method = 'BH')

##Significant differences in abundance of compound type for each treatment?
source("function_vocs.R")
stat_compound_type(flowers, comp_fl_type)

############################## NMDS & perMANOVA ################################
flowers_ng <- read_excel('Volatiles.xlsx', sheet = "Flower_ng")
flowers_ng <- flowers_ng[-c(8,9, 10),] # remove HF 8 and 9 because there was a problem during sampling, remove HF10 because outlier 
cols_ng <- c("Treatment", "Sample")
flowers_ng[cols_ng] <- lapply(flowers_ng[cols_ng], factor) 

fl_matrix <- flowers_ng[3:23]
rownames(fl_matrix) <- flowers_ng$Sample


## NMDS
set.seed(1)
fl.mds <- metaMDS(fl_matrix, k = 2, try=100, distance = 'bray')
NMDSstress_fl <- round(fl.mds$stress, 2)

#extract scores
data.scores <- as.data.frame(scores(fl.mds, "sites"))  #  extract the site scores 
data.scores$site <- rownames(fl_matrix)  # create a column of site names
data.scores$treat <- flowers_ng$Treatment  #  add the treatment variable 

#Plot NMDS
NMDS_fl <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=treat),size=4) + 
  stat_ellipse(data=data.scores,aes(x=NMDS1,y=NMDS2,color=treat))+
  scale_color_manual(labels = c("Female-stage \nhermaphrodite ", "Male-stage \nhermaphrodite", "Male"), 
                     values=c("HF" = "brown3", "HM" = "lightgoldenrod1", "M"="lightskyblue2" )) +
  labs(color = "Flower type", x = glue("NMDS1 \nStress = {NMDSstress_fl}"))+
  guides(color = guide_legend(byrow = TRUE))+
  xlim(-1.1,1.1)+
  ylim(-1.1,1.1)+
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size=22),
        title = element_text(size =18),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.box.spacing = unit(0.1, "pt"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        plot.background = element_blank(),
        axis.ticks = element_blank())

#ggsave("Flower_NMDS.jpg",width = 30, height = 20, units = "cm")


## perMANOVA
fl.dist <- vegdist(fl_matrix, method="bray")

fl.treat <- data.frame(Treatment =flowers_ng$Treatment)
d.manova <- adonis2(fl_matrix ~ Treatment, method = "euclidean", data= fl.treat)
d.manova #non significant


################################################################################
################################### 2 - Fruits #################################

fruits <- read_excel('Volatiles.xlsx', sheet = "Fruits")
fruits <- fruits[-c(19:36),] # remove FRO2 because outlier 

cols <- c("Treatment", "Sample", "Type")
fruits[cols] <- lapply(fruits[cols], factor) 
fruits$Compounds <- factor(fruits$Compounds, levels = c("2-Heptanol", "Butyl_acetate", "(E)-2-Octenal", 
                                                        "3-Hexen-1-ol", "Butyl_propionate", "a-Pinene", "Linalyl_acetate",
                                                        "2-Acetoxy-1,8-cineole", "Octanal", "(3E)-3-hexenyl_acetate", 
                                                        "p-Cymene", "D-Limonene", "Eucalyptol", "b-cis-Ocimene", "Erucic_acid",
                                                        "Nonanal", "Decanal", "Palmitic_acid"))





comp_fr <- c("2-Heptanol", "Butyl_acetate", "(E)-2-Octenal", 
             "3-Hexen-1-ol", "Butyl_propionate", "a-Pinene", "Linalyl_acetate",
             "2-Acetoxy-1,8-cineole", "Octanal", "(3E)-3-hexenyl_acetate", 
             "p-Cymene", "D-Limonene", "Eucalyptol", "b-cis-Ocimene", "Erucic_acid",
             "Nonanal", "Decanal", "Palmitic_acid")

comp_fr_type <- c("aliphatic", "fatty acid", "terpenoid")

# Basic dot plot
ggplot(fruits, aes(x=Compounds, y=Abundance, fill = Treatment)) + 
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=2, binwidth = 1.5)+
  scale_fill_manual(labels = c("Unwounded", "Predated", "Wounded"), 
                    values=c("FRO" = "darkseagreen3", "FRW" = "tan", "FRP"="plum3" )) +
  labs(x = 'Compounds', y = 'Abundance (ng)', 
       title = "Abundance of compounds found in fruits", 
       fill = "Fruit type") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=12, vjust=1, hjust = 1), 
        axis.text.y = element_text( size=12), 
        axis.title = element_text(size=15),
        title = element_text(size =18),
        legend.text = element_text(size =15),
        legend.title = element_text(size =15),
        axis.ticks = element_blank()) 
  
#ggsave("Fruit_dotplot.jpg",width = 30, height = 20, units = "cm")


############################### Compound abundance #############################
##Separate compounds at a certain abundance threshold to plot them better
#compounds with abundance >250
high_ab_fr <- subset(fruits, Compounds %in% c("3-Hexen-1-ol", "a-Pinene","(3E)-3-hexenyl_acetate"))
#compounds with abundance <250
low_ab_fr  <- subset(fruits, !Compounds %in% c("3-Hexen-1-ol", "a-Pinene","(3E)-3-hexenyl_acetate"))

#Plot high abundance compounds
high_fr <- ggplot(high_ab_fr, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3","goldenrod1", "orchid3"), 
                    labels = c("Aliphatic", "Fatty acid", "Terpenoid"), drop= F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Unwounded", "Predated", "Wounded"))+
  labs(x = 'Compounds', y = 'Abundance (ng)', alpha = "Fruit type", fill = "Compound type") +
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=18, vjust=1, hjust = 1), 
        axis.text.y = element_text( size=18), 
        axis.title = element_text(size=22),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.box.spacing = unit(0.1, "pt"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 2/1)


#Plot low abundance compounds
low_fr <-ggplot(low_ab_fr, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "goldenrod1","orchid3"), 
                    labels = c("Aliphatic", "Fatty acid", "Terpenoid"), drop= F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Unwounded", "Predated", "Wounded"))+
  labs(x = 'Compounds', y = 'Abundance (ng)', alpha = "Fruit type", fill = "Compound type") +
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=18, vjust=1, hjust = 1), 
        axis.text.y = element_text( size=18), 
        axis.title = element_text(size=22),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.box.spacing = unit(0.1, "pt"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        aspect.ratio = 1/2)


## Saving the plot  
fruit_comp <- ggarrange(high_fr  +rremove("ylab") + rremove("xlab"), 
                        low_fr + rremove("ylab") + rremove("xlab"), 
                        ncol = 2, nrow = 1, common.legend = T, 
                        legend = "right",widths = c(1,3))
fruit_comp <- annotate_figure(fruit_comp, 
                              left = textGrob("Abundance (ng)", rot = 90, vjust = 1, gp = gpar(cex = 1.3,  fontsize = 20)))
fruit_comp

#ggsave("Fruit_compounds.jpg",width = 35, height = 20, units = "cm")


## Significant differences in abundances between treatments (for each compound) 
source("function_vocs.R")
stat_compound(fruits, comp_fr)

### Plot only the compounds that are significantly different between fruit treatments
significant_comp_fr <- subset(fruits, Compounds %in% c("Erucic_acid"))

ggplot(significant_comp_fr, aes(Compounds, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge", width = 0.4) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.4) +
  scale_fill_manual(values = c("lightblue3", "goldenrod1","orchid3"), 
                    labels = c("Aliphatic", "Fatty acid", "Terpenoid"), drop = F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Unwounded", "Predated", "Wounded"))+
  labs(x = 'Compounds', y = 'Abundance (ng)', alpha = "Fruit type", fill = "Compound type") +
  theme_bw()+
  theme(axis.text = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.ticks = element_blank(),
        title = element_text(size =18),
        legend.title = element_text(size=15), 
        legend.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.border = element_blank()) 

#ggsave("Fruit_significant_compound.jpg",width = 35, height = 20, units = "cm")


################# Total amount of VOCs emitted per treatment ###################
#Get the total emission for each sample
tot_fr <- aggregate(fruits$Abundance, by = list(fruits$Sample, fruits$Treatment), FUN = sum)
colnames(tot_fr) <- c("Sample", "Treatment", "Abundance")

#Test
kruskal.test(Abundance ~ Treatment, data = tot_fr)
pairwise.wilcox.test(tot_fr$Abundance, tot_fr$Treatment, p.adjust.method = "BH")


################################ Compound type #################################
#Average compound type per treatment
comp_type_fr <- ggplot(fruits, aes(Type, Abundance, fill = Type, alpha = Treatment)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "goldenrod1", "orchid3"), 
                    labels = c("Aliphatic", "Fatty acid", "Terpenoid"), drop = F)+
  scale_alpha_discrete(range = c(0.5,1), labels = c("Unwounded", "Predated", "Wounded"))+
  guides(alpha = guide_legend(order = 1),  fill = guide_legend(order = 2))+
  labs(x = 'Compound Type', y = 'Abundance (ng)', alpha = "Fruit type") +
  theme_bw()+
  theme(axis.text.x = element_text(size=15, angle = 45, vjust = 0.6),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=15),
        legend.title = element_text(size=15), 
        legend.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#ggsave("Fruit_compound_type.jpg",width = 30, height = 20, units = "cm")


#Significant differences in abundance according to compound type?
kruskal.test(Abundance ~ Type, data = fruits)
pairwise.wilcox.test(fruits$Abundance, fruits$Type, p.adjust.method = 'BH')


##Significant differences in abundance of compound type for each treatment?
source("function_vocs.R")
stat_compound_type(fruits, comp_fr_type)


############################## NMDS & perMANOVA ################################
fruits_ng <- read_excel('Volatiles.xlsx', sheet = "Fruit_ng")
fruits_ng <- fruits_ng[-c(2),] # remove FRO2 because outlier 

cols_ng <- c("Treatment", "Sample")
fruits_ng[cols_ng] <- lapply(fruits_ng[cols_ng], factor) 

fr_matrix <- fruits_ng[3:21]
rownames(fr_matrix) <- fruits_ng$Sample


## NMDS
set.seed(1)
fr.mds <- metaMDS(fr_matrix, k = 2, try=100, distance = 'bray')
NMDSstress_fr <- round(fr.mds$stress, 2)

#extract scores
data.scores <- as.data.frame(scores(fr.mds, "sites"))  # extract the site scores 
data.scores$site <- rownames(fr_matrix)  # create a column of site names
data.scores$treat <- fruits_ng$Treatment  #  add the treatment variable 

#Plot NMDS
NMDS_fr <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=treat),size=4) + 
  stat_ellipse(data=data.scores,aes(x=NMDS1,y=NMDS2,color=treat))+
  scale_colour_manual(labels = c("Unwounded", "Predated", "Wounded"),
                      values=c("FRO" = "darkseagreen3", "FRW" = "tan", "FRP"="plum3" )) +
  labs(color = "Fruit type", x = glue("NMDS1 \nStress = {NMDSstress_fr}"))+
  guides(color = guide_legend(byrow = TRUE))+
  xlim(-1.1,1.1)+
  ylim(-1.1,1.1)+
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size=22),
        title = element_text(size =18),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.box.spacing = unit(0.1, "pt"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        plot.background = element_blank(),
        axis.ticks = element_blank())

#ggsave("Fruit_NMDS.jpg",width = 30, height = 20, units = "cm")


## perMANOVA
fr.dist <- vegdist(fr_matrix, method="bray")

fr.treat <- data.frame(Treatment =fruits_ng$Treatment)
d.manova <- adonis2(fr_matrix ~ Treatment, method = "euclidean", data= fr.treat)
d.manova #non significant



################################################################################
####################### 3- Comparing flowers and fruits ########################
flower_fruit<- merge(flowers, fruits, sort = T, all = T)

cols <- c("Treatment", "Sample", "Type")
flower_fruit[cols] <- lapply(flower_fruit[cols], factor)
flower_fruit$Plant<- c(rep("Flowers", times = 540, sep=""),rep("Fruits", times = 522, sep="") )
flower_fruit$Compounds <- factor(flower_fruit$Compounds, levels = c("2-Heptanol",	"Butyl_acetate",	"(E)-2-Octenal",	
                                                                    "3-Hexen-1-ol",	"Butyl_propionate",	"a-Pinene",	"Linalyl_acetate",	
                                                                    "2-Acetoxy-1,8-cineole",	"(+)-3-Carene",	"a-Fenchene",	"Benzaldehyde",	
                                                                    "Octanal",	"(3E)-3-hexenyl_acetate",	"2-Ethylhexanol",	"p-Cymene",	
                                                                    "D-Limonene",	"Eucalyptol",	"b-Ocimene",	"b-cis-Ocimene",	"Erucic_acid",	
                                                                    "Linalool",	"Nonanal","Carveol",	"Decanal",	"Caryophyllene",	"Palmitic_acid"))



comp_fl_fr <- c("2-Heptanol",	"Butyl_acetate",	"(E)-2-Octenal",	
                "3-Hexen-1-ol",	"Butyl_propionate",	"a-Pinene",	"Linalyl_acetate",	
                "2-Acetoxy-1,8-cineole",	"(+)-3-Carene",	"a-Fenchene",	"Benzaldehyde",	
                "Octanal",	"(3E)-3-hexenyl_acetate",	"2-Ethylhexanol",	"p-Cymene",	
                "D-Limonene",	"Eucalyptol",	"b-Ocimene",	"b-cis-Ocimene",	"Erucic_acid",	
                "Linalool",	"Nonanal","Carveol",	"Decanal",	"Caryophyllene",	"Palmitic_acid")



comp_fl_fr_type <- c("aliphatic", "terpenoid") #only compounds in both flowers and fruits

################# Total amount of VOCs emitted per plant type ###################
#Get the total emission for each sample
tot_fl_fr <- aggregate(flower_fruit$Abundance, 
                       by = list(flower_fruit$Sample, flower_fruit$Treatment, flower_fruit$Plant), FUN = sum)
colnames(tot_fl_fr) <- c("Sample", "Treatment", "Plant" ,"Abundance")

#Test
kruskal.test(Abundance ~ Plant, data = tot_fl_fr)
pairwise.wilcox.test(tot_fl_fr$Abundance, tot_fl_fr$Treatment, p.adjust.method = "BH")


#How big is the difference in total abundance between flowers and fruits?
mean_emission_fl <- mean(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Flowers"]) #329.73
mean_emission_fr <- mean(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Fruits"]) #1050.87

sd_emission_fl <- sd(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Flowers"])/
  sqrt(length(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Flowers"])) #54.65
sd_emission_fr <- sd(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Fruits"])/
  sqrt(length(tot_fl_fr$Abundance[tot_fl_fr$Plant=="Fruits"])) #246.26

mean_emission_fr/mean_emission_fl
#Fruits emit about 3.3 times more VOCs than flowers

################################ Compound type #################################
#Average compound type per treatment
comp_type_flfr <- ggplot(flower_fruit, aes(Type, Abundance, fill = Type, alpha = Plant)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = c("lightblue3", "darkolivegreen3", "orchid3", "goldenrod1"), 
                    labels = c("Aliphatic", "Benzenoid", "Terpenoid", "Fatty acid"), drop = F)+
  scale_alpha_discrete(range = c(0.7,1), labels = c("Flowers", "Fruits"))+
  guides(alpha = guide_legend(order = 1), fill = guide_legend(order = 2))+
  labs(x = 'Compound Type', y = 'Abundance (ng)', alpha = "Plant organ") +
  theme_bw()+
  theme(axis.text.x = element_text(size=15, angle = 45, vjust = 0.6),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=15),
        legend.title = element_text(size=15), 
        legend.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#ggsave("Flower_Fruit_compound_type.jpg",width = 30, height = 20, units = "cm")


#Significant differences in abundance according to compound type?
kruskal.test(Abundance ~ Type, data = flower_fruit)
pairwise.wilcox.test(flower_fruit$Abundance, flower_fruit$Type, p.adjust.method = 'BH')

##Significant differences in abundance of compound type for each treatment?
source("function_vocs.R")
stat_plant_type(flower_fruit, comp_fl_fr_type)


############################## NMDS & perMANOVA #################################
tot_ng <- read_excel('Volatiles.xlsx', sheet = "Tot_dataset")
tot_ng <- tot_ng[-c(8,9,10,32 ), ] # remove HF 8 and 9 because there was a problem during sampling, remove HF10 and FRO2 because outlier 

cols_ng_frfl <- c("Treatment", "Sample", "Type")
tot_ng[cols_ng_frfl] <- lapply(tot_ng[cols_ng_frfl], factor)

tot_ng_matrix <- tot_ng[4:30]
rownames(tot_ng_matrix) <- tot_ng$Sample


## NMDS
set.seed(1)
tot.mds <- metaMDS(tot_ng_matrix, k = 2, try=100, distance ='bray')
NMDSstress_fl_fr <- round(tot.mds$stress, 2)

#extract scores
data.scores <- as.data.frame(scores(tot.mds, "sites"))  # extract the site scores 
data.scores$site <- rownames(tot_ng_matrix)  # create a column of site names
data.scores$treat <- tot_ng$Treatment  #  add the treatment variable 
data.scores$type <- tot_ng$Type  #  add the type variable 


#Plot NMDS
NMDS_flfr <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=type),size=4) + 
  stat_ellipse(data=data.scores,aes(x=NMDS1,y=NMDS2,color=type))+
  scale_colour_manual(values=c("Flower" = "lightpink", "Fruit" = "palegoldenrod")) +
  labs(color = "Plant organ", x = glue("NMDS1 \nStress = {NMDSstress_fl_fr}"))+
  guides(color = guide_legend(byrow = TRUE))+
  xlim(-1.1,1.1)+
  ylim(-1.1,1.1)+
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size=22),
        title = element_text(size =18),
        legend.text = element_text(size =18),
        legend.title = element_text(size =20),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.box.spacing = unit(0.1, "pt"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        plot.background = element_blank(),
        axis.ticks = element_blank())

#ggsave("Flower_Fruit_NMDS.jpg",width = 30, height = 20, units = "cm")


## perMANOVA
tot.dist <- vegdist(tot_ng_matrix, method="bray")

tot.type <- data.frame(Type = tot_ng$Type)
d.manova <- adonis2(tot_ng_matrix ~ Type, method = "euclidean", data= tot.type)
d.manova # significant

#pairwise adonis
tot.adonis.pw <- pairwise.adonis(x = tot_ng_matrix,
                                  factors= tot_ng$Treatment,
                                  sim.method='bray',
                                  p.adjust.m='BH')
tot.adonis.pw <- as.data.frame(tot.adonis.pw)
tot.adonis.pw$F.Model <- round(tot.adonis.pw$F.Model, 2)
tot.adonis.pw$R2 <- round(tot.adonis.pw$R2, 2)
tot.adonis.pw



################################################################################
############################### 4 - General plots ##############################

##Compound type
comp_type <- plot_grid(comp_type_fl +
                        theme(axis.title.x = element_blank(),
                              legend.justification = "left"),
                       comp_type_fr +
                         theme(axis.title.x = element_blank(),
                               legend.justification = "left"),
                       comp_type_flfr +
                         theme(axis.title.x = element_blank(),
                               legend.justification = "left"),
                      align = "hv",
                      ncol = 2,
                      labels = c('(A)', '(B)', '(C)'),
                      label_size = 22) 
  
#ggsave("Compound_type.jpg",width = 35, height = 20, units = "cm")


## NMDS 
NMDS <-plot_grid(NMDS_fl +
                   theme(legend.justification = "left"),
                 NMDS_fr + 
                   theme(legend.justification = "left"),
                 NMDS_flfr +
                   theme(legend.justification = "left"),
                 align = "hv",
                 ncol = 2,
                 labels = c('(A)', '(B)', '(C)'),
                 label_size = 22)

#ggsave("NMDS.jpg",width = 35, height = 20, units = "cm")


##Flower and fruit compound abundance
flfr <- ggarrange(flower_comp, fruit_comp, 
                  ncol = 1, nrow = 2,
                  labels = c("(A)", "(B)"),
                  font.label=list(color="black",size=22))

#ggsave("Flower_Fruit_compounds.jpg",width = 45, height = 38, units = "cm")
