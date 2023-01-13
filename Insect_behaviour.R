### Insect behaviour assay - analysis

#Load packages 
library("readxl")
library("ggplot2")
library("pwr")
library("ggpubr")


################################################################################
################################# 1 - Phytomyza ################################

phyt <- read_excel('Insect_behaviour.xlsx', sheet = "Phytomyza")
cols_phyt <- c("Insect", "Sex_insect", "Flower_treat", "Flower_orientation", "Choice_yn", "Choice")
phyt[cols_phyt] <- lapply(phyt[cols_phyt], factor) 

############################### Final flower choice ############################

phyt_plot <- ggplot(phyt, aes(Flower_treat, Choice_num , fill = Choice)) + 
  geom_bar(stat = "identity", width = 0.2) +    
  coord_flip() +  
  labs(y = "Number of Phytomyza chosing either flower type") +
  scale_fill_manual(labels = c("Female-stage hermaphrodite", "Male"), 
                    values = c("brown3", "lightskyblue2") ) +
  scale_y_continuous(labels = abs)+
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=18, vjust = 22),
        axis.title.x = element_text(size=20, vjust = 18),
        axis.title.y = element_blank(),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18),
        legend.position = c(0.27, 0.78),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 1/2)
phyt_plot

#ggsave("Phyt_behav.jpg",width = 30, height = 20, units = "cm")


#Chisquare for the final preference
phyt_count <- data.frame(x= c("HF", "M"), y= c(5,2))
colnames(phyt_count) = c("Flower", "Count")
chisq.test(phyt_count$Count)


################# Is the final choice impacted by Insect sex  ? ################
#create a matrix of choice made by each insect sex
phyt_test <- matrix(data = c(3,2,2,0), nrow = 2, ncol = 2)
colnames(phyt_test) = c("Insect_M", "Insect_F")
rownames(phyt_test) = c("Flower_HF", "Flower_M")

fisher.test(phyt_test)



################################################################################
################################### 2 - Wasp ###################################

wasp <- read_excel('Insect_behaviour.xlsx', sheet = "Wasps") 
cols_wasps <- c("Insect", "Fruit_treat", "Fruit_orientation", "Choice_yn", "Choice")
wasp[cols_wasps] <- lapply(wasp[cols_wasps], factor) 

############################### Final fruit choice #############################

wasp_plot <- ggplot(wasp, aes(factor(Fruit_treat, level = c("PU", "UW", "PW")), Choice_num , fill = Choice)) +   # Fill column
  geom_bar(stat = "identity", width = 0.5) +     
  coord_flip() +  
  labs(y = "Number of Wasps choising either fruit type") +
  scale_fill_manual(labels = c("Predated", "Unwounded", "Wounded"), 
                    values = c("plum3", "darkseagreen3","tan") ) +
  scale_y_continuous(labels = abs)+
  theme_bw() +  
  theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size=18, vjust = 5),
      axis.title.x = element_text(size=20, vjust = 1),
      axis.title.y = element_blank(),
      legend.title = element_text(size=20),
      legend.text = element_text(size=18),
      legend.position = c(0.12, 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      aspect.ratio = 1/2)
wasp_plot

#ggsave("Wasp_behav.jpg",width = 30, height = 20, units = "cm")


###################### Looking at each treatment combination ###################
#Create a subset for all the wasps that did make a choice
wasp_sub <- subset(wasp, Choice_yn == "Y")

  ##For Treatment = PW
PW_sub <- subset(wasp_sub, Fruit_treat == "PW")

Prop_PW <- c(sum(PW_sub$Choice == 'P'), sum(PW_sub$Choice == 'W'))
names_PW <- c('P','W')
PW <- data.frame(names_PW, Prop_PW)

#total preference so no need for a chisquare


  ##For Treatment = UW
UW_sub <- subset(wasp_sub, Fruit_treat == "UW")

Prop_UW <- c(sum(UW_sub$Choice == 'U'), sum(UW_sub$Choice == 'W'))
names_UW <- c('U','W')
UW <- data.frame(names_UW, Prop_UW)

chisq.test(UW$Prop_UW)


  ##For Treatment = PU
PU_sub <- subset(wasp_sub, Fruit_treat == "PU")

Prop_PU <- c(sum(PU_sub$Choice == 'P'), sum(PU_sub$Choice == 'U'))
names_PU <- c('P','U')
PU <- data.frame(names_PU, Prop_PU)
chisq.test(PU$Prop_PU)


#################################################################################
############################### 3 - power analysis ##############################
# two-choice experiment, null hypothesis =p2= 0.5 (no preference); 
# alternative hypothesis= p1= 0.7 (differ by 0.2)

pwr.p.test(h = ES.h(p1 = 0.7, p2 = 0.50), 
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")



################################################################################
################################### Final plot #################################
ggarrange(phyt_plot, wasp_plot, 
          labels = c("(A)", "(B)"), font.label=list(color="black",size=18),
          ncol = 1, nrow = 2, label.x = 0.18) 

#ggsave("Behav_final.jpg",width = 30, height = 20, units = "cm")

