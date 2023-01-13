##For each compound: is the abundance significantly different beetween treatment?
stat_compound <- function(data, compounds){
  
  pvalues<- numeric()  
  
  for (compound in compounds){
      dataset_compound <- subset(data, data$Compounds == compound)
      
    
      #test normality
      pval <- shapiro.test(dataset_compound$Abundance)[2] #extract p value
      
      if (pval > 0.05){
        #if normal
        model <- lm(Abundance ~ Treatment, data = dataset_compound)
        a <- anova(model)
        pvalues[compound]<-  a$`Pr(>F)`[1]
        em <- emmeans(model, specs = "Treatment")
        
        out <- capture.output(pairs(em, adjust = "bh"))
        cat(file = paste(compound, "_pairs.txt"), out, sep="\n")
        
      }
      
      else {
        #if not normal
        
          k <- kruskal.test(Abundance ~ Treatment, data = dataset_compound)
          pvalues[compound]<- k[3]
          
          out <- capture.output(pairwise.wilcox.test(dataset_compound$Abundance, 
                                                     dataset_compound$Treatment, 
                                                     p.adjust.method = 'BH'))
          cat(file = paste(compound, "_wilcox.txt"), out, sep="\n")
        
      }
    }
    
p.adjust(pvalues, method = "BH") 
  
  }
  

##For each compound type: is the abundance significantly different beetween treatment?
stat_compound_type <- function(data, types){
  
  pvalues<- numeric()  
  
  for (type in types){
    dataset_type <- subset(data, data$Type == type)
    
    #test norality
    pval <- shapiro.test(dataset_type$Abundance)[2] #extraire p value
    
    if (pval > 0.05){
      #if normal
      model <- lm(Abundance ~ Treatment, data = dataset_type)
      a <- anova(model)
      pvalues[type]<-  a$`Pr(>F)`[1]
      em <- emmeans(model, specs = "Treatment")
      
      out <- capture.output(pairs(em, adjust = "bh"))
      cat(file = paste(type, "_pairs.txt"), out, sep="\n")
      
    }
    
    else {
      #if not normal
      
      k <- kruskal.test(Abundance ~ Treatment, data = dataset_type)
      pvalues[type]<- k[3]
      
      out <- capture.output(pairwise.wilcox.test(dataset_type$Abundance, 
                                                 dataset_type$Treatment, 
                                                 p.adjust.method = 'BH'))
      cat(file = paste(type, "_wilcox.txt"), out, sep="\n")
      
    }
  }
  
  p.adjust(pvalues, method = "BH") 
  
}


##For each compound type: is the abundance significantly different beetween plant organ?
stat_plant_type <- function(data, types){
  
  pvalues<- numeric()  
  
  for (type in types){
    dataset_type <- subset(data, data$Type == type)
    
    #test normality
    pval <- shapiro.test(dataset_type$Abundance)[2] #extraire p value
    
    if (pval > 0.05){
      #if normal
      model <- lm(Abundance ~ Plant, data = dataset_type)
      a <- anova(model)
      pvalues[type]<-  a$`Pr(>F)`[1]
      em <- emmeans(model, specs = "Treatment")
      
      out <- capture.output(pairs(em, adjust = "bh"))
      cat(file = paste(type, "_pairs.txt"), out, sep="\n")
      
    }
    
    else {
      #if not normal
      
      k <- kruskal.test(Abundance ~ Plant, data = dataset_type)
      pvalues[type]<- k[3]
      
      out <- capture.output(pairwise.wilcox.test(dataset_type$Abundance, 
                                                 dataset_type$Plant, 
                                                 p.adjust.method = 'BH'))
      cat(file = paste(type, "_wilcox.txt"), out, sep="\n")
      
    }
  }
  
  p.adjust(pvalues, method = "BH") 
  
}
