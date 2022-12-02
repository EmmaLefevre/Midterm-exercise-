#Setting the parameters
dat = read.csv("alpineplants.csv")
x1 = dat$soil_moist
y = dat$snow
group = dat$Carex.bigelowii
group2= dat$Thalictrum.alpinum

#Model Choice 
#For Carex Bigelowii
m1 = lm(y ~ x1 * group)# ANCOVA
m2 = lm(y ~ x1 + group)# Multiple regression 
m3 = lm(y ~ x1) #Linear regression 
m4 = lm(y ~ group)# ANOVA 
m5 = lm(y ~ 1) # LMM
mlist = list(m1, m2, m3, m4, m5)
AICTab = AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#For Thalictrum Alpinum
m1B = lm(y ~ x1 * group2)# ANCOVA
m2B = lm(y ~ x1 + group2)# Multiple regression 
m3B = lm(y ~ x1) #Linear regression 
m4B = lm(y ~ group2)# ANOVA 
m5B = lm(y ~ 1) # LMM
mlist = list(m1B, m2B, m3B, m4B, m5B)
AICTab = AIC(m1B, m2B, m3B, m4B, m5B) # create model selection table 
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# Looking at the summary for both more fitted model 
summary(m1)
summary(m1B)

#opening the package needed for the plots 
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library("ggplot2")


#plot 1 - Soil moisture according to species 
  data(dat)
  colors <- c("Carex Bigelowii" = "green", "Thalictrum Alpinum" = "orange")
  "Species Abundance" <- c("Carex Bigelowii","Thalictrum Alpinum")
 Plot1 <- ggplot(dat, aes(x = soil_moist, y= Carex.bigelowii)) +
    geom_point(aes(x = soil_moist, y = Carex.bigelowii, color = "Carex Bigelowii"), ) +
    geom_smooth(method = glm,  aes(x= soil_moist, y=Carex.bigelowii, color="Carex Bigelowii"))+
  geom_point(aes(x = soil_moist,y = Thalictrum.alpinum, color = "Thalictrum Alpinum")) +
    geom_smooth( method = glm, aes(x=soil_moist, y= Thalictrum.alpinum, color="Thalictrum Alpinum"))+
    labs(x = "Soil Moisture (%)",
         y = "Species Abundance",
         color = "Legend") +
    scale_color_manual(values = colors)
  
#Plot 2 - Species according to snow 
Plot2 <- ggplot(dat, aes(x = snow, y=Carex.bigolewii)) +
    geom_point(aes(x = snow, y = Carex.bigelowii, color = "Carex Bigelowii"), ) +
    geom_smooth(method = glm,  aes(x= snow, y=Carex.bigelowii, color="Carex Bigelowii"))+
    geom_point(aes(x =snow ,y = Thalictrum.alpinum, color = "Thalictrum Alpinum")) +
    geom_smooth( method = glm, aes(x=snow, y= Thalictrum.alpinum, color="Thalictrum Alpinum"))+
    labs(x = "Snow coverage (cm)",
         y = "Species Abundance",
         color = "Legend") +
    scale_color_manual(values = colors)

ggarrange(Plot1,Plot2 + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

#GLM Analysis 

# For Carex Bigelowii
y2 <-((group>1)*1)
m = glm(y2~x1+group, family=gaussian(link="identity"))
summary(m)
coefs = summary(m)$coef
library(MuMIn)
r.squaredGLMM(m)
y_hat = coefs[1,1] + coefs[2,1]*x1

# For Thalictrum Alpinum
y3 <-((group2>1)*1)
m = glm(y3~x1+group2, family=gaussian(link="identity"))
summary(m)
coefs = summary(m)$coef
library(MuMIn)
r.squaredGLMM(m)
y_hat = coefs[1,1] + coefs[2,1]*x1



#Plot 3 - Snow and Soil moisture impact to Carex Bigelowii 
Plot3 <- ggplot(dat, aes(x=soil_moist, y=snow, group=Carex.bigelowii, scale_shape_bined(Carex.bigelowii))) +
  geom_point() +
  facet_wrap(~Carex.bigelowii , dir="h")+
  geom_smooth(method=lm, color="red", se=TRUE)+
  theme(legend.position="none") +
  ggtitle("Abundance of Carex")+
  labs(x = "Soil Moisture (%)",
       y = "Snow coverage (cm)")+
  facet_wrap(~Carex.bigelowii, scale="free_y")

#Plot 4 - Snow and soil moisture impact to Thalictrum Alpinum
Plot4 <- ggplot(dat, aes(x=soil_moist, y=snow, group=Thalictrum.alpinum, scale_shape_bined(Thalictrum.alpinum))) +
    geom_point() +
    facet_wrap(~Thalictrum.alpinum , dir="h")+
    geom_smooth(method=lm, color="red", se=TRUE)+
    theme(legend.position="none") +
    ggtitle("Abundance of Thalictrum")+
  labs(x = "Soil Moisture (%)",
       y = "Snow coverage (cm)")+
  facet_wrap(~Thalictrum.alpinum, scale="free_y")
  
  ggarrange(Plot3,Plot4 + rremove("x.text"), 
            labels = c("C", "D"),
            ncol = 1, nrow = 2)

#Ancova analysis
#For Carex Bigelowii 
  anova(lm(y ~ x1 * group))
  AnovaCarex = lm(y ~ -1 + group + x1:group)
  summary(AnovaCarex)
  anova(AnovaCarex)
  
# For Thalictrum Alpinum
  anova(lm(y ~ x1 * group2))
  AnovaThalictrum = lm(y ~ -1 + group2 + x1:group2)
  summary(AnovaThalictrum)
  anova(AnovaThalictrum)


