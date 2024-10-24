View(ABW_data)

library(car)
library(lme4)
library(lmerTest)
library(tidyverse)
library(agricolae)
library(emmeans)


ABW_data$comb_trt <- (ABW_data$Treatment) ##created new column with combined data
##The below lines are combining the early and late pesticide treatments
ABW_data$comb_trt <- ifelse(ABW_data$comb_trt == "T7", "T2", ABW_data$comb_trt) ##combined Suprado
ABW_data$comb_trt <- ifelse(ABW_data$comb_trt == "T8", "T3", ABW_data$comb_trt)##combined Tetrino
ABW_data$comb_trt <- ifelse(ABW_data$comb_trt == "T9", "T4", ABW_data$comb_trt)##combined Ference
ABW_data$comb_trt <- ifelse(ABW_data$comb_trt == "T10", "T5", ABW_data$comb_trt)##Combined Plinazolin
levels(ABW_data$comb_trt) ##used this to see how the levels were listed and then renames the levels below based on the order they were listed here

ABW_data$comb_trt <- as.factor(ABW_data$comb_trt)
levels(ABW_data$comb_trt)

levels(ABW_data$comb_trt) <- c("Control","*Matchpoint", "*Provuant","*Dylox", "Suprado", "Tetrino","Ference","Plinazolin","*Acelepryn")

table(ABW_data$comb_trt)

ABW_data$comb_trt ##double check the labels are showing on the table


####Pairwise comparisons with all treatments####

ggplot(ABW_data, aes(x = Treatment, y = Larvae_per_square_foot, fill = Treatment)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Larvae per Square Foot by Treatment",
       x = "Treatment",
       y = "Larvae per Square Foot") 
##comparing to total live larvae versus sq/ft--should be same stats as larvae per sqft
ggplot(ABW_data, aes(x = Treatment, y = Total_Larvae_New_adults, fill = Treatment)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Total ABW counts by Treatment",
       x = "Treatment",
       y = "# Larvae and Adults Alive") 

summary(ABW_data$Treatment)

fit_1<-lm(Larvae_per_square_foot~Treatment, data=ABW_data)
anova(fit_1)
##anova shows that treatment groups differ, and then emmeans to look into differences (ad hoc)
emmeans(fit_1, pairwise~Treatment)
library(multcomp)
##in the future we will perhaps add GLM
ABW_data$comb_trt =as.factor(ABW_data$comb_trt)

ABW_data$Treatment=as.factor(ABW_data$Treatment)

tukey_fit_1<-glht(fit_1, linfct=mcp(Treatment = "Tukey"))

summary(tukey_fit_1)

####Graphs & Pairwise comparisons for combined treatments####
ggplot(ABW_data, mapping = aes(y=Larvae_per_square_foot, x=comb_trt))+
  geom_boxplot()+
  theme_classic()+
  ylab("Number of Larvae per Sqft")+
  xlab("Treatments total (early+late)")

ggplot(ABW_data, mapping = aes(y=Larvae_per_square_foot, x=comb_trt, fill=comb_trt))+
  geom_bar(stat = "identity") +
  theme_classic()+
  ylab("Number of Larvae per Sqft")+
  xlab("Treatments total (early+late)")


fit_1.1<-lm(Larvae_per_square_foot~comb_trt, data=ABW_data)
anova(fit_1.1)
emmeans(fit_1.1, pairwise~comb_trt)

tukey_fit_1.1<-glht(fit_1.1, linfct=mcp(comb_trt = "Tukey"))
summary(tukey_fit_1.1)

###Now lets try to combine early treatments###
ABW_data$Treatment_early<- factor(ABW_data$Treatment, levels=c("T1","T2","T3","T4","T5","T6"))

levels(ABW_data$Treatment_early) <- c("Control","Suprado","Tetrino", "Ference","Plinazolin","Acelepryn")

ggplot(ABW_data, aes(x = Treatment_early, y = Larvae_per_square_foot, fill = Treatment_early)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Larvae per Square Foot by Early Treatment",
       x = "Early Treatment",
       y = "Larvae per Square Foot") 

ggplot(ABW_data, mapping = aes(y=Larvae_per_square_foot, x=Treatment_early))+
  geom_boxplot()+
  theme_classic()+
  ylab("Number of Larvae per Sqft")+
  xlab("Early Treatment")

fit_1_early<-lm(Larvae_per_square_foot~Treatment_early, data=ABW_data)

anova(fit_1_early)
##anova shows that treatment groups differ, and then emmeans to look into differences (ad hoc)
emmeans(fit_1_early, pairwise~Treatment_early)
library(multcomp)
##in the future we will perhaps add GLM

tukey_fit_1_early<-glht(fit_1_early, linfct=mcp(Treatment_early = "Tukey"))

summary(tukey_fit_1_early)

##In the early treatment it appears that all pesticides had a significant difference in reduction of larvae as compared to the controls. 
##However, it does not appear that there was not a significantly clear difference between pesticides


