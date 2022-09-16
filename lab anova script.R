##Install packages
install.packages("readr")
library(readr)

##First import your data and then attach it
attach(lab_meeting_data_ANOVA_)

#check for factors
is.numeric(Height)
is.factor(Population)
Population=as.factor(Population)
is.factor(System)
System=as.factor(System)

#######################
####### 
#Box plots
boxplot(Height~ Population, xlab="Pop ", ylab="Height (mm)", col= c("ivory","khaki","yellowgreen") )
boxplot(Height~ System, xlab="Col ", ylab="Height (mm)", col= c("navy","gold") )

###TEST ASSUMPTIONS

##Now test assumptions for Height vs Pop

#Shapiro hypothesis test for testing normality
shapiro.test(Height[Population=="City"])
shapiro.test(Height[Population=="Desert"])
shapiro.test(Height[Population=="Forest"])

#data are normal, now do test of homoscedascity using Bartlett test
bartlett.test(Height~ Population)

##Now test assumptions for Height vs System

shapiro.test(Height[System=="Dioecy"]) #normal
shapiro.test(Height[System=="Hermaphrodite"])


###If data is not normal use the following variance test
install.packages("car")
library(car)
leveneTest(Height,System)


#now anova 
model<-aov(Height~Population +System + Population:System)
summary(model)#this prints the ANOVA table ; 
TukeyHSD(model) # tukey's post hoc test to check which factor type is deviating


######

#simpler bar plots without shading
library(ggplot2)
#Pop#
#gives mean, sd, and n for different levels of factors 
anovan <- aggregate(Height,
                    by = list(sal = Population), FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))

#above data frame gives a matrices but we want it in vector form:                                                                                                                 
anovan <-do.call(data.frame,anovan) #puts it in vector form

#calculate st error and add it to our data.frame
anovan$se<-anovan$x.sd/sqrt(anovan$x.n)

#makes it look nicer
colnames(anovan) <- c("Population","Mean","sd","n","se")
anovan #looking good

#dodge I think gives bin width, but I'm not sure
#limits gives error bars
dodge <- position_dodge(width = 1)
limits <- aes(ymax = anovan$Mean + anovan$se,
              ymin = anovan$Mean - anovan$se)

#fill is the color coding of the bars
p <- ggplot(data = anovan, aes(x = Population, y = Mean, fill = Population))

#not sure what stat="identity" does but yeah, this should give us the bar plot with error bars
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) + labs(x = "Pop",y="Height (mm)")


##if you need to change colors
cbp2 <- c("#000000", "#E69F00", "#56B4E9")
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) +
  labs(x = "Color",y="Mean diameter (mm)") + scale_fill_manual(values = cbp2)

