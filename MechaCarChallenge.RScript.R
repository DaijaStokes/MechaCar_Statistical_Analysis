### Deliverable 1 MODULE 15
library(dplyr)
mecha_mpg <- read.csv(file='/Users/daij/Desktop/Data BootCamp Dec. Cohort 2021/MechaCar_Statistical_Analysis/MechaCar_Statistical_Analysis/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg))
lm(mpg ~ vehicle_length + ground_clearance, data=mecha_mpg)
summary(lm(mpg ~ vehicle_length + ground_clearance, data=mecha_mpg)) 

### Deliverable 2 MODULE 15
mecha_coil <- read.csv(file='/Users/daij/Desktop/Data BootCamp Dec. Cohort 2021/MechaCar_Statistical_Analysis/MechaCar_Statistical_Analysis/Suspension_Coil.csv',check.names=F,stringsAsFactors = F) 
total_summary <- mecha_coil %>% summarize(Mean_PSI=mean(PSI),
                                          Median_PSI=median(PSI),
                                          Var_PSI=var(PSI),
                                          Std_Dev_PSI=sd(PSI),
                                          Num_Coil=n(), .groups = 'keep') 
lot_summary <- mecha_coil  %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                         Median_PSI=median(PSI),
                                                                         Var_PSI=var(PSI),
                                                                         Std_Dev_PSI=sd(PSI),
                                                                         Num_Coil=n(), .groups = 'keep')
library(ggplot2)
plt1 <- ggplot(mecha_coil,aes(y=PSI)) #import dataset into ggplot2
plt1 + geom_boxplot() #add boxplot
plt2 <- ggplot(mecha_coil,aes(x=Manufacturing_Lot,y=PSI)) #import dataset into ggplot2
plt2 + geom_boxplot()

### Deliverable 3
t.test(mecha_coil$PSI,mu=1500)
lot1 <- subset(mecha_coil, Manufacturing_Lot=="Lot1")
lot2 <- subset(mecha_coil, Manufacturing_Lot=="Lot2")
lot3 <- subset(mecha_coil, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
