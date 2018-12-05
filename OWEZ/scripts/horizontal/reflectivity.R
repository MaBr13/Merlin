my_table <- read.csv('2005_Flagfile_S-band.csv',sep=";")

my_table <- subset(my_table, group=='')
is.numeric(my_table$rangem)

starling <- subset(my_table, species=='Starling',select=species:RangeReflectivity)
pipit <- subset(my_table, species=='Meadow Pipit',select=species:RangeReflectivity)
gulls <- subset(my_table, species=='Herring Gull',select=species:RangeReflectivity) 


summary(starling$Area)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.00    9.00   16.50   18.61   25.75   46.00 
summary(starling$AvReflectivity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#652.0   774.5   853.0   849.8   918.2  1131.0
summary(pipit$Area)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.00   11.00   18.00   19.55   25.00   47.00 
summary(pipit$AvReflectivity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#612.0   723.0   806.0   810.7   865.0  1264.0 
summary(gulls$Area)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.00    7.00   11.00   15.58   18.00  193.00 
summary(gulls$AvReflectivity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#501.0   726.0   790.0   798.7   859.0  1510.0 


####COMPARISON BETWEEN THE GROUPS (BOXPLOT AND ANOVA)

library(dplyr)
group_by(my_table, group) %>%
  summarise(
    count = n(),
    mean = mean(Area, na.rm = TRUE),
    sd = sd(Area, na.rm = TRUE)
  )

install.packages("ggpubr")

library("ggpubr")
ggboxplot(my_table, x = "group", y = "AvReflectivity", 
          color = "group",
          ylab = "Av. Reflectivity", xlab = "Species")+
  theme(axis.title.y = element_text(size=18), axis.title.x = element_blank(),legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "none",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=10,angle=90), plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(size = 18, face = "bold"))

res.aov <- aov(AvReflectivity ~ group, data = my_table)
summary(res.aov)
TukeyHSD(res.aov)

