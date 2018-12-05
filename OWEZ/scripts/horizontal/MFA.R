library("FactoMineR")
library("factoextra")

setwd("C:/Users/mbradar/Documents/Merlin/OWEZ/data/reflectivity filter")
my_table <- read.csv('2005_Flagfile_S-band.csv',sep=";")#[ ,c(15,18:43)]
bla <- my_table[ ,c(15,18:43)]

pcamt <- prcomp(my_table[ ,c(15,18:43)],center = TRUE,scale.=TRUE)

res.mfa <- MFA(my_table, 
               group = c(2, 5, 3, 10, 9, 2))#, 
               type = c("n", "s", "s", "s", "s", "s"),
               name.group = c("origin","odor","visual",
                              "odor.after.shaking", "taste","overall"),
               num.group.sup = c(1, 6),
               graph = FALSE)