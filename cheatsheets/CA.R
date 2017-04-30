# Dane z 
# https://www.kaggle.com/mylesoneill/game-of-thrones

battles <- read.csv("battlesGoT.csv")
selectedBattles <- battles[,c(6:9,15)]

library(tidyr)
selectedBattles <- gather(selectedBattles, atracker, family, -battle_type)
table(selectedBattles$family, selectedBattles$battle_type)[-1,]

library(ca)
tab <- table(House=selectedBattles$family, Battle_Type=selectedBattles$battle_type)[-1,-1]
tab <- tab[rowSums(tab) > 1,]
battles <- tab
plot(ca(battles))


library("FactoMineR")
res.ca <- CA(battles, graph = FALSE)
res.ca

library("factoextra")
# Result for row variables
get_ca_row(res.ca)
# Result for column variables
get_ca_col(res.ca)

pl1 <- fviz_ca_biplot(res.ca, repel = TRUE)
pl2 <- fviz_ca_biplot(res.ca, repel = TRUE, arrow = c(FALSE, TRUE), 
               col.row = "contrib", col.col = "red4",
               gradient.cols = c("#BBAFBB", "#00AFBB"))



# Graph of row points
fviz_ca_row(res.ca, repel = TRUE)
# Graph of column points
fviz_ca_col(res.ca)


# Row contributions to Dimension 1
fviz_contrib(res.ca, choice ="row", axes = 1)
# Column contributions to Dimension 1
fviz_contrib(res.ca, choice ="col", axes = 1)







ggsave(pl1, filename = "ca_1.pdf", width = 5.5, height = 5.5)
ggsave(pl2, filename = "ca_2.pdf", width = 6, height = 5.5)
ggsave(pl3, filename = "ca_3.pdf", width = 5.5, height = 5.5)
ggsave(pl4, filename = "ca_4.pdf", width = 5.5, height = 5.5)

