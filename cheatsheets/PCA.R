library("rvest")
wikiPL <- "https://en.wikipedia.org/wiki/Letter_frequency"
webpage <- read_html(wikiPL)
table_links <- html_nodes(webpage, '.wikitable')
tables <- html_table(table_links, fill=TRUE)
tab <- tables[[3]]
head(tab)
colnames(tab) <- gsub(colnames(tab), pattern = "[^A-Za-z]", replacement = "")
tab2 <- apply(tab[,-1], 1:2, gsub, pattern = "[^0-9\\.]", replacement = "")
tab3 <- apply(tab2, 1:2, as.numeric)
tab4 <- data.frame(letter = tab[,1], tab3)

library(FactoMineR)
library(factoextra)

tab5 <- tab4[1:26,-1]
rownames(tab5) <- tab4[1:26,1]

tab6 <- tab5
for (i in 2:ncol(tab5))
  tab6[,i] <- tab5[,i]/sum(tab5[,i])

ob <- PCA(tab6, scale.unit = FALSE, ncp = 3)
PCA(tab6, scale.unit = FALSE, ncp = 3, axes = c(2,3))

ob
ob$eig



library(archivist)
filmy <- archivist::aread("pbiecek/Przewodnik/arepo/10aab376f2bc0001cbd1db1802e9fb53")
filmy2015 <- na.omit(filmy[filmy$year == "2015", c(1, 4:8)])
filmy2015 <- na.omit(filmy[filmy$year == "2015" &
                             filmy$Genre == "action", c(1,3,4:8)])
rownames(filmy2015) <- filmy2015[,1]
head(filmy2015)

library(factoextra)
filmy2015[,2] <- factor(filmy2015[,2])
model <- PCA(filmy2015[,3:7])

model <- PCA(filmy2015[,3:7])
model
summary(model)


pl1 <- fviz_screeplot(model)
get_eig(model)
pl2 <- fviz_pca_var(model) + coord_fixed()
pl3 <- fviz_pca_ind(model) 
pl4 <- fviz_pca_biplot(model,  
                habillage = filmy2015$script.type) + 
  theme(legend.position = "top")

ggsave(pl1, filename = "pca_1.pdf", width = 5.5, height = 5.5)
ggsave(pl2, filename = "pca_2.pdf", width = 5.5, height = 5.5)
ggsave(pl3, filename = "pca_3.pdf", width = 5.5, height = 5.5)
ggsave(pl4, filename = "pca_4.pdf", width = 12, height = 7)

