## ---------------------------
## Script name: Trabalho_v4
##
## Purpose of script: Realizar Trabalho para Fundamentos em Metodos de Aprendizagem Nao Supervisionada
##
## Version 4:
##            * Incluida a caracterização sócio demográfica das classes latentes
##            * Adicionado o AIC à tabela dos resultados para assessorar a escolha do número de classes latentes
##
## Version 3
##            * Feita a an?lise das classes latentes
##            (mas ainda n?o est? feita a segmenta??o atrav?s da perfilagem utilizando as vari?veis sociodemogr?ficas)
##
## Version: 2
##            * Feita a ACP - solução pca5r
##            * Iniciou-se as classes latentes
##
## Version: 1 
##            * Labels
##            * Tabela de Proporcoes
##            * Cluster de Variaveis
##          
## Author: Pedro Laranjeira + Edgar Basto + Tiago Dias + Bruno Araujo
##
## Date Created: 2020-01-11
##
## ---------------------------
## Notes:
##  Feita a ACP - solução pca5r
##  Iniciou-se as classes latentes
## ---------------------------

## set working directory ####
wd <- "/home/president/ISCTE/ML-Unsupervised/Fundamentos em Métodos de Aprendizagem Não Supervisionada/Trabalho de Grupo/"
setwd(wd)


## options  ####
options("scipen" = 999)

## load up the packages  ####
library(dplyr)
library(Hmisc)
library(ClustOfVar)
library(psych)
## ---------------------------

#1) Importa Data #################################################################################################################################
luxo_df <- read.csv(file = paste0(wd, "Luxo.csv"), header = TRUE,sep = ",", stringsAsFactors = FALSE)


#2) Explore dataframe   #################################################################################################################################
summary(luxo_df)


#Remove NA rows
luxo_df_NoNA <- luxo_df %>% 
                  na.omit()
#luxo_df_NoNA <- na.omit(luxo_df)

# luxo_df %>% 
#   filter(complete.cases(.))


#3) Variables Description   #################################################################################################################################
describe(luxo_df_NoNA)
summary(luxo_df_NoNA)


# Proportions Table
x <- Hmisc::describe(luxo_df)
x_df <- data.frame( "Variable_Name"= as.character(colnames(luxo_df)[5:19]),
                    "A" = as.integer(NA),
                    "B" = as.integer(NA),
                    "C" = as.integer(NA),
                    "D" = as.integer(NA),
                    "E" = as.integer(NA))



for (i in 5:19){
  
  x_df[i-4, 2:6] <- round(x[[i]][["values"]][["frequency"]] / sum(x[[i]][["values"]][["frequency"]]),2)
  
}




#4) Levels & Labels   #################################################################################################################################
luxo_df_factor <- luxo_df_NoNA
colnames(luxo_df_factor)[1] <- "ID"

# Sexo 
sexo_levels <- c(0,1)
sexo_labels <- c("Masculino", "Feminino")
luxo_df_factor$Sexo_factor <- factor(luxo_df_factor$Sexo, sexo_levels, sexo_labels)

# Idade 
summary(luxo_df_factor$Idade)

  #1: <= 27;
  #2: 28 - 43;
  #3: 44 - 54;
  #4: >= 55 


luxo_df_factor<- luxo_df_factor %>% 
                  mutate(Idade_factor = case_when(
                                                Idade <= 27 ~ "Idade até 27 Anos",
                                                Idade >= 28 & Idade <= 43 ~ "Idade entre 28 e 43 Anos", 
                                                Idade >= 44 & Idade <= 54 ~ "Idade entre 44 e 54 Anos", 
                                                Idade >= 55 ~ "Idade superior a 55 Anos"))

luxo_df_factor$Idade_factor <- as.factor(luxo_df_factor$Idade_factor)

# Rendimento
rendimento_levels <- c(1,2,3,4)
rendimento_labels <- c("At? 499?", "Entre 500 Euros e 1999 Euros", "Entre 2000 Euros e 4999 Euros", "5000 Euros ou mais")
luxo_df_factor$Rendimento_factor <- factor(luxo_df_factor$Rendimento, rendimento_levels, rendimento_labels)

# Variaveis P1:P15: Mesma Escala e as mesmas Labels
p1_p15_levels <- c(1,2,3,4,5)
p1_p15_labels <- c("Discordo Totalmente", "Discordo", "Não Concordo nem Discordo", "Concordo", "Concordo Totalmente")

luxo_df_factor$p1_factor <- factor(luxo_df_factor$p1, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p2_factor <- factor(luxo_df_factor$p2, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p3_factor <- factor(luxo_df_factor$p3, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p4_factor <- factor(luxo_df_factor$p4, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p5_factor <- factor(luxo_df_factor$p5, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p6_factor <- factor(luxo_df_factor$p6, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p7_factor <- factor(luxo_df_factor$p7, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p8_factor <- factor(luxo_df_factor$p8, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p9_factor <- factor(luxo_df_factor$p9, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p10_factor <- factor(luxo_df_factor$p10, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p11_factor <- factor(luxo_df_factor$p11, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p12_factor <- factor(luxo_df_factor$p12, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p13_factor <- factor(luxo_df_factor$p13, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p14_factor <- factor(luxo_df_factor$p14, p1_p15_levels, p1_p15_labels)
luxo_df_factor$p15_factor <- factor(luxo_df_NoNA$p15, p1_p15_levels, p1_p15_labels)

# Order Variables Position 
luxo_df_factor <- luxo_df_factor %>% 
                    dplyr::select(ID, 
                                  Sexo, Sexo_factor, 
                                  Idade, Idade_factor, 
                                  Rendimento, Rendimento_factor,
                                  p1, p1_factor,
                                  p2, p2_factor,
                                  p3, p3_factor,
                                  p4, p4_factor,
                                  p5, p5_factor,
                                  p6, p6_factor,
                                  p7, p7_factor,
                                  p8, p8_factor,
                                  p9, p9_factor,
                                  p10, p10_factor,
                                  p11, p11_factor,
                                  p12, p12_factor,
                                  p13, p13_factor,
                                  p14, p14_factor,
                                  p15, p15_factor)



#5) ACP   #################################################################################################################################
# estava feito antes cor(luxo_df[ ,5:19], method = "spearman", use = "complete.obs")


head(luxo_df_NoNA[,c(5:19)])
corrmat <- cor(luxo_df_NoNA[,c(5:19)], method = "pearson")  
round(corrmat, 2)  
#Teste de Bartlet para testar se a matriz de correlações não é uma matriz identidade
cortest.bartlett(corrmat, n=dim(luxo_df_NoNA[,c(5:19)])[1])

#KMO 0,7 valor mínimo. Dá um valor generalizado das correlações.
KMO(luxo_df_NoNA[,c(5:19)]) #como dá 0.86 podemos realizar 

#PCA1 Começa-se como número total das variáveis (neste caso 15)
pca1 <- principal(luxo_df_NoNA[,c(5:19)], nfactors=length(luxo_df_NoNA[,c(5:19)]), rotate="none", scores=TRUE)
pca1$values
pca1$loadings
plot(pca1$values, type = "b")

#PCA4
pca4 <- principal(luxo_df_NoNA[,c(5:19)], nfactors=4, rotate="none")
pca4$values
pca4$loadings
pca4$communality

#PCA4r
pca4r <- principal(luxo_df_NoNA[,c(5:19)], nfactors=4, rotate="varimax")
pca4r$values
pca4r$loadings
pca4r$communality


#PCA4r - sem p13, p14
pca4r_v2_df <- luxo_df_NoNA %>% 
                select(p1:p12, p15)


pca4r_v2 <- principal(pca4r_v2_df, nfactors=4, rotate="varimax")
pca4r_v2$values
pca4r_v2$loadings
pca4r_v2$communality


#PCA3r
pca3r <- principal(luxo_df_NoNA[,c(5:19)], nfactors=3, rotate="varimax")
pca3r$values
pca3r$loadings
pca3r$communality


#PCA5r - Decidiu-se fazer uma PCA com 5 componentes principais, com o intuito de tentar isolar as variáveis p13 e p14.
#Porque quando fizemos a PCA4r a p13 e p14 estavam em duas componentes princiais.

cor(luxo_df_NoNA$p13, luxo_df_NoNA$p14, method = 'pearson')

pca5r <- principal(luxo_df_NoNA[,c(5:19)], nfactors=5, rotate="varimax", scores = TRUE)
pca5r$values
pca5r$loadings
pca5r$communality

#Calcula o alpha de Cronbach
pc2 <- luxo_df_NoNA %>% 
          select(p1:p5)
pc1 <- luxo_df_NoNA %>% 
          select(p8:p10, p15)
pc4 <- luxo_df_NoNA %>% 
          select(p11, p12)
pc3 <- luxo_df_NoNA %>% 
          select(p6, p7)
pc5 <- luxo_df_NoNA %>% 
          select(p13, p14)

alphaPC2<-psych::alpha(pc2)
alphaPC2[["total"]][["raw_alpha"]]
alphaPC1<-psych::alpha(pc1)
alphaPC1[["total"]][["raw_alpha"]]
alphaPC4<-psych::alpha(pc4)
alphaPC4[["total"]][["raw_alpha"]]
alphaPC3<-psych::alpha(pc3)
alphaPC3[["total"]][["raw_alpha"]]
alphaPC5<-psych::alpha(pc5)
alphaPC5[["total"]][["raw_alpha"]]

#Todos os Alphas de Cronbach são superiores a 0.6, por isso podemos utilizar as 5 componentes principais.

#Gravar scores
pca5r$scores

#Cria um indice para substituir a 1?CP
# exemplo JORNAIS ??jornais$IndForma <-(1/6)*(jornais$p5c + jornais$p5d + jornais$p5e + jornais$p5f + jornais$p5g + jornais$p5i)
# fa?a os mesmo para as outras CP




#6) Variable Clustering   #################################################################################################################################
cluster01_df <- luxo_df_NoNA[ ,5:19]

# Alternativa 1
tree <- hclustvar(cluster01_df)
plot(tree)

# Alternativa 2 -> Distancia Angular (Prof )
var_dist    <- as.dist(1-cor(cluster01_df)) 
var_hclust <- hclust(var_dist)
var_name <-colnames(cluster01_df)
plot(var_hclust,label=var_name,hang=-1)
abline(h = 0.8, v = 0, col = "red")
abline(h = 0.45, v = 6, col = "blue")

#7) Classes Latentes #########################################################################
colnames(luxo_df_factor)
df_latentes <- luxo_df_factor %>% 
                  select(Sexo_factor, Idade_factor, Rendimento_factor, p1_factor,
                         p2_factor, p3_factor, p4_factor, p5_factor, p6_factor,
                         p7_factor, p8_factor, p9_factor, p10_factor, p11_factor,
                         p12_factor, p13_factor, p14_factor, p15_factor)


#Model based on p3, p4, p7, p9, p12, p15 (factor???)
f <- cbind(p3_factor, p4_factor, p7_factor, 
           p9_factor, p12_factor, p15_factor)~1
                    

#install.packages("poLCA")
library("poLCA")

# Model
lcamodel.k1 <- poLCA(f, df_latentes, nclass=1, maxiter=1000, graphs=FALSE, 
                     tol=1e-10, na.rm=TRUE, probs.start=NULL, nrep=1, verbose=TRUE, calc.se=TRUE)

#estimate the model with k-classes
lc1<-poLCA(f, df_latentes, nclass=1, nrep=10) #, na.rm=FALSE, Graph=TRUE)
lc2<-poLCA(f, df_latentes, nclass=2, nrep=10) #, na.rm=FALSE, Graph=TRUE)
lc3<-poLCA(f, df_latentes, nclass=3, nrep=10) #, na.rm=FALSE, Graph=TRUE)
lc4<-poLCA(f, df_latentes, nclass=4, nrep=10) #, na.rm=FALSE, Graph=TRUE)
lc5<-poLCA(f, df_latentes, nclass=5, nrep=10) #, na.rm=FALSE, Graph=TRUE)
lc6<-poLCA(f, df_latentes, nclass=6, nrep=10) #, na.rm=FALSE, Graph=TRUE)

###################################################################
#######                                                     #######
#######                 Model selection (K)                 #######
#######                                                     #######
###################################################################

# generate dataframe with fit-values and plot
results <- data.frame(Model=c("Model 1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      BIC=lc1$bic,
                      AIC=lc1$aic)

results$Model<-as.integer(results$Model)
results[1,1]<-c("Model 1")
results[2,1]<-c("Model 2")
results[3,1]<-c("Model 3")
results[4,1]<-c("Model 4")
results[5,1]<-c("Model 5")
results[6,1]<-c("Model 6")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df

results[2,4]<-lc2$bic
results[3,4]<-lc3$bic
results[4,4]<-lc4$bic
results[5,4]<-lc5$bic
results[6,4]<-lc6$bic

results[2,5]<-lc2$aic
results[3,5]<-lc3$aic
results[4,5]<-lc4$aic
results[5,5]<-lc5$aic
results[6,5]<-lc6$aic

results

## O Modelo com duas classes latentes ? o que tem o menor BIC.

###################################################################
#######                                                     #######
#######           Interpret the latent classes              #######
#######                                                     #######
###################################################################
library("ggplot2")
library("reshape2")
#set.seed(2012) #N?o sei se devemos deixar ficar isto
lc2<-poLCA(f, df_latentes, nclass=2, nrep=5, graphs = TRUE) #nrep= numero de vezes que vai ser corrida a estima??o (afina??o?)
lc2


#TESTE 
lc3<-poLCA(f, df_latentes, nclass=3, nrep=5, graphs = TRUE) #nrep= numero de vezes que vai ser corrida a estima??o (afina??o?)
lc3

#reorder classes
#SE PRECISARMOS DE TROCAR A ORDEM DAS CLASSES
#NO NOSSO CASO N?O ACHO QUE FA?A SENTIDO
#probsstart<-lc2$probs.start
#probsstart
#probsstart<-poLCA.reorder(probsstart, c(2,1))
#lc2<-poLCA(f, dataset, nclass=2, nrep=1, probs.start=probsstart, graphs = TRUE)


# Items within classes
lcmodel <- reshape2::melt(lc2$probs, level=2)

zp <- ggplot(lcmodel[,],aes(x = L2, y = value, fill = Var2))
zp <- zp + geom_bar(stat = "identity", position = "stack")
zp <- zp + facet_grid(Var1 ~ .) 
zp <- zp + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
zp <- zp + labs(x = "Items",y="Probability", fill ="Legend")
zp <- zp + theme( axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),                    
                  panel.grid.major.y=element_blank())
zp <- zp + guides(fill = guide_legend(reverse=TRUE))
print(zp)



###################################################################
#######                                                     #######
#######           Demographics within classes             #######
#######                                                     #######
###################################################################
# 
#
#                 AINDA N?O EST? COMPLETO
#
#
#install.packages("fastDummies")
library("fastDummies")
#install.packages("optimbase")
library("optimbase")

#p3_factor, p4_factor, p7_factor, p9_factor, p12_factor, p15_factor 

#Sexo
sexo <-- dummy_columns(df_latentes$Sexo_factor)
(t(sexo[,2:3]) %*% lc2$posterior ) / (ones(2,1) %*% colSums(lc2$posterior))

#Idade
idade <-- dummy_columns(df_latentes$Idade_factor)
(t(idade[,2:5]) %*% lc2$posterior ) / (ones(4,1) %*% colSums(lc2$posterior))

#Rendimento
rendimento <-- dummy_columns(df_latentes$Rendimento_factor)
(t(rendimento[,2:5]) %*% lc2$posterior ) / (ones(4,1) %*% colSums(lc2$posterior))






