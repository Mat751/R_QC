library(gplots)
library(genefu)
library(xtable)
library(rmeta)
library(Biobase)
library(caret)

#PAM50
ddata <- t(exprs(eset))
dannot <- fData(eset)

#LE PRIME TRE COLONNE DEVONO CORRISPONDERE AI SEGUENTI NOMI
colnames(dannot)<-c("probe","Gene.Symbol","EntrezGene.ID")

PAM50Preds<-molecular.subtyping(sbt.model = "pam50",data=ddata,
                                annot=dannot,do.mapping=TRUE)
pData(eset)$PAM50 <- PAM50Preds$subtype

#SCMOD2
SubtypePredictions<-molecular.subtyping(sbt.model = "scmod2",data = ddata,
                                        annot = dannot,do.mapping = TRUE)

SubtypePredictions$subtype <- ifelse(SubtypePredictions$subtype == "ER-/HER2-","Basal",
                                     ifelse(SubtypePredictions$subtype =="HER2+","Her2",
                                            ifelse(SubtypePredictions$subtype =="ER+/HER2- High Prolif","LumB","LumA")))
pData(eset)$scmod2 <- SubtypePredictions$subtype
