pdf("")
boxplot(exprs(eset),
        range=0,
        las=2,ylab=expression(log[2](data)),
        main="SST-RMA")
dev.off()

#Pearson Correlations distance
mydist <- function(x)(dist(x,method="euclidean"))

#Ward.D clustering (minum variance between dataset)
myclust <- function(x)hclust(x,method = "ward.D2")

correl <- cor(exprs(eset))
eset$Class <- gsub("^ ","",gsub("^ |RAW |RAW  " ,"",eset$Class))
eset$Class <- gsub(" ","_",eset$Class)
eset$Class <- gsub("\\+","plus",eset$Class)
class <- vettore.colori(class = eset$Class,colori = brewer.pal(11,"Spectral"))
clab <- cbind(Class=class$Color)

legenda <- unique(class)

pdf("QC/Heatmap.pdf")
par(cex.axis=1,cex.main=1,mar=c(7,4,4,2)+0.1)
heatmap.3(correl,
          key = TRUE,
          keysize = 1.2,
          KeyValueName = "Pearson",
          Rowv=T,cexRow = 0.5,
          ColSideColors=clab,
          ColSideColorsSize = 1.5,
          dendrogram = "both",
          main = "SST-RMA data",
          col=greenred(75),
          symkey=FALSE, 
          density.info="none", 
          trace="none",          
          distfun=mydist,
          hclustfun= myclust,
          labCol=F,
          labRow=F)

legend("topright",legend=legenda[,1],cex=0.55,
       bty="n",fill=legenda[,2],
       xpd=T,x=0.75,y=1.15,border = F)

dev.off()
