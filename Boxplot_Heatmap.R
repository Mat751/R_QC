
library(gplots)
library(RColorBrewer)

source("F:/nuovi/funzioni/VETTORE_COLORI.R")
source("F:/nuovi/funzioni/HEATMAP3.R")

pdf("")
par(mfrow(2,1))

boxplot(exprs(eset),
        range=0,
        las=2,
        ylab=expression(log[2](data)),
        main="",
        xlab="",
        xaxt="n")
mtext(at = 1:ncol(eset),side=3,las=2,cex=.7,text=colnames(eset))

dev.off()

#Pearson Correlations distance
mydist <- function(x)(dist(x,method="euclidean"))

#Ward.D clustering (minum variance between dataset)
myclust <- function(x)hclust(x,method = "ward.D2")

correl <- cor(exprs(eset))
class <- vettore.colori(class = eset$Class,colori = brewer.pal(11,"Spectral"))

clab <- cbind(Class=class$Color)
legenda <- unique(class)

pdf("")
par(cex.axis=1,cex.main=1,mar=c(7,4,4,2)+0.1)
heatmap.3(correl,
          key = TRUE,
          keysize = 1.2,
          KeyValueName = "Pearson",
          Rowv=T,cexRow = 0.5,
          ColSideColors=clab,
          ColSideColorsSize = 1.5,
          dendrogram = "both",
          main = "",
          col=greenred(75),
          symkey=FALSE, 
          density.info="none", 
          trace="none",          
          distfun=mydist,
          hclustfun= myclust,
          labCol=F,
          labRow=F)

legend("topright",legend=legenda[,1],
       cex=0.55,
       bty="n",fill=legenda[,2],
       xpd=T,x=0.75,y=1.15,border = F)

dev.off()
