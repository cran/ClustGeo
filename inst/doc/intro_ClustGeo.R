## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,eval=TRUE,fig.align="center",fig.width = 7,fig.height = 6)

## -----------------------------------------------------------------------------
library(ClustGeo)
data(estuary)
dat <- estuary$dat
head(dat)
D.geo <- estuary$D.geo
map <- estuary$map

## ----out.width="55%"----------------------------------------------------------
# description of 5 municipalities in the map
head(map@data[,4:8]) 

# plot of the municipalities
library(sp)
?"SpatialPolygonsDataFrame-class"
sp::plot(map, border="grey") # plot method
sel <- map$NOM_COMM%in% c("BORDEAUX", "ARCACHON", "ROYAN") # label of 3 municipalities
text(sp::coordinates(map)[sel,],
     labels = map$NOM_COMM[sel])

# we check that the municipalities in map are the same than those in X
identical(as.vector(map$INSEE_COM),rownames(dat))

## ----out.width="80%"----------------------------------------------------------
D0 <- dist(dat) # the socio-economic distances
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 5, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

## ----out.width="70%"----------------------------------------------------------
# cut the dendrogram to get the partition in 5 clusters
P5 <- cutree(tree,5)
city_label <- as.vector(map$"NOM_COMM")
names(P5) <- city_label

plot(map, border = "grey", col = P5, 
         main = "Partition P5 obtained with D0 only")
legend("topleft", legend = paste("cluster",1:5), 
       fill = 1:5, bty = "n", border = "white")

## -----------------------------------------------------------------------------
# list of the municipalities in cluster 5
city_label[which(P5==5)]

## -----------------------------------------------------------------------------
D1 <- as.dist(D.geo) # the geographic distances between the municipalities

## -----------------------------------------------------------------------------
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha, 
  K, graph = FALSE)
cr$Q # proportion of explained inertia

## ----out.width="60%"----------------------------------------------------------
?plot.choicealpha
plot(cr)

## ----echo=FALSE,eval=FALSE----------------------------------------------------
#  #postscript("plotQ-1.eps",width = 950,height = 489)
#  plot(cr)
#  #dev.off()
#  #postscript("plotQnorm-1.eps")
#  plot(cr,norm=TRUE)
#  #dev.off()

## -----------------------------------------------------------------------------
tree <- hclustgeo(D0,D1,alpha=0.2)
P5bis <- cutree(tree,5)

## ----out.width="70%"----------------------------------------------------------
sp::plot(map, border = "grey", col = P5bis, 
         main = "Partition P5bis obtained with alpha=0.2 
         and geographical distances")
legend("topleft", legend=paste("cluster",1:5), 
       fill=1:5, bty="n",border="white")

## ----fig.height=4,fig.width=4,fig.align="center",warning=FALSE,message=FALSE----
library(spdep)
?poly2nb
list.nb <- poly2nb(map, row.names = rownames(dat)) #list of neighbours of each city
?nb2mat
A <- nb2mat(list.nb,style="B")
diag(A) <- 1
colnames(A) <- rownames(A) <- city_label
A[1:5,1:5]

## -----------------------------------------------------------------------------
D1 <- as.dist(1-A)

## ----out.width="70%"----------------------------------------------------------
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha,
                  K, graph=FALSE)
plot(cr)

## ----out.width='70%'----------------------------------------------------------
cr$Qnorm # normalized proportion of explained inertia
plot(cr, norm = TRUE)

## ----out.width='70%'----------------------------------------------------------
tree <- hclustgeo(D0, D1, alpha  =0.2)
P5ter <- cutree(tree,5)
sp::plot(map, border="grey", col=P5ter, 
         main=" Partition P5ter obtained with
         alpha=0.2 and neighborhood dissimilarities")
legend("topleft", legend=1:5, fill=1:5, col=P5ter)

