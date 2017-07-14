dat <- estuary$dat
D.geo <- estuary$D.geo
map <- estuary$map

# wardinit function
n <- nrow(dat)
d <- dist(dat)
delta <- wardinit(d) #same weight 1/n
sum(as.matrix(delta)==as.matrix(d^2/(2*n)))==n^2

wt <- map@data$POPULATION
delta <- wardinit(d,wt) #different weights
i=1
j=4
as.matrix(delta)[i,j]==as.matrix(d)[i,j]^2*(wt[i]*wt[j])/(wt[i]+wt[j])

# hclustgeo function
# one dissimilarity matrix and wi=1/n
head(dat)
n <- nrow(dat)
D <- dist(dat)
tree <- hclustgeo(D)
tree2 <- hclust(D^2/(2*n),method="ward.D")
all.equal(tree$height,tree2$height)

# one dissimilarity matrix and non uniform wi
wt <- map@data$POPULATION
tree <- hclustgeo(D,wt=wt)
delta <- wardinit(D,wt) #comparison with hclust
tree2 <- hclust(delta,method="ward.D",members=wt)
sum(tree2$height==tree$height)==length(tree$height)
all.equal(inertdiss(D,wt=wt),sum(tree$height)) #the inertia

# one dissimilarity matrix with non euclidean distances
D <- 1-cor(dat)^2 #dissimilarity between the variables
n <- nrow(D)
D <- as.dist(D)
tree <- hclustgeo(D)
all.equal(inertdiss(D),sum(tree$height)) #the inertia

K=2
P <- cutree(tree,k=K)
W <- sum(tree$height[1:(n-K)])
all.equal(withindiss(D,P),W)

# two dissimilarity matrices
data(estuary)
head(dat)
n <- nrow(dat)
D0 <- dist(dat)
D1 <- as.dist(D.geo)
tree <- hclustgeo(D0,D1,alpha=0.3) 
alpha <- 0.3
TT <- (1-alpha)*inertdiss(D0/max(D0))+alpha*inertdiss(D1/max(D1))
all.equal(TT,sum(tree$height))

K=4
P <- cutree(tree,k=K)
W <- sum(tree$height[1:(n-K)])
WW <-  (1-alpha)*withindiss(D0/max(D0),P)+alpha*withindiss(D1/max(D1),P)
all.equal(WW,W)

# choicealpha function
r.alpha <- seq(0,1,0.1)
K <- 5
res <- choicealpha(D0,D1,r.alpha,K)
#avec alpha=0.2
tree <- hclustgeo(D0,D1,alpha=0.2)
P5 <- cutree(tree,5)
sp::plot(map,border="grey",col=P5)
legend("topleft", legend=1:5, fill=1:5, border=0,box.lty=0,col=P5,cex=0.7)

#with the neighbourhood matrix
library(spdep)
list.nb <- poly2nb(map,row.names = rownames(dat))
A <- nb2mat(list.nb,style="B")
colnames(A) <- rownames(A)
diag(A) <- 1
D1 <- as.dist(1-A)
r.alpha <- seq(0,1,0.1)
K <- 5
x <- choicealpha(D0,D1,r.alpha,K)

#avec alpha=0.2
tree <- hclustgeo(D0,D1,alpha=0.2)
P5 <- cutree(tree,5)
plot(map,border="grey",col=P5)
legend("topleft", legend=1:5, fill=1:5, border=0,box.lty=0,col=P5,cex=0.7)


#plot method
?plot.choicealpha
plot(res,cex=0.8,norm=TRUE,cex.lab=0.8)
