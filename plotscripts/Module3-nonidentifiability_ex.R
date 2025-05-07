library(mnormt)

# covariance matrices
Sigma1 = diag(2)
Sigma2 = matrix(c(1,.85,.85,1), nrow = 2)

# generate random draws
n = 5000
Y = rnorm(n)
bvn1 = rmnorm(n, mean = c(0,0), varcov = Sigma1)
bvn2 = rmnorm(n, mean = c(0,0), varcov = Sigma2)
bvn3 = cbind(Y,Y)
sims = list(bvn1,bvn2,bvn3)

# plotting
pdf("Module3-nonidentifiability.pdf", width = 15, height = 5)  # Adjust size as needed
par(mfrow = c(1,3))
par(mar = rep(6,4))
r = c(0,.85,1)
for (i in 1:3){
    mat = sims[[i]]
    plot(mat[,1],mat[,2], 
             main = paste("r =", r[i]),
             cex.main = 3,
             cex.axis = 2,
             xlim = c(-5,5),
             ylim = c(-5,5),
             cex.lab = 3,
             xlab = "Y(0)",
             ylab = "Y(1)",
             axes = F)
    Axis(side=1, labels=FALSE)
    Axis(side=2, labels=FALSE)
}

# Finish plotting
dev.off()


