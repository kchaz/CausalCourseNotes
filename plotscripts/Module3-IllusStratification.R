#' 
#' this code is clunky - would be nice to clean up sometime
#' 

n = 16

Y1 <- c(rep(-2,4), rep(.5,4),
        rep(8,4), rep(10,4)) # rnorm(n,0,.25)

Y0 <- c(rep(-1,4), rep(0,4),
        rep(0,4), rep(5,4)) #+ rnorm(n,0,.25)

tau = mean(Y1)-mean(Y0)

par(mfrow = c(2,2))
par(mar = c(3, 2, 2, 1))

# version with stratification
l = min(Y1,Y0)
u = max(Y1,Y0)
plot(1:n,1:n, "n",
     ylim = c(l-1, u+1),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.main = 1.2,
     cex.lab = 1.5,
     main = "Stratified Assignment")
segments(1:n, Y0, 1:n, Y1, col = "grey", lty = 2, lwd = 2)
points(1:n, Y0, cex = 1.5, lwd = 2,
       col = rep(c("blue","blue","grey","grey"),4))
points(1:n,Y1, pch = 20, cex = 2,
       col = rep(c("grey","grey","red","red"),4))
axis(1, at = c(2,6,10,14)+.5,
     labels = paste("Group", 1:4),
     cex.axis = 1)


Y1bar = mean(Y1[c(3,4,7,8,11,12,15,16)])
Y0bar = mean(Y0[c(1,2,5,6,9,10,13,14)])
abline(h = Y1bar, col = "red", lty = 2)
abline(h = Y0bar, col = "blue", lty = 2)
text(1,Y1bar+1,expression(bar(Y)[1]), col = "red")
text(1,Y0bar+1,expression(bar(Y)[0]), col = "blue")
#abline(h=tau, lty = 2)
#text(1,tau+.5, expression(tau), cex = 2)


# problem without stratification
l = min(Y1,Y0)
u = max(Y1,Y0)
plot(1:n,1:n, "n",
     ylim = c(l-1, u+1),
     ylab = "",
     xlab = "",
     cex.main = 1.2,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n",
     main = "Completely Randomized (extreme 1)")
segments(1:n, Y0, 1:n, Y1, col = "grey", lty = 2, lwd = 2)
points(1:n, Y0, cex = 1.5, lwd = 2,
       col = c(rep("blue",8), rep("grey",8)))
points(1:n,Y1, pch = 20, cex = 2,
       col = c(rep("grey",8), rep("red",8)))
axis(1, at = c(2,6,10,14)+.5,
     labels = paste("Group", 1:4),
     cex.axis = 1)
Y1bar = mean(Y1[9:16])
Y0bar = mean(Y0[1:8])
abline(h = Y1bar, col = "red", lty = 2)
abline(h = Y0bar, col = "blue", lty = 2)
text(1,Y1bar+1,expression(bar(Y)[1]), col = "red")
text(1,Y0bar+1,expression(bar(Y)[0]), col = "blue")
#abline(h=tau, lty = 2)
#text(1,tau+.5, expression(tau), cex = 2)


# problem without stratification
l = min(Y1,Y0)
u = max(Y1,Y0)
plot(1:n,1:n, "n",
     ylim = c(l-1, u+1),
     ylab = "",
     xlab = "",
     cex.main = 1.2,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n",
     main = "Completely Randomized (extreme 2)")
segments(1:n, Y0, 1:n, Y1, col = "grey", lty = 2, lwd = 2)
points(1:n, Y0, cex = 1.5, lwd = 2,
       col = c(rep("grey",8), rep("blue",8)))
points(1:n,Y1, pch = 20, cex = 2,
       col = c(rep("red",8), rep("grey",8)))
axis(1, at = c(2,6,10,14)+.5,
     labels = paste("Group", 1:4),
     cex.axis = 1)

Y1bar = mean(Y1[1:8])
Y0bar = mean(Y0[9:16])
abline(h = Y1bar, col = "red", lty = 2)
abline(h = Y0bar, col = "blue", lty = 2)
text(16,Y1bar+1,expression(bar(Y)[1]), col = "red")
text(16,Y0bar+1,expression(bar(Y)[0]), col = "blue")



plot(1:n,1:n, "n", axes = F, xlab = "", ylab = "")

legend("center", c("unobserved Y(0)",
                    "observed Y(0)",
                    "unobserved Y(1)",
                    "observed Y(1)"),
       col= c("grey","blue","grey","red"),
       pch = c(1,1,20,20),
       bty = "n", cex = 1.8)
