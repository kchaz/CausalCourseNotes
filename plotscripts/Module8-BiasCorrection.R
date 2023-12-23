#' 
#' Comment: the spacing of this plot is a little finicky.
#' Very annoying to create.
#' 

#general plot set-ups
par(mfrow = c(1,1),
    mar = c(4,4,2,2),
    cex.axis = 1.25)

#setup plot
plot(1:10, 1:10, "n",
     axes = F,
     xlab = "", 
     ylab = "",
     xlim = c(0,14),
     cex.lab = 1)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)

#draw some line to represent a line of best fit
alpha = 2
beta = .8
abline(a = alpha, b = beta, lwd = 3)


#treated unit
tx = 5; ty = 8
text(tx-.5,ty+.5, "Treated Unit i", col = "red")
segments(tx,0,tx,ty, lty = 2, col = "grey")
segments(0,ty,tx,ty, lty = 2, col = "grey")
points(tx,ty, col = "red", cex = 3, pch = 18)


#matched control
cx = 6; cy = 5
text(cx+2.5, cy, "Matched Control Unit k", col = "blue")
segments(cx,0, cx,cy, lty = 2, col = "grey")
segments(0,cy, cx,cy, lty = 2, col = "grey")
points(cx,cy, col = "blue", cex = 3, pch = 20)

#other controls scattered around line
lblue = scales::alpha("blue",.45)
x = c(1, 2, 2.5, 3,  7, 6.7, 7.8, 9)
y = c(3.2, 4.8, 2.5, 4.5,  9.5, 8, 6.9, 8.7)
points(x,y, col = lblue, cex = 3, pch = 20)
#text(7.5, 10, "Other Controls", col = lblue)
text(11, 9, "Regression line \n fit on all controls", col = lblue)


#fitted values and correction
v=alpha + beta*5
segments(0,v,5,v, lty = 2, col = "grey")
c=alpha + beta*6
segments(0,c,6,c, lty = 2, col = "grey")
#segments(cx,v,tx,v, col = "grey", lty = 2, lwd = 1)
segments(cx,c,cx,v, col = "purple", lty = 3, lwd = 3)
points(6,c, col = "black", cex = 2, pch = 17)
points(5,v, col = "black", cex = 2, pch = 17)
text(8.5,v+.43, 
     expression("Correction " * hat(Y)[i](0) - hat(Y)[k](0)),
     col = "black")


#corrected estimate
z = cy+(v-c)
points(tx, z, cex = 3, pch = 18, col = "purple")
text(tx+3, z, 
     expression("Corrected Estimate of " * Y[i](0)),
     col ="purple")
segments(tx,z,tx,cy, col = "purple", lty = 3, lwd = 3)


#labels
Axis(side=1, 
     at = c(tx,cx),
     labels = c(expression(X[i]),expression(X[k])),
     tick = T
     )

Axis(side=2, 
     at = c(cy,ty,v,c),
     labels = c(expression(Y[k](0)),
                expression(Y[i](1)),
                expression(hat(Y)[i](0)),
                expression(hat(Y)[k](0))
                ),
     tick = T,
     las = 2
)


