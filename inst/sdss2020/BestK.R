
library(regtools)
data(peDumms)
ped <- peDumms[,c(1,20,22,24:29,31,32)]
x <- ped[,-10]
y <- ped[,10]
bkpp <- bestKperPoint(x,y,50)
plot(density(bkpp))


