library(latticeExtra)

para <- (read.csv('Build/para.csv'))
dan <- (read.csv('Build/danielsson.csv'))
maurer <- (read.csv('Build/maurerws.csv'))

grp <- c(rep('para', nrow(para)), rep('dan', nrow(dan)), rep('maurer', nrow(maurer)))

comb <- rbind(para, dan, maurer)
grprad <- paste(grp, comb$radius,sep=".")
comb <- cbind(grp, grprad, comb)

p1 <- xyplot(lab_dilate_timed~ threads, group=grp, data=comb, subset=comb$radius==10, type='l', auto.key = list(x = .6, y = .7, corner = c(1, 0), lines=TRUE, points=FALSE,columns=2, title="Method", cex.title=1), ylab="Execution time(s)", main="Execution times for labelled 3D dilation, radius 10.")

c2 <- comb[comb$radius==10,]
cc <- c2[c2$grp=='para', "lab_dilate_timed"]
sp1 <- cc[1]/cc

cc <- c2[c2$grp=='dan', "lab_dilate_timed"]
sp2 <- cc[1]/cc

cc <- c2[c2$grp=='maurer', "lab_dilate_timed"]
sp3 <- cc[1]/cc

speedup <- c(sp1, sp2, sp3)
c2 <- cbind(c2, speedup)
p2 <- xyplot(speedup ~ threads, group=grp, data=c2, type='l', auto.key = list(x = .6, y = .7, corner = c(1, 0), lines=TRUE, points=FALSE,columns=2, title="Method", cex.title=1), ylab="Execution time(s)", main="Speedup of labelled 3D dilation, radius 10.")

pdf("exectimes.pdf")
print(p1)
dev.off()

pdf("speedups.pdf")
print(p2)
dev.off()
