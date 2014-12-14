
mymat <- data.frame(y1=rnorm(n=5, mean=5, sd=3), y2=rnorm(n=5, mean=3, sd=5))

mymat$y1hat <- jitter(mymat$y1, factor=10)
mymat$y2hat <- jitter(mymat$y2, factor=10)

mymat$y1bar <- mean(mymat$y1)
mymat$y2bar <- mean(mymat$y2)


ssres <- sapply(1:nrow(mymat), function(j){
    (mymat$y1[ j ] -mymat$y1hat[ j ])^2 + (mymat$y2[ j ] - mymat$y2hat[ j ])^2
})

sst <- sapply(1:nrow(mymat), function(j){
    (mymat$y1[ j ] -mymat$y1bar[ j ])^2 + (mymat$y2[ j ] - mymat$y2bar[ j ])^2
})

ssm <- sapply(1:nrow(mymat), function(j){
    (mymat$y1bar[ j ] -mymat$y1hat[ j ])^2 + (mymat$y2bar[ j ] - mymat$y2hat[ j ])^2
})

sink("output/geometric_note.txt")
    print(paste("corresponds to a model with R-squared =", 1 - sum(ssres)/sum(sst)))
sink(file=NULL)

png("output/geometric1.png", width=4, height=4, units="in", res=300)
plot(mymat[ , c("y1", "y2" ) ], yaxt="n", xaxt="n", pch=19, col=rgb(1,0,0,0.5), 
     ylim=c(min(c(mymat$y2, mymat$y2hat)),max(mymat$y2, mymat$y2hat)),
     xlim=c(min(c(mymat$y1, mymat$y1hat)),max(mymat$y1, mymat$y1hat)))
points(mymat[ , c("y1bar", "y2bar") ], pch=19)
for(j in 1:nrow(mymat)){
    lines(c(mymat$y1[ j ],mymat$y1bar[ j ]), c(mymat$y2[ j ],mymat$y2bar[ j ]))
}
dev.off()

png("output/geometric2.png", width=4, height=4, units="in", res=300)
plot(mymat[ , c("y1", "y2" ) ], yaxt="n", xaxt="n", pch=19, col=rgb(1,0,0,0.5), 
     ylim=c(min(c(mymat$y2, mymat$y2hat)),max(mymat$y2, mymat$y2hat)),
     xlim=c(min(c(mymat$y1, mymat$y1hat)),max(mymat$y1, mymat$y1hat)))
points(mymat[ , c("y1hat", "y2hat") ], pch=17, col=rgb(0,0,1,0.5))
for(j in 1:nrow(mymat)){
    lines(c(mymat$y1[ j ],mymat$y1hat[ j ]), c(mymat$y2[ j ],mymat$y2hat[ j ]))
}
dev.off()


ssres <- sapply(1:nrow(mymat), function(j){
    (mymat$y1[ j ] -mymat$y1hat[ j ])^2 + (mymat$y2[ j ] - mymat$y2hat[ j ])^2
})

sst <- sapply(1:nrow(mymat), function(j){
    (mymat$y1[ j ] -mymat$y1bar[ j ])^2 + (mymat$y2[ j ] - mymat$y2bar[ j ])^2
})

ssm <- sapply(1:nrow(mymat), function(j){
    (mymat$y1bar[ j ] -mymat$y1hat[ j ])^2 + (mymat$y2bar[ j ] - mymat$y2hat[ j ])^2
})


