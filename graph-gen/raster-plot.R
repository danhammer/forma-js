

library(TeachingDemos)
m <- matrix(1:100, ncol=10)

v <- c(1, 3, 11, 12, 39, 74,75,84,85,73, 100, 99, 90, 80)
cols <- sapply(1:100, function(x) {ifelse(x %in% v, "#33CCFF", 0)})

vals <- sapply(1:100, function(x) {ifelse(x %in% v, 1, 0)})


png(file = "../raster1.png", width = 650, height = 650)
par(bg = NA, bty = 'n')
image(1:ncol(m), 1:nrow(m), t(m),
      col = cols,
      xlab="",
      ylab="",
      axes = FALSE)
dev.off()

png(file = "../raster2.png", width = 650, height = 650)
par(bg = NA, bty = 'n')
image(1:ncol(m), 1:nrow(m), t(m),
      col = cols,
      xlab="",
      ylab="",
      axes = FALSE)

my.symbols(1, 1, cbind( c(-1,-1,4,4,-1), c(-1,4,4,-1,-1) ),
           col = "#FF6666", lwd = 3, add=TRUE)

my.symbols(1, 3, cbind( c(-1,-1,4,4,-1), c(-1,4,4,-1,-1) ),
           col = "#FF6666", lwd = 3, add=TRUE)

my.symbols(8.05, 7.85, cbind( c(-1,-1,4,4,-1), c(-1,4,4,-1,-1) ),
           col = "#FF6666", lwd = 3, add=TRUE)

dev.off()
