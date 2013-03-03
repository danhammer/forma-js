

library(TeachingDemos)
m <- matrix(1:100, ncol=10)

v <- c(1, 3, 11, 12, 39, 74,75,84,85,73, 100, 99, 90, 80)
cols <- sapply(1:100, function(x) {ifelse(x %in% v, "#33CCFF", 0)})

vals <- sapply(1:100, function(x) {ifelse(x %in% v, 1, 0)})


png(file = "../images/raster1.png", width = 650, height = 650)
par(bg = NA, bty = 'n')
image(1:ncol(m), 1:nrow(m), t(m),
      col = cols,
      xlab="",
      ylab="",
      axes = FALSE)
dev.off()

png(file = "../images/raster2.png", width = 650, height = 650)
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

library(scatterplot3d)
## example 6; by Martin Maechler
cubedraw <- function(res3d, min = 0, max = 100, cex = 0) {
  ## Purpose: Draw nice cube with corners
  cube01 <- rbind(c(0,0,1), 0, c(1,0,0), c(1,1,0), 1, c(0,1,1), # < 6 outer
                  c(1,0,1), c(0,1,0)) # <- "inner": fore- & back-ground
  cub <- min + (max - min) * cube01
  ## visibile corners + lines:
  res3d$points3d(cub[c(1:6,1,7,3,7,5) ,], cex = cex, type = "l", col = "#FF6666", lwd = 5)
  ## hidden corner + lines
  res3d$points3d(cub[c(2,8,4,8,6), ], cex = cex, type = "l", lty = 3, col = "#FF6666", lwd = 3)
}

z <- 120 + rnorm(100, sd = 75)
x <- 120 + rnorm(100, sd = 75)
y <- 120 + rnorm(100, sd = 75)

png(file = "../images/cube.png", width = 850, height = 850)
par(bg = NA, bty = 'n')
rR <- scatterplot3d(x, y, z, box = FALSE, angle = 24,
                    xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300),
                    xlab = "", ylab = "", zlab = "",
                    col.axis = "#33CCFF",
                    pch = 20,
                    cex = 3,
                    color = "#33CCFF",
                    col.lab  = "#33CCFF",
                    tick.marks = FALSE,
                    axis = FALSE,
                    cex.axis = 1.5,
                    cex.lab  = 1.3)
cubedraw(rR, min = 0, max = 75)
cubedraw(rR, min = 80, max = 155)
cubedraw(rR, min = 180, max = 255)
dev.off()


z <- 120 + rnorm(450, sd = 150)
x <- 120 + rnorm(450, sd = 150)
y <- 120 + rnorm(450, sd = 150)

png(file = "../images/joke.png", width = 850, height = 850)
par(bg = NA, bty = 'n')
scatterplot3d(x, y, z, box = FALSE, angle = 24,
              xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300),
              xlab = "", ylab = "", zlab = "",
              col.axis = "#33CCFF",
              type = "l",
              lwd = 5,
              cex = 3,
              color = "#33CCFF",
              col.lab  = "#33CCFF",
              tick.marks = FALSE,
              grid = FALSE,
              axis = FALSE,
              cex.axis = 1.5,
              cex.lab  = 1.3)
dev.off()

mat <- rev(c(0, 1, 1, 1, 0, 1, 2, 4, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0, 0, 2, 4, 4, 1, 0, 0, 0, 0, 3, 5, 5, 2, 1, 0, 0, 0, 2, 3, 3, 4, 2, 0, 0, 0, 1, 1, 1))

cols <- sapply(c, function(x) {ifelse(mat > 0, "#33CCFF", 0)})

m <- matrix(1:64, ncol=8)
png(file = "../images/raster1.png", width = 650, height = 650)
par(bg = NA, bty = 'n')
image(1:ncol(m), 1:nrow(m), m,
      col = cols,
      xlab="",
      ylab="",
      axes = FALSE)
dev.off()
