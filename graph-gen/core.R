
## set parameters
interval.num <- 23

annual.intervals <- function(year) {
  ## Creates a full set of FORMA dates for a year: starting with Jan
  ## 01, and incrementing 16 days until the end of the year
  init.str <- paste(year, "-01-01", sep="")
  seq(as.Date(init.str), length.out = interval.num, by="16 days")
}

forma.date <- function(forma.pd) {
  ## Convert forma index to the date period; can accept a sequence of
  ## indices.
  max.year <- max(ceiling(forma.pd/interval.num) + 2003)
  full <- do.call(c, lapply(2003:max.year, annual.intervals))
  full[forma.pd + interval.num]
}

ndvi <- c(0.9, 0.89, 0.88, 0.88, 0.89, 0.9, 0.89, 0.89, 0.87, 0.88,
          0.86, 0.83, 0.8, 0.77, 0.76, 0.82, 0.79, 0.82, 0.83, 0.82,
          0.84, 0.82, 0.86, 0.89, 0.87, 0.9, 0.88, 0.87, 0.89, 0.9,
          0.84, 0.87, 0.85, 0.81, 0.76, 0.74, 0.77, 0.79, 0.75, 0.69,
          0.69, 0.69, 0.72, 0.72, 0.76, 0.78, 0.78, 0.82, 0.85, 0.85,
          0.83, 0.86, 0.85, 0.86, 0.84, 0.82, 0.82, 0.81, 0.78, 0.75,
          0.77, 0.73, 0.75, 0.73, 0.75, 0.77, 0.77, 0.76, 0.77, 0.79,
          0.79, 0.79, 0.81, 0.85, 0.84, 0.87, 0.86, 0.82, 0.82, 0.83,
          0.81, 0.79, 0.79, 0.76, 0.79, 0.75, 0.77, 0.8, 0.8, 0.83,
          0.84, 0.85, 0.85, 0.84, 0.84, 0.84, 0.88, 0.86, 0.85, 0.86,
          0.86, 0.86, 0.85, 0.84, 0.73, 0.62, 0.66, 0.58, 0.52, 0.45,
          0.42, 0.39, 0.4, 0.42, 0.48, 0.52, 0.56, 0.52, 0.49, 0.49,
          0.45, 0.47, 0.53, 0.48, 0.44, 0.42, 0.42, 0.42, 0.38, 0.32,
          0.34, 0.31, 0.3, 0.3, 0.3, 0.29, 0.34, 0.32, 0.33, 0.35,
          0.36, 0.37, 0.38, 0.38, 0.38, 0.45, 0.4, 0.37, 0.39, 0.38,
          0.33, 0.31, 0.33, 0.39, 0.47, 0.43, 0.41, 0.43, 0.43, 0.43,
          0.45, 0.46, 0.49, 0.52, 0.55, 0.6, 0.59, 0.65, 0.64, 0.64,
          0.61, 0.62, 0.55, 0.54, 0.54, 0.56, 0.53, 0.59, 0.63, 0.68,
          0.69, 0.68, 0.74, 0.72, 0.71, 0.68, 0.7, 0.69, 0.73, 0.72,
          0.72, 0.73, 0.76, 0.71, 0.76, 0.69, 0.65, 0.64, 0.68)

date.range <- forma.date(1:length(ndvi))

png(file = "../../images/ndvi.png", width = 1000, height = 650)
par(bg = NA, bty = 'n')
plot(date.range,
     ndvi,
     type     = "l",
     lwd      = 3,
     xlab     = "Date",
     ylab     = "NDVI",
     col      = "#33CCFF",
     col.axis = "#33CCFF",
     col.lab  = "#33CCFF",
     cex.axis = 1.5,
     cex.lab  = 1.3)
dev.off()

blockLinear <- function(start.idx, len = 20) {
  idx.seq <- seq(start.idx, start.idx + len)
  x <- date.range[idx.seq]
  z <- lm( ndvi[idx.seq] ~ x )
  a <- z$coefficients[[1]]; b <- z$coefficients[[2]] 
  curve(a + b * as.numeric(x),
        from = forma.date(start.idx),
        to = forma.date(start.idx + len),
        add = T,
        col = "#FF6666",
        lwd = 3)
}

png(file = "../../images/ndvi-breaks.png", width = 1000, height = 650)
par(bg = NA, bty = 'n')
plot(date.range,
     ndvi,
     type     = "l",
     lwd      = 3,
     xlab     = "Date",
     ylab     = "NDVI",
     col      = "#33CCFF",
     col.axis = "#33CCFF",
     col.lab  = "#33CCFF",
     cex.axis = 1.5,
     cex.lab  = 1.3)

blockLinear(1)
blockLinear(11)
blockLinear(21)
blockLinear(100)
blockLinear(110)
blockLinear(120)
blockLinear(130)
blockLinear(140)

dev.off()

png(file = "../../images/ndvi-breaks2.png", width = 1000, height = 650)
par(bg = NA, bty = 'n')
plot(date.range,
     ndvi,
     type     = "l",
     lwd      = 3,
     xlab     = "Date",
     ylab     = "NDVI",
     col      = "#33CCFF",
     col.axis = "#33CCFF",
     col.lab  = "#33CCFF",
     cex.axis = 1.5,
     cex.lab  = 1.3)

for (i in  seq(1, 179)) {
  blockLinear(i)
}

dev.off()

load("data/total.Rda")
png(file = "../../images/total-defor.png", width = 1000, height = 400)
par(bg = NA, bty = 'n')
plot(g.data$date,
     g.data$rate,
     type     = "l",
     lwd      = 5,
     xlab     = "Date",
     ylab     = "Total deforestation",
     col      = "white",
     col.axis = "white",
     col.lab  = "white",
     cex.axis = 1.5,
     cex.lab  = 1.3)
dev.off()

load("data/entropy.Rda")
png(file = "../../images/entropy.png", width = 1000, height = 400)
par(bg = NA, bty = 'n')
plot(a$date,
     a$entropy,
     type     = "l",
     lwd      = 5,
     xlab     = "Date",
     ylab     = "Entropy",
     col      = "white",
     col.axis = "white",
     col.lab  = "white",
     cex.axis = 1.5,
     cex.lab  = 1.3)
dev.off()
