vs <- read.csv2("vizseb2014.csv", dec=".", stringsAsFactors = FALSE, head=F)
## Egy keresztszelvény
test <- vs[6:12,]

## Keresztszelvény létrehozása
make.ksz <- function(x){
  ## x: Az adott keresztszelvény mérés első két sora
  fuggely.index <- is.na(x[1,])
  ## Függély helyének keresése
  fuggely.hely <- x[1, !fuggely.index]
  fuggely.hely <- as.numeric(fuggely.hely[,-(1:2)])
  ## Mélység
  melyseg <- as.numeric(x[2,fuggely.index])
  ## Vízszint
  vizszint <- x[2,!fuggely.index]
  vizszint <- as.numeric(vizszint[,-(1:2)])
  ## Adattábla összeállítás
  data.frame(fuggely.hely,-melyseg, vizszint)
}

i <- 76
example <- make.ksz(vs[c(i,i+1),])
example[,3] <- c(NA,example[-nrow(example),3]) 
plot(example[,1:2], typ="l")
lines(example[,1],example[,2] + example[,3])

#par(ask=TRUE)
for(i in c(6,20,34,48,62,76,90)){
example <- make.ksz(vs[c(i,i+1),])
plot(example[,1:2], typ="l", main=paste(i, "sor"))
lines(example[,1],example[,2] + example[,3])
}
#par(ask=FALSE)

make.ksz(vs[c(76,77),])
