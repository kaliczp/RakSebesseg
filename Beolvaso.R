vs <- read.csv2("vizseb2014.csv", dec=".", stringsAsFactors = FALSE, head=F)
## Egy keresztszelvény
test <- vs[6:12,]

## Keresztszelvény létrehozása
make.ksz <- function(x){
  ## x: Az adott keresztszelvény mérés első két sora
  fuggely.index <- is.na(x[1,])
  ## Függély helyének keresése
  fuggely.hely <- x[1, !fuggely.index]
  fuggely.hely <- as.numeric(fuggely.hely[-(1:2)])
  ## Mélység
  melyseg <- as.numeric(x[2,fuggely.index])
  ## Adattábla összeállítás
  data.frame(fuggely.hely,-melyseg)
}

plot(make.ksz(test), typ="l")
