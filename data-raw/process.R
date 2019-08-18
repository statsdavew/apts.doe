## apts.doe package
## data import and processing - dcw 15-0-18

## FF
cirfab <- read.csv("data-raw/cir_fab.csv", row.names = 1)

## bacteriocin experiment
bact <- read.csv("data-raw/bact.csv", row.names = 1)

## GSK SSD
ssd <- read.csv("data-raw/ssd.csv", row.names = 1)

## EBM computer experiment
des.frame <- read.csv("data-raw/lhd.csv", header = F)
temp <- as.numeric(read.table("data-raw/meanTemp.txt"))
ebm <- data.frame(temp, des.frame)
names(ebm) <- c("y", "x1", "x2")

usethis::use_data(cirfab, bact, ssd, ebm, overwrite = T)
