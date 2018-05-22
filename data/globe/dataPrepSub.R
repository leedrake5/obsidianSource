library(data.table)
library(SDMTools)

setwd("~/GitHub/obsidianSource")
#data.header <- header <- scan("/Users/lee/Dropbox/Documents/Global Obsidian/First Draft/Global Obsidian Database/Data-Table 1.csv", nlines = 1, what = character(), sep=",")
data <- read.csv(file="data/globe/Data-Table 1.csv", skip=1, header=TRUE)
data <- subset(data, Literature.Rank==10)
#names(data) <- data.header

metadata <- read.csv(file="data/globe/Master List-Table 1.csv")

simp.data <- subset(data, select = -c(Literature, m, m.1, m.2,m.3, m.4, m.5, m.6, m.7, m.8, m.9, m.10, m.11, m.12, m.13, m.14, m.15, m.16, m.17, s, s.1, s.2,s.3, s.4, s.5, s.6, s.7, s.8, s.9, s.10, s.11, s.12, s.13, s.14, s.15, s.16, s.17, Sample, Source, Type, X, X.1, Method) )


simpler.data <- subset(data, select = -c(Literature, m, m.1, m.2,m.3, m.4, m.5, m.6, m.7, m.8, m.9, m.10, m.11, m.12, m.13, m.14, m.15, m.16, m.17, s, s.1, s.2,s.3, s.4, s.5, s.6, s.7, s.8, s.9, s.10, s.11, s.12, s.13, s.14, s.15, s.16, s.17, Sample, Source, Type, X, X.1, Method, Serial, n) )

####Find non-numeric columns
#which(sapply(data, is.numeric)==FALSE)

###Find non-numeric data
#data$SDCe[which(is.na(as.numeric(as.character(data$SDCe))))]


#simpler.data[] <- lapply(simpler.data, gsub, pattern=',', replacement='')
#simpler.data <- apply(simpler.data, 2, as.numeric)
simpler.data <- as.data.frame(simpler.data)
simpler.data$Serial <- simp.data$Serial
simpler.data$n <- simp.data$n


simp.dat <- data.table(simpler.data)
mean.dat <- data.table(simpler.data[,c("Serial", "n", "MeanNa", "MeanMg", "MeanK", "MeanSc", "MeanTi", "MeanMn", "MeanFe", "MeanCo", "MeanZn", "MeanGa", "MeanRb", "MeanSr", "MeanY", "MeanZr", "MeanNb", "MeanSb", "MeanCs", "MeanBa", "MeanLa", "MeanCe", "MeanNd", "MeanTb", "MeanDy", "MeanSm", "MeanEu", "MeanYb", "MeanLu", "MeanHf", "MeanTa", "MeanPb", "MeanTh", "MeanU")])
sd.dat <- data.table(simpler.data[,c("Serial", "n", "SDNa", "SDMg", "SDK", "SDSc", "SDTi", "SDMn", "SDFe", "SDCo", "SDZn", "SDGa", "SDRb", "SDSr", "SDY", "SDZr", "SDNb", "SDSb", "SDCs", "SDBa", "SDLa", "SDCe", "SDNd", "SDTb", "SDDy", "SDSm", "SDEu", "SDYb", "SDLu", "SDHf", "SDTa", "SDPb", "SDTh", "SDU")])
sd.dat[sd.dat == 0] <- NA
n.dat <- data.table(simpler.data[, c("Serial", "n")])



#summary.dat <- simp.dat[,lapply(.SD,weighted.mean,w=n, na.rm=TRUE),by=Serial]
summary.mean.dat <- mean.dat[,lapply(.SD ,weighted.mean,w=n, na.rm=TRUE),by=Serial]
summary.sd.dat <- sd.dat[,lapply(.SD ,weighted.mean,w=n, na.rm=TRUE),by=Serial]
summary.sd.dat[summary.sd.dat == 0] <- NA
summary.n.dat <- n.dat[,lapply(.SD, sum), by=Serial]

summary.sd.mean.dat <- mean.dat[,lapply(.SD , wt.sd,wt=n),by=Serial]
colnames(summary.sd.mean.dat) <- c("Serial", "n", "SDNa", "SDMg", "SDK", "SDSc", "SDTi", "SDMn", "SDFe", "SDCo", "SDZn", "SDGa", "SDRb", "SDSr", "SDY", "SDZr", "SDNb", "SDSb", "SDCs", "SDBa", "SDLa", "SDCe", "SDNd", "SDTb", "SDDy", "SDSm", "SDEu", "SDYb", "SDLu", "SDHf", "SDTa", "SDPb", "SDTh", "SDU")
summary.sd.mean.dat[summary.sd.mean.dat == 0] <- NA

summary.mean.dat <- mean.dat[,lapply(.SD ,weighted.mean,w=n, na.rm=TRUE),by=Serial]

is.nan.data.frame <- function(x){
do.call(cbind, lapply(x, is.nan))
}

summary.mean.dat[is.nan.data.frame(summary.mean.dat)] <- NA
summary.sd.dat[is.nan.data.frame(summary.sd.dat)] <- NA
summary.sd.mean.dat[is.nan.data.frame(summary.sd.mean.dat)] <- NA




sd.data.all <- as.data.frame(rbind(summary.sd.dat, summary.sd.mean.dat))



sd.data.all.dt <- data.table(sd.data.all)
summary.sd.data.all <- sd.data.all.dt[,lapply(.SD ,base::mean, na.rm=TRUE),by=Serial]
summary.sd.data.all[is.nan.data.frame(summary.sd.data.all)] <- NA

summary.sd.data.all <- do.call(data.frame,lapply(summary.sd.data.all, function(x) replace(x, is.infinite(x),NA)))
sd.means <- colMeans(summary.sd.data.all[,-1], na.rm=TRUE)
summary.mean.dat <- do.call(data.frame,lapply(summary.mean.dat, function(x) replace(x, is.infinite(x),NA)))
mean.means <- colMeans(summary.mean.dat[,-1], na.rm=TRUE)

rel.sd.means <- as.data.frame(sd.means/mean.means)

summary.sd.data.0 <- summary.sd.data.all
summary.sd.data.0[is.na(summary.sd.data.0)] <- 0

summary.data <- merge(x=summary.mean.dat, y=summary.sd.data.0, by.x="Serial", by.y="Serial")



write.table(summary.data, file="/Users/lee/Dropbox/Documents/Global Obsidian/First Draft/presd.csv", sep=",")




summary.data$SDNa <- with(summary.data, ifelse(SDNa==0, summary.data$MeanNa*rel.sd.means["SDNa",], SDNa))
summary.data$SDMg <- with(summary.data, ifelse(SDMg==0, summary.data$MeanMg*rel.sd.means["SDMg",], SDMg))
summary.data$SDK <- with(summary.data, ifelse(SDK==0, summary.data$MeanK*rel.sd.means["SDK",], SDK))
summary.data$SDSc <- with(summary.data, ifelse(SDSc==0, summary.data$MeanSc*rel.sd.means["SDSc",], SDSc))
summary.data$SDTi <- with(summary.data, ifelse(SDTi==0, summary.data$MeanTi*rel.sd.means["SDTi",], SDTi))
summary.data$SDMn <- with(summary.data, ifelse(SDMn==0, summary.data$MeanMn*rel.sd.means["SDMn",], SDMn))
summary.data$SDFe <- with(summary.data, ifelse(SDFe==0, summary.data$MeanFe*rel.sd.means["SDFe",], SDFe))
summary.data$SDCo <- with(summary.data, ifelse(SDCo==0, summary.data$MeanCo*rel.sd.means["SDCo",], SDCo))
summary.data$SDZn <- with(summary.data, ifelse(SDZn==0, summary.data$MeanZn*rel.sd.means["SDZn",], SDZn))
summary.data$SDGa <- with(summary.data, ifelse(SDGa==0, summary.data$MeanGa*rel.sd.means["SDGa",], SDGa))
summary.data$SDRb <- with(summary.data, ifelse(SDRb==0, summary.data$MeanRb*rel.sd.means["SDRb",], SDRb))
summary.data$SDSr <- with(summary.data, ifelse(SDSr==0, summary.data$MeanSr*rel.sd.means["SDSr",], SDSr))
summary.data$SDY <- with(summary.data, ifelse(SDY==0, summary.data$MeanY*rel.sd.means["SDY",], SDY))
summary.data$SDZr <- with(summary.data, ifelse(SDZr==0, summary.data$MeanZr*rel.sd.means["SDZr",], SDZr))
summary.data$SDNb <- with(summary.data, ifelse(SDNb==0, summary.data$MeanNb*rel.sd.means["SDNb",], SDNb))
summary.data$SDSb <- with(summary.data, ifelse(SDSb==0, summary.data$MeanSb*rel.sd.means["SDSb",], SDSb))
summary.data$SDCs <- with(summary.data, ifelse(SDCs==0, summary.data$MeanCs*rel.sd.means["SDCs",], SDCs))
summary.data$SDBa <- with(summary.data, ifelse(SDBa==0, summary.data$MeanBa*rel.sd.means["SDBa",], SDBa))
summary.data$SDLa <- with(summary.data, ifelse(SDLa==0, summary.data$MeanLa*rel.sd.means["SDLa",], SDLa))
summary.data$SDCe <- with(summary.data, ifelse(SDCe==0, summary.data$MeanCe*rel.sd.means["SDCe",], SDCe))
summary.data$SDNd <- with(summary.data, ifelse(SDNd==0, summary.data$MeanNd*rel.sd.means["SDNd",], SDNd))
summary.data$SDTb <- with(summary.data, ifelse(SDTb==0, summary.data$MeanTb*rel.sd.means["SDTb",], SDTb))
summary.data$SDDy <- with(summary.data, ifelse(SDDy==0, summary.data$MeanDy*rel.sd.means["SDDy",], SDDy))
summary.data$SDSm <- with(summary.data, ifelse(SDSm==0, summary.data$MeanSm*rel.sd.means["SDSm",], SDSm))
summary.data$SDEu <- with(summary.data, ifelse(SDEu==0, summary.data$MeanEu*rel.sd.means["SDEu",], SDEu))
summary.data$SDYb <- with(summary.data, ifelse(SDYb==0, summary.data$MeanYb*rel.sd.means["SDYb",], SDYb))
summary.data$SDLu <- with(summary.data, ifelse(SDLu==0, summary.data$MeanLu*rel.sd.means["SDLu",], SDLu))
summary.data$SDHf <- with(summary.data, ifelse(SDHf==0, summary.data$MeanHf*rel.sd.means["SDHf",], SDHf))
summary.data$SDTa <- with(summary.data, ifelse(SDTa==0, summary.data$MeanTa*rel.sd.means["SDTa",], SDTa))
summary.data$SDPb <- with(summary.data, ifelse(SDPb==0, summary.data$MeanPb*rel.sd.means["SDPb",], SDPb))
summary.data$SDTh <- with(summary.data, ifelse(SDTh==0, summary.data$MeanTh*rel.sd.means["SDTh",], SDTh))
summary.data$SDU <- with(summary.data, ifelse(SDU==0, summary.data$MeanU*rel.sd.means["SDU",], SDU))





summary.data[ is.na(summary.data) ] <- NA

write.table(summary.data, file="/Users/lee/Dropbox/Documents/Global Obsidian/First Draft/postsd.csv", sep=",")


#test
apply(summary.data, 2, function(x) length(which(!is.na(x))))



all.data <- merge(x=metadata, y=summary.data, by.x="Serial", by.y="Serial", all=TRUE, sort=FALSE)
all.data <- merge(x=all.data[,!(names(all.data)) %in% c("n.x", "n.y")], y=summary.n.dat, by.x="Serial", by.y="Serial", all=TRUE, sort=FALSE)
rownames(all.data) <- all.data$Serial

write.csv(all.data, file="data/globe/readydatasub.csv")

factor_vector <- function(vector){
    levels(as.factor(vector))
}

single.source <- subset(all.data, as.vector(all.data$Source.Common.Name)==as.vector(all.data$Source.General))
multiple.source <- subset(all.data, as.vector(all.data$Source.Common.Name)!=as.vector(all.data$Source.General))

multiple.source.list <- split(multiple.source, f=as.vector(multiple.source$Source.General))
names(multiple.source.list) <- sort(unique(as.vector(multiple.source$Source.General)))





element_extend <- function(mean, sd){
    rnorm(1000, mean=mean, sd=sd)
}

