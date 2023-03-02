get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}



list.of.packages <- c("pbapply", "reshape2", "TTR", "dplyr", "ggtern", "ggplot2", "shiny", "rhandsontable", "random", "data.table", "DT", "shinythemes", "Cairo", "gghighlight", "ggmap", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))



if(strsplit(strsplit(version[['version.string']], ' ')[[1]][3], '\\.')[[1]][1]=="3"){
    if("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.0.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) NULL)
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.0.tgz?raw=true", type="binary", repos=NULL), error=function(e) NULL)
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.0.tar.gz?raw=true", type="source", repos=NULL), error=function(e) NULL)
    }
} else if(strsplit(strsplit(version[['version.string']], ' ')[[1]][3], '\\.')[[1]][1]=="4"){
    if("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.1.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/rPDZ", subdir="rPDZ"), error=function(e) NULL))
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.1.tgz?raw=true", type="binary", repos=NULL), error=function(e) NULL)
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.1.tar.gz?raw=true", type="source", repos=NULL), error=function(e) NULL)
    }
}


tryCatch(library(rPDZ), error=function(e) NULL)



library(pbapply)
library(reshape2)
library(TTR)
library(plyr)
library(dplyr)
library(ggtern)
library(ggplot2)
library(shiny)
library(ggmap)
library(geosphere)
library(parallel)
library(pbapply)
library(data.table)
library(scales)



Hodder.v <- function(y){
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- y[i+1] - y[i]
        y[1:(n-1)]
        y <- abs(y)
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}

round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
}


#Rcpp::sourceCpp("pdz.cpp")

readPDZ25Data <- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ25(filepath, start=481, size=nbrOfRecords)
    
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[21]/10)
    
    data.frame(Energy=energy, CPS=counts, Sample=filename.vector)
    
}


readPDZ24Data<- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ24(filepath, start=357, size=nbrOfRecords)
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[21]/10)
    
    data.frame(Energy=energy, CPS=counts, Sample=filename.vector)
    
}



readPDZData <- function(filepath, filename) {
    nbrOfRecords <- 10000
    
    
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    
    if(floats[[9]]=="5"){
        readPDZ25Data(filepath, filename)
    }else {
        readPDZ24Data(filepath, filename)
    }
    
    
}




read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}



read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}



read_csv_net <- function(filepath) {
    
    ret <- read.csv(file=filepath, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    eline <- paste(element, line, sep="-")
    
    simple.table <- data.frame(net)
    colnames(simple.table) <- NULL
    simple.transpose <- as.data.frame(t(simple.table))
    colnames(simple.transpose) <- eline
    
    simple.transpose
    
}

file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}

is.0 <- function(cps, file) {
    file.0 <- function(file) {
        if (length(file) > 0)
        {
            return(file)
        }else{
            return(levels(file))
        }
    }
    if (length(cps) > 0)
    {
        hope <-data.frame(cps, file.0(file))
        return(hope)
    } else {
        empty <- rep(0, length(file.0(file)))
        framed <- data.frame(empty, file.0(file))
        return(framed)
    }
}

dt_options <- reactive({
    # dynamically create options for `aoColumns` depending on how many columns are selected.
    toggles <- lapply(1:length(input$show_vars), function(x) list(bSearchable = F))
    # for `species` columns
    toggles[[length(toggles) + 1]] <- list(bSearchable = T)
    
    list(
    aoColumns = toggles,
    bFilter = 1, bSortClasses = 1,
    aLengthMenu = list(c(10,25,50, -1), list('10','25', '50', 'Todas')),
    iDisplayLength = 10
    )
})

ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}


scree_crunch <- function(dataframe, dependent, independent){
    
    simple.frame <- data.frame(
    newY = dataframe[,dependent]/max(dataframe[,dependent]),
    newX = dataframe[,independent]/max(dataframe[,independent]))
    
    sims <-data.frame(
    sims1 = seq(from=1, to=nrow(dataframe)-1, by=1),
    sims2 = seq(from=2, to=nrow(dataframe), by=1)
    )
    
    n <- seq(from=1, to=nrow(sims), by=1)
    
    lm.sims <- pbapply::pblapply(n, function(x) summary(lm(newY~newX, data=simple.frame[sims[,1][x]:sims[,2][x],])))
    
    
    slopes <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["coefficients"][[1]][2])))
    
    #rsquared <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["r.squared"])))
    
    greater.1 <- which(abs(slopes) > 1)
    
    greater.1[length(greater.1)]+1
    
    
}

black.diamond <- read.csv("data/blackdiamond.csv", header=FALSE, sep=",")
black.diamond.melt <- read.csv(file="data/blackdiamondmelt.csv")

######Load lines
k.lines <- read.csv(file="data/K Line-Table 1.csv", sep=",")
l.lines <- read.csv(file="data/L Line-Table 1.csv", sep=",")
fluorescence.lines <- read.csv("data/FluorescenceLines.csv")

#k.lines[k.lines < 0.01] <- 1
#l.lines[l.lines < 0.01] <- 1

lines <- data.frame(k.lines, l.lines)

H.lines <- data.frame(lines$Ka1[1], lines$Ka2[1], lines$Kb1[1], lines$Kb2[1], lines$Kb3[1], lines$La1[1], lines$La2[1], lines$Lb1[1], lines$Lb2[1], lines$Lb3[1], lines$Lb4[1], lines$Lg1[1], lines$Lg2[1], lines$Lg3[1], lines$Ll[1])
colnames(H.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

He.lines <- data.frame(lines$Ka1[2], lines$Ka2[2], lines$Kb1[2], lines$Kb2[2], lines$Kb3[2], lines$La1[2], lines$La2[2], lines$Lb1[2], lines$Lb2[2], lines$Lb3[2], lines$Lb4[2], lines$Lg1[2], lines$Lg2[2], lines$Lg3[2], lines$Ll[2])
colnames(He.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Li.lines <- data.frame(lines$Ka1[3], lines$Ka2[3], lines$Kb1[3], lines$Kb2[3], lines$Kb3[3], lines$La1[3], lines$La2[3], lines$Lb1[3], lines$Lb2[3], lines$Lb3[3], lines$Lb4[3], lines$Lg1[3], lines$Lg2[3], lines$Lg3[3], lines$Ll[3])
colnames(Li.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Be.lines <- data.frame(lines$Ka1[4], lines$Ka2[4], lines$Kb1[4], lines$Kb2[4], lines$Kb3[4], lines$La1[4], lines$La2[4], lines$Lb1[4], lines$Lb2[4], lines$Lb3[4], lines$Lb4[4], lines$Lg1[4], lines$Lg2[4], lines$Lg3[4], lines$Ll[4])
colnames(Be.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

B.lines <- data.frame(lines$Ka1[5], lines$Ka2[5], lines$Kb1[5], lines$Kb2[5], lines$Kb3[5], lines$La1[5], lines$La2[5], lines$Lb1[5], lines$Lb2[5], lines$Lb3[5], lines$Lb4[5], lines$Lg1[5], lines$Lg2[5], lines$Lg3[5], lines$Ll[5])
colnames(B.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

C.lines <- data.frame(lines$Ka1[6], lines$Ka2[6], lines$Kb1[6], lines$Kb2[6], lines$Kb3[6], lines$La1[6], lines$La2[6], lines$Lb1[6], lines$Lb2[6], lines$Lb3[6], lines$Lb4[6], lines$Lg1[6], lines$Lg2[6], lines$Lg3[6], lines$Ll[6])
colnames(C.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

N.lines <- data.frame(lines$Ka1[7], lines$Ka2[7], lines$Kb1[7], lines$Kb2[7], lines$Kb3[7], lines$La1[7], lines$La2[7], lines$Lb1[7], lines$Lb2[7], lines$Lb3[7], lines$Lb4[7], lines$Lg1[7], lines$Lg2[7], lines$Lg3[7], lines$Ll[7])
colnames(N.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

O.lines <- data.frame(lines$Ka1[8], lines$Ka2[8], lines$Kb1[8], lines$Kb2[8], lines$Kb3[8], lines$La1[8], lines$La2[8], lines$Lb1[8], lines$Lb2[8], lines$Lb3[8], lines$Lb4[8], lines$Lg1[8], lines$Lg2[8], lines$Lg3[8], lines$Ll[8])
colnames(O.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

F.lines <- data.frame(lines$Ka1[9], lines$Ka2[9], lines$Kb1[9], lines$Kb2[9], lines$Kb3[9], lines$La1[9], lines$La2[9], lines$Lb1[9], lines$Lb2[9], lines$Lb3[9], lines$Lb4[9], lines$Lg1[9], lines$Lg2[9], lines$Lg3[9], lines$Ll[9])
colnames(F.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ne.lines <- data.frame(lines$Ka1[10], lines$Ka2[10], lines$Kb1[10], lines$Kb2[10], lines$Kb3[10], lines$La1[10], lines$La2[10], lines$Lb1[10], lines$Lb2[10], lines$Lb3[10], lines$Lb4[10], lines$Lg1[10], lines$Lg2[10], lines$Lg3[10], lines$Ll[10])
colnames(Ne.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Na.lines <- data.frame(lines$Ka1[11], lines$Ka2[11], lines$Kb1[11], lines$Kb2[11], lines$Kb3[11], lines$La1[11], lines$La2[11], lines$Lb1[11], lines$Lb2[11], lines$Lb3[11], lines$Lb4[11], lines$Lg1[11], lines$Lg2[11], lines$Lg3[11], lines$Ll[11])
colnames(Na.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mg.lines <- data.frame(lines$Ka1[12], lines$Ka2[12], lines$Kb1[12], lines$Kb2[12], lines$Kb3[12], lines$La1[12], lines$La2[12], lines$Lb1[12], lines$Lb2[12], lines$Lb3[12], lines$Lb4[12], lines$Lg1[12], lines$Lg2[12], lines$Lg3[12], lines$Ll[12])
colnames(Mg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Al.lines <- data.frame(lines$Ka1[13], lines$Ka2[13], lines$Kb1[13], lines$Kb2[13], lines$Kb3[13], lines$La1[13], lines$La2[13], lines$Lb1[13], lines$Lb2[13], lines$Lb3[13], lines$Lb4[13], lines$Lg1[13], lines$Lg2[13], lines$Lg3[13], lines$Ll[13])
colnames(Al.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Si.lines <- data.frame(lines$Ka1[14], lines$Ka2[14], lines$Kb1[14], lines$Kb2[14], lines$Kb3[14], lines$La1[14], lines$La2[14], lines$Lb1[14], lines$Lb2[14], lines$Lb3[14], lines$Lb4[14], lines$Lg1[14], lines$Lg2[14], lines$Lg3[14], lines$Ll[14])
colnames(Si.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

P.lines <- data.frame(lines$Ka1[15], lines$Ka2[15], lines$Kb1[15], lines$Kb2[15], lines$Kb3[15], lines$La1[15], lines$La2[15], lines$Lb1[15], lines$Lb2[15], lines$Lb3[15], lines$Lb4[15], lines$Lg1[15], lines$Lg2[15], lines$Lg3[15], lines$Ll[15])
colnames(P.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

S.lines <- data.frame(lines$Ka1[16], lines$Ka2[16], lines$Kb1[16], lines$Kb2[16], lines$Kb3[16], lines$La1[16], lines$La2[16], lines$Lb1[16], lines$Lb2[16], lines$Lb3[16], lines$Lb4[16], lines$Lg1[16], lines$Lg2[16], lines$Lg3[16], lines$Ll[16])
colnames(S.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cl.lines <- data.frame(lines$Ka1[17], lines$Ka2[17], lines$Kb1[17], lines$Kb2[17], lines$Kb3[17], lines$La1[17], lines$La2[17], lines$Lb1[17], lines$Lb2[17], lines$Lb3[17], lines$Lb4[17], lines$Lg1[17], lines$Lg2[17], lines$Lg3[17], lines$Ll[17])
colnames(Cl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ar.lines <- data.frame(lines$Ka1[18], lines$Ka2[18], lines$Kb1[18], lines$Kb2[18], lines$Kb3[18], lines$La1[18], lines$La2[18], lines$Lb1[18], lines$Lb2[18], lines$Lb3[18], lines$Lb4[18], lines$Lg1[18], lines$Lg2[18], lines$Lg3[18], lines$Ll[18])
colnames(Ar.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

K.lines <- data.frame(lines$Ka1[19], lines$Ka2[19], lines$Kb1[19], lines$Kb2[19], lines$Kb3[19], lines$La1[19], lines$La2[19], lines$Lb1[19], lines$Lb2[19], lines$Lb3[19], lines$Lb4[19], lines$Lg1[19], lines$Lg2[19], lines$Lg3[19], lines$Ll[19])
colnames(K.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ca.lines <- data.frame(lines$Ka1[20], lines$Ka2[20], lines$Kb1[20], lines$Kb2[20], lines$Kb3[20], lines$La1[20], lines$La2[20], lines$Lb1[20], lines$Lb2[20], lines$Lb3[20], lines$Lb4[20], lines$Lg1[20], lines$Lg2[20], lines$Lg3[20], lines$Ll[20])
colnames(Ca.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sc.lines <- data.frame(lines$Ka1[21], lines$Ka2[21], lines$Kb1[21], lines$Kb2[21], lines$Kb3[21], lines$La1[21], lines$La2[21], lines$Lb1[21], lines$Lb2[21], lines$Lb3[21], lines$Lb4[21], lines$Lg1[21], lines$Lg2[21], lines$Lg3[21], lines$Ll[21])
colnames(Sc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ti.lines <- data.frame(lines$Ka1[22], lines$Ka2[22], lines$Kb1[22], lines$Kb2[22], lines$Kb3[22], lines$La1[22], lines$La2[22], lines$Lb1[22], lines$Lb2[22], lines$Lb3[22], lines$Lb4[22], lines$Lg1[22], lines$Lg2[22], lines$Lg3[22], lines$Ll[22])
colnames(Ti.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

V.lines <- data.frame(lines$Ka1[23], lines$Ka2[23], lines$Kb1[23], lines$Kb2[23], lines$Kb3[23], lines$La1[23], lines$La2[23], lines$Lb1[23], lines$Lb2[23], lines$Lb3[23], lines$Lb4[23], lines$Lg1[23], lines$Lg2[23], lines$Lg3[23], lines$Ll[23])
colnames(V.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cr.lines <- data.frame(lines$Ka1[24], lines$Ka2[24], lines$Kb1[24], lines$Kb2[24], lines$Kb3[24], lines$La1[24], lines$La2[24], lines$Lb1[24], lines$Lb2[24], lines$Lb3[24], lines$Lb4[24], lines$Lg1[24], lines$Lg2[24], lines$Lg3[24], lines$Ll[24])
colnames(Cr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mn.lines <- data.frame(lines$Ka1[25], lines$Ka2[25], lines$Kb1[25], lines$Kb2[25], lines$Kb3[25], lines$La1[25], lines$La2[25], lines$Lb1[25], lines$Lb2[25], lines$Lb3[25], lines$Lb4[25], lines$Lg1[25], lines$Lg2[25], lines$Lg3[25], lines$Ll[25])
colnames(Mn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fe.lines <- data.frame(lines$Ka1[26], lines$Ka2[26], lines$Kb1[26], lines$Kb2[26], lines$Kb3[26], lines$La1[26], lines$La2[26], lines$Lb1[26], lines$Lb2[26], lines$Lb3[26], lines$Lb4[26], lines$Lg1[26], lines$Lg2[26], lines$Lg3[26], lines$Ll[26])
colnames(Fe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Co.lines <- data.frame(lines$Ka1[27], lines$Ka2[27], lines$Kb1[27], lines$Kb2[27], lines$Kb3[27], lines$La1[27], lines$La2[27], lines$Lb1[27], lines$Lb2[27], lines$Lb3[27], lines$Lb4[27], lines$Lg1[27], lines$Lg2[27], lines$Lg3[27], lines$Ll[27])
colnames(Co.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ni.lines <- data.frame(lines$Ka1[28], lines$Ka2[28], lines$Kb1[28], lines$Kb2[28], lines$Kb3[28], lines$La1[28], lines$La2[28], lines$Lb1[28], lines$Lb2[28], lines$Lb3[28], lines$Lb4[28], lines$Lg1[28], lines$Lg2[28], lines$Lg3[28], lines$Ll[28])
colnames(Ni.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cu.lines <- data.frame(lines$Ka1[29], lines$Ka2[29], lines$Kb1[29], lines$Kb2[29], lines$Kb3[29], lines$La1[29], lines$La2[29], lines$Lb1[29], lines$Lb2[29], lines$Lb3[29], lines$Lb4[29], lines$Lg1[29], lines$Lg2[29], lines$Lg3[29], lines$Ll[29])
colnames(Cu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zn.lines <- data.frame(lines$Ka1[30], lines$Ka2[30], lines$Kb1[30], lines$Kb2[30], lines$Kb3[30], lines$La1[30], lines$La2[30], lines$Lb1[30], lines$Lb2[30], lines$Lb3[30], lines$Lb4[30], lines$Lg1[30], lines$Lg2[30], lines$Lg3[30], lines$Ll[30])
colnames(Zn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ga.lines <- data.frame(lines$Ka1[31], lines$Ka2[31], lines$Kb1[31], lines$Kb2[31], lines$Kb3[31], lines$La1[31], lines$La2[31], lines$Lb1[31], lines$Lb2[31], lines$Lb3[31], lines$Lb4[31], lines$Lg1[31], lines$Lg2[31], lines$Lg3[31], lines$Ll[31])
colnames(Ga.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ge.lines <- data.frame(lines$Ka1[32], lines$Ka2[32], lines$Kb1[32], lines$Kb2[32], lines$Kb3[32], lines$La1[32], lines$La2[32], lines$Lb1[32], lines$Lb2[32], lines$Lb3[32], lines$Lb4[32], lines$Lg1[32], lines$Lg2[32], lines$Lg3[32], lines$Ll[32])
colnames(Ge.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

As.lines <- data.frame(lines$Ka1[33], lines$Ka2[33], lines$Kb1[33], lines$Kb2[33], lines$Kb3[33], lines$La1[33], lines$La2[33], lines$Lb1[33], lines$Lb2[33], lines$Lb3[33], lines$Lb4[33], lines$Lg1[33], lines$Lg2[33], lines$Lg3[33], lines$Ll[33])
colnames(As.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Se.lines <- data.frame(lines$Ka1[34], lines$Ka2[34], lines$Kb1[34], lines$Kb2[34], lines$Kb3[34], lines$La1[34], lines$La2[34], lines$Lb1[34], lines$Lb2[34], lines$Lb3[34], lines$Lb4[34], lines$Lg1[34], lines$Lg2[34], lines$Lg3[34], lines$Ll[34])
colnames(Se.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Br.lines <- data.frame(lines$Ka1[35], lines$Ka2[35], lines$Kb1[35], lines$Kb2[35], lines$Kb3[35], lines$La1[35], lines$La2[35], lines$Lb1[35], lines$Lb2[35], lines$Lb3[35], lines$Lb4[35], lines$Lg1[35], lines$Lg2[35], lines$Lg3[35], lines$Ll[35])
colnames(Br.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Kr.lines <- data.frame(lines$Ka1[36], lines$Ka2[36], lines$Kb1[36], lines$Kb2[36], lines$Kb3[36], lines$La1[36], lines$La2[36], lines$Lb1[36], lines$Lb2[36], lines$Lb3[36], lines$Lb4[36], lines$Lg1[36], lines$Lg2[36], lines$Lg3[36], lines$Ll[36])
colnames(Kr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rb.lines <- data.frame(lines$Ka1[37], lines$Ka2[37], lines$Kb1[37], lines$Kb2[37], lines$Kb3[37], lines$La1[37], lines$La2[37], lines$Lb1[37], lines$Lb2[37], lines$Lb3[37], lines$Lb4[37], lines$Lg1[37], lines$Lg2[37], lines$Lg3[37], lines$Ll[37])
colnames(Rb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sr.lines <- data.frame(lines$Ka1[38], lines$Ka2[38], lines$Kb1[38], lines$Kb2[38], lines$Kb3[38], lines$La1[38], lines$La2[38], lines$Lb1[38], lines$Lb2[38], lines$Lb3[38], lines$Lb4[38], lines$Lg1[38], lines$Lg2[38], lines$Lg3[38], lines$Ll[38])
colnames(Sr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Y.lines <- data.frame(lines$Ka1[39], lines$Ka2[39], lines$Kb1[39], lines$Kb2[39], lines$Kb3[39], lines$La1[39], lines$La2[39], lines$Lb1[39], lines$Lb2[39], lines$Lb3[39], lines$Lb4[39], lines$Lg1[39], lines$Lg2[39], lines$Lg3[39], lines$Ll[39])
colnames(Y.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zr.lines <- data.frame(lines$Ka1[40], lines$Ka2[40], lines$Kb1[40], lines$Kb2[40], lines$Kb3[40], lines$La1[40], lines$La2[40], lines$Lb1[40], lines$Lb2[40], lines$Lb3[40], lines$Lb4[40], lines$Lg1[40], lines$Lg2[40], lines$Lg3[40], lines$Ll[40])
colnames(Zr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nb.lines <- data.frame(lines$Ka1[41], lines$Ka2[41], lines$Kb1[41], lines$Kb2[41], lines$Kb3[41], lines$La1[41], lines$La2[41], lines$Lb1[41], lines$Lb2[41], lines$Lb3[41], lines$Lb4[41], lines$Lg1[41], lines$Lg2[41], lines$Lg3[41], lines$Ll[41])
colnames(Nb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mo.lines <- data.frame(lines$Ka1[42], lines$Ka2[42], lines$Kb1[42], lines$Kb2[42], lines$Kb3[42], lines$La1[42], lines$La2[42], lines$Lb1[42], lines$Lb2[42], lines$Lb3[42], lines$Lb4[42], lines$Lg1[42], lines$Lg2[42], lines$Lg3[42], lines$Ll[42])
colnames(Mo.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tc.lines <- data.frame(lines$Ka1[43], lines$Ka2[43], lines$Kb1[43], lines$Kb2[43], lines$Kb3[43], lines$La1[43], lines$La2[43], lines$Lb1[43], lines$Lb2[43], lines$Lb3[43], lines$Lb4[43], lines$Lg1[43], lines$Lg2[43], lines$Lg3[43], lines$Ll[43])
colnames(Tc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ru.lines <- data.frame(lines$Ka1[44], lines$Ka2[44], lines$Kb1[44], lines$Kb2[44], lines$Kb3[44], lines$La1[44], lines$La2[44], lines$Lb1[44], lines$Lb2[44], lines$Lb3[44], lines$Lb4[44], lines$Lg1[44], lines$Lg2[44], lines$Lg3[44], lines$Ll[44])
colnames(Ru.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rh.lines <- data.frame(lines$Ka1[45], lines$Ka2[45], lines$Kb1[45], lines$Kb2[45], lines$Kb3[45], lines$La1[45], lines$La2[45], lines$Lb1[45], lines$Lb2[45], lines$Lb3[45], lines$Lb4[45], lines$Lg1[45], lines$Lg2[45], lines$Lg3[45], lines$Ll[45])
colnames(Rh.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pd.lines <- data.frame(lines$Ka1[46], lines$Ka2[46], lines$Kb1[46], lines$Kb2[46], lines$Kb3[46], lines$La1[46], lines$La2[46], lines$Lb1[46], lines$Lb2[46], lines$Lb3[46], lines$Lb4[46], lines$Lg1[46], lines$Lg2[46], lines$Lg3[46], lines$Ll[46])
colnames(Pd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ag.lines <- data.frame(lines$Ka1[47], lines$Ka2[47], lines$Kb1[47], lines$Kb2[47], lines$Kb3[47], lines$La1[47], lines$La2[47], lines$Lb1[47], lines$Lb2[47], lines$Lb3[47], lines$Lb4[47], lines$Lg1[47], lines$Lg2[47], lines$Lg3[47], lines$Ll[47])
colnames(Ag.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cd.lines <- data.frame(lines$Ka1[48], lines$Ka2[48], lines$Kb1[48], lines$Kb2[48], lines$Kb3[48], lines$La1[48], lines$La2[48], lines$Lb1[48], lines$Lb2[48], lines$Lb3[48], lines$Lb4[48], lines$Lg1[48], lines$Lg2[48], lines$Lg3[48], lines$Ll[48])
colnames(Cd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

In.lines <- data.frame(lines$Ka1[49], lines$Ka2[49], lines$Kb1[49], lines$Kb2[49], lines$Kb3[49], lines$La1[49], lines$La2[49], lines$Lb1[49], lines$Lb2[49], lines$Lb3[49], lines$Lb4[49], lines$Lg1[49], lines$Lg2[49], lines$Lg3[49], lines$Ll[49])
colnames(In.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sn.lines <- data.frame(lines$Ka1[50], lines$Ka2[50], lines$Kb1[50], lines$Kb2[50], lines$Kb3[50], lines$La1[50], lines$La2[50], lines$Lb1[50], lines$Lb2[50], lines$Lb3[50], lines$Lb4[50], lines$Lg1[50], lines$Lg2[50], lines$Lg3[50], lines$Ll[50])
colnames(Sn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sb.lines <- data.frame(lines$Ka1[51], lines$Ka2[51], lines$Kb1[51], lines$Kb2[51], lines$Kb3[51], lines$La1[51], lines$La2[51], lines$Lb1[51], lines$Lb2[51], lines$Lb3[51], lines$Lb4[51], lines$Lg1[51], lines$Lg2[51], lines$Lg3[51], lines$Ll[51])
colnames(Sb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Te.lines <- data.frame(lines$Ka1[52], lines$Ka2[52], lines$Kb1[52], lines$Kb2[52], lines$Kb3[52], lines$La1[52], lines$La2[52], lines$Lb1[52], lines$Lb2[52], lines$Lb3[52], lines$Lb4[52], lines$Lg1[52], lines$Lg2[52], lines$Lg3[52], lines$Ll[52])
colnames(Te.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

I.lines <- data.frame(lines$Ka1[53], lines$Ka2[53], lines$Kb1[53], lines$Kb2[53], lines$Kb3[53], lines$La1[53], lines$La2[53], lines$Lb1[53], lines$Lb2[53], lines$Lb3[53], lines$Lb4[53], lines$Lg1[53], lines$Lg2[53], lines$Lg3[53], lines$Ll[53])
colnames(I.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Xe.lines <- data.frame(lines$Ka1[54], lines$Ka2[54], lines$Kb1[54], lines$Kb2[54], lines$Kb3[54], lines$La1[54], lines$La2[54], lines$Lb1[54], lines$Lb2[54], lines$Lb3[54], lines$Lb4[54], lines$Lg1[54], lines$Lg2[54], lines$Lg3[54], lines$Ll[54])
colnames(Xe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cs.lines <- data.frame(lines$Ka1[55], lines$Ka2[55], lines$Kb1[55], lines$Kb2[55], lines$Kb3[55], lines$La1[55], lines$La2[55], lines$Lb1[55], lines$Lb2[55], lines$Lb3[55], lines$Lb4[55], lines$Lg1[55], lines$Lg2[55], lines$Lg3[55], lines$Ll[55])
colnames(Cs.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ba.lines <- data.frame(lines$Ka1[56], lines$Ka2[56], lines$Kb1[56], lines$Kb2[56], lines$Kb3[56], lines$La1[56], lines$La2[56], lines$Lb1[56], lines$Lb2[56], lines$Lb3[56], lines$Lb4[56], lines$Lg1[56], lines$Lg2[56], lines$Lg3[56], lines$Ll[56])
colnames(Ba.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

La.lines <- data.frame(lines$Ka1[57], lines$Ka2[57], lines$Kb1[57], lines$Kb2[57], lines$Kb3[57], lines$La1[57], lines$La2[57], lines$Lb1[57], lines$Lb2[57], lines$Lb3[57], lines$Lb4[57], lines$Lg1[57], lines$Lg2[57], lines$Lg3[57], lines$Ll[57])
colnames(La.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ce.lines <- data.frame(lines$Ka1[58], lines$Ka2[58], lines$Kb1[58], lines$Kb2[58], lines$Kb3[58], lines$La1[58], lines$La2[58], lines$Lb1[58], lines$Lb2[58], lines$Lb3[58], lines$Lb4[58], lines$Lg1[58], lines$Lg2[58], lines$Lg3[58], lines$Ll[58])
colnames(Ce.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pr.lines <- data.frame(lines$Ka1[59], lines$Ka2[59], lines$Kb1[59], lines$Kb2[59], lines$Kb3[59], lines$La1[59], lines$La2[59], lines$Lb1[59], lines$Lb2[59], lines$Lb3[59], lines$Lb4[59], lines$Lg1[59], lines$Lg2[59], lines$Lg3[59], lines$Ll[59])
colnames(Pr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nd.lines <- data.frame(lines$Ka1[60], lines$Ka2[60], lines$Kb1[60], lines$Kb2[60], lines$Kb3[60], lines$La1[60], lines$La2[60], lines$Lb1[60], lines$Lb2[60], lines$Lb3[60], lines$Lb4[60], lines$Lg1[60], lines$Lg2[60], lines$Lg3[60], lines$Ll[60])
colnames(Nd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pm.lines <- data.frame(lines$Ka1[61], lines$Ka2[61], lines$Kb1[61], lines$Kb2[61], lines$Kb3[61], lines$La1[61], lines$La2[61], lines$Lb1[61], lines$Lb2[61], lines$Lb3[61], lines$Lb4[61], lines$Lg1[61], lines$Lg2[61], lines$Lg3[61], lines$Ll[61])
colnames(Pm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sm.lines <- data.frame(lines$Ka1[62], lines$Ka2[62], lines$Kb1[62], lines$Kb2[62], lines$Kb3[62], lines$La1[62], lines$La2[62], lines$Lb1[62], lines$Lb2[62], lines$Lb3[62], lines$Lb4[62], lines$Lg1[62], lines$Lg2[62], lines$Lg3[62], lines$Ll[62])
colnames(Sm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Eu.lines <- data.frame(lines$Ka1[63], lines$Ka2[63], lines$Kb1[63], lines$Kb2[63], lines$Kb3[63], lines$La1[63], lines$La2[63], lines$Lb1[63], lines$Lb2[63], lines$Lb3[63], lines$Lb4[63], lines$Lg1[63], lines$Lg2[63], lines$Lg3[63], lines$Ll[63])
colnames(Eu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Gd.lines <- data.frame(lines$Ka1[64], lines$Ka2[64], lines$Kb1[64], lines$Kb2[64], lines$Kb3[64], lines$La1[64], lines$La2[64], lines$Lb1[64], lines$Lb2[64], lines$Lb3[64], lines$Lb4[64], lines$Lg1[64], lines$Lg2[64], lines$Lg3[64], lines$Ll[64])
colnames(Gd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tb.lines <- data.frame(lines$Ka1[65], lines$Ka2[65], lines$Kb1[65], lines$Kb2[65], lines$Kb3[65], lines$La1[65], lines$La2[65], lines$Lb1[65], lines$Lb2[65], lines$Lb3[65], lines$Lb4[65], lines$Lg1[65], lines$Lg2[65], lines$Lg3[65], lines$Ll[65])
colnames(Tb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Dy.lines <- data.frame(lines$Ka1[66], lines$Ka2[66], lines$Kb1[66], lines$Kb2[66], lines$Kb3[66], lines$La1[66], lines$La2[66], lines$Lb1[66], lines$Lb2[66], lines$Lb3[66], lines$Lb4[66], lines$Lg1[66], lines$Lg2[66], lines$Lg3[66], lines$Ll[66])
colnames(Dy.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ho.lines <- data.frame(lines$Ka1[67], lines$Ka2[67], lines$Kb1[67], lines$Kb2[67], lines$Kb3[67], lines$La1[67], lines$La2[67], lines$Lb1[67], lines$Lb2[67], lines$Lb3[67], lines$Lb4[67], lines$Lg1[67], lines$Lg2[67], lines$Lg3[67], lines$Ll[67])
colnames(Ho.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Er.lines <- data.frame(lines$Ka1[68], lines$Ka2[68], lines$Kb1[68], lines$Kb2[68], lines$Kb3[68], lines$La1[68], lines$La2[68], lines$Lb1[68], lines$Lb2[68], lines$Lb3[68], lines$Lb4[68], lines$Lg1[68], lines$Lg2[68], lines$Lg3[68], lines$Ll[68])
colnames(Er.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tm.lines <- data.frame(lines$Ka1[69], lines$Ka2[69], lines$Kb1[69], lines$Kb2[69], lines$Kb3[69], lines$La1[69], lines$La2[69], lines$Lb1[69], lines$Lb2[69], lines$Lb3[69], lines$Lb4[69], lines$Lg1[69], lines$Lg2[69], lines$Lg3[69], lines$Ll[69])
colnames(Tm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Yb.lines <- data.frame(lines$Ka1[70], lines$Ka2[70], lines$Kb1[70], lines$Kb2[70], lines$Kb3[70], lines$La1[70], lines$La2[70], lines$Lb1[70], lines$Lb2[70], lines$Lb3[70], lines$Lb4[70], lines$Lg1[70], lines$Lg2[70], lines$Lg3[70], lines$Ll[70])
colnames(Yb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Lu.lines <- data.frame(lines$Ka1[71], lines$Ka2[71], lines$Kb1[71], lines$Kb2[71], lines$Kb3[71], lines$La1[71], lines$La2[71], lines$Lb1[71], lines$Lb2[71], lines$Lb3[71], lines$Lb4[71], lines$Lg1[71], lines$Lg2[71], lines$Lg3[71], lines$Ll[71])
colnames(Lu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hf.lines <- data.frame(lines$Ka1[72], lines$Ka2[72], lines$Kb1[72], lines$Kb2[72], lines$Kb3[72], lines$La1[72], lines$La2[72], lines$Lb1[72], lines$Lb2[72], lines$Lb3[72], lines$Lb4[72], lines$Lg1[72], lines$Lg2[72], lines$Lg3[72], lines$Ll[72])
colnames(Hf.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ta.lines <- data.frame(lines$Ka1[73], lines$Ka2[73], lines$Kb1[73], lines$Kb2[73], lines$Kb3[73], lines$La1[73], lines$La2[73], lines$Lb1[73], lines$Lb2[73], lines$Lb3[73], lines$Lb4[73], lines$Lg1[73], lines$Lg2[73], lines$Lg3[73], lines$Ll[73])
colnames(Ta.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

W.lines <- data.frame(lines$Ka1[74], lines$Ka2[74], lines$Kb1[74], lines$Kb2[74], lines$Kb3[74], lines$La1[74], lines$La2[74], lines$Lb1[74], lines$Lb2[74], lines$Lb3[74], lines$Lb4[74], lines$Lg1[74], lines$Lg2[74], lines$Lg3[74], lines$Ll[74])
colnames(W.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Re.lines <- data.frame(lines$Ka1[75], lines$Ka2[75], lines$Kb1[75], lines$Kb2[75], lines$Kb3[75], lines$La1[75], lines$La2[75], lines$Lb1[75], lines$Lb2[75], lines$Lb3[75], lines$Lb4[75], lines$Lg1[75], lines$Lg2[75], lines$Lg3[75], lines$Ll[75])
colnames(Re.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Os.lines <- data.frame(lines$Ka1[76], lines$Ka2[76], lines$Kb1[76], lines$Kb2[76], lines$Kb3[76], lines$La1[76], lines$La2[76], lines$Lb1[76], lines$Lb2[76], lines$Lb3[76], lines$Lb4[76], lines$Lg1[76], lines$Lg2[76], lines$Lg3[76], lines$Ll[76])
colnames(Os.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ir.lines <- data.frame(lines$Ka1[77], lines$Ka2[77], lines$Kb1[77], lines$Kb2[77], lines$Kb3[77], lines$La1[77], lines$La2[77], lines$Lb1[77], lines$Lb2[77], lines$Lb3[77], lines$Lb4[77], lines$Lg1[77], lines$Lg2[77], lines$Lg3[77], lines$Ll[77])
colnames(Ir.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pt.lines <- data.frame(lines$Ka1[78], lines$Ka2[78], lines$Kb1[78], lines$Kb2[78], lines$Kb3[78], lines$La1[78], lines$La2[78], lines$Lb1[78], lines$Lb2[78], lines$Lb3[78], lines$Lb4[78], lines$Lg1[78], lines$Lg2[78], lines$Lg3[78], lines$Ll[78])
colnames(Pt.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Au.lines <- data.frame(lines$Ka1[79], lines$Ka2[79], lines$Kb1[79], lines$Kb2[79], lines$Kb3[79], lines$La1[79], lines$La2[79], lines$Lb1[79], lines$Lb2[79], lines$Lb3[79], lines$Lb4[79], lines$Lg1[79], lines$Lg2[79], lines$Lg3[79], lines$Ll[79])
colnames(Au.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hg.lines <- data.frame(lines$Ka1[80], lines$Ka2[80], lines$Kb1[80], lines$Kb2[80], lines$Kb3[80], lines$La1[80], lines$La2[80], lines$Lb1[80], lines$Lb2[80], lines$Lb3[80], lines$Lb4[80], lines$Lg1[80], lines$Lg2[80], lines$Lg3[80], lines$Ll[80])
colnames(Hg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tl.lines <- data.frame(lines$Ka1[81], lines$Ka2[81], lines$Kb1[81], lines$Kb2[81], lines$Kb3[81], lines$La1[81], lines$La2[81], lines$Lb1[81], lines$Lb2[81], lines$Lb3[81], lines$Lb4[81], lines$Lg1[81], lines$Lg2[81], lines$Lg3[81], lines$Ll[81])
colnames(Tl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pb.lines <- data.frame(lines$Ka1[82], lines$Ka2[82], lines$Kb1[82], lines$Kb2[82], lines$Kb3[82], lines$La1[82], lines$La2[82], lines$Lb1[82], lines$Lb2[82], lines$Lb3[82], lines$Lb4[82], lines$Lg1[82], lines$Lg2[82], lines$Lg3[82], lines$Ll[82])
colnames(Pb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Bi.lines <- data.frame(lines$Ka1[83], lines$Ka2[83], lines$Kb1[83], lines$Kb2[83], lines$Kb3[83], lines$La1[83], lines$La2[83], lines$Lb1[83], lines$Lb2[83], lines$Lb3[83], lines$Lb4[83], lines$Lg1[83], lines$Lg2[83], lines$Lg3[83], lines$Ll[83])
colnames(Bi.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Po.lines <- data.frame(lines$Ka1[84], lines$Ka2[84], lines$Kb1[84], lines$Kb2[84], lines$Kb3[84], lines$La1[84], lines$La2[84], lines$Lb1[84], lines$Lb2[84], lines$Lb3[84], lines$Lb4[84], lines$Lg1[84], lines$Lg2[84], lines$Lg3[84], lines$Ll[84])
colnames(Po.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

At.lines <- data.frame(lines$Ka1[85], lines$Ka2[85], lines$Kb1[85], lines$Kb2[85], lines$Kb3[85], lines$La1[85], lines$La2[85], lines$Lb1[85], lines$Lb2[85], lines$Lb3[85], lines$Lb4[85], lines$Lg1[85], lines$Lg2[85], lines$Lg3[85], lines$Ll[85])
colnames(At.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rn.lines <- data.frame(lines$Ka1[86], lines$Ka2[86], lines$Kb1[86], lines$Kb2[86], lines$Kb3[86], lines$La1[86], lines$La2[86], lines$Lb1[86], lines$Lb2[86], lines$Lb3[86], lines$Lb4[86], lines$Lg1[86], lines$Lg2[86], lines$Lg3[86], lines$Ll[86])
colnames(Rn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fr.lines <- data.frame(lines$Ka1[87], lines$Ka2[87], lines$Kb1[87], lines$Kb2[87], lines$Kb3[87], lines$La1[87], lines$La2[87], lines$Lb1[87], lines$Lb2[87], lines$Lb3[87], lines$Lb4[87], lines$Lg1[87], lines$Lg2[87], lines$Lg3[87], lines$Ll[87])
colnames(Fr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ra.lines <- data.frame(lines$Ka1[88], lines$Ka2[88], lines$Kb1[88], lines$Kb2[88], lines$Kb3[88], lines$La1[88], lines$La2[88], lines$Lb1[88], lines$Lb2[88], lines$Lb3[88], lines$Lb4[88], lines$Lg1[88], lines$Lg2[88], lines$Lg3[88], lines$Ll[88])
colnames(Ra.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ac.lines <- data.frame(lines$Ka1[89], lines$Ka2[89], lines$Kb1[89], lines$Kb2[89], lines$Kb3[89], lines$La1[89], lines$La2[89], lines$Lb1[89], lines$Lb2[89], lines$Lb3[89], lines$Lb4[89], lines$Lg1[89], lines$Lg2[89], lines$Lg3[89], lines$Ll[89])
colnames(Ac.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Th.lines <- data.frame(lines$Ka1[90], lines$Ka2[90], lines$Kb1[90], lines$Kb2[90], lines$Kb3[90], lines$La1[90], lines$La2[90], lines$Lb1[90], lines$Lb2[90], lines$Lb3[90], lines$Lb4[90], lines$Lg1[90], lines$Lg2[90], lines$Lg3[90], lines$Ll[90])
colnames(Th.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pa.lines <- data.frame(lines$Ka1[91], lines$Ka2[91], lines$Kb1[91], lines$Kb2[91], lines$Kb3[91], lines$La1[91], lines$La2[91], lines$Lb1[91], lines$Lb2[91], lines$Lb3[91], lines$Lb4[91], lines$Lg1[91], lines$Lg2[91], lines$Lg3[91], lines$Ll[91])
colnames(Pa.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

U.lines <- data.frame(lines$Ka1[92], lines$Ka2[92], lines$Kb1[92], lines$Kb2[92], lines$Kb3[92], lines$La1[92], lines$La2[92], lines$Lb1[92], lines$Lb2[92], lines$Lb3[92], lines$Lb4[92], lines$Lg1[92], lines$Lg2[92], lines$Lg3[92], lines$Ll[92])
colnames(U.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

H.K <- c(H.lines$Ka1, H.lines$Ka2, H.lines$Kb1, H.lines$Kb2, H.lines$Kb3)
He.K <- c(He.lines$Ka1, He.lines$Ka2, He.lines$Kb1, He.lines$Kb2, He.lines$Kb3)
Li.K <- c(Li.lines$Ka1, Li.lines$Ka2, Li.lines$Kb1, Li.lines$Kb2, Li.lines$Kb3)
Be.K <- c(Be.lines$Ka1, Be.lines$Ka2, Be.lines$Kb1, Be.lines$Kb2, Be.lines$Kb3)
B.K <- c(B.lines$Ka1, B.lines$Ka2, B.lines$Kb1, B.lines$Kb2, B.lines$Kb3)
C.K <- c(C.lines$Ka1, C.lines$Ka2, C.lines$Kb1, C.lines$Kb2, C.lines$Kb3)
N.K <- c(N.lines$Ka1, N.lines$Ka2, N.lines$Kb1, N.lines$Kb2, N.lines$Kb3)
O.K <- c(O.lines$Ka1, O.lines$Ka2, O.lines$Kb1, O.lines$Kb2, O.lines$Kb3)
F.K <- c(F.lines$Ka1, F.lines$Ka2, F.lines$Kb1, F.lines$Kb2, F.lines$Kb3)
Ne.K <- c(Ne.lines$Ka1, Ne.lines$Ka2, Ne.lines$Kb1, Ne.lines$Kb2, Ne.lines$Kb3)
Na.K <- c(Na.lines$Ka1, Na.lines$Ka2, Na.lines$Kb1, Na.lines$Kb2, Na.lines$Kb3)
Mg.K <- c(Mg.lines$Ka1, Mg.lines$Ka2, Mg.lines$Kb1, Mg.lines$Kb2, Mg.lines$Kb3)
Al.K <- c(Al.lines$Ka1, Al.lines$Ka2, Al.lines$Kb1, Al.lines$Kb2, Al.lines$Kb3)
Si.K <- c(Si.lines$Ka1, Si.lines$Ka2, Si.lines$Kb1, Si.lines$Kb2, Si.lines$Kb3)
P.K <- c(P.lines$Ka1, P.lines$Ka2, P.lines$Kb1, P.lines$Kb2, P.lines$Kb3)
S.K <- c(S.lines$Ka1, S.lines$Ka2, S.lines$Kb1, S.lines$Kb2, S.lines$Kb3)
Cl.K <- c(Cl.lines$Ka1, Cl.lines$Ka2, Cl.lines$Kb1, Cl.lines$Kb2, Cl.lines$Kb3)
Ar.K <- c(Ar.lines$Ka1, Ar.lines$Ka2, Ar.lines$Kb1, Ar.lines$Kb2, Ar.lines$Kb3)
K.K <- c(K.lines$Ka1, K.lines$Ka2, K.lines$Kb1, K.lines$Kb2, K.lines$Kb3)
Ca.K <- c(Ca.lines$Ka1, Ca.lines$Ka2, Ca.lines$Kb1, Ca.lines$Kb2, Ca.lines$Kb3)
Sc.K <- c(Sc.lines$Ka1, Sc.lines$Ka2, Sc.lines$Kb1, Sc.lines$Kb2, Sc.lines$Kb3)
Ti.K <- c(Ti.lines$Ka1, Ti.lines$Ka2, Ti.lines$Kb1, Ti.lines$Kb2, Ti.lines$Kb3)
V.K <- c(V.lines$Ka1, V.lines$Ka2, V.lines$Kb1, V.lines$Kb2, V.lines$Kb3)
Cr.K <- c(Cr.lines$Ka1, Cr.lines$Ka2, Cr.lines$Kb1, Cr.lines$Kb2, Cr.lines$Kb3)
Mn.K <- c(Mn.lines$Ka1, Mn.lines$Ka2, Mn.lines$Kb1, Mn.lines$Kb2, Mn.lines$Kb3)
Fe.K <- c(Fe.lines$Ka1, Fe.lines$Ka2, Fe.lines$Kb1, Fe.lines$Kb2, Fe.lines$Kb3)
Co.K <- c(Co.lines$Ka1, Co.lines$Ka2, Co.lines$Kb1, Co.lines$Kb2, Co.lines$Kb3)
Ni.K <- c(Ni.lines$Ka1, Ni.lines$Ka2, Ni.lines$Kb1, Ni.lines$Kb2, Ni.lines$Kb3)
Cu.K <- c(Cu.lines$Ka1, Cu.lines$Ka2, Cu.lines$Kb1, Cu.lines$Kb2, Cu.lines$Kb3)
Zn.K <- c(Zn.lines$Ka1, Zn.lines$Ka2, Zn.lines$Kb1, Zn.lines$Kb2, Zn.lines$Kb3)
Ga.K <- c(Ga.lines$Ka1, Ga.lines$Ka2, Ga.lines$Kb1, Ga.lines$Kb2, Ga.lines$Kb3)
Ge.K <- c(Ge.lines$Ka1, Ge.lines$Ka2, Ge.lines$Kb1, Ge.lines$Kb2, Ge.lines$Kb3)
As.K <- c(As.lines$Ka1, As.lines$Ka2, As.lines$Kb1, As.lines$Kb2, As.lines$Kb3)
Se.K <- c(Se.lines$Ka1, Se.lines$Ka2, Se.lines$Kb1, Se.lines$Kb2, Se.lines$Kb3)
Br.K <- c(Br.lines$Ka1, Br.lines$Ka2, Br.lines$Kb1, Br.lines$Kb2, Br.lines$Kb3)
Kr.K <- c(Kr.lines$Ka1, Kr.lines$Ka2, Kr.lines$Kb1, Kr.lines$Kb2, Kr.lines$Kb3)
Rb.K <- c(Rb.lines$Ka1, Rb.lines$Ka2, Rb.lines$Kb1, Rb.lines$Kb2, Rb.lines$Kb3)
Sr.K <- c(Sr.lines$Ka1, Sr.lines$Ka2, Sr.lines$Kb1, Sr.lines$Kb2, Sr.lines$Kb3)
Y.K <- c(Y.lines$Ka1, Y.lines$Ka2, Y.lines$Kb1, Y.lines$Kb2, Y.lines$Kb3)
Zr.K <- c(Zr.lines$Ka1, Zr.lines$Ka2, Zr.lines$Kb1, Zr.lines$Kb2, Zr.lines$Kb3)
Nb.K <- c(Nb.lines$Ka1, Nb.lines$Ka2, Nb.lines$Kb1, Nb.lines$Kb2, Nb.lines$Kb3)
Mo.K <- c(Mo.lines$Ka1, Mo.lines$Ka2, Mo.lines$Kb1, Mo.lines$Kb2, Mo.lines$Kb3)
Tc.K <- c(Tc.lines$Ka1, Tc.lines$Ka2, Tc.lines$Kb1, Tc.lines$Kb2, Tc.lines$Kb3)
Ru.K <- c(Ru.lines$Ka1, Ru.lines$Ka2, Ru.lines$Kb1, Ru.lines$Kb2, Ru.lines$Kb3)
Rh.K <- c(Rh.lines$Ka1, Rh.lines$Ka2, Rh.lines$Kb1, Rh.lines$Kb2, Rh.lines$Kb3)
Pd.K <- c(Pd.lines$Ka1, Pd.lines$Ka2, Pd.lines$Kb1, Pd.lines$Kb2, Pd.lines$Kb3)
Ag.K <- c(Ag.lines$Ka1, Ag.lines$Ka2, Ag.lines$Kb1, Ag.lines$Kb2, Ag.lines$Kb3)
Cd.K <- c(Cd.lines$Ka1, Cd.lines$Ka2, Cd.lines$Kb1, Cd.lines$Kb2, Cd.lines$Kb3)
In.K <- c(In.lines$Ka1, In.lines$Ka2, In.lines$Kb1, In.lines$Kb2, In.lines$Kb3)
Sn.K <- c(Sn.lines$Ka1, Sn.lines$Ka2, Sn.lines$Kb1, Sn.lines$Kb2, Sn.lines$Kb3)
Sb.K <- c(Sb.lines$Ka1, Sb.lines$Ka2, Sb.lines$Kb1, Sb.lines$Kb2, Sb.lines$Kb3)
Te.K <- c(Te.lines$Ka1, Te.lines$Ka2, Te.lines$Kb1, Te.lines$Kb2, Te.lines$Kb3)
I.K <- c(I.lines$Ka1, I.lines$Ka2, I.lines$Kb1, I.lines$Kb2, I.lines$Kb3)
Xe.K <- c(Xe.lines$Ka1, Xe.lines$Ka2, Xe.lines$Kb1, Xe.lines$Kb2, Xe.lines$Kb3)
Cs.K <- c(Cs.lines$Ka1, Cs.lines$Ka2, Cs.lines$Kb1, Cs.lines$Kb2, Cs.lines$Kb3)
Ba.K <- c(Ba.lines$Ka1, Ba.lines$Ka2, Ba.lines$Kb1, Ba.lines$Kb2, Ba.lines$Kb3)
La.K <- c(La.lines$Ka1, La.lines$Ka2, La.lines$Kb1, La.lines$Kb2, La.lines$Kb3)
Ce.K <- c(Ce.lines$Ka1, Ce.lines$Ka2, Ce.lines$Kb1, Ce.lines$Kb2, Ce.lines$Kb3)
Pr.K <- c(Pr.lines$Ka1, Pr.lines$Ka2, Pr.lines$Kb1, Pr.lines$Kb2, Pr.lines$Kb3)
Nd.K <- c(Nd.lines$Ka1, Nd.lines$Ka2, Nd.lines$Kb1, Nd.lines$Kb2, Nd.lines$Kb3)
Pm.K <- c(Pm.lines$Ka1, Pm.lines$Ka2, Pm.lines$Kb1, Pm.lines$Kb2, Pm.lines$Kb3)
Sm.K <- c(Sm.lines$Ka1, Sm.lines$Ka2, Sm.lines$Kb1, Sm.lines$Kb2, Sm.lines$Kb3)
Eu.K <- c(Eu.lines$Ka1, Eu.lines$Ka2, Eu.lines$Kb1, Eu.lines$Kb2, Eu.lines$Kb3)
Gd.K <- c(Gd.lines$Ka1, Gd.lines$Ka2, Gd.lines$Kb1, Gd.lines$Kb2, Gd.lines$Kb3)
Tb.K <- c(Tb.lines$Ka1, Tb.lines$Ka2, Tb.lines$Kb1, Tb.lines$Kb2, Tb.lines$Kb3)
Dy.K <- c(Dy.lines$Ka1, Dy.lines$Ka2, Dy.lines$Kb1, Dy.lines$Kb2, Dy.lines$Kb3)
Ho.K <- c(Ho.lines$Ka1, Ho.lines$Ka2, Ho.lines$Kb1, Ho.lines$Kb2, Ho.lines$Kb3)
Er.K <- c(Er.lines$Ka1, Er.lines$Ka2, Er.lines$Kb1, Er.lines$Kb2, Er.lines$Kb3)
Tm.K <- c(Tm.lines$Ka1, Tm.lines$Ka2, Tm.lines$Kb1, Tm.lines$Kb2, Tm.lines$Kb3)
Yb.K <- c(Yb.lines$Ka1, Yb.lines$Ka2, Yb.lines$Kb1, Yb.lines$Kb2, Yb.lines$Kb3)
Lu.K <- c(Lu.lines$Ka1, Lu.lines$Ka2, Lu.lines$Kb1, Lu.lines$Kb2, Lu.lines$Kb3)
Hf.K <- c(Hf.lines$Ka1, Hf.lines$Ka2, Hf.lines$Kb1, Hf.lines$Kb2, Hf.lines$Kb3)
Ta.K <- c(Ta.lines$Ka1, Ta.lines$Ka2, Ta.lines$Kb1, Ta.lines$Kb2, Ta.lines$Kb3)
W.K <- c(W.lines$Ka1, W.lines$Ka2, W.lines$Kb1, W.lines$Kb2, W.lines$Kb3)
Re.K <- c(Re.lines$Ka1, Re.lines$Ka2, Re.lines$Kb1, Re.lines$Kb2, Re.lines$Kb3)
Os.K <- c(Os.lines$Ka1, Os.lines$Ka2, Os.lines$Kb1, Os.lines$Kb2, Os.lines$Kb3)
Ir.K <- c(Ir.lines$Ka1, Ir.lines$Ka2, Ir.lines$Kb1, Ir.lines$Kb2, Ir.lines$Kb3)
Pt.K <- c(Pt.lines$Ka1, Pt.lines$Ka2, Pt.lines$Kb1, Pt.lines$Kb2, Pt.lines$Kb3)
Au.K <- c(Au.lines$Ka1, Au.lines$Ka2, Au.lines$Kb1, Au.lines$Kb2, Au.lines$Kb3)
Hg.K <- c(Hg.lines$Ka1, Hg.lines$Ka2, Hg.lines$Kb1, Hg.lines$Kb2, Hg.lines$Kb3)
Tl.K <- c(Tl.lines$Ka1, Tl.lines$Ka2, Tl.lines$Kb1, Tl.lines$Kb2, Tl.lines$Kb3)
Pb.K <- c(Pb.lines$Ka1, Pb.lines$Ka2, Pb.lines$Kb1, Pb.lines$Kb2, Pb.lines$Kb3)
Bi.K <- c(Bi.lines$Ka1, Bi.lines$Ka2, Bi.lines$Kb1, Bi.lines$Kb2, Bi.lines$Kb3)
Po.K <- c(Po.lines$Ka1, Po.lines$Ka2, Po.lines$Kb1, Po.lines$Kb2, Po.lines$Kb3)
At.K <- c(At.lines$Ka1, At.lines$Ka2, At.lines$Kb1, At.lines$Kb2, At.lines$Kb3)
Rn.K <- c(Rn.lines$Ka1, Rn.lines$Ka2, Rn.lines$Kb1, Rn.lines$Kb2, Rn.lines$Kb3)
Fr.K <- c(Fr.lines$Ka1, Fr.lines$Ka2, Fr.lines$Kb1, Fr.lines$Kb2, Fr.lines$Kb3)
Ra.K <- c(Ra.lines$Ka1, Ra.lines$Ka2, Ra.lines$Kb1, Ra.lines$Kb2, Ra.lines$Kb3)
Ac.K <- c(Ac.lines$Ka1, Ac.lines$Ka2, Ac.lines$Kb1, Ac.lines$Kb2, Ac.lines$Kb3)
Th.K <- c(Th.lines$Ka1, Th.lines$Ka2, Th.lines$Kb1, Th.lines$Kb2, Th.lines$Kb3)
Pa.K <- c(Pa.lines$Ka1, Pa.lines$Ka2, Pa.lines$Kb1, Pa.lines$Kb2, Pa.lines$Kb3)
U.K <- c(U.lines$Ka1, U.lines$Ka2, U.lines$Kb1, U.lines$Kb2, U.lines$Kb3)



H.L <- c(H.lines$La1, H.lines$La2, H.lines$Lb1, H.lines$Lb2, H.lines$Lb3,  H.lines$Lb4, H.lines$Lg1, H.lines$Lg2, H.lines$Lg3, H.lines$Ll, H.lines$Lb4, H.lines$Lg1, H.lines$Lg2, H.lines$Lg3, H.lines$Ll)
He.L <- c(He.lines$La1, He.lines$La2, He.lines$Lb1, He.lines$Lb2, He.lines$Lb3,  He.lines$Lb4, He.lines$Lg1, He.lines$Lg2, He.lines$Lg3, He.lines$Ll)
Li.L <- c(Li.lines$La1, Li.lines$La2, Li.lines$Lb1, Li.lines$Lb2, Li.lines$Lb3,  Li.lines$Lb4, Li.lines$Lg1, Li.lines$Lg2, Li.lines$Lg3, Li.lines$Ll)
Be.L <- c(Be.lines$La1, Be.lines$La2, Be.lines$Lb1, Be.lines$Lb2, Be.lines$Lb3,  Be.lines$Lb4, Be.lines$Lg1, Be.lines$Lg2, Be.lines$Lg3, Be.lines$Ll)
B.L <- c(B.lines$La1, B.lines$La2, B.lines$Lb1, B.lines$Lb2, B.lines$Lb3,  B.lines$Lb4, B.lines$Lg1, B.lines$Lg2, B.lines$Lg3, B.lines$Ll)
C.L <- c(C.lines$La1, C.lines$La2, C.lines$Lb1, C.lines$Lb2, C.lines$Lb3,  C.lines$Lb4, C.lines$Lg1, C.lines$Lg2, C.lines$Lg3, C.lines$Ll)
N.L <- c(N.lines$La1, N.lines$La2, N.lines$Lb1, N.lines$Lb2, N.lines$Lb3,  N.lines$Lb4, N.lines$Lg1, N.lines$Lg2, N.lines$Lg3, N.lines$Ll)
O.L <- c(O.lines$La1, O.lines$La2, O.lines$Lb1, O.lines$Lb2, O.lines$Lb3,  O.lines$Lb4, O.lines$Lg1, O.lines$Lg2, O.lines$Lg3, O.lines$Ll)
F.L <- c(F.lines$La1, F.lines$La2, F.lines$Lb1, F.lines$Lb2, F.lines$Lb3,  F.lines$Lb4, F.lines$Lg1, F.lines$Lg2, F.lines$Lg3, F.lines$Ll)
Ne.L <- c(Ne.lines$La1, Ne.lines$La2, Ne.lines$Lb1, Ne.lines$Lb2, Ne.lines$Lb3,  Ne.lines$Lb4, Ne.lines$Lg1, Ne.lines$Lg2, Ne.lines$Lg3, Ne.lines$Ll)
Na.L <- c(Na.lines$La1, Na.lines$La2, Na.lines$Lb1, Na.lines$Lb2, Na.lines$Lb3,  Na.lines$Lb4, Na.lines$Lg1, Na.lines$Lg2, Na.lines$Lg3, Na.lines$Ll)
Mg.L <- c(Mg.lines$La1, Mg.lines$La2, Mg.lines$Lb1, Mg.lines$Lb2, Mg.lines$Lb3,  Mg.lines$Lb4, Mg.lines$Lg1, Mg.lines$Lg2, Mg.lines$Lg3, Mg.lines$Ll)
Al.L <- c(Al.lines$La1, Al.lines$La2, Al.lines$Lb1, Al.lines$Lb2, Al.lines$Lb3,  Al.lines$Lb4, Al.lines$Lg1, Al.lines$Lg2, Al.lines$Lg3, Al.lines$Ll)
Si.L <- c(Si.lines$La1, Si.lines$La2, Si.lines$Lb1, Si.lines$Lb2, Si.lines$Lb3,  Si.lines$Lb4, Si.lines$Lg1, Si.lines$Lg2, Si.lines$Lg3, Si.lines$Ll)
P.L <- c(P.lines$La1, P.lines$La2, P.lines$Lb1, P.lines$Lb2, P.lines$Lb3,  P.lines$Lb4, P.lines$Lg1, P.lines$Lg2, P.lines$Lg3, P.lines$Ll)
S.L <- c(S.lines$La1, S.lines$La2, S.lines$Lb1, S.lines$Lb2, S.lines$Lb3,  S.lines$Lb4, S.lines$Lg1, S.lines$Lg2, S.lines$Lg3, S.lines$Ll)
Cl.L <- c(Cl.lines$La1, Cl.lines$La2, Cl.lines$Lb1, Cl.lines$Lb2, Cl.lines$Lb3,  Cl.lines$Lb4, Cl.lines$Lg1, Cl.lines$Lg2, Cl.lines$Lg3, Cl.lines$Ll)
Ar.L <- c(Ar.lines$La1, Ar.lines$La2, Ar.lines$Lb1, Ar.lines$Lb2, Ar.lines$Lb3,  Ar.lines$Lb4, Ar.lines$Lg1, Ar.lines$Lg2, Ar.lines$Lg3, Ar.lines$Ll)
K.L <- c(K.lines$La1, K.lines$La2, K.lines$Lb1, K.lines$Lb2, K.lines$Lb3,  K.lines$Lb4, K.lines$Lg1, K.lines$Lg2, K.lines$Lg3, K.lines$Ll)
Ca.L <- c(Ca.lines$La1, Ca.lines$La2, Ca.lines$Lb1, Ca.lines$Lb2, Ca.lines$Lb3,  Ca.lines$Lb4, Ca.lines$Lg1, Ca.lines$Lg2, Ca.lines$Lg3, Ca.lines$Ll)
Sc.L <- c(Sc.lines$La1, Sc.lines$La2, Sc.lines$Lb1, Sc.lines$Lb2, Sc.lines$Lb3,  Sc.lines$Lb4, Sc.lines$Lg1, Sc.lines$Lg2, Sc.lines$Lg3, Sc.lines$Ll)
Ti.L <- c(Ti.lines$La1, Ti.lines$La2, Ti.lines$Lb1, Ti.lines$Lb2, Ti.lines$Lb3,  Ti.lines$Lb4, Ti.lines$Lg1, Ti.lines$Lg2, Ti.lines$Lg3, Ti.lines$Ll)
V.L <- c(V.lines$La1, V.lines$La2, V.lines$Lb1, V.lines$Lb2, V.lines$Lb3,  V.lines$Lb4, V.lines$Lg1, V.lines$Lg2, V.lines$Lg3, V.lines$Ll)
Cr.L <- c(Cr.lines$La1, Cr.lines$La2, Cr.lines$Lb1, Cr.lines$Lb2, Cr.lines$Lb3,  Cr.lines$Lb4, Cr.lines$Lg1, Cr.lines$Lg2, Cr.lines$Lg3, Cr.lines$Ll)
Mn.L <- c(Mn.lines$La1, Mn.lines$La2, Mn.lines$Lb1, Mn.lines$Lb2, Mn.lines$Lb3,  Mn.lines$Lb4, Mn.lines$Lg1, Mn.lines$Lg2, Mn.lines$Lg3, Mn.lines$Ll)
Fe.L <- c(Fe.lines$La1, Fe.lines$La2, Fe.lines$Lb1, Fe.lines$Lb2, Fe.lines$Lb3,  Fe.lines$Lb4, Fe.lines$Lg1, Fe.lines$Lg2, Fe.lines$Lg3, Fe.lines$Ll)
Co.L <- c(Co.lines$La1, Co.lines$La2, Co.lines$Lb1, Co.lines$Lb2, Co.lines$Lb3,  Co.lines$Lb4, Co.lines$Lg1, Co.lines$Lg2, Co.lines$Lg3, Co.lines$Ll)
Ni.L <- c(Ni.lines$La1, Ni.lines$La2, Ni.lines$Lb1, Ni.lines$Lb2, Ni.lines$Lb3,  Ni.lines$Lb4, Ni.lines$Lg1, Ni.lines$Lg2, Ni.lines$Lg3, Ni.lines$Ll)
Cu.L <- c(Cu.lines$La1, Cu.lines$La2, Cu.lines$Lb1, Cu.lines$Lb2, Cu.lines$Lb3,  Cu.lines$Lb4, Cu.lines$Lg1, Cu.lines$Lg2, Cu.lines$Lg3, Cu.lines$Ll)
Zn.L <- c(Zn.lines$La1, Zn.lines$La2, Zn.lines$Lb1, Zn.lines$Lb2, Zn.lines$Lb3,  Zn.lines$Lb4, Zn.lines$Lg1, Zn.lines$Lg2, Zn.lines$Lg3, Zn.lines$Ll)
Ga.L <- c(Ga.lines$La1, Ga.lines$La2, Ga.lines$Lb1, Ga.lines$Lb2, Ga.lines$Lb3,  Ga.lines$Lb4, Ga.lines$Lg1, Ga.lines$Lg2, Ga.lines$Lg3, Ga.lines$Ll)
Ge.L <- c(Ge.lines$La1, Ge.lines$La2, Ge.lines$Lb1, Ge.lines$Lb2, Ge.lines$Lb3,  Ge.lines$Lb4, Ge.lines$Lg1, Ge.lines$Lg2, Ge.lines$Lg3, Ge.lines$Ll)
As.L <- c(As.lines$La1, As.lines$La2, As.lines$Lb1, As.lines$Lb2, As.lines$Lb3,  As.lines$Lb4, As.lines$Lg1, As.lines$Lg2, As.lines$Lg3, As.lines$Ll)
Se.L <- c(Se.lines$La1, Se.lines$La2, Se.lines$Lb1, Se.lines$Lb2, Se.lines$Lb3,  Se.lines$Lb4, Se.lines$Lg1, Se.lines$Lg2, Se.lines$Lg3, Se.lines$Ll)
Br.L <- c(Br.lines$La1, Br.lines$La2, Br.lines$Lb1, Br.lines$Lb2, Br.lines$Lb3,  Br.lines$Lb4, Br.lines$Lg1, Br.lines$Lg2, Br.lines$Lg3, Br.lines$Ll)
Kr.L <- c(Kr.lines$La1, Kr.lines$La2, Kr.lines$Lb1, Kr.lines$Lb2, Kr.lines$Lb3,  Kr.lines$Lb4, Kr.lines$Lg1, Kr.lines$Lg2, Kr.lines$Lg3, Kr.lines$Ll)
Rb.L <- c(Rb.lines$La1, Rb.lines$La2, Rb.lines$Lb1, Rb.lines$Lb2, Rb.lines$Lb3,  Rb.lines$Lb4, Rb.lines$Lg1, Rb.lines$Lg2, Rb.lines$Lg3, Rb.lines$Ll)
Sr.L <- c(Sr.lines$La1, Sr.lines$La2, Sr.lines$Lb1, Sr.lines$Lb2, Sr.lines$Lb3,  Sr.lines$Lb4, Sr.lines$Lg1, Sr.lines$Lg2, Sr.lines$Lg3, Sr.lines$Ll)
Y.L <- c(Y.lines$La1, Y.lines$La2, Y.lines$Lb1, Y.lines$Lb2, Y.lines$Lb3,  Y.lines$Lb4, Y.lines$Lg1, Y.lines$Lg2, Y.lines$Lg3, Y.lines$Ll)
Zr.L <- c(Zr.lines$La1, Zr.lines$La2, Zr.lines$Lb1, Zr.lines$Lb2, Zr.lines$Lb3,  Zr.lines$Lb4, Zr.lines$Lg1, Zr.lines$Lg2, Zr.lines$Lg3, Zr.lines$Ll)
Nb.L <- c(Nb.lines$La1, Nb.lines$La2, Nb.lines$Lb1, Nb.lines$Lb2, Nb.lines$Lb3,  Nb.lines$Lb4, Nb.lines$Lg1, Nb.lines$Lg2, Nb.lines$Lg3, Nb.lines$Ll)
Mo.L <- c(Mo.lines$La1, Mo.lines$La2, Mo.lines$Lb1, Mo.lines$Lb2, Mo.lines$Lb3,  Mo.lines$Lb4, Mo.lines$Lg1, Mo.lines$Lg2, Mo.lines$Lg3, Mo.lines$Ll)
Tc.L <- c(Tc.lines$La1, Tc.lines$La2, Tc.lines$Lb1, Tc.lines$Lb2, Tc.lines$Lb3,  Tc.lines$Lb4, Tc.lines$Lg1, Tc.lines$Lg2, Tc.lines$Lg3, Tc.lines$Ll)
Ru.L <- c(Ru.lines$La1, Ru.lines$La2, Ru.lines$Lb1, Ru.lines$Lb2, Ru.lines$Lb3,  Ru.lines$Lb4, Ru.lines$Lg1, Ru.lines$Lg2, Ru.lines$Lg3, Ru.lines$Ll)
Rh.L <- c(Rh.lines$La1, Rh.lines$La2, Rh.lines$Lb1, Rh.lines$Lb2, Rh.lines$Lb3,  Rh.lines$Lb4, Rh.lines$Lg1, Rh.lines$Lg2, Rh.lines$Lg3, Rh.lines$Ll)
Pd.L <- c(Pd.lines$La1, Pd.lines$La2, Pd.lines$Lb1, Pd.lines$Lb2, Pd.lines$Lb3,  Pd.lines$Lb4, Pd.lines$Lg1, Pd.lines$Lg2, Pd.lines$Lg3, Pd.lines$Ll)
Ag.L <- c(Ag.lines$La1, Ag.lines$La2, Ag.lines$Lb1, Ag.lines$Lb2, Ag.lines$Lb3,  Ag.lines$Lb4, Ag.lines$Lg1, Ag.lines$Lg2, Ag.lines$Lg3, Ag.lines$Ll)
Cd.L <- c(Cd.lines$La1, Cd.lines$La2, Cd.lines$Lb1, Cd.lines$Lb2, Cd.lines$Lb3,  Cd.lines$Lb4, Cd.lines$Lg1, Cd.lines$Lg2, Cd.lines$Lg3, Cd.lines$Ll)
In.L <- c(In.lines$La1, In.lines$La2, In.lines$Lb1, In.lines$Lb2, In.lines$Lb3,  In.lines$Lb4, In.lines$Lg1, In.lines$Lg2, In.lines$Lg3, In.lines$Ll)
Sn.L <- c(Sn.lines$La1, Sn.lines$La2, Sn.lines$Lb1, Sn.lines$Lb2, Sn.lines$Lb3,  Sn.lines$Lb4, Sn.lines$Lg1, Sn.lines$Lg2, Sn.lines$Lg3, Sn.lines$Ll)
Sb.L <- c(Sb.lines$La1, Sb.lines$La2, Sb.lines$Lb1, Sb.lines$Lb2, Sb.lines$Lb3,  Sb.lines$Lb4, Sb.lines$Lg1, Sb.lines$Lg2, Sb.lines$Lg3, Sb.lines$Ll)
Te.L <- c(Te.lines$La1, Te.lines$La2, Te.lines$Lb1, Te.lines$Lb2, Te.lines$Lb3,  Te.lines$Lb4, Te.lines$Lg1, Te.lines$Lg2, Te.lines$Lg3, Te.lines$Ll)
I.L <- c(I.lines$La1, I.lines$La2, I.lines$Lb1, I.lines$Lb2, I.lines$Lb3,  I.lines$Lb4, I.lines$Lg1, I.lines$Lg2, I.lines$Lg3, I.lines$Ll)
Xe.L <- c(Xe.lines$La1, Xe.lines$La2, Xe.lines$Lb1, Xe.lines$Lb2, Xe.lines$Lb3,  Xe.lines$Lb4, Xe.lines$Lg1, Xe.lines$Lg2, Xe.lines$Lg3, Xe.lines$Ll)
Cs.L <- c(Cs.lines$La1, Cs.lines$La2, Cs.lines$Lb1, Cs.lines$Lb2, Cs.lines$Lb3,  Cs.lines$Lb4, Cs.lines$Lg1, Cs.lines$Lg2, Cs.lines$Lg3, Cs.lines$Ll)
Ba.L <- c(Ba.lines$La1, Ba.lines$La2, Ba.lines$Lb1, Ba.lines$Lb2, Ba.lines$Lb3,  Ba.lines$Lb4, Ba.lines$Lg1, Ba.lines$Lg2, Ba.lines$Lg3, Ba.lines$Ll)
La.L <- c(La.lines$La1, La.lines$La2, La.lines$Lb1, La.lines$Lb2, La.lines$Lb3,  La.lines$Lb4, La.lines$Lg1, La.lines$Lg2, La.lines$Lg3, La.lines$Ll)
Ce.L <- c(Ce.lines$La1, Ce.lines$La2, Ce.lines$Lb1, Ce.lines$Lb2, Ce.lines$Lb3,  Ce.lines$Lb4, Ce.lines$Lg1, Ce.lines$Lg2, Ce.lines$Lg3, Ce.lines$Ll)
Pr.L <- c(Pr.lines$La1, Pr.lines$La2, Pr.lines$Lb1, Pr.lines$Lb2, Pr.lines$Lb3,  Pr.lines$Lb4, Pr.lines$Lg1, Pr.lines$Lg2, Pr.lines$Lg3, Pr.lines$Ll)
Nd.L <- c(Nd.lines$La1, Nd.lines$La2, Nd.lines$Lb1, Nd.lines$Lb2, Nd.lines$Lb3,  Nd.lines$Lb4, Nd.lines$Lg1, Nd.lines$Lg2, Nd.lines$Lg3, Nd.lines$Ll)
Pm.L <- c(Pm.lines$La1, Pm.lines$La2, Pm.lines$Lb1, Pm.lines$Lb2, Pm.lines$Lb3,  Pm.lines$Lb4, Pm.lines$Lg1, Pm.lines$Lg2, Pm.lines$Lg3, Pm.lines$Ll)
Sm.L <- c(Sm.lines$La1, Sm.lines$La2, Sm.lines$Lb1, Sm.lines$Lb2, Sm.lines$Lb3,  Sm.lines$Lb4, Sm.lines$Lg1, Sm.lines$Lg2, Sm.lines$Lg3, Sm.lines$Ll)
Eu.L <- c(Eu.lines$La1, Eu.lines$La2, Eu.lines$Lb1, Eu.lines$Lb2, Eu.lines$Lb3,  Eu.lines$Lb4, Eu.lines$Lg1, Eu.lines$Lg2, Eu.lines$Lg3, Eu.lines$Ll)
Gd.L <- c(Gd.lines$La1, Gd.lines$La2, Gd.lines$Lb1, Gd.lines$Lb2, Gd.lines$Lb3,  Gd.lines$Lb4, Gd.lines$Lg1, Gd.lines$Lg2, Gd.lines$Lg3, Gd.lines$Ll)
Tb.L <- c(Tb.lines$La1, Tb.lines$La2, Tb.lines$Lb1, Tb.lines$Lb2, Tb.lines$Lb3,  Tb.lines$Lb4, Tb.lines$Lg1, Tb.lines$Lg2, Tb.lines$Lg3, Tb.lines$Ll)
Dy.L <- c(Dy.lines$La1, Dy.lines$La2, Dy.lines$Lb1, Dy.lines$Lb2, Dy.lines$Lb3,  Dy.lines$Lb4, Dy.lines$Lg1, Dy.lines$Lg2, Dy.lines$Lg3, Dy.lines$Ll)
Ho.L <- c(Ho.lines$La1, Ho.lines$La2, Ho.lines$Lb1, Ho.lines$Lb2, Ho.lines$Lb3,  Ho.lines$Lb4, Ho.lines$Lg1, Ho.lines$Lg2, Ho.lines$Lg3, Ho.lines$Ll)
Er.L <- c(Er.lines$La1, Er.lines$La2, Er.lines$Lb1, Er.lines$Lb2, Er.lines$Lb3,  Er.lines$Lb4, Er.lines$Lg1, Er.lines$Lg2, Er.lines$Lg3, Er.lines$Ll)
Tm.L <- c(Tm.lines$La1, Tm.lines$La2, Tm.lines$Lb1, Tm.lines$Lb2, Tm.lines$Lb3,  Tm.lines$Lb4, Tm.lines$Lg1, Tm.lines$Lg2, Tm.lines$Lg3, Tm.lines$Ll)
Yb.L <- c(Yb.lines$La1, Yb.lines$La2, Yb.lines$Lb1, Yb.lines$Lb2, Yb.lines$Lb3,  Yb.lines$Lb4, Yb.lines$Lg1, Yb.lines$Lg2, Yb.lines$Lg3, Yb.lines$Ll)
Lu.L <- c(Lu.lines$La1, Lu.lines$La2, Lu.lines$Lb1, Lu.lines$Lb2, Lu.lines$Lb3,  Lu.lines$Lb4, Lu.lines$Lg1, Lu.lines$Lg2, Lu.lines$Lg3, Lu.lines$Ll)
Hf.L <- c(Hf.lines$La1, Hf.lines$La2, Hf.lines$Lb1, Hf.lines$Lb2, Hf.lines$Lb3,  Hf.lines$Lb4, Hf.lines$Lg1, Hf.lines$Lg2, Hf.lines$Lg3, Hf.lines$Ll)
Ta.L <- c(Ta.lines$La1, Ta.lines$La2, Ta.lines$Lb1, Ta.lines$Lb2, Ta.lines$Lb3,  Ta.lines$Lb4, Ta.lines$Lg1, Ta.lines$Lg2, Ta.lines$Lg3, Ta.lines$Ll)
W.L <- c(W.lines$La1, W.lines$La2, W.lines$Lb1, W.lines$Lb2, W.lines$Lb3,  W.lines$Lb4, W.lines$Lg1, W.lines$Lg2, W.lines$Lg3, W.lines$Ll)
Re.L <- c(Re.lines$La1, Re.lines$La2, Re.lines$Lb1, Re.lines$Lb2, Re.lines$Lb3,  Re.lines$Lb4, Re.lines$Lg1, Re.lines$Lg2, Re.lines$Lg3, Re.lines$Ll)
Os.L <- c(Os.lines$La1, Os.lines$La2, Os.lines$Lb1, Os.lines$Lb2, Os.lines$Lb3,  Os.lines$Lb4, Os.lines$Lg1, Os.lines$Lg2, Os.lines$Lg3, Os.lines$Ll)
Ir.L <- c(Ir.lines$La1, Ir.lines$La2, Ir.lines$Lb1, Ir.lines$Lb2, Ir.lines$Lb3,  Ir.lines$Lb4, Ir.lines$Lg1, Ir.lines$Lg2, Ir.lines$Lg3, Ir.lines$Ll)
Pt.L <- c(Pt.lines$La1, Pt.lines$La2, Pt.lines$Lb1, Pt.lines$Lb2, Pt.lines$Lb3,  Pt.lines$Lb4, Pt.lines$Lg1, Pt.lines$Lg2, Pt.lines$Lg3, Pt.lines$Ll)
Au.L <- c(Au.lines$La1, Au.lines$La2, Au.lines$Lb1, Au.lines$Lb2, Au.lines$Lb3,  Au.lines$Lb4, Au.lines$Lg1, Au.lines$Lg2, Au.lines$Lg3, Au.lines$Ll)
Hg.L <- c(Hg.lines$La1, Hg.lines$La2, Hg.lines$Lb1, Hg.lines$Lb2, Hg.lines$Lb3,  Hg.lines$Lb4, Hg.lines$Lg1, Hg.lines$Lg2, Hg.lines$Lg3, Hg.lines$Ll)
Tl.L <- c(Tl.lines$La1, Tl.lines$La2, Tl.lines$Lb1, Tl.lines$Lb2, Tl.lines$Lb3,  Tl.lines$Lb4, Tl.lines$Lg1, Tl.lines$Lg2, Tl.lines$Lg3, Tl.lines$Ll)
Pb.L <- c(Pb.lines$La1, Pb.lines$La2, Pb.lines$Lb1, Pb.lines$Lb2, Pb.lines$Lb3,  Pb.lines$Lb4, Pb.lines$Lg1, Pb.lines$Lg2, Pb.lines$Lg3, Pb.lines$Ll)
Bi.L <- c(Bi.lines$La1, Bi.lines$La2, Bi.lines$Lb1, Bi.lines$Lb2, Bi.lines$Lb3,  Bi.lines$Lb4, Bi.lines$Lg1, Bi.lines$Lg2, Bi.lines$Lg3, Bi.lines$Ll)
Po.L <- c(Po.lines$La1, Po.lines$La2, Po.lines$Lb1, Po.lines$Lb2, Po.lines$Lb3,  Po.lines$Lb4, Po.lines$Lg1,  Po.lines$Lg2, Po.lines$Lg3, Po.lines$Ll)
At.L <- c(At.lines$La1, At.lines$La2, At.lines$Lb1, At.lines$Lb2, At.lines$Lb3,  At.lines$Lb4, At.lines$Lg1, At.lines$Lg2, At.lines$Lg3, At.lines$Ll)
Rn.L <- c(Rn.lines$La1, Rn.lines$La2, Rn.lines$Lb1, Rn.lines$Lb2, Rn.lines$Lb3,  Rn.lines$Lb4, Rn.lines$Lg1, Rn.lines$Lg2, Rn.lines$Lg3, Rn.lines$Ll)
Fr.L <- c(Fr.lines$La1, Fr.lines$La2, Fr.lines$Lb1, Fr.lines$Lb2, Fr.lines$Lb3,  Fr.lines$Lb4, Fr.lines$Lg1, Fr.lines$Lg2, Fr.lines$Lg3, Fr.lines$Ll)
Ra.L <- c(Ra.lines$La1, Ra.lines$La2, Ra.lines$Lb1, Ra.lines$Lb2, Ra.lines$Lb3,  Ra.lines$Lb4, Ra.lines$Lg1, Ra.lines$Lg2, Ra.lines$Lg3, Ra.lines$Ll)
Ac.L <- c(Ac.lines$La1, Ac.lines$La2, Ac.lines$Lb1, Ac.lines$Lb2, Ac.lines$Lb3,  Ac.lines$Lb4, Ac.lines$Lg1, Ac.lines$Lg2, Ac.lines$Lg3, Ac.lines$Ll)
Th.L <- c(Th.lines$La1, Th.lines$La2, Th.lines$Lb1, Th.lines$Lb2, Th.lines$Lb3,  Th.lines$Lb4, Th.lines$Lg1, Th.lines$Lg2, Th.lines$Lg3, Th.lines$Ll)
Pa.L <- c(Pa.lines$La1, Pa.lines$La2, Pa.lines$Lb1, Pa.lines$Lb2, Pa.lines$Lb3,  Pa.lines$Lb4, Pa.lines$Lg1, Pa.lines$Lg2, Pa.lines$Lg3, Pa.lines$Ll)
U.L <- c(U.lines$La1, U.lines$La2, U.lines$Lb1, U.lines$Lb2, U.lines$Lb3,  U.lines$Lb4, U.lines$Lg1, U.lines$Lg2, U.lines$Lg3, U.lines$Ll)

K.intensity <- c(56, 29, 9, 2, 5)
L.intensity <- c(68, 8, 78, 17, 40, 34, 17, 11, 15, 4)
Intensity <- c(K.intensity, L.intensity)

H.table <- data.frame(as.vector(t(H.lines)), Intensity)
colnames(H.table) <- c("Line", "Intensity")

He.table <- data.frame(as.vector(t(He.lines)), Intensity)
colnames(He.table) <- c("Line", "Intensity")

Li.table <- data.frame(as.vector(t(Li.lines)), Intensity)
colnames(Li.table) <- c("Line", "Intensity")

Be.table <- data.frame(as.vector(t(Be.lines)), Intensity)
colnames(Be.table) <- c("Line", "Intensity")

B.table <- data.frame(as.vector(t(B.lines)), Intensity)
colnames(B.table) <- c("Line", "Intensity")

C.table <- data.frame(as.vector(t(C.lines)), Intensity)
colnames(C.table) <- c("Line", "Intensity")

N.table <- data.frame(as.vector(t(N.lines)), Intensity)
colnames(N.table) <- c("Line", "Intensity")

O.table <- data.frame(as.vector(t(O.lines)), Intensity)
colnames(O.table) <- c("Line", "Intensity")

F.table <- data.frame(as.vector(t(F.lines)), Intensity)
colnames(F.table) <- c("Line", "Intensity")

Ne.table <- data.frame(as.vector(t(Ne.lines)), Intensity)
colnames(Ne.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity)
colnames(Na.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity)
colnames(Na.table) <- c("Line", "Intensity")

Mg.table <- data.frame(as.vector(t(Mg.lines)), Intensity)
colnames(Mg.table) <- c("Line", "Intensity")

Al.table <- data.frame(as.vector(t(Al.lines)), Intensity)
colnames(Al.table) <- c("Line", "Intensity")

Si.table <- data.frame(as.vector(t(Si.lines)), Intensity)
colnames(Si.table) <- c("Line", "Intensity")

P.table <- data.frame(as.vector(t(P.lines)), Intensity)
colnames(P.table) <- c("Line", "Intensity")

S.table <- data.frame(as.vector(t(S.lines)), Intensity)
colnames(S.table) <- c("Line", "Intensity")

Cl.table <- data.frame(as.vector(t(Cl.lines)), Intensity)
colnames(Cl.table) <- c("Line", "Intensity")

Ar.table <- data.frame(as.vector(t(Ar.lines)), Intensity)
colnames(Ar.table) <- c("Line", "Intensity")

K.table <- data.frame(as.vector(t(K.lines)), Intensity)
colnames(K.table) <- c("Line", "Intensity")

Ca.table <- data.frame(as.vector(t(Ca.lines)), Intensity)
colnames(Ca.table) <- c("Line", "Intensity")

Sc.table <- data.frame(as.vector(t(Sc.lines)), Intensity)
colnames(Sc.table) <- c("Line", "Intensity")

Ti.table <- data.frame(as.vector(t(Ti.lines)), Intensity)
colnames(Ti.table) <- c("Line", "Intensity")

V.table <- data.frame(as.vector(t(V.lines)), Intensity)
colnames(V.table) <- c("Line", "Intensity")

Cr.table <- data.frame(as.vector(t(Cr.lines)), Intensity)
colnames(Cr.table) <- c("Line", "Intensity")

Mn.table <- data.frame(as.vector(t(Mn.lines)), Intensity)
colnames(Mn.table) <- c("Line", "Intensity")

Fe.table <- data.frame(as.vector(t(Fe.lines)), Intensity)
colnames(Fe.table) <- c("Line", "Intensity")

Co.table <- data.frame(as.vector(t(Co.lines)), Intensity)
colnames(Co.table) <- c("Line", "Intensity")

Ni.table <- data.frame(as.vector(t(Ni.lines)), Intensity)
colnames(Ni.table) <- c("Line", "Intensity")

Cu.table <- data.frame(as.vector(t(Cu.lines)), Intensity)
colnames(Cu.table) <- c("Line", "Intensity")

Zn.table <- data.frame(as.vector(t(Zn.lines)), Intensity)
colnames(Zn.table) <- c("Line", "Intensity")

Ga.table <- data.frame(as.vector(t(Ga.lines)), Intensity)
colnames(Ga.table) <- c("Line", "Intensity")

Ge.table <- data.frame(as.vector(t(Ge.lines)), Intensity)
colnames(Ge.table) <- c("Line", "Intensity")

As.table <- data.frame(as.vector(t(As.lines)), Intensity)
colnames(As.table) <- c("Line", "Intensity")

Se.table <- data.frame(as.vector(t(Se.lines)), Intensity)
colnames(Se.table) <- c("Line", "Intensity")

Br.table <- data.frame(as.vector(t(Br.lines)), Intensity)
colnames(Br.table) <- c("Line", "Intensity")

Kr.table <- data.frame(as.vector(t(Kr.lines)), Intensity)
colnames(Kr.table) <- c("Line", "Intensity")

Rb.table <- data.frame(as.vector(t(Rb.lines)), Intensity)
colnames(Rb.table) <- c("Line", "Intensity")

Sr.table <- data.frame(as.vector(t(Sr.lines)), Intensity)
colnames(Sr.table) <- c("Line", "Intensity")

Y.table <- data.frame(as.vector(t(Y.lines)), Intensity)
colnames(Y.table) <- c("Line", "Intensity")

Zr.table <- data.frame(as.vector(t(Zr.lines)), Intensity)
colnames(Zr.table) <- c("Line", "Intensity")

Nb.table <- data.frame(as.vector(t(Nb.lines)), Intensity)
colnames(Nb.table) <- c("Line", "Intensity")

Mo.table <- data.frame(as.vector(t(Mo.lines)), Intensity)
colnames(Mo.table) <- c("Line", "Intensity")

Tc.table <- data.frame(as.vector(t(Tc.lines)), Intensity)
colnames(Tc.table) <- c("Line", "Intensity")

Ru.table <- data.frame(as.vector(t(Ru.lines)), Intensity)
colnames(Ru.table) <- c("Line", "Intensity")

Rh.table <- data.frame(as.vector(t(Rh.lines)), Intensity)
colnames(Rh.table) <- c("Line", "Intensity")

Pd.table <- data.frame(as.vector(t(Pd.lines)), Intensity)
colnames(Pd.table) <- c("Line", "Intensity")

In.table <- data.frame(as.vector(t(In.lines)), Intensity)
colnames(In.table) <- c("Line", "Intensity")

Sn.table <- data.frame(as.vector(t(Sn.lines)), Intensity)
colnames(Sn.table) <- c("Line", "Intensity")

Sb.table <- data.frame(as.vector(t(Sb.lines)), Intensity)
colnames(Sb.table) <- c("Line", "Intensity")

Te.table <- data.frame(as.vector(t(Te.lines)), Intensity)
colnames(Te.table) <- c("Line", "Intensity")

I.table <- data.frame(as.vector(t(I.lines)), Intensity)
colnames(I.table) <- c("Line", "Intensity")

Xe.table <- data.frame(as.vector(t(Xe.lines)), Intensity)
colnames(Xe.table) <- c("Line", "Intensity")

Cs.table <- data.frame(as.vector(t(Cs.lines)), Intensity)
colnames(Cs.table) <- c("Line", "Intensity")

Ba.table <- data.frame(as.vector(t(Ba.lines)), Intensity)
colnames(Ba.table) <- c("Line", "Intensity")

La.table <- data.frame(as.vector(t(La.lines)), Intensity)
colnames(La.table) <- c("Line", "Intensity")

Ce.table <- data.frame(as.vector(t(Ce.lines)), Intensity)
colnames(Ce.table) <- c("Line", "Intensity")

Pr.table <- data.frame(as.vector(t(Pr.lines)), Intensity)
colnames(Pr.table) <- c("Line", "Intensity")

Nd.table <- data.frame(as.vector(t(Nd.lines)), Intensity)
colnames(Nd.table) <- c("Line", "Intensity")

Pm.table <- data.frame(as.vector(t(Pm.lines)), Intensity)
colnames(Pm.table) <- c("Line", "Intensity")

Sm.table <- data.frame(as.vector(t(Sm.lines)), Intensity)
colnames(Sm.table) <- c("Line", "Intensity")

Eu.table <- data.frame(as.vector(t(Eu.lines)), Intensity)
colnames(Eu.table) <- c("Line", "Intensity")

Gd.table <- data.frame(as.vector(t(Gd.lines)), Intensity)
colnames(Gd.table) <- c("Line", "Intensity")

Tb.table <- data.frame(as.vector(t(Tb.lines)), Intensity)
colnames(Tb.table) <- c("Line", "Intensity")

Dy.table <- data.frame(as.vector(t(Dy.lines)), Intensity)
colnames(Dy.table) <- c("Line", "Intensity")

Ho.table <- data.frame(as.vector(t(Ho.lines)), Intensity)
colnames(Ho.table) <- c("Line", "Intensity")

Er.table <- data.frame(as.vector(t(Er.lines)), Intensity)
colnames(Er.table) <- c("Line", "Intensity")

Tm.table <- data.frame(as.vector(t(Tm.lines)), Intensity)
colnames(Tm.table) <- c("Line", "Intensity")

Yb.table <- data.frame(as.vector(t(Yb.lines)), Intensity)
colnames(Yb.table) <- c("Line", "Intensity")

Lu.table <- data.frame(as.vector(t(Lu.lines)), Intensity)
colnames(Lu.table) <- c("Line", "Intensity")

Hf.table <- data.frame(as.vector(t(Hf.lines)), Intensity)
colnames(Hf.table) <- c("Line", "Intensity")

Ta.table <- data.frame(as.vector(t(Ta.lines)), Intensity)
colnames(Ta.table) <- c("Line", "Intensity")

W.table <- data.frame(as.vector(t(W.lines)), Intensity)
colnames(W.table) <- c("Line", "Intensity")

Re.table <- data.frame(as.vector(t(Re.lines)), Intensity)
colnames(Re.table) <- c("Line", "Intensity")

Os.table <- data.frame(as.vector(t(Os.lines)), Intensity)
colnames(Os.table) <- c("Line", "Intensity")

Ir.table <- data.frame(as.vector(t(Ir.lines)), Intensity)
colnames(Ir.table) <- c("Line", "Intensity")

Pt.table <- data.frame(as.vector(t(Pt.lines)), Intensity)
colnames(Pt.table) <- c("Line", "Intensity")

Au.table <- data.frame(as.vector(t(Au.lines)), Intensity)
colnames(Au.table) <- c("Line", "Intensity")

Hg.table <- data.frame(as.vector(t(Hg.lines)), Intensity)
colnames(Hg.table) <- c("Line", "Intensity")

Tl.table <- data.frame(as.vector(t(Tl.lines)), Intensity)
colnames(Tl.table) <- c("Line", "Intensity")

Pb.table <- data.frame(as.vector(t(Pb.lines)), Intensity)
colnames(Pb.table) <- c("Line", "Intensity")

Bi.table <- data.frame(as.vector(t(Bi.lines)), Intensity)
colnames(Bi.table) <- c("Line", "Intensity")

Po.table <- data.frame(as.vector(t(Po.lines)), Intensity)
colnames(Pb.table) <- c("Line", "Intensity")

At.table <- data.frame(as.vector(t(At.lines)), Intensity)
colnames(At.table) <- c("Line", "Intensity")

Rn.table <- data.frame(as.vector(t(Rn.lines)), Intensity)
colnames(Rn.table) <- c("Line", "Intensity")

Fr.table <- data.frame(as.vector(t(Fr.lines)), Intensity)
colnames(Fr.table) <- c("Line", "Intensity")

Ra.table <- data.frame(as.vector(t(Ra.lines)), Intensity)
colnames(Ra.table) <- c("Line", "Intensity")

Ac.table <- data.frame(as.vector(t(Ac.lines)), Intensity)
colnames(Ac.table) <- c("Line", "Intensity")

Th.table <- data.frame(as.vector(t(Th.lines)), Intensity)
colnames(Th.table) <- c("Line", "Intensity")

Pa.table <- data.frame(as.vector(t(Pa.lines)), Intensity)
colnames(Pa.table) <- c("Line", "Intensity")

U.table <- data.frame(as.vector(t(U.lines)), Intensity)
colnames(U.table) <- c("Line", "Intensity")

Li.absorption <- read.csv(file="data/Absorption-Li.csv")
Be.absorption <- read.csv(file="data/Absorption-Be.csv")
B.absorption <- read.csv(file="data/Absorption-B.csv")
C.absorption <- read.csv(file="data/Absorption-C.csv")
N.absorption <- read.csv(file="data/Absorption-N.csv")
O.absorption <- read.csv(file="data/Absorption-O.csv")
F.absorption <- read.csv(file="data/Absorption-F.csv")
Ne.absorption <- read.csv(file="data/Absorption-Ne.csv")
Na.absorption <- read.csv(file="data/Absorption-Na.csv")
Mg.absorption <- read.csv(file="data/Absorption-Mg.csv")
Al.absorption <- read.csv(file="data/Absorption-Al.csv")
Si.absorption <- read.csv(file="data/Absorption-Si.csv")
P.absorption <- read.csv(file="data/Absorption-P.csv")
S.absorption <- read.csv(file="data/Absorption-S.csv")
K.absorption <- read.csv(file="data/Absorption-K.csv")
Ca.absorption <- read.csv(file="data/Absorption-Ca.csv")
Sc.absorption <- read.csv(file="data/Absorption-Sc.csv")
Ti.absorption <- read.csv(file="data/Absorption-Ti.csv")
Cr.absorption <- read.csv(file="data/Absorption-Cr.csv")
Mn.absorption <- read.csv(file="data/Absorption-Mn.csv")
Fe.absorption <- read.csv(file="data/Absorption-Fe.csv")
Co.absorption <- read.csv(file="data/Absorption-Co.csv")
Ni.absorption <- read.csv(file="data/Absorption-Ni.csv")
Cu.absorption <- read.csv(file="data/Absorption-Cu.csv")
Zn.absorption <- read.csv(file="data/Absorption-Zn.csv")
Ga.absorption <- read.csv(file="data/Absorption-Ga.csv")
Ge.absorption <- read.csv(file="data/Absorption-Ge.csv")
As.absorption <- read.csv(file="data/Absorption-As.csv")
Se.absorption <- read.csv(file="data/Absorption-Se.csv")
Br.absorption <- read.csv(file="data/Absorption-Br.csv")
Kr.absorption <- read.csv(file="data/Absorption-Kr.csv")
Rb.absorption <- read.csv(file="data/Absorption-Rb.csv")
Sr.absorption <- read.csv(file="data/Absorption-Sr.csv")
Y.absorption <- read.csv(file="data/Absorption-Y.csv")
Zr.absorption <- read.csv(file="data/Absorption-Zr.csv")
Nb.absorption <- read.csv(file="data/Absorption-Nb.csv")
Mo.absorption <- read.csv(file="data/Absorption-Mo.csv")
Tc.absorption <- read.csv(file="data/Absorption-Tc.csv")
Ru.absorption <- read.csv(file="data/Absorption-Ru.csv")
Rh.absorption <- read.csv(file="data/Absorption-Rh.csv")
Pd.absorption <- read.csv(file="data/Absorption-Pd.csv")
Ag.absorption <- read.csv(file="data/Absorption-Ag.csv")
Cd.absorption <- read.csv(file="data/Absorption-Cd.csv")
In.absorption <- read.csv(file="data/Absorption-In.csv")
Sn.absorption <- read.csv(file="data/Absorption-Sn.csv")
Sb.absorption <- read.csv(file="data/Absorption-Sb.csv")
Te.absorption <- read.csv(file="data/Absorption-Te.csv")
I.absorption <- read.csv(file="data/Absorption-I.csv")
Xe.absorption <- read.csv(file="data/Absorption-Xe.csv")
Cs.absorption <- read.csv(file="data/Absorption-Cs.csv")
Ba.absorption <- read.csv(file="data/Absorption-Ba.csv")
La.absorption <- read.csv(file="data/Absorption-La.csv")
Ce.absorption <- read.csv(file="data/Absorption-Ce.csv")
Pr.absorption <- read.csv(file="data/Absorption-Pr.csv")
Nd.absorption <- read.csv(file="data/Absorption-Nd.csv")
Pm.absorption <- read.csv(file="data/Absorption-Pm.csv")
Sm.absorption <- read.csv(file="data/Absorption-Sm.csv")
Eu.absorption <- read.csv(file="data/Absorption-Eu.csv")
Gd.absorption <- read.csv(file="data/Absorption-Gd.csv")
Tb.absorption <- read.csv(file="data/Absorption-Tb.csv")
Dy.absorption <- read.csv(file="data/Absorption-Dy.csv")
Ho.absorption <- read.csv(file="data/Absorption-Ho.csv")
Er.absorption <- read.csv(file="data/Absorption-Er.csv")
Tm.absorption <- read.csv(file="data/Absorption-Tm.csv")
Yb.absorption <- read.csv(file="data/Absorption-Yb.csv")
Lu.absorption <- read.csv(file="data/Absorption-Lu.csv")
Hf.absorption <- read.csv(file="data/Absorption-Hf.csv")
Ta.absorption <- read.csv(file="data/Absorption-Ta.csv")
W.absorption <- read.csv(file="data/Absorption-W.csv")
Re.absorption <- read.csv(file="data/Absorption-Re.csv")
Os.absorption <- read.csv(file="data/Absorption-Os.csv")
Ir.absorption <- read.csv(file="data/Absorption-Ir.csv")
Pt.absorption <- read.csv(file="data/Absorption-Pt.csv")
Au.absorption <- read.csv(file="data/Absorption-Au.csv")
Hg.absorption <- read.csv(file="data/Absorption-Hg.csv")
Tl.absorption <- read.csv(file="data/Absorption-Tl.csv")
Pb.absorption <- read.csv(file="data/Absorption-Pb.csv")
Bi.absorption <- read.csv(file="data/Absorption-Bi.csv")
Po.absorption <- read.csv(file="data/Absorption-Po.csv")
At.absorption <- read.csv(file="data/Absorption-At.csv")
Rn.absorption <- read.csv(file="data/Absorption-Rn.csv")
Fr.absorption <- read.csv(file="data/Absorption-Fr.csv")
Ra.absorption <- read.csv(file="data/Absorption-Ra.csv")
Ac.absorption <- read.csv(file="data/Absorption-Ac.csv")
Th.absorption <- read.csv(file="data/Absorption-Th.csv")
Pa.absorption <- read.csv(file="data/Absorption-Pa.csv")
U.absorption <- read.csv(file="data/Absorption-U.csv")


spectra.line.fn <- function(data) {
    Ne.Ka.cps <- subset(data$CPS, !(data$Energy < Ne.K[2]-0.02 | data$Energy > Ne.K[1]+0.02))
    Ne.file <- subset(data$Sample, !(data$Energy < Ne.K[2]-0.02 | data$Energy > Ne.K[1]+0.02))
    Ne.Ka.frame <- data.frame(is.0(Ne.Ka.cps, Ne.file))
    colnames(Ne.Ka.frame) <- c("Counts", "Sample")
    Ne.Ka.ag <- aggregate(list(Ne.Ka.frame$Counts), by=list(Ne.Ka.frame$Sample), FUN="sum")
    colnames(Ne.Ka.ag) <- c("Sample", "Ne K-alpha")
    
    Na.Ka.cps <- subset(data$CPS, !(data$Energy < Na.K[2]-0.02 | data$Energy > Na.K[1]+0.02))
    Na.file <- subset(data$Sample, !(data$Energy < Na.K[2]-0.02 | data$Energy > Na.K[1]+0.02))
    Na.Ka.frame <- data.frame(is.0(Na.Ka.cps, Na.file))
    colnames(Na.Ka.frame) <- c("Counts", "Sample")
    Na.Ka.ag <- aggregate(list(Na.Ka.frame$Counts), by=list(Na.Ka.frame$Sample), FUN="sum")
    colnames(Na.Ka.ag) <- c("Sample", "Na K-alpha")
    
    Mg.Ka.cps <- subset(data$CPS, !(data$Energy < Mg.K[2]-0.02 | data$Energy > Mg.K[1]+0.02))
    Mg.file <- subset(data$Sample, !(data$Energy < Mg.K[2]-0.02 | data$Energy > Mg.K[1]+0.02))
    Mg.Ka.frame <- data.frame(is.0(Mg.Ka.cps, Mg.file))
    colnames(Mg.Ka.frame) <- c("Counts", "Sample")
    Mg.Ka.ag <- aggregate(list(Mg.Ka.frame$Counts), by=list(Mg.Ka.frame$Sample), FUN="sum")
    colnames(Mg.Ka.ag) <- c("Sample", "Mg K-alpha")
    
    Al.Ka.cps <- subset(data$CPS, !(data$Energy < Al.K[2]-0.02 | data$Energy > Al.K[1]+0.02))
    Al.file <- subset(data$Sample, !(data$Energy < Al.K[2]-0.02 | data$Energy > Al.K[1]+0.02))
    Al.Ka.frame <- data.frame(is.0(Al.Ka.cps, Al.file))
    colnames(Al.Ka.frame) <- c("Counts", "Sample")
    Al.Ka.ag <- aggregate(list(Al.Ka.frame$Counts), by=list(Al.Ka.frame$Sample), FUN="sum")
    colnames(Al.Ka.ag) <- c("Sample", "Al K-alpha")
    
    Si.Ka.cps <- subset(data$CPS, !(data$Energy < Si.K[2]-0.02 | data$Energy > Si.K[1]+0.02))
    Si.file <- subset(data$Sample, !(data$Energy < Si.K[2]-0.02 | data$Energy > Si.K[1]+0.02))
    Si.Ka.frame <- data.frame(is.0(Si.Ka.cps, Si.file))
    colnames(Si.Ka.frame) <- c("Counts", "Sample")
    Si.Ka.ag <- aggregate(list(Si.Ka.frame$Counts), by=list(Si.Ka.frame$Sample), FUN="sum")
    colnames(Si.Ka.ag) <- c("Sample", "Si K-alpha")
    
    P.Ka.cps <- subset(data$CPS, !(data$Energy < P.K[2]-0.02 | data$Energy > P.K[1]+0.02))
    P.file <- subset(data$Sample, !(data$Energy < P.K[2]-0.02 | data$Energy > P.K[1]+0.02))
    P.Ka.frame <- data.frame(is.0(P.Ka.cps, P.file))
    colnames(P.Ka.frame) <- c("Counts", "Sample")
    P.Ka.ag <- aggregate(list(P.Ka.frame$Counts), by=list(P.Ka.frame$Sample), FUN="sum")
    colnames(P.Ka.ag) <- c("Sample", "P K-alpha")
    
    S.Ka.cps <- subset(data$CPS, !(data$Energy < S.K[2]-0.02 | data$Energy > S.K[1]+0.02))
    S.file <- subset(data$Sample, !(data$Energy < S.K[2]-0.02 | data$Energy > S.K[1]+0.02))
    S.Ka.frame <- data.frame(is.0(S.Ka.cps, S.file))
    colnames(S.Ka.frame) <- c("Counts", "Sample")
    S.Ka.ag <- aggregate(list(S.Ka.frame$Counts), by=list(S.Ka.frame$Sample), FUN="sum")
    colnames(S.Ka.ag) <- c("Sample", "S K-alpha")
    
    Cl.Ka.cps <- subset(data$CPS, !(data$Energy < Cl.K[2]-0.02 | data$Energy > Cl.K[1]+0.02))
    Cl.file <- subset(data$Sample, !(data$Energy < Cl.K[2]-0.02 | data$Energy > Cl.K[1]+0.02))
    Cl.Ka.frame <- data.frame(is.0(Cl.Ka.cps, Cl.file))
    colnames(Cl.Ka.frame) <- c("Counts", "Sample")
    Cl.Ka.ag <- aggregate(list(Cl.Ka.frame$Counts), by=list(Cl.Ka.frame$Sample), FUN="sum")
    colnames(Cl.Ka.ag) <- c("Sample", "Cl K-alpha")
    
    Ar.Ka.cps <- subset(data$CPS, !(data$Energy < Ar.K[2]-0.02 | data$Energy > Ar.K[1]+0.02))
    Ar.file <- subset(data$Sample, !(data$Energy < Ar.K[2]-0.02 | data$Energy > Ar.K[1]+0.02))
    Ar.Ka.frame <- data.frame(is.0(Ar.Ka.cps, Ar.file))
    colnames(Ar.Ka.frame) <- c("Counts", "Sample")
    Ar.Ka.ag <- aggregate(list(Ar.Ka.frame$Counts), by=list(Ar.Ka.frame$Sample), FUN="sum")
    colnames(Ar.Ka.ag) <- c("Sample", "Ar K-alpha")
    
    K.Ka.cps <- subset(data$CPS, !(data$Energy < K.K[2]-0.02 | data$Energy > K.K[1]+0.02))
    K.file <- subset(data$Sample, !(data$Energy < K.K[2]-0.02 | data$Energy > K.K[1]+0.02))
    K.Ka.frame <- data.frame(is.0(K.Ka.cps, K.file))
    colnames(K.Ka.frame) <- c("Counts", "Sample")
    K.Ka.ag <- aggregate(list(K.Ka.frame$Counts), by=list(K.Ka.frame$Sample), FUN="sum")
    colnames(K.Ka.ag) <- c("Sample", "K K-alpha")
    
    Ca.Ka.cps <- subset(data$CPS, !(data$Energy < Ca.K[2]-0.02 | data$Energy > Ca.K[1]+0.02))
    Ca.file <- subset(data$Sample, !(data$Energy < Ca.K[2]-0.02 | data$Energy > Ca.K[1]+0.02))
    Ca.Ka.frame <- data.frame(is.0(Ca.Ka.cps, Ca.file))
    colnames(Ca.Ka.frame) <- c("Counts", "Sample")
    Ca.Ka.ag <- aggregate(list(Ca.Ka.frame$Counts), by=list(Ca.Ka.frame$Sample), FUN="sum")
    colnames(Ca.Ka.ag) <- c("Sample", "Ca K-alpha")
    
    Sc.Ka.cps <- subset(data$CPS, !(data$Energy < Sc.K[2]-0.02 | data$Energy > Sc.K[1]+0.02))
    Sc.file <- subset(data$Sample, !(data$Energy < Sc.K[2]-0.02 | data$Energy > Sc.K[1]+0.02))
    Sc.Ka.frame <- data.frame(is.0(Sc.Ka.cps, Sc.file))
    colnames(Sc.Ka.frame) <- c("Counts", "Sample")
    Sc.Ka.ag <- aggregate(list(Sc.Ka.frame$Counts), by=list(Sc.Ka.frame$Sample), FUN="sum")
    colnames(Sc.Ka.ag) <- c("Sample", "Sc K-alpha")
    
    Ti.Ka.cps <- subset(data$CPS, !(data$Energy < Ti.K[2]-0.02 | data$Energy > Ti.K[1]+0.02))
    Ti.file <- subset(data$Sample, !(data$Energy < Ti.K[2]-0.02 | data$Energy > Ti.K[1]+0.02))
    Ti.Ka.frame <- data.frame(is.0(Ti.Ka.cps, Ti.file))
    colnames(Ti.Ka.frame) <- c("Counts", "Sample")
    Ti.Ka.ag <- aggregate(list(Ti.Ka.frame$Counts), by=list(Ti.Ka.frame$Sample), FUN="sum")
    colnames(Ti.Ka.ag) <- c("Sample", "Ti K-alpha")
    
    V.Ka.cps <- subset(data$CPS, !(data$Energy < V.K[2]-0.02 | data$Energy > V.K[1]+0.02))
    V.file <- subset(data$Sample, !(data$Energy < V.K[2]-0.02 | data$Energy > V.K[1]+0.02))
    V.Ka.frame <- data.frame(is.0(V.Ka.cps, V.file))
    colnames(V.Ka.frame) <- c("Counts", "Sample")
    V.Ka.ag <- aggregate(list(V.Ka.frame$Counts), by=list(V.Ka.frame$Sample), FUN="sum")
    colnames(V.Ka.ag) <- c("Sample", "V K-alpha")
    
    Cr.Ka.cps <- subset(data$CPS, !(data$Energy < Cr.K[2]-0.02 | data$Energy > Cr.K[1]+0.02))
    Cr.file <- subset(data$Sample, !(data$Energy < Cr.K[2]-0.02 | data$Energy > Cr.K[1]+0.02))
    Cr.Ka.frame <- data.frame(is.0(Cr.Ka.cps, Cr.file))
    colnames(Cr.Ka.frame) <- c("Counts", "Sample")
    Cr.Ka.ag <- aggregate(list(Cr.Ka.frame$Counts), by=list(Cr.Ka.frame$Sample), FUN="sum")
    colnames(Cr.Ka.ag) <- c("Sample", "Cr K-alpha")
    
    Mn.Ka.cps <- subset(data$CPS, !(data$Energy < Mn.K[2]-0.02 | data$Energy > Mn.K[1]+0.02))
    Mn.file <- subset(data$Sample, !(data$Energy < Mn.K[2]-0.02 | data$Energy > Mn.K[1]+0.02))
    Mn.Ka.frame <- data.frame(is.0(Mn.Ka.cps, Mn.file))
    colnames(Mn.Ka.frame) <- c("Counts", "Sample")
    Mn.Ka.ag <- aggregate(list(Mn.Ka.frame$Counts), by=list(Mn.Ka.frame$Sample), FUN="sum")
    colnames(Mn.Ka.ag) <- c("Sample", "Mn K-alpha")
    
    Fe.Ka.cps <- subset(data$CPS, !(data$Energy < Fe.K[2]-0.02 | data$Energy > Fe.K[1]+0.02))
    Fe.file <- subset(data$Sample, !(data$Energy < Fe.K[2]-0.02 | data$Energy > Fe.K[1]+0.02))
    Fe.Ka.frame <- data.frame(is.0(Fe.Ka.cps, Fe.file))
    colnames(Fe.Ka.frame) <- c("Counts", "Sample")
    Fe.Ka.ag <- aggregate(list(Fe.Ka.frame$Counts), by=list(Fe.Ka.frame$Sample), FUN="sum")
    colnames(Fe.Ka.ag) <- c("Sample", "Fe K-alpha")
    
    Co.Ka.cps <- subset(data$CPS, !(data$Energy < Co.K[2]-0.02 | data$Energy > Co.K[1]+0.02))
    Co.file <- subset(data$Sample, !(data$Energy < Co.K[2]-0.02 | data$Energy > Co.K[1]+0.02))
    Co.Ka.frame <- data.frame(is.0(Co.Ka.cps, Co.file))
    colnames(Co.Ka.frame) <- c("Counts", "Sample")
    Co.Ka.ag <- aggregate(list(Co.Ka.frame$Counts), by=list(Co.Ka.frame$Sample), FUN="sum")
    colnames(Co.Ka.ag) <- c("Sample", "Co K-alpha")
    
    Ni.Ka.cps <- subset(data$CPS, !(data$Energy < Ni.K[2]-0.02 | data$Energy > Ni.K[1]+0.02))
    Ni.file <- subset(data$Sample, !(data$Energy < Ni.K[2]-0.02 | data$Energy > Ni.K[1]+0.02))
    Ni.Ka.frame <- data.frame(is.0(Ni.Ka.cps, Ni.file))
    colnames(Ni.Ka.frame) <- c("Counts", "Sample")
    Ni.Ka.ag <- aggregate(list(Ni.Ka.frame$Counts), by=list(Ni.Ka.frame$Sample), FUN="sum")
    colnames(Ni.Ka.ag) <- c("Sample", "Ni K-alpha")
    
    Cu.Ka.cps <- subset(data$CPS, !(data$Energy < Cu.K[2]-0.02 | data$Energy > Cu.K[1]+0.02))
    Cu.file <- subset(data$Sample, !(data$Energy < Cu.K[2]-0.02 | data$Energy > Cu.K[1]+0.02))
    Cu.Ka.frame <- data.frame(is.0(Cu.Ka.cps, Cu.file))
    colnames(Cu.Ka.frame) <- c("Counts", "Sample")
    Cu.Ka.ag <- aggregate(list(Cu.Ka.frame$Counts), by=list(Cu.Ka.frame$Sample), FUN="sum")
    colnames(Cu.Ka.ag) <- c("Sample", "Cu K-alpha")
    
    Zn.Ka.cps <- subset(data$CPS, !(data$Energy < Zn.K[2]-0.02 | data$Energy > Zn.K[1]+0.02))
    Zn.file <- subset(data$Sample, !(data$Energy < Zn.K[2]-0.02 | data$Energy > Zn.K[1]+0.02))
    Zn.Ka.frame <- data.frame(is.0(Zn.Ka.cps, Zn.file))
    colnames(Zn.Ka.frame) <- c("Counts", "Sample")
    Zn.Ka.ag <- aggregate(list(Zn.Ka.frame$Counts), by=list(Zn.Ka.frame$Sample), FUN="sum")
    colnames(Zn.Ka.ag) <- c("Sample", "Zn K-alpha")
    
    Ga.Ka.cps <- subset(data$CPS, !(data$Energy < Ga.K[2]-0.02 | data$Energy > Ga.K[1]+0.02))
    Ga.file <- subset(data$Sample, !(data$Energy < Ga.K[2]-0.02 | data$Energy > Ga.K[1]+0.02))
    Ga.Ka.frame <- data.frame(is.0(Ga.Ka.cps, Ga.file))
    colnames(Ga.Ka.frame) <- c("Counts", "Sample")
    Ga.Ka.ag <- aggregate(list(Ga.Ka.frame$Counts), by=list(Ga.Ka.frame$Sample), FUN="sum")
    colnames(Ga.Ka.ag) <- c("Sample", "Ga K-alpha")
    
    Ge.Ka.cps <- subset(data$CPS, !(data$Energy < Ge.K[2]-0.02 | data$Energy > Ge.K[1]+0.02))
    Ge.file <- subset(data$Sample, !(data$Energy < Ge.K[2]-0.02 | data$Energy > Ge.K[1]+0.02))
    Ge.Ka.frame <- data.frame(is.0(Ge.Ka.cps, Ge.file))
    colnames(Ge.Ka.frame) <- c("Counts", "Sample")
    Ge.Ka.ag <- aggregate(list(Ge.Ka.frame$Counts), by=list(Ge.Ka.frame$Sample), FUN="sum")
    colnames(Ge.Ka.ag) <- c("Sample", "Ge K-alpha")
    
    As.Ka.cps <- subset(data$CPS, !(data$Energy < As.K[2]-0.02 | data$Energy > As.K[1]+0.02))
    As.file <- subset(data$Sample, !(data$Energy < As.K[2]-0.02 | data$Energy > As.K[1]+0.02))
    As.Ka.frame <- data.frame(is.0(As.Ka.cps, As.file))
    colnames(As.Ka.frame) <- c("Counts", "Sample")
    As.Ka.ag <- aggregate(list(As.Ka.frame$Counts), by=list(As.Ka.frame$Sample), FUN="sum")
    colnames(As.Ka.ag) <- c("Sample", "As K-alpha")
    
    Se.Ka.cps <- subset(data$CPS, !(data$Energy < Se.K[2]-0.02 | data$Energy > Se.K[1]+0.02))
    Se.file <- subset(data$Sample, !(data$Energy < Se.K[2]-0.02 | data$Energy > Se.K[1]+0.02))
    Se.Ka.frame <- data.frame(is.0(Se.Ka.cps, Se.file))
    colnames(Se.Ka.frame) <- c("Counts", "Sample")
    Se.Ka.ag <- aggregate(list(Se.Ka.frame$Counts), by=list(Se.Ka.frame$Sample), FUN="sum")
    colnames(Se.Ka.ag) <- c("Sample", "Se K-alpha")
    
    Br.Ka.cps <- subset(data$CPS, !(data$Energy < Br.K[2]-0.02 | data$Energy > Br.K[1]+0.02))
    Br.file <- subset(data$Sample, !(data$Energy < Br.K[2]-0.02 | data$Energy > Br.K[1]+0.02))
    Br.Ka.frame <- data.frame(is.0(Br.Ka.cps, Br.file))
    colnames(Br.Ka.frame) <- c("Counts", "Sample")
    Br.Ka.ag <- aggregate(list(Br.Ka.frame$Counts), by=list(Br.Ka.frame$Sample), FUN="sum")
    colnames(Br.Ka.ag) <- c("Sample", "Br K-alpha")
    
    Kr.Ka.cps <- subset(data$CPS, !(data$Energy < Kr.K[2]-0.02 | data$Energy > Kr.K[1]+0.02))
    Kr.file <- subset(data$Sample, !(data$Energy < Kr.K[2]-0.02 | data$Energy > Kr.K[1]+0.02))
    Kr.Ka.frame <- data.frame(is.0(Kr.Ka.cps, Kr.file))
    colnames(Kr.Ka.frame) <- c("Counts", "Sample")
    Kr.Ka.ag <- aggregate(list(Kr.Ka.frame$Counts), by=list(Kr.Ka.frame$Sample), FUN="sum")
    colnames(Kr.Ka.ag) <- c("Sample", "Kr K-alpha")
    
    Rb.Ka.cps <- subset(data$CPS, !(data$Energy < Rb.K[2]-0.02 | data$Energy > Rb.K[1]+0.02))
    Rb.file <- subset(data$Sample, !(data$Energy < Rb.K[2]-0.02 | data$Energy > Rb.K[1]+0.02))
    Rb.Ka.frame <- data.frame(is.0(Rb.Ka.cps, Rb.file))
    colnames(Rb.Ka.frame) <- c("Counts", "Sample")
    Rb.Ka.ag <- aggregate(list(Rb.Ka.frame$Counts), by=list(Rb.Ka.frame$Sample), FUN="sum")
    colnames(Rb.Ka.ag) <- c("Sample", "Rb K-alpha")
    
    Sr.Ka.cps <- subset(data$CPS, !(data$Energy < Sr.K[2]-0.02 | data$Energy > Sr.K[1]+0.02))
    Sr.file <- subset(data$Sample, !(data$Energy < Sr.K[2]-0.02 | data$Energy > Sr.K[1]+0.02))
    Sr.Ka.frame <- data.frame(is.0(Sr.Ka.cps, Sr.file))
    colnames(Sr.Ka.frame) <- c("Counts", "Sample")
    Sr.Ka.ag <- aggregate(list(Sr.Ka.frame$Counts), by=list(Sr.Ka.frame$Sample), FUN="sum")
    colnames(Sr.Ka.ag) <- c("Sample", "Sr K-alpha")
    
    Y.Ka.cps <- subset(data$CPS, !(data$Energy < Y.K[2]-0.02 | data$Energy > Y.K[1]+0.02))
    Y.file <- subset(data$Sample, !(data$Energy < Y.K[2]-0.02 | data$Energy > Y.K[1]+0.02))
    Y.Ka.frame <- data.frame(is.0(Y.Ka.cps, Y.file))
    colnames(Y.Ka.frame) <- c("Counts", "Sample")
    Y.Ka.ag <- aggregate(list(Y.Ka.frame$Counts), by=list(Y.Ka.frame$Sample), FUN="sum")
    colnames(Y.Ka.ag) <- c("Sample", "Y K-alpha")
    
    Zr.Ka.cps <- subset(data$CPS, !(data$Energy < Zr.K[2]-0.02 | data$Energy > Zr.K[1]+0.02))
    Zr.file <- subset(data$Sample, !(data$Energy < Zr.K[2]-0.02 | data$Energy > Zr.K[1]+0.02))
    Zr.Ka.frame <- data.frame(is.0(Zr.Ka.cps, Zr.file))
    colnames(Zr.Ka.frame) <- c("Counts", "Sample")
    Zr.Ka.ag <- aggregate(list(Zr.Ka.frame$Counts), by=list(Zr.Ka.frame$Sample), FUN="sum")
    colnames(Zr.Ka.ag) <- c("Sample", "Zr K-alpha")
    
    Nb.Ka.cps <- subset(data$CPS, !(data$Energy < Nb.K[2]-0.02 | data$Energy > Nb.K[1]+0.02))
    Nb.file <- subset(data$Sample, !(data$Energy < Nb.K[2]-0.02 | data$Energy > Nb.K[1]+0.02))
    Nb.Ka.frame <- data.frame(is.0(Nb.Ka.cps, Nb.file))
    colnames(Nb.Ka.frame) <- c("Counts", "Sample")
    Nb.Ka.ag <- aggregate(list(Nb.Ka.frame$Counts), by=list(Nb.Ka.frame$Sample), FUN="sum")
    colnames(Nb.Ka.ag) <- c("Sample", "Nb K-alpha")
    
    Mo.Ka.cps <- subset(data$CPS, !(data$Energy < Mo.K[2]-0.02 | data$Energy > Mo.K[1]+0.02))
    Mo.file <- subset(data$Sample, !(data$Energy < Mo.K[2]-0.02 | data$Energy > Mo.K[1]+0.02))
    Mo.Ka.frame <- data.frame(is.0(Mo.Ka.cps, Mo.file))
    colnames(Mo.Ka.frame) <- c("Counts", "Sample")
    Mo.Ka.ag <- aggregate(list(Mo.Ka.frame$Counts), by=list(Mo.Ka.frame$Sample), FUN="sum")
    colnames(Mo.Ka.ag) <- c("Sample", "Mo K-alpha")
    
    
    Mo.La.cps <- subset(data$CPS, !(data$Energy < Mo.L[2]-0.02 | data$Energy > Mo.L[1]+0.02))
    Mo.file <- subset(data$Sample, !(data$Energy < Mo.L[2]-0.02 | data$Energy > Mo.L[1]+0.02))
    Mo.La.frame <- is.0(Mo.La.cps,Mo.file)
    colnames(Mo.La.frame) <- c("Counts", "Sample")
    Mo.La.ag <- aggregate(list(Mo.La.frame$Counts), by=list(Mo.La.frame$Sample), FUN="sum")
    colnames(Mo.La.ag) <- c("Sample", "Mo L-alpha")

    
    Tc.Ka.cps <- subset(data$CPS, !(data$Energy < Tc.K[2]-0.02 | data$Energy > Tc.K[1]+0.02))
    Tc.file <- subset(data$Sample, !(data$Energy < Tc.K[2]-0.02 | data$Energy > Tc.K[1]+0.02))
    Tc.Ka.frame <- data.frame(is.0(Tc.Ka.cps, Tc.file))
    colnames(Tc.Ka.frame) <- c("Counts", "Sample")
    Tc.Ka.ag <- aggregate(list(Tc.Ka.frame$Counts), by=list(Tc.Ka.frame$Sample), FUN="sum")
    colnames(Tc.Ka.ag) <- c("Sample", "Tc K-alpha")
    
    Ru.Ka.cps <- subset(data$CPS, !(data$Energy < Ru.K[2]-0.02 | data$Energy > Ru.K[1]+0.02))
    Ru.file <- subset(data$Sample, !(data$Energy < Ru.K[2]-0.02 | data$Energy > Ru.K[1]+0.02))
    Ru.Ka.frame <- data.frame(is.0(Ru.Ka.cps, Ru.file))
    colnames(Ru.Ka.frame) <- c("Counts", "Sample")
    Ru.Ka.ag <- aggregate(list(Ru.Ka.frame$Counts), by=list(Ru.Ka.frame$Sample), FUN="sum")
    colnames(Ru.Ka.ag) <- c("Sample", "Ru K-alpha")
    
    Rh.Ka.cps <- subset(data$CPS, !(data$Energy < Rh.K[2]-0.02 | data$Energy > Rh.K[1]+0.02))
    Rh.file <- subset(data$Sample, !(data$Energy < Rh.K[2]-0.02 | data$Energy > Rh.K[1]+0.02))
    Rh.Ka.frame <- data.frame(is.0(Rh.Ka.cps, Rh.file))
    colnames(Rh.Ka.frame) <- c("Counts", "Sample")
    Rh.Ka.ag <- aggregate(list(Rh.Ka.frame$Counts), by=list(Rh.Ka.frame$Sample), FUN="sum")
    colnames(Rh.Ka.ag) <- c("Sample", "Rh K-alpha")
    
    Pd.Ka.cps <- subset(data$CPS, !(data$Energy < Pd.K[2]-0.02 | data$Energy > Pd.K[1]+0.02))
    Pd.file <- subset(data$Sample, !(data$Energy < Pd.K[2]-0.02 | data$Energy > Pd.K[1]+0.02))
    Pd.Ka.frame <- data.frame(is.0(Pd.Ka.cps, Pd.file))
    colnames(Pd.Ka.frame) <- c("Counts", "Sample")
    Pd.Ka.ag <- aggregate(list(Pd.Ka.frame$Counts), by=list(Pd.Ka.frame$Sample), FUN="sum")
    colnames(Pd.Ka.ag) <- c("Sample", "Pd K-alpha")
    
    Ag.Ka.cps <- subset(data$CPS, !(data$Energy < Ag.K[2]-0.02 | data$Energy > Ag.K[1]+0.02))
    Ag.file <- subset(data$Sample, !(data$Energy < Ag.K[2]-0.02 | data$Energy > Ag.K[1]+0.02))
    Ag.Ka.frame <- data.frame(is.0(Ag.Ka.cps, Ag.file))
    colnames(Ag.Ka.frame) <- c("Counts", "Sample")
    Ag.Ka.ag <- aggregate(list(Ag.Ka.frame$Counts), by=list(Ag.Ka.frame$Sample), FUN="sum")
    colnames(Ag.Ka.ag) <- c("Sample", "Ag K-alpha")
    
    Cd.Ka.cps <- subset(data$CPS, !(data$Energy < Cd.K[2]-0.02 | data$Energy > Cd.K[1]+0.02))
    Cd.file <- subset(data$Sample, !(data$Energy < Cd.K[2]-0.02 | data$Energy > Cd.K[1]+0.02))
    Cd.Ka.frame <- data.frame(is.0(Cd.Ka.cps, Cd.file))
    colnames(Cd.Ka.frame) <- c("Counts", "Sample")
    Cd.Ka.ag <- aggregate(list(Cd.Ka.frame$Counts), by=list(Cd.Ka.frame$Sample), FUN="sum")
    colnames(Cd.Ka.ag) <- c("Sample", "Cd K-alpha")
    
    In.Ka.cps <- subset(data$CPS, !(data$Energy < In.K[2]-0.02 | data$Energy > In.K[1]+0.02))
    In.file <- subset(data$Sample, !(data$Energy < In.K[2]-0.02 | data$Energy > In.K[1]+0.02))
    In.Ka.frame <- data.frame(is.0(In.Ka.cps, In.file))
    colnames(In.Ka.frame) <- c("Counts", "Sample")
    In.Ka.ag <- aggregate(list(In.Ka.frame$Counts), by=list(In.Ka.frame$Sample), FUN="sum")
    colnames(In.Ka.ag) <- c("Sample", "In K-alpha")
    
    Sn.Ka.cps <- subset(data$CPS, !(data$Energy < Sn.K[2]-0.02 | data$Energy > Sn.K[1]+0.02))
    Sn.file <- subset(data$Sample, !(data$Energy < Sn.K[2]-0.02 | data$Energy > Sn.K[1]+0.02))
    Sn.Ka.frame <- data.frame(is.0(Sn.Ka.cps, Sn.file))
    colnames(Sn.Ka.frame) <- c("Counts", "Sample")
    Sn.Ka.ag <- aggregate(list(Sn.Ka.frame$Counts), by=list(Sn.Ka.frame$Sample), FUN="sum")
    colnames(Sn.Ka.ag) <- c("Sample", "Sn K-alpha")
    
    Sb.Ka.cps <- subset(data$CPS, !(data$Energy < Sb.K[2]-0.02 | data$Energy > Sb.K[1]+0.02))
    Sb.file <- subset(data$Sample, !(data$Energy < Sb.K[2]-0.02 | data$Energy > Sb.K[1]+0.02))
    Sb.Ka.frame <- data.frame(is.0(Sb.Ka.cps, Sb.file))
    colnames(Sb.Ka.frame) <- c("Counts", "Sample")
    Sb.Ka.ag <- aggregate(list(Sb.Ka.frame$Counts), by=list(Sb.Ka.frame$Sample), FUN="sum")
    colnames(Sb.Ka.ag) <- c("Sample", "Sb K-alpha")
    
    Te.Ka.cps <- subset(data$CPS, !(data$Energy < Te.K[2]-0.02 | data$Energy > Te.K[1]+0.02))
    Te.file <- subset(data$Sample, !(data$Energy < Te.K[2]-0.02 | data$Energy > Te.K[1]+0.02))
    Te.Ka.frame <- data.frame(is.0(Te.Ka.cps, Te.file))
    colnames(Te.Ka.frame) <- c("Counts", "Sample")
    Te.Ka.ag <- aggregate(list(Te.Ka.frame$Counts), by=list(Te.Ka.frame$Sample), FUN="sum")
    colnames(Te.Ka.ag) <- c("Sample", "Te K-alpha")
    
    I.Ka.cps <- subset(data$CPS, !(data$Energy < I.K[2]-0.02 | data$Energy > I.K[1]+0.02))
    I.file <- subset(data$Sample, !(data$Energy < I.K[2]-0.02 | data$Energy > I.K[1]+0.02))
    I.Ka.frame <- data.frame(is.0(I.Ka.cps, I.file))
    colnames(I.Ka.frame) <- c("Counts", "Sample")
    I.Ka.ag <- aggregate(list(I.Ka.frame$Counts), by=list(I.Ka.frame$Sample), FUN="sum")
    colnames(I.Ka.ag) <- c("Sample", "I K-alpha")
    
    Xe.Ka.cps <- subset(data$CPS, !(data$Energy < Xe.K[2]-0.02 | data$Energy > Xe.K[1]+0.02))
    Xe.file <- subset(data$Sample, !(data$Energy < Xe.K[2]-0.02 | data$Energy > Xe.K[1]+0.02))
    Xe.Ka.frame <- data.frame(is.0(Xe.Ka.cps, Xe.file))
    colnames(Xe.Ka.frame) <- c("Counts", "Sample")
    Xe.Ka.ag <- aggregate(list(Xe.Ka.frame$Counts), by=list(Xe.Ka.frame$Sample), FUN="sum")
    colnames(Xe.Ka.ag) <- c("Sample", "Xe K-alpha")
    
    Cs.Ka.cps <- subset(data$CPS, !(data$Energy < Cs.K[2]-0.02 | data$Energy > Cs.K[1]+0.02))
    Cs.file <- subset(data$Sample, !(data$Energy < Cs.K[2]-0.02 | data$Energy > Cs.K[1]+0.02))
    Cs.Ka.frame <- data.frame(is.0(Cs.Ka.cps, Cs.file))
    colnames(Cs.Ka.frame) <- c("Counts", "Sample")
    Cs.Ka.ag <- aggregate(list(Cs.Ka.frame$Counts), by=list(Cs.Ka.frame$Sample), FUN="sum")
    colnames(Cs.Ka.ag) <- c("Sample", "Cs K-alpha")
    
    Ba.Ka.cps <- subset(data$CPS, !(data$Energy < Ba.K[2]-0.02 | data$Energy > Ba.K[1]+0.02))
    Ba.file <- subset(data$Sample, !(data$Energy < Ba.K[2]-0.02 | data$Energy > Ba.K[1]+0.02))
    Ba.Ka.frame <- data.frame(is.0(Ba.Ka.cps, Ba.file))
    colnames(Ba.Ka.frame) <- c("Counts", "Sample")
    Ba.Ka.ag <- aggregate(list(Ba.Ka.frame$Counts), by=list(Ba.Ka.frame$Sample), FUN="sum")
    colnames(Ba.Ka.ag) <- c("Sample", "Ba K-alpha")
    
    La.Ka.cps <- subset(data$CPS, !(data$Energy < La.K[2]-0.02 | data$Energy > La.K[1]+0.02))
    La.file <- subset(data$Sample, !(data$Energy < La.K[2]-0.02 | data$Energy > La.K[1]+0.02))
    La.Ka.frame <- data.frame(is.0(La.Ka.cps, La.file))
    colnames(La.Ka.frame) <- c("Counts", "Sample")
    La.Ka.ag <- aggregate(list(La.Ka.frame$Counts), by=list(La.Ka.frame$Sample), FUN="sum")
    colnames(La.Ka.ag) <- c("Sample", "La K-alpha")
    
    Ce.Ka.cps <- subset(data$CPS, !(data$Energy < Ce.K[2]-0.02 | data$Energy > Ce.K[1]+0.02))
    Ce.file <- subset(data$Sample, !(data$Energy < Ce.K[2]-0.02 | data$Energy > Ce.K[1]+0.02))
    Ce.Ka.frame <- data.frame(is.0(Ce.Ka.cps, Ce.file))
    colnames(Ce.Ka.frame) <- c("Counts", "Sample")
    Ce.Ka.ag <- aggregate(list(Ce.Ka.frame$Counts), by=list(Ce.Ka.frame$Sample), FUN="sum")
    colnames(Ce.Ka.ag) <- c("Sample", "Ce K-alpha")
    
    Pr.Ka.cps <- subset(data$CPS, !(data$Energy < Pr.K[2]-0.02 | data$Energy > Pr.K[1]+0.02))
    Pr.file <- subset(data$Sample, !(data$Energy < Pr.K[2]-0.02 | data$Energy > Pr.K[1]+0.02))
    Pr.Ka.frame <- data.frame(is.0(Pr.Ka.cps, Pr.file))
    colnames(Pr.Ka.frame) <- c("Counts", "Sample")
    Pr.Ka.ag <- aggregate(list(Pr.Ka.frame$Counts), by=list(Pr.Ka.frame$Sample), FUN="sum")
    colnames(Pr.Ka.ag) <- c("Sample", "Pr K-alpha")
    
    Nd.Ka.cps <- subset(data$CPS, !(data$Energy < Nd.K[2]-0.02 | data$Energy > Nd.K[1]+0.02))
    Nd.file <- subset(data$Sample, !(data$Energy < Nd.K[2]-0.02 | data$Energy > Nd.K[1]+0.02))
    Nd.Ka.frame <- data.frame(is.0(Nd.Ka.cps, Nd.file))
    colnames(Nd.Ka.frame) <- c("Counts", "Sample")
    Nd.Ka.ag <- aggregate(list(Nd.Ka.frame$Counts), by=list(Nd.Ka.frame$Sample), FUN="sum")
    colnames(Nd.Ka.ag) <- c("Sample", "Nd K-alpha")
    

    Tc.La.cps <- subset(data$CPS, !(data$Energy < Tc.L[2]-0.02 | data$Energy > Tc.L[1]+0.02))
    Tc.file <- subset(data$Sample, !(data$Energy < Tc.L[2]-0.02 | data$Energy > Tc.L[1]+0.02))
    Tc.La.frame <- is.0(Tc.La.cps,Tc.file)
    colnames(Tc.La.frame) <- c("Counts", "Sample")
    Tc.La.ag <- aggregate(list(Tc.La.frame$Counts), by=list(Tc.La.frame$Sample), FUN="sum")
    colnames(Tc.La.ag) <- c("Sample", "Tc L-alpha")
    
    Ru.La.cps <- subset(data$CPS, !(data$Energy < Ru.L[2]-0.02 | data$Energy > Ru.L[1]+0.02))
    Ru.file <- subset(data$Sample, !(data$Energy < Ru.L[2]-0.02 | data$Energy > Ru.L[1]+0.02))
    Ru.La.frame <- is.0(Ru.La.cps,Ru.file)
    colnames(Ru.La.frame) <- c("Counts", "Sample")
    Ru.La.ag <- aggregate(list(Ru.La.frame$Counts), by=list(Ru.La.frame$Sample), FUN="sum")
    colnames(Ru.La.ag) <- c("Sample", "Ru L-alpha")
    
    Rh.La.cps <- subset(data$CPS, !(data$Energy < Rh.L[2]-0.02 | data$Energy > Rh.L[1]+0.02))
    Rh.file <- subset(data$Sample, !(data$Energy < Rh.L[2]-0.02 | data$Energy > Rh.L[1]+0.02))
    Rh.La.frame <- data.frame(is.0(Rh.La.cps, Rh.file))
    colnames(Rh.La.frame) <- c("Counts", "Sample")
    Rh.La.ag <- aggregate(list(Rh.La.frame$Counts), by=list(Rh.La.frame$Sample), FUN="sum")
    colnames(Rh.La.ag) <- c("Sample", "Rh L-alpha")
    
    Pd.La.cps <- subset(data$CPS, !(data$Energy < Pd.L[2]-0.02 | data$Energy > Pd.L[1]+0.02))
    Pd.file <- subset(data$Sample, !(data$Energy < Pd.L[2]-0.02 | data$Energy > Pd.L[1]+0.02))
    Pd.La.frame <- data.frame(is.0(Pd.La.cps, Pd.file))
    colnames(Pd.La.frame) <- c("Counts", "Sample")
    Pd.La.ag <- aggregate(list(Pd.La.frame$Counts), by=list(Pd.La.frame$Sample), FUN="sum")
    colnames(Pd.La.ag) <- c("Sample", "Pd L-alpha")
    
    Ag.La.cps <- subset(data$CPS, !(data$Energy < Ag.L[2]-0.02 | data$Energy > Ag.L[1]+0.02))
    Ag.file <- subset(data$Sample, !(data$Energy < Ag.L[2]-0.02 | data$Energy > Ag.L[1]+0.02))
    Ag.La.frame <- data.frame(is.0(Ag.La.cps, Ag.file))
    colnames(Ag.La.frame) <- c("Counts", "Sample")
    Ag.La.ag <- aggregate(list(Ag.La.frame$Counts), by=list(Ag.La.frame$Sample), FUN="sum")
    colnames(Ag.La.ag) <- c("Sample", "Ag L-alpha")
    
    Cd.La.cps <- subset(data$CPS, !(data$Energy < Cd.L[2]-0.02 | data$Energy > Cd.L[1]+0.02))
    Cd.file <- subset(data$Sample, !(data$Energy < Cd.L[2]-0.02 | data$Energy > Cd.L[1]+0.02))
    Cd.La.frame <- data.frame(is.0(Cd.La.cps, Cd.file))
    colnames(Cd.La.frame) <- c("Counts", "Sample")
    Cd.La.ag <- aggregate(list(Cd.La.frame$Counts), by=list(Cd.La.frame$Sample), FUN="sum")
    colnames(Cd.La.ag) <- c("Sample", "Cd L-alpha")
    
    In.La.cps <- subset(data$CPS, !(data$Energy < In.L[2]-0.02 | data$Energy > In.L[1]+0.02))
    In.file <- subset(data$Sample, !(data$Energy < In.L[2]-0.02 | data$Energy > In.L[1]+0.02))
    In.La.frame <- data.frame(is.0(In.La.cps, In.file))
    colnames(In.La.frame) <- c("Counts", "Sample")
    In.La.ag <- aggregate(list(In.La.frame$Counts), by=list(In.La.frame$Sample), FUN="sum")
    colnames(In.La.ag) <- c("Sample", "In L-alpha")
    
    Sn.La.cps <- subset(data$CPS, !(data$Energy < Sn.L[2]-0.02 | data$Energy > Sn.L[1]+0.02))
    Sn.file <- subset(data$Sample, !(data$Energy < Sn.L[2]-0.02 | data$Energy > Sn.L[1]+0.02))
    Sn.La.frame <- data.frame(is.0(Sn.La.cps, Sn.file))
    colnames(Sn.La.frame) <- c("Counts", "Sample")
    Sn.La.ag <- aggregate(list(Sn.La.frame$Counts), by=list(Sn.La.frame$Sample), FUN="sum")
    colnames(Sn.La.ag) <- c("Sample", "Sn L-alpha")
    
    Sb.La.cps <- subset(data$CPS, !(data$Energy < Sb.L[2]-0.02 | data$Energy > Sb.L[1]+0.02))
    Sb.file <- subset(data$Sample, !(data$Energy < Sb.L[2]-0.02 | data$Energy > Sb.L[1]+0.02))
    Sb.La.frame <- data.frame(is.0(Sb.La.cps, Sb.file))
    colnames(Sb.La.frame) <- c("Counts", "Sample")
    Sb.La.ag <- aggregate(list(Sb.La.frame$Counts), by=list(Sb.La.frame$Sample), FUN="sum")
    colnames(Sb.La.ag) <- c("Sample", "Sb L-alpha")
    
    Te.La.cps <- subset(data$CPS, !(data$Energy < Te.L[2]-0.02 | data$Energy > Te.L[1]+0.02))
    Te.file <- subset(data$Sample, !(data$Energy < Te.L[2]-0.02 | data$Energy > Te.L[1]+0.02))
    Te.La.frame <- data.frame(is.0(Te.La.cps, Te.file))
    colnames(Te.La.frame) <- c("Counts", "Sample")
    Te.La.ag <- aggregate(list(Te.La.frame$Counts), by=list(Te.La.frame$Sample), FUN="sum")
    colnames(Te.La.ag) <- c("Sample", "Te L-alpha")
    
    I.La.cps <- subset(data$CPS, !(data$Energy < I.L[2]-0.02 | data$Energy > I.L[1]+0.02))
    I.file <- subset(data$Sample, !(data$Energy < I.L[2]-0.02 | data$Energy > I.L[1]+0.02))
    I.La.frame <- data.frame(is.0(I.La.cps, I.file))
    colnames(I.La.frame) <- c("Counts", "Sample")
    I.La.ag <- aggregate(list(I.La.frame$Counts), by=list(I.La.frame$Sample), FUN="sum")
    colnames(I.La.ag) <- c("Sample", "I L-alpha")
    
    Xe.La.cps <- subset(data$CPS, !(data$Energy < Xe.L[2]-0.02 | data$Energy > Xe.L[1]+0.02))
    Xe.file <- subset(data$Sample, !(data$Energy < Xe.L[2]-0.02 | data$Energy > Xe.L[1]+0.02))
    Xe.La.frame <- data.frame(is.0(Xe.La.cps, Xe.file))
    colnames(Xe.La.frame) <- c("Counts", "Sample")
    Xe.La.ag <- aggregate(list(Xe.La.frame$Counts), by=list(Xe.La.frame$Sample), FUN="sum")
    colnames(Xe.La.ag) <- c("Sample", "Xe L-alpha")
    
    Cs.La.cps <- subset(data$CPS, !(data$Energy < Cs.L[2]-0.02 | data$Energy > Cs.L[1]+0.02))
    Cs.file <- subset(data$Sample, !(data$Energy < Cs.L[2]-0.02 | data$Energy > Cs.L[1]+0.02))
    Cs.La.frame <- data.frame(is.0(Cs.La.cps, Cs.file))
    colnames(Cs.La.frame) <- c("Counts", "Sample")
    Cs.La.ag <- aggregate(list(Cs.La.frame$Counts), by=list(Cs.La.frame$Sample), FUN="sum")
    colnames(Cs.La.ag) <- c("Sample", "Cs L-alpha")
    
    Ba.La.cps <- subset(data$CPS, !(data$Energy < Ba.L[2]-0.02 | data$Energy > Ba.L[1]+0.02))
    Ba.file <- subset(data$Sample, !(data$Energy < Ba.L[2]-0.02 | data$Energy > Ba.L[1]+0.02))
    Ba.La.frame <- data.frame(is.0(Ba.La.cps, Ba.file))
    colnames(Ba.La.frame) <- c("Counts", "Sample")
    Ba.La.ag <- aggregate(list(Ba.La.frame$Counts), by=list(Ba.La.frame$Sample), FUN="sum")
    colnames(Ba.La.ag) <- c("Sample", "Ba L-alpha")
    
    La.La.cps <- subset(data$CPS, !(data$Energy < La.L[2]-0.02 | data$Energy > La.L[1]+0.02))
    La.file <- subset(data$Sample, !(data$Energy < La.L[2]-0.02 | data$Energy > La.L[1]+0.02))
    La.La.frame <- data.frame(is.0(La.La.cps, La.file))
    colnames(La.La.frame) <- c("Counts", "Sample")
    La.La.ag <- aggregate(list(La.La.frame$Counts), by=list(La.La.frame$Sample), FUN="sum")
    colnames(La.La.ag) <- c("Sample", "La L-alpha")
    
    Ce.La.cps <- subset(data$CPS, !(data$Energy < Ce.L[2]-0.02 | data$Energy > Ce.L[1]+0.02))
    Ce.file <- subset(data$Sample, !(data$Energy < Ce.L[2]-0.02 | data$Energy > Ce.L[1]+0.02))
    Ce.La.frame <- data.frame(is.0(Ce.La.cps, Ce.file))
    colnames(Ce.La.frame) <- c("Counts", "Sample")
    Ce.La.ag <- aggregate(list(Ce.La.frame$Counts), by=list(Ce.La.frame$Sample), FUN="sum")
    colnames(Ce.La.ag) <- c("Sample", "Ce L-alpha")
    
    Pr.La.cps <- subset(data$CPS, !(data$Energy < Pr.L[2]-0.02 | data$Energy > Pr.L[1]+0.02))
    Pr.file <- subset(data$Sample, !(data$Energy < Pr.L[2]-0.02 | data$Energy > Pr.L[1]+0.02))
    Pr.La.frame <- data.frame(is.0(Pr.La.cps, Pr.file))
    colnames(Pr.La.frame) <- c("Counts", "Sample")
    Pr.La.ag <- aggregate(list(Pr.La.frame$Counts), by=list(Pr.La.frame$Sample), FUN="sum")
    colnames(Pr.La.ag) <- c("Sample", "Pr L-alpha")
    
    Nd.La.cps <- subset(data$CPS, !(data$Energy < Nd.L[2]-0.02 | data$Energy > Nd.L[1]+0.02))
    Nd.file <- subset(data$Sample, !(data$Energy < Nd.L[2]-0.02 | data$Energy > Nd.L[1]+0.02))
    Nd.La.frame <- data.frame(is.0(Nd.La.cps, Nd.file))
    colnames(Nd.La.frame) <- c("Counts", "Sample")
    Nd.La.ag <- aggregate(list(Nd.La.frame$Counts), by=list(Nd.La.frame$Sample), FUN="sum")
    colnames(Nd.La.ag) <- c("Sample", "Nd L-alpha")
    
    Pm.La.cps <- subset(data$CPS, !(data$Energy < Pm.L[2]-0.02 | data$Energy > Pm.L[1]+0.02))
    Pm.file <- subset(data$Sample, !(data$Energy < Pm.L[2]-0.02 | data$Energy > Pm.L[1]+0.02))
    Pm.La.frame <- data.frame(is.0(Pm.La.cps, Pm.file))
    colnames(Pm.La.frame) <- c("Counts", "Sample")
    Pm.La.ag <- aggregate(list(Pm.La.frame$Counts), by=list(Pm.La.frame$Sample), FUN="sum")
    colnames(Pm.La.ag) <- c("Sample", "Pm L-alpha")
    
    Sm.La.cps <- subset(data$CPS, !(data$Energy < Sm.L[2]-0.02 | data$Energy > Sm.L[1]+0.02))
    Sm.file <- subset(data$Sample, !(data$Energy < Sm.L[2]-0.02 | data$Energy > Sm.L[1]+0.02))
    Sm.La.frame <- data.frame(is.0(Sm.La.cps, Sm.file))
    colnames(Sm.La.frame) <- c("Counts", "Sample")
    Sm.La.ag <- aggregate(list(Sm.La.frame$Counts), by=list(Sm.La.frame$Sample), FUN="sum")
    colnames(Sm.La.ag) <- c("Sample", "Sm L-alpha")
    
    Eu.La.cps <- subset(data$CPS, !(data$Energy < Eu.L[2]-0.02 | data$Energy > Eu.L[1]+0.02))
    Eu.file <- subset(data$Sample, !(data$Energy < Eu.L[2]-0.02 | data$Energy > Eu.L[1]+0.02))
    Eu.La.frame <- data.frame(is.0(Eu.La.cps, Eu.file))
    colnames(Eu.La.frame) <- c("Counts", "Sample")
    Eu.La.ag <- aggregate(list(Eu.La.frame$Counts), by=list(Eu.La.frame$Sample), FUN="sum")
    colnames(Eu.La.ag) <- c("Sample", "Eu L-alpha")
    
    Gd.La.cps <- subset(data$CPS, !(data$Energy < Gd.L[2]-0.02 | data$Energy > Gd.L[1]+0.02))
    Gd.file <- subset(data$Sample, !(data$Energy < Gd.L[2]-0.02 | data$Energy > Gd.L[1]+0.02))
    Gd.La.frame <- data.frame(is.0(Gd.La.cps, Gd.file))
    colnames(Gd.La.frame) <- c("Counts", "Sample")
    Gd.La.ag <- aggregate(list(Gd.La.frame$Counts), by=list(Gd.La.frame$Sample), FUN="sum")
    colnames(Gd.La.ag) <- c("Sample", "Gd L-alpha")
    
    Tb.La.cps <- subset(data$CPS, !(data$Energy < Tb.L[2]-0.02 | data$Energy > Tb.L[1]+0.02))
    Tb.file <- subset(data$Sample, !(data$Energy < Tb.L[2]-0.02 | data$Energy > Tb.L[1]+0.02))
    Tb.La.frame <- data.frame(is.0(Tb.La.cps, Tb.file))
    colnames(Tb.La.frame) <- c("Counts", "Sample")
    Tb.La.ag <- aggregate(list(Tb.La.frame$Counts), by=list(Tb.La.frame$Sample), FUN="sum")
    colnames(Tb.La.ag) <- c("Sample", "Tb L-alpha")
    
    Dy.La.cps <- subset(data$CPS, !(data$Energy < Dy.L[2]-0.02 | data$Energy > Dy.L[1]+0.02))
    Dy.file <- subset(data$Sample, !(data$Energy < Dy.L[2]-0.02 | data$Energy > Dy.L[1]+0.02))
    Dy.La.frame <- data.frame(is.0(Dy.La.cps, Dy.file))
    colnames(Dy.La.frame) <- c("Counts", "Sample")
    Dy.La.ag <- aggregate(list(Dy.La.frame$Counts), by=list(Dy.La.frame$Sample), FUN="sum")
    colnames(Dy.La.ag) <- c("Sample", "Dy L-alpha")
    
    Ho.La.cps <- subset(data$CPS, !(data$Energy < Ho.L[2]-0.02 | data$Energy > Ho.L[1]+0.02))
    Ho.file <- subset(data$Sample, !(data$Energy < Ho.L[2]-0.02 | data$Energy > Ho.L[1]+0.02))
    Ho.La.frame <- data.frame(is.0(Ho.La.cps, Ho.file))
    colnames(Ho.La.frame) <- c("Counts", "Sample")
    Ho.La.ag <- aggregate(list(Ho.La.frame$Counts), by=list(Ho.La.frame$Sample), FUN="sum")
    colnames(Ho.La.ag) <- c("Sample", "Ho L-alpha")
    
    Er.La.cps <- subset(data$CPS, !(data$Energy < Er.L[2]-0.02 | data$Energy > Er.L[1]+0.02))
    Er.file <- subset(data$Sample, !(data$Energy < Er.L[2]-0.02 | data$Energy > Er.L[1]+0.02))
    Er.La.frame <- data.frame(is.0(Er.La.cps, Er.file))
    colnames(Er.La.frame) <- c("Counts", "Sample")
    Er.La.ag <- aggregate(list(Er.La.frame$Counts), by=list(Er.La.frame$Sample), FUN="sum")
    colnames(Er.La.ag) <- c("Sample", "Er L-alpha")
    
    Tm.La.cps <- subset(data$CPS, !(data$Energy < Tm.L[2]-0.02 | data$Energy > Tm.L[1]+0.02))
    Tm.file <- subset(data$Sample, !(data$Energy < Tm.L[2]-0.02 | data$Energy > Tm.L[1]+0.02))
    Tm.La.frame <- data.frame(is.0(Tm.La.cps, Tm.file))
    colnames(Tm.La.frame) <- c("Counts", "Sample")
    Tm.La.ag <- aggregate(list(Tm.La.frame$Counts), by=list(Tm.La.frame$Sample), FUN="sum")
    colnames(Tm.La.ag) <- c("Sample", "Tm L-alpha")
    
    Yb.La.cps <- subset(data$CPS, !(data$Energy < Yb.L[2]-0.02 | data$Energy > Yb.L[1]+0.02))
    Yb.file <- subset(data$Sample, !(data$Energy < Yb.L[2]-0.02 | data$Energy > Yb.L[1]+0.02))
    Yb.La.frame <- data.frame(is.0(Yb.La.cps, Yb.file))
    colnames(Yb.La.frame) <- c("Counts", "Sample")
    Yb.La.ag <- aggregate(list(Yb.La.frame$Counts), by=list(Yb.La.frame$Sample), FUN="sum")
    colnames(Yb.La.ag) <- c("Sample", "Yb L-alpha")
    
    Lu.La.cps <- subset(data$CPS, !(data$Energy < Lu.L[2]-0.02 | data$Energy > Lu.L[1]+0.02))
    Lu.file <- subset(data$Sample, !(data$Energy < Lu.L[2]-0.02 | data$Energy > Lu.L[1]+0.02))
    Lu.La.frame <- data.frame(is.0(Lu.La.cps, Lu.file))
    colnames(Lu.La.frame) <- c("Counts", "Sample")
    Lu.La.ag <- aggregate(list(Lu.La.frame$Counts), by=list(Lu.La.frame$Sample), FUN="sum")
    colnames(Lu.La.ag) <- c("Sample", "Lu L-alpha")
    
    Hf.La.cps <- subset(data$CPS, !(data$Energy < Hf.L[2]-0.02 | data$Energy > Hf.L[1]+0.02))
    Hf.file <- subset(data$Sample, !(data$Energy < Hf.L[2]-0.02 | data$Energy > Hf.L[1]+0.02))
    Hf.La.frame <- data.frame(is.0(Hf.La.cps, Hf.file))
    colnames(Hf.La.frame) <- c("Counts", "Sample")
    Hf.La.ag <- aggregate(list(Hf.La.frame$Counts), by=list(Hf.La.frame$Sample), FUN="sum")
    colnames(Hf.La.ag) <- c("Sample", "Hf L-alpha")
    
    Ta.La.cps <- subset(data$CPS, !(data$Energy < Ta.L[2]-0.02 | data$Energy > Ta.L[1]+0.02))
    Ta.file <- subset(data$Sample, !(data$Energy < Ta.L[2]-0.02 | data$Energy > Ta.L[1]+0.02))
    Ta.La.frame <- data.frame(is.0(Ta.La.cps, Ta.file))
    colnames(Ta.La.frame) <- c("Counts", "Sample")
    Ta.La.ag <- aggregate(list(Ta.La.frame$Counts), by=list(Ta.La.frame$Sample), FUN="sum")
    colnames(Ta.La.ag) <- c("Sample", "Ta L-alpha")
    
    W.La.cps <- subset(data$CPS, !(data$Energy < W.L[2]-0.02 | data$Energy > W.L[1]+0.02))
    W.file <- subset(data$Sample, !(data$Energy < W.L[2]-0.02 | data$Energy > W.L[1]+0.02))
    W.La.frame <- data.frame(is.0(W.La.cps, W.file))
    colnames(W.La.frame) <- c("Counts", "Sample")
    W.La.ag <- aggregate(list(W.La.frame$Counts), by=list(W.La.frame$Sample), FUN="sum")
    colnames(W.La.ag) <- c("Sample", "W L-alpha")
    
    Re.La.cps <- subset(data$CPS, !(data$Energy < Re.L[2]-0.02 | data$Energy > Re.L[1]+0.02))
    Re.file <- subset(data$Sample, !(data$Energy < Re.L[2]-0.02 | data$Energy > Re.L[1]+0.02))
    Re.La.frame <- data.frame(is.0(Re.La.cps, Re.file))
    colnames(Re.La.frame) <- c("Counts", "Sample")
    Re.La.ag <- aggregate(list(Re.La.frame$Counts), by=list(Re.La.frame$Sample), FUN="sum")
    colnames(Re.La.ag) <- c("Sample", "Re L-alpha")
    
    Os.La.cps <- subset(data$CPS, !(data$Energy < Os.L[2]-0.02 | data$Energy > Os.L[1]+0.02))
    Os.file <- subset(data$Sample, !(data$Energy < Os.L[2]-0.02 | data$Energy > Os.L[1]+0.02))
    Os.La.frame <- data.frame(is.0(Os.La.cps, Os.file))
    colnames(Os.La.frame) <- c("Counts", "Sample")
    Os.La.ag <- aggregate(list(Os.La.frame$Counts), by=list(Os.La.frame$Sample), FUN="sum")
    colnames(Os.La.ag) <- c("Sample", "Os L-alpha")
    
    Ir.La.cps <- subset(data$CPS, !(data$Energy < Ir.L[2]-0.02 | data$Energy > Ir.L[1]+0.02))
    Ir.file <- subset(data$Sample, !(data$Energy < Ir.L[2]-0.02 | data$Energy > Ir.L[1]+0.02))
    Ir.La.frame <- data.frame(is.0(Ir.La.cps, Ir.file))
    colnames(Ir.La.frame) <- c("Counts", "Sample")
    Ir.La.ag <- aggregate(list(Ir.La.frame$Counts), by=list(Ir.La.frame$Sample), FUN="sum")
    colnames(Ir.La.ag) <- c("Sample", "Ir L-alpha")
    
    Pt.La.cps <- subset(data$CPS, !(data$Energy < Pt.L[2]-0.02 | data$Energy > Pt.L[1]+0.02))
    Pt.file <- subset(data$Sample, !(data$Energy < Pt.L[2]-0.02 | data$Energy > Pt.L[1]+0.02))
    Pt.La.frame <- data.frame(is.0(Pt.La.cps, Pt.file))
    colnames(Pt.La.frame) <- c("Counts", "Sample")
    Pt.La.ag <- aggregate(list(Pt.La.frame$Counts), by=list(Pt.La.frame$Sample), FUN="sum")
    colnames(Pt.La.ag) <- c("Sample", "Pt L-alpha")
    
    Au.La.cps <- subset(data$CPS, !(data$Energy < Au.L[2]-0.02 | data$Energy > Au.L[1]+0.02))
    Au.file <- subset(data$Sample, !(data$Energy < Au.L[2]-0.02 | data$Energy > Au.L[1]+0.02))
    Au.La.frame <- data.frame(is.0(Au.La.cps, Au.file))
    colnames(Au.La.frame) <- c("Counts", "Sample")
    Au.La.ag <- aggregate(list(Au.La.frame$Counts), by=list(Au.La.frame$Sample), FUN="sum")
    colnames(Au.La.ag) <- c("Sample", "Au L-alpha")
    
    Hg.La.cps <- subset(data$CPS, !(data$Energy < Hg.L[2]-0.02 | data$Energy > Hg.L[1]+0.02))
    Hg.file <- subset(data$Sample, !(data$Energy < Hg.L[2]-0.02 | data$Energy > Hg.L[1]+0.02))
    Hg.La.frame <- data.frame(is.0(Hg.La.cps, Hg.file))
    colnames(Hg.La.frame) <- c("Counts", "Sample")
    Hg.La.ag <- aggregate(list(Hg.La.frame$Counts), by=list(Hg.La.frame$Sample), FUN="sum")
    colnames(Hg.La.ag) <- c("Sample", "Hg L-alpha")
    
    Tl.La.cps <- subset(data$CPS, !(data$Energy < Tl.L[2]-0.02 | data$Energy > Tl.L[1]+0.02))
    Tl.file <- subset(data$Sample, !(data$Energy < Tl.L[2]-0.02 | data$Energy > Tl.L[1]+0.02))
    Tl.La.frame <- data.frame(is.0(Tl.La.cps, Tl.file))
    colnames(Tl.La.frame) <- c("Counts", "Sample")
    Tl.La.ag <- aggregate(list(Tl.La.frame$Counts), by=list(Tl.La.frame$Sample), FUN="sum")
    colnames(Tl.La.ag) <- c("Sample", "Tl L-alpha")
    
    Pb.La.cps <- subset(data$CPS, !(data$Energy < Pb.L[2]-0.02 | data$Energy > Pb.L[1]+0.02))
    Pb.file <- subset(data$Sample, !(data$Energy < Pb.L[2]-0.02 | data$Energy > Pb.L[1]+0.02))
    Pb.La.frame <- data.frame(is.0(Pb.La.cps, Pb.file))
    colnames(Pb.La.frame) <- c("Counts", "Sample")
    Pb.La.ag <- aggregate(list(Pb.La.frame$Counts), by=list(Pb.La.frame$Sample), FUN="sum")
    colnames(Pb.La.ag) <- c("Sample", "Pb L-alpha")
    
    Bi.La.cps <- subset(data$CPS, !(data$Energy < Bi.L[2]-0.02 | data$Energy > Bi.L[1]+0.02))
    Bi.file <- subset(data$Sample, !(data$Energy < Bi.L[2]-0.02 | data$Energy > Bi.L[1]+0.02))
    Bi.La.frame <- data.frame(is.0(Bi.La.cps, Bi.file))
    colnames(Bi.La.frame) <- c("Counts", "Sample")
    Bi.La.ag <- aggregate(list(Bi.La.frame$Counts), by=list(Bi.La.frame$Sample), FUN="sum")
    colnames(Bi.La.ag) <- c("Sample", "Bi L-alpha")
    
    Po.La.cps <- subset(data$CPS, !(data$Energy < Po.L[2]-0.02 | data$Energy > Po.L[1]+0.02))
    Po.file <- subset(data$Sample, !(data$Energy < Po.L[2]-0.02 | data$Energy > Po.L[1]+0.02))
    Po.La.frame <- data.frame(is.0(Po.La.cps, Po.file))
    colnames(Po.La.frame) <- c("Counts", "Sample")
    Po.La.ag <- aggregate(list(Po.La.frame$Counts), by=list(Po.La.frame$Sample), FUN="sum")
    colnames(Po.La.ag) <- c("Sample", "Po L-alpha")
    
    At.La.cps <- subset(data$CPS, !(data$Energy < At.L[2]-0.02 | data$Energy > At.L[1]+0.02))
    At.file <- subset(data$Sample, !(data$Energy < At.L[2]-0.02 | data$Energy > At.L[1]+0.02))
    At.La.frame <- data.frame(is.0(At.La.cps, At.file))
    colnames(At.La.frame) <- c("Counts", "Sample")
    At.La.ag <- aggregate(list(At.La.frame$Counts), by=list(At.La.frame$Sample), FUN="sum")
    colnames(At.La.ag) <- c("Sample", "At L-alpha")
    
    Rn.La.cps <- subset(data$CPS, !(data$Energy < Rn.L[2]-0.02 | data$Energy > Rn.L[1]+0.02))
    Rn.file <- subset(data$Sample, !(data$Energy < Rn.L[2]-0.02 | data$Energy > Rn.L[1]+0.02))
    Rn.La.frame <- data.frame(is.0(Rn.La.cps, Rn.file))
    colnames(Rn.La.frame) <- c("Counts", "Sample")
    Rn.La.ag <- aggregate(list(Rn.La.frame$Counts), by=list(Rn.La.frame$Sample), FUN="sum")
    colnames(Rn.La.ag) <- c("Sample", "Rn L-alpha")
    
    Fr.La.cps <- subset(data$CPS, !(data$Energy < Fr.L[2]-0.02 | data$Energy > Fr.L[1]+0.02))
    Fr.file <- subset(data$Sample, !(data$Energy < Fr.L[2]-0.02 | data$Energy > Fr.L[1]+0.02))
    Fr.La.frame <- data.frame(is.0(Fr.La.cps, Fr.file))
    colnames(Fr.La.frame) <- c("Counts", "Sample")
    Fr.La.ag <- aggregate(list(Fr.La.frame$Counts), by=list(Fr.La.frame$Sample), FUN="sum")
    colnames(Fr.La.ag) <- c("Sample", "Fr L-alpha")
    
    Ra.La.cps <- subset(data$CPS, !(data$Energy < Ra.L[2]-0.02 | data$Energy > Ra.L[1]+0.02))
    Ra.file <- subset(data$Sample, !(data$Energy < Ra.L[2]-0.02 | data$Energy > Ra.L[1]+0.02))
    Ra.La.frame <- data.frame(is.0(Ra.La.cps, Ra.file))
    colnames(Ra.La.frame) <- c("Counts", "Sample")
    Ra.La.ag <- aggregate(list(Ra.La.frame$Counts), by=list(Ra.La.frame$Sample), FUN="sum")
    colnames(Ra.La.ag) <- c("Sample", "Ra L-alpha")
    
    Ac.La.cps <- subset(data$CPS, !(data$Energy < Ac.L[2]-0.02 | data$Energy > Ac.L[1]+0.02))
    Ac.file <- subset(data$Sample, !(data$Energy < Ac.L[2]-0.02 | data$Energy > Ac.L[1]+0.02))
    Ac.La.frame <- data.frame(is.0(Ac.La.cps, Ac.file))
    colnames(Ac.La.frame) <- c("Counts", "Sample")
    Ac.La.ag <- aggregate(list(Ac.La.frame$Counts), by=list(Ac.La.frame$Sample), FUN="sum")
    colnames(Ac.La.ag) <- c("Sample", "Ac L-alpha")
    
    Th.La.cps <- subset(data$CPS, !(data$Energy < Th.L[2]-0.02 | data$Energy > Th.L[1]+0.02))
    Th.file <- subset(data$Sample, !(data$Energy < Th.L[2]-0.02 | data$Energy > Th.L[1]+0.02))
    Th.La.frame <- data.frame(is.0(Th.La.cps, Th.file))
    colnames(Th.La.frame) <- c("Counts", "Sample")
    Th.La.ag <- aggregate(list(Th.La.frame$Counts), by=list(Th.La.frame$Sample), FUN="sum")
    colnames(Th.La.ag) <- c("Sample", "Th L-alpha")
    
    Pa.La.cps <- subset(data$CPS, !(data$Energy < Pa.L[2]-0.02 | data$Energy > Pa.L[1]+0.02))
    Pa.file <- subset(data$Sample, !(data$Energy < Pa.L[2]-0.02 | data$Energy > Pa.L[1]+0.02))
    Pa.La.frame <- data.frame(is.0(Pa.La.cps, Pa.file))
    colnames(Pa.La.frame) <- c("Counts", "Sample")
    Pa.La.ag <- aggregate(list(Pa.La.frame$Counts), by=list(Pa.La.frame$Sample), FUN="sum")
    colnames(Pa.La.ag) <- c("Sample", "Pa L-alpha")
    
    U.La.cps <- subset(data$CPS, !(data$Energy < U.L[2]-0.02 | data$Energy > U.L[1]+0.02))
    U.file <- subset(data$Sample, !(data$Energy < U.L[2]-0.02 | data$Energy > U.L[1]+0.02))
    U.La.frame <- data.frame(is.0(U.La.cps, U.file))
    colnames(U.La.frame) <- c("Counts", "Sample")
    U.La.ag <- aggregate(list(U.La.frame$Counts), by=list(U.La.frame$Sample), FUN="sum")
    colnames(U.La.ag) <- c("Sample", "U L-alpha")
    
    Mo.Lb.cps <- subset(data$CPS, !(data$Energy < Mo.L[3]-0.02 | data$Energy > Mo.L[5]+0.02))
    Mo.file <- subset(data$Sample, !(data$Energy < Mo.L[3]-0.02 | data$Energy > Mo.L[5]+0.02))
    Mo.Lb.frame <- is.0(Mo.Lb.cps,Mo.file)
    colnames(Mo.Lb.frame) <- c("Counts", "Sample")
    Mo.Lb.ag <- aggregate(list(Mo.Lb.frame$Counts), by=list(Mo.Lb.frame$Sample), FUN="sum")
    colnames(Mo.Lb.ag) <- c("Sample", "Mo L-beta")
    
    Tc.Lb.cps <- subset(data$CPS, !(data$Energy < Tc.L[3]-0.02 | data$Energy > Tc.L[5]+0.02))
    Tc.file <- subset(data$Sample, !(data$Energy < Tc.L[3]-0.02 | data$Energy > Tc.L[5]+0.02))
    Tc.Lb.frame <- is.0(Tc.Lb.cps,Tc.file)
    colnames(Tc.Lb.frame) <- c("Counts", "Sample")
    Tc.Lb.ag <- aggregate(list(Tc.Lb.frame$Counts), by=list(Tc.Lb.frame$Sample), FUN="sum")
    colnames(Tc.Lb.ag) <- c("Sample", "Tc L-beta")
    
    Ru.Lb.cps <- subset(data$CPS, !(data$Energy < Ru.L[3]-0.02 | data$Energy > Ru.L[5]+0.02))
    Ru.file <- subset(data$Sample, !(data$Energy < Ru.L[3]-0.02 | data$Energy > Ru.L[5]+0.02))
    Ru.Lb.frame <- is.0(Ru.Lb.cps,Ru.file)
    colnames(Ru.Lb.frame) <- c("Counts", "Sample")
    Ru.Lb.ag <- aggregate(list(Ru.Lb.frame$Counts), by=list(Ru.Lb.frame$Sample), FUN="sum")
    colnames(Ru.Lb.ag) <- c("Sample", "Ru L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.file <- subset(data$Sample, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.Lb.frame <- is.0(Rh.Lb.cps,Rh.file)
    colnames(Rh.Lb.frame) <- c("Counts", "Sample")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Sample), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Sample", "Rh L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.file <- subset(data$Sample, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.Lb.frame <- data.frame(is.0(Rh.Lb.cps, Rh.file))
    colnames(Rh.Lb.frame) <- c("Counts", "Sample")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Sample), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Sample", "Rh L-beta")
    
    Pd.Lb.cps <- subset(data$CPS, !(data$Energy < Pd.L[3]-0.02 | data$Energy > Pd.L[5]+0.02))
    Pd.file <- subset(data$Sample, !(data$Energy < Pd.L[3]-0.02 | data$Energy > Pd.L[5]+0.02))
    Pd.Lb.frame <- data.frame(is.0(Pd.Lb.cps, Pd.file))
    colnames(Pd.Lb.frame) <- c("Counts", "Sample")
    Pd.Lb.ag <- aggregate(list(Pd.Lb.frame$Counts), by=list(Pd.Lb.frame$Sample), FUN="sum")
    colnames(Pd.Lb.ag) <- c("Sample", "Pd L-beta")
    
    Ag.Lb.cps <- subset(data$CPS, !(data$Energy < Ag.L[3]-0.02 | data$Energy > Ag.L[5]+0.02))
    Ag.file <- subset(data$Sample, !(data$Energy < Ag.L[3]-0.02 | data$Energy > Ag.L[5]+0.02))
    Ag.Lb.frame <- data.frame(is.0(Ag.Lb.cps, Ag.file))
    colnames(Ag.Lb.frame) <- c("Counts", "Sample")
    Ag.Lb.ag <- aggregate(list(Ag.Lb.frame$Counts), by=list(Ag.Lb.frame$Sample), FUN="sum")
    colnames(Ag.Lb.ag) <- c("Sample", "Ag L-beta")
    
    Cd.Lb.cps <- subset(data$CPS, !(data$Energy < Cd.L[3]-0.02 | data$Energy > Cd.L[5]+0.02))
    Cd.file <- subset(data$Sample, !(data$Energy < Cd.L[3]-0.02 | data$Energy > Cd.L[5]+0.02))
    Cd.Lb.frame <- data.frame(is.0(Cd.Lb.cps, Cd.file))
    colnames(Cd.Lb.frame) <- c("Counts", "Sample")
    Cd.Lb.ag <- aggregate(list(Cd.Lb.frame$Counts), by=list(Cd.Lb.frame$Sample), FUN="sum")
    colnames(Cd.Lb.ag) <- c("Sample", "Cd L-beta")
    
    In.Lb.cps <- subset(data$CPS, !(data$Energy < In.L[3]-0.02 | data$Energy > In.L[5]+0.02))
    In.file <- subset(data$Sample, !(data$Energy < In.L[3]-0.02 | data$Energy > In.L[5]+0.02))
    In.Lb.frame <- data.frame(is.0(In.Lb.cps, In.file))
    colnames(In.Lb.frame) <- c("Counts", "Sample")
    In.Lb.ag <- aggregate(list(In.Lb.frame$Counts), by=list(In.Lb.frame$Sample), FUN="sum")
    colnames(In.Lb.ag) <- c("Sample", "In L-beta")
    
    Sn.Lb.cps <- subset(data$CPS, !(data$Energy < Sn.L[3]-0.02 | data$Energy > Sn.L[5]+0.02))
    Sn.file <- subset(data$Sample, !(data$Energy < Sn.L[3]-0.02 | data$Energy > Sn.L[5]+0.02))
    Sn.Lb.frame <- data.frame(is.0(Sn.Lb.cps, Sn.file))
    colnames(Sn.Lb.frame) <- c("Counts", "Sample")
    Sn.Lb.ag <- aggregate(list(Sn.Lb.frame$Counts), by=list(Sn.Lb.frame$Sample), FUN="sum")
    colnames(Sn.Lb.ag) <- c("Sample", "Sn L-beta")
    
    Sb.Lb.cps <- subset(data$CPS, !(data$Energy < Sb.L[3]-0.02 | data$Energy > Sb.L[5]+0.02))
    Sb.file <- subset(data$Sample, !(data$Energy < Sb.L[3]-0.02 | data$Energy > Sb.L[5]+0.02))
    Sb.Lb.frame <- data.frame(is.0(Sb.Lb.cps, Sb.file))
    colnames(Sb.Lb.frame) <- c("Counts", "Sample")
    Sb.Lb.ag <- aggregate(list(Sb.Lb.frame$Counts), by=list(Sb.Lb.frame$Sample), FUN="sum")
    colnames(Sb.Lb.ag) <- c("Sample", "Sb L-beta")
    
    Te.Lb.cps <- subset(data$CPS, !(data$Energy < Te.L[3]-0.02 | data$Energy > Te.L[5]+0.02))
    Te.file <- subset(data$Sample, !(data$Energy < Te.L[3]-0.02 | data$Energy > Te.L[5]+0.02))
    Te.Lb.frame <- data.frame(is.0(Te.Lb.cps, Te.file))
    colnames(Te.Lb.frame) <- c("Counts", "Sample")
    Te.Lb.ag <- aggregate(list(Te.Lb.frame$Counts), by=list(Te.Lb.frame$Sample), FUN="sum")
    colnames(Te.Lb.ag) <- c("Sample", "Te L-beta")
    
    I.Lb.cps <- subset(data$CPS, !(data$Energy < I.L[3]-0.02 | data$Energy > I.L[5]+0.02))
    I.file <- subset(data$Sample, !(data$Energy < I.L[3]-0.02 | data$Energy > I.L[5]+0.02))
    I.Lb.frame <- data.frame(is.0(I.Lb.cps, I.file))
    colnames(I.Lb.frame) <- c("Counts", "Sample")
    I.Lb.ag <- aggregate(list(I.Lb.frame$Counts), by=list(I.Lb.frame$Sample), FUN="sum")
    colnames(I.Lb.ag) <- c("Sample", "I L-beta")
    
    Xe.Lb.cps <- subset(data$CPS, !(data$Energy < Xe.L[3]-0.02 | data$Energy > Xe.L[5]+0.02))
    Xe.file <- subset(data$Sample, !(data$Energy < Xe.L[3]-0.02 | data$Energy > Xe.L[5]+0.02))
    Xe.Lb.frame <- data.frame(is.0(Xe.Lb.cps, Xe.file))
    colnames(Xe.Lb.frame) <- c("Counts", "Sample")
    Xe.Lb.ag <- aggregate(list(Xe.Lb.frame$Counts), by=list(Xe.Lb.frame$Sample), FUN="sum")
    colnames(Xe.Lb.ag) <- c("Sample", "Xe L-beta")
    
    Cs.Lb.cps <- subset(data$CPS, !(data$Energy < Cs.L[3]-0.02 | data$Energy > Cs.L[5]+0.02))
    Cs.file <- subset(data$Sample, !(data$Energy < Cs.L[3]-0.02 | data$Energy > Cs.L[5]+0.02))
    Cs.Lb.frame <- data.frame(is.0(Cs.Lb.cps, Cs.file))
    colnames(Cs.Lb.frame) <- c("Counts", "Sample")
    Cs.Lb.ag <- aggregate(list(Cs.Lb.frame$Counts), by=list(Cs.Lb.frame$Sample), FUN="sum")
    colnames(Cs.Lb.ag) <- c("Sample", "Cs L-beta")
    
    Ba.Lb.cps <- subset(data$CPS, !(data$Energy < Ba.L[3]-0.02 | data$Energy > Ba.L[5]+0.02))
    Ba.file <- subset(data$Sample, !(data$Energy < Ba.L[3]-0.02 | data$Energy > Ba.L[5]+0.02))
    Ba.Lb.frame <- data.frame(is.0(Ba.Lb.cps, Ba.file))
    colnames(Ba.Lb.frame) <- c("Counts", "Sample")
    Ba.Lb.ag <- aggregate(list(Ba.Lb.frame$Counts), by=list(Ba.Lb.frame$Sample), FUN="sum")
    colnames(Ba.Lb.ag) <- c("Sample", "Ba L-beta")
    
    La.Lb.cps <- subset(data$CPS, !(data$Energy < La.L[3]-0.02 | data$Energy > La.L[5]+0.02))
    La.file <- subset(data$Sample, !(data$Energy < La.L[3]-0.02 | data$Energy > La.L[5]+0.02))
    La.Lb.frame <- data.frame(is.0(La.Lb.cps, La.file))
    colnames(La.Lb.frame) <- c("Counts", "Sample")
    La.Lb.ag <- aggregate(list(La.Lb.frame$Counts), by=list(La.Lb.frame$Sample), FUN="sum")
    colnames(La.Lb.ag) <- c("Sample", "La L-beta")
    
    Ce.Lb.cps <- subset(data$CPS, !(data$Energy < Ce.L[3]-0.02 | data$Energy > Ce.L[5]+0.02))
    Ce.file <- subset(data$Sample, !(data$Energy < Ce.L[3]-0.02 | data$Energy > Ce.L[5]+0.02))
    Ce.Lb.frame <- data.frame(is.0(Ce.Lb.cps, Ce.file))
    colnames(Ce.Lb.frame) <- c("Counts", "Sample")
    Ce.Lb.ag <- aggregate(list(Ce.Lb.frame$Counts), by=list(Ce.Lb.frame$Sample), FUN="sum")
    colnames(Ce.Lb.ag) <- c("Sample", "Ce L-beta")
    
    Pr.Lb.cps <- subset(data$CPS, !(data$Energy < Pr.L[3]-0.02 | data$Energy > Pr.L[5]+0.02))
    Pr.file <- subset(data$Sample, !(data$Energy < Pr.L[3]-0.02 | data$Energy > Pr.L[5]+0.02))
    Pr.Lb.frame <- data.frame(is.0(Pr.Lb.cps, Pr.file))
    colnames(Pr.Lb.frame) <- c("Counts", "Sample")
    Pr.Lb.ag <- aggregate(list(Pr.Lb.frame$Counts), by=list(Pr.Lb.frame$Sample), FUN="sum")
    colnames(Pr.Lb.ag) <- c("Sample", "Pr L-beta")
    
    Nd.Lb.cps <- subset(data$CPS, !(data$Energy < Nd.L[3]-0.02 | data$Energy > Nd.L[5]+0.02))
    Nd.file <- subset(data$Sample, !(data$Energy < Nd.L[3]-0.02 | data$Energy > Nd.L[5]+0.02))
    Nd.Lb.frame <- data.frame(is.0(Nd.Lb.cps, Nd.file))
    colnames(Nd.Lb.frame) <- c("Counts", "Sample")
    Nd.Lb.ag <- aggregate(list(Nd.Lb.frame$Counts), by=list(Nd.Lb.frame$Sample), FUN="sum")
    colnames(Nd.Lb.ag) <- c("Sample", "Nd L-beta")
    
    Pm.Lb.cps <- subset(data$CPS, !(data$Energy < Pm.L[3]-0.02 | data$Energy > Pm.L[5]+0.02))
    Pm.file <- subset(data$Sample, !(data$Energy < Pm.L[3]-0.02 | data$Energy > Pm.L[5]+0.02))
    Pm.Lb.frame <- data.frame(is.0(Pm.Lb.cps, Pm.file))
    colnames(Pm.Lb.frame) <- c("Counts", "Sample")
    Pm.Lb.ag <- aggregate(list(Pm.Lb.frame$Counts), by=list(Pm.Lb.frame$Sample), FUN="sum")
    colnames(Pm.Lb.ag) <- c("Sample", "Pm L-beta")
    
    Sm.Lb.cps <- subset(data$CPS, !(data$Energy < Sm.L[3]-0.02 | data$Energy > Sm.L[5]+0.02))
    Sm.file <- subset(data$Sample, !(data$Energy < Sm.L[3]-0.02 | data$Energy > Sm.L[5]+0.02))
    Sm.Lb.frame <- data.frame(is.0(Sm.Lb.cps, Sm.file))
    colnames(Sm.Lb.frame) <- c("Counts", "Sample")
    Sm.Lb.ag <- aggregate(list(Sm.Lb.frame$Counts), by=list(Sm.Lb.frame$Sample), FUN="sum")
    colnames(Sm.Lb.ag) <- c("Sample", "Sm L-beta")
    
    Eu.Lb.cps <- subset(data$CPS, !(data$Energy < Eu.L[3]-0.02 | data$Energy > Eu.L[5]+0.02))
    Eu.file <- subset(data$Sample, !(data$Energy < Eu.L[3]-0.02 | data$Energy > Eu.L[5]+0.02))
    Eu.Lb.frame <- data.frame(is.0(Eu.Lb.cps, Eu.file))
    colnames(Eu.Lb.frame) <- c("Counts", "Sample")
    Eu.Lb.ag <- aggregate(list(Eu.Lb.frame$Counts), by=list(Eu.Lb.frame$Sample), FUN="sum")
    colnames(Eu.Lb.ag) <- c("Sample", "Eu L-beta")
    
    Gd.Lb.cps <- subset(data$CPS, !(data$Energy < Gd.L[3]-0.02 | data$Energy > Gd.L[5]+0.02))
    Gd.file <- subset(data$Sample, !(data$Energy < Gd.L[3]-0.02 | data$Energy > Gd.L[5]+0.02))
    Gd.Lb.frame <- data.frame(is.0(Gd.Lb.cps, Gd.file))
    colnames(Gd.Lb.frame) <- c("Counts", "Sample")
    Gd.Lb.ag <- aggregate(list(Gd.Lb.frame$Counts), by=list(Gd.Lb.frame$Sample), FUN="sum")
    colnames(Gd.Lb.ag) <- c("Sample", "Gd L-beta")
    
    Tb.Lb.cps <- subset(data$CPS, !(data$Energy < Tb.L[3]-0.02 | data$Energy > Tb.L[5]+0.02))
    Tb.file <- subset(data$Sample, !(data$Energy < Tb.L[3]-0.02 | data$Energy > Tb.L[5]+0.02))
    Tb.Lb.frame <- data.frame(is.0(Tb.Lb.cps, Tb.file))
    colnames(Tb.Lb.frame) <- c("Counts", "Sample")
    Tb.Lb.ag <- aggregate(list(Tb.Lb.frame$Counts), by=list(Tb.Lb.frame$Sample), FUN="sum")
    colnames(Tb.Lb.ag) <- c("Sample", "Tb L-beta")
    
    Dy.Lb.cps <- subset(data$CPS, !(data$Energy < Dy.L[3]-0.02 | data$Energy > Dy.L[5]+0.02))
    Dy.file <- subset(data$Sample, !(data$Energy < Dy.L[3]-0.02 | data$Energy > Dy.L[5]+0.02))
    Dy.Lb.frame <- data.frame(is.0(Dy.Lb.cps, Dy.file))
    colnames(Dy.Lb.frame) <- c("Counts", "Sample")
    Dy.Lb.ag <- aggregate(list(Dy.Lb.frame$Counts), by=list(Dy.Lb.frame$Sample), FUN="sum")
    colnames(Dy.Lb.ag) <- c("Sample", "Dy L-beta")
    
    Ho.Lb.cps <- subset(data$CPS, !(data$Energy < Ho.L[3]-0.02 | data$Energy > Ho.L[5]+0.02))
    Ho.file <- subset(data$Sample, !(data$Energy < Ho.L[3]-0.02 | data$Energy > Ho.L[5]+0.02))
    Ho.Lb.frame <- data.frame(is.0(Ho.Lb.cps, Ho.file))
    colnames(Ho.Lb.frame) <- c("Counts", "Sample")
    Ho.Lb.ag <- aggregate(list(Ho.Lb.frame$Counts), by=list(Ho.Lb.frame$Sample), FUN="sum")
    colnames(Ho.Lb.ag) <- c("Sample", "Ho L-beta")
    
    Er.Lb.cps <- subset(data$CPS, !(data$Energy < Er.L[3]-0.02 | data$Energy > Er.L[5]+0.02))
    Er.file <- subset(data$Sample, !(data$Energy < Er.L[3]-0.02 | data$Energy > Er.L[5]+0.02))
    Er.Lb.frame <- data.frame(is.0(Er.Lb.cps, Er.file))
    colnames(Er.Lb.frame) <- c("Counts", "Sample")
    Er.Lb.ag <- aggregate(list(Er.Lb.frame$Counts), by=list(Er.Lb.frame$Sample), FUN="sum")
    colnames(Er.Lb.ag) <- c("Sample", "Er L-beta")
    
    Tm.Lb.cps <- subset(data$CPS, !(data$Energy < Tm.L[3]-0.02 | data$Energy > Tm.L[5]+0.02))
    Tm.file <- subset(data$Sample, !(data$Energy < Tm.L[3]-0.02 | data$Energy > Tm.L[5]+0.02))
    Tm.Lb.frame <- data.frame(is.0(Tm.Lb.cps, Tm.file))
    colnames(Tm.Lb.frame) <- c("Counts", "Sample")
    Tm.Lb.ag <- aggregate(list(Tm.Lb.frame$Counts), by=list(Tm.Lb.frame$Sample), FUN="sum")
    colnames(Tm.Lb.ag) <- c("Sample", "Tm L-beta")
    
    Yb.Lb.cps <- subset(data$CPS, !(data$Energy < Yb.L[3]-0.02 | data$Energy > Yb.L[5]+0.02))
    Yb.file <- subset(data$Sample, !(data$Energy < Yb.L[3]-0.02 | data$Energy > Yb.L[5]+0.02))
    Yb.Lb.frame <- data.frame(is.0(Yb.Lb.cps, Yb.file))
    colnames(Yb.Lb.frame) <- c("Counts", "Sample")
    Yb.Lb.ag <- aggregate(list(Yb.Lb.frame$Counts), by=list(Yb.Lb.frame$Sample), FUN="sum")
    colnames(Yb.Lb.ag) <- c("Sample", "Yb L-beta")
    
    Lu.Lb.cps <- subset(data$CPS, !(data$Energy < Lu.L[3]-0.02 | data$Energy > Lu.L[5]+0.02))
    Lu.file <- subset(data$Sample, !(data$Energy < Lu.L[3]-0.02 | data$Energy > Lu.L[5]+0.02))
    Lu.Lb.frame <- data.frame(is.0(Lu.Lb.cps, Lu.file))
    colnames(Lu.Lb.frame) <- c("Counts", "Sample")
    Lu.Lb.ag <- aggregate(list(Lu.Lb.frame$Counts), by=list(Lu.Lb.frame$Sample), FUN="sum")
    colnames(Lu.Lb.ag) <- c("Sample", "Lu L-beta")
    
    Hf.Lb.cps <- subset(data$CPS, !(data$Energy < Hf.L[3]-0.02 | data$Energy > Hf.L[5]+0.02))
    Hf.file <- subset(data$Sample, !(data$Energy < Hf.L[3]-0.02 | data$Energy > Hf.L[5]+0.02))
    Hf.Lb.frame <- data.frame(is.0(Hf.Lb.cps, Hf.file))
    colnames(Hf.Lb.frame) <- c("Counts", "Sample")
    Hf.Lb.ag <- aggregate(list(Hf.Lb.frame$Counts), by=list(Hf.Lb.frame$Sample), FUN="sum")
    colnames(Hf.Lb.ag) <- c("Sample", "Hf L-beta")
    
    Ta.Lb.cps <- subset(data$CPS, !(data$Energy < Ta.L[3]-0.02 | data$Energy > Ta.L[5]+0.02))
    Ta.file <- subset(data$Sample, !(data$Energy < Ta.L[3]-0.02 | data$Energy > Ta.L[5]+0.02))
    Ta.Lb.frame <- data.frame(is.0(Ta.Lb.cps, Ta.file))
    colnames(Ta.Lb.frame) <- c("Counts", "Sample")
    Ta.Lb.ag <- aggregate(list(Ta.Lb.frame$Counts), by=list(Ta.Lb.frame$Sample), FUN="sum")
    colnames(Ta.Lb.ag) <- c("Sample", "Ta L-beta")
    
    W.Lb.cps <- subset(data$CPS, !(data$Energy < W.L[3]-0.02 | data$Energy > W.L[5]+0.02))
    W.file <- subset(data$Sample, !(data$Energy < W.L[3]-0.02 | data$Energy > W.L[5]+0.02))
    W.Lb.frame <- data.frame(is.0(W.Lb.cps, W.file))
    colnames(W.Lb.frame) <- c("Counts", "Sample")
    W.Lb.ag <- aggregate(list(W.Lb.frame$Counts), by=list(W.Lb.frame$Sample), FUN="sum")
    colnames(W.Lb.ag) <- c("Sample", "W L-beta")
    
    Re.Lb.cps <- subset(data$CPS, !(data$Energy < Re.L[3]-0.02 | data$Energy > Re.L[5]+0.02))
    Re.file <- subset(data$Sample, !(data$Energy < Re.L[3]-0.02 | data$Energy > Re.L[5]+0.02))
    Re.Lb.frame <- data.frame(is.0(Re.Lb.cps, Re.file))
    colnames(Re.Lb.frame) <- c("Counts", "Sample")
    Re.Lb.ag <- aggregate(list(Re.Lb.frame$Counts), by=list(Re.Lb.frame$Sample), FUN="sum")
    colnames(Re.Lb.ag) <- c("Sample", "Re L-beta")
    
    Os.Lb.cps <- subset(data$CPS, !(data$Energy < Os.L[3]-0.02 | data$Energy > Os.L[5]+0.02))
    Os.file <- subset(data$Sample, !(data$Energy < Os.L[3]-0.02 | data$Energy > Os.L[5]+0.02))
    Os.Lb.frame <- data.frame(is.0(Os.Lb.cps, Os.file))
    colnames(Os.Lb.frame) <- c("Counts", "Sample")
    Os.Lb.ag <- aggregate(list(Os.Lb.frame$Counts), by=list(Os.Lb.frame$Sample), FUN="sum")
    colnames(Os.Lb.ag) <- c("Sample", "Os L-beta")
    
    Ir.Lb.cps <- subset(data$CPS, !(data$Energy < Ir.L[3]-0.02 | data$Energy > Ir.L[5]+0.02))
    Ir.file <- subset(data$Sample, !(data$Energy < Ir.L[3]-0.02 | data$Energy > Ir.L[5]+0.02))
    Ir.Lb.frame <- data.frame(is.0(Ir.Lb.cps, Ir.file))
    colnames(Ir.Lb.frame) <- c("Counts", "Sample")
    Ir.Lb.ag <- aggregate(list(Ir.Lb.frame$Counts), by=list(Ir.Lb.frame$Sample), FUN="sum")
    colnames(Ir.Lb.ag) <- c("Sample", "Ir L-beta")
    
    Pt.Lb.cps <- subset(data$CPS, !(data$Energy < Pt.L[3]-0.02 | data$Energy > Pt.L[5]+0.02))
    Pt.file <- subset(data$Sample, !(data$Energy < Pt.L[3]-0.02 | data$Energy > Pt.L[5]+0.02))
    Pt.Lb.frame <- data.frame(is.0(Pt.Lb.cps, Pt.file))
    colnames(Pt.Lb.frame) <- c("Counts", "Sample")
    Pt.Lb.ag <- aggregate(list(Pt.Lb.frame$Counts), by=list(Pt.Lb.frame$Sample), FUN="sum")
    colnames(Pt.Lb.ag) <- c("Sample", "Pt L-beta")
    
    Au.Lb.cps <- subset(data$CPS, !(data$Energy < Au.L[3]-0.02 | data$Energy > Au.L[5]+0.02))
    Au.file <- subset(data$Sample, !(data$Energy < Au.L[3]-0.02 | data$Energy > Au.L[5]+0.02))
    Au.Lb.frame <- data.frame(is.0(Au.Lb.cps, Au.file))
    colnames(Au.Lb.frame) <- c("Counts", "Sample")
    Au.Lb.ag <- aggregate(list(Au.Lb.frame$Counts), by=list(Au.Lb.frame$Sample), FUN="sum")
    colnames(Au.Lb.ag) <- c("Sample", "Au L-beta")
    
    Hg.Lb.cps <- subset(data$CPS, !(data$Energy < Hg.L[3]-0.02 | data$Energy > Hg.L[5]+0.02))
    Hg.file <- subset(data$Sample, !(data$Energy < Hg.L[3]-0.02 | data$Energy > Hg.L[5]+0.02))
    Hg.Lb.frame <- data.frame(is.0(Hg.Lb.cps, Hg.file))
    colnames(Hg.Lb.frame) <- c("Counts", "Sample")
    Hg.Lb.ag <- aggregate(list(Hg.Lb.frame$Counts), by=list(Hg.Lb.frame$Sample), FUN="sum")
    colnames(Hg.Lb.ag) <- c("Sample", "Hg L-beta")
    
    Tl.Lb.cps <- subset(data$CPS, !(data$Energy < Tl.L[3]-0.02 | data$Energy > Tl.L[5]+0.02))
    Tl.file <- subset(data$Sample, !(data$Energy < Tl.L[3]-0.02 | data$Energy > Tl.L[5]+0.02))
    Tl.Lb.frame <- data.frame(is.0(Tl.Lb.cps, Tl.file))
    colnames(Tl.Lb.frame) <- c("Counts", "Sample")
    Tl.Lb.ag <- aggregate(list(Tl.Lb.frame$Counts), by=list(Tl.Lb.frame$Sample), FUN="sum")
    colnames(Tl.Lb.ag) <- c("Sample", "Tl L-beta")
    
    Pb.Lb.cps <- subset(data$CPS, !(data$Energy < Pb.L[3]-0.02 | data$Energy > Pb.L[5]+0.02))
    Pb.file <- subset(data$Sample, !(data$Energy < Pb.L[3]-0.02 | data$Energy > Pb.L[5]+0.02))
    Pb.Lb.frame <- data.frame(is.0(Pb.Lb.cps, Pb.file))
    colnames(Pb.Lb.frame) <- c("Counts", "Sample")
    Pb.Lb.ag <- aggregate(list(Pb.Lb.frame$Counts), by=list(Pb.Lb.frame$Sample), FUN="sum")
    colnames(Pb.Lb.ag) <- c("Sample", "Pb L-beta")
    
    Bi.Lb.cps <- subset(data$CPS, !(data$Energy < Bi.L[3]-0.02 | data$Energy > Bi.L[5]+0.02))
    Bi.file <- subset(data$Sample, !(data$Energy < Bi.L[3]-0.02 | data$Energy > Bi.L[5]+0.02))
    Bi.Lb.frame <- data.frame(is.0(Bi.Lb.cps, Bi.file))
    colnames(Bi.Lb.frame) <- c("Counts", "Sample")
    Bi.Lb.ag <- aggregate(list(Bi.Lb.frame$Counts), by=list(Bi.Lb.frame$Sample), FUN="sum")
    colnames(Bi.Lb.ag) <- c("Sample", "Bi L-beta")
    
    Po.Lb.cps <- subset(data$CPS, !(data$Energy < Po.L[3]-0.02 | data$Energy > Po.L[5]+0.02))
    Po.file <- subset(data$Sample, !(data$Energy < Po.L[3]-0.02 | data$Energy > Po.L[5]+0.02))
    Po.Lb.frame <- data.frame(is.0(Po.Lb.cps, Po.file))
    colnames(Po.Lb.frame) <- c("Counts", "Sample")
    Po.Lb.ag <- aggregate(list(Po.Lb.frame$Counts), by=list(Po.Lb.frame$Sample), FUN="sum")
    colnames(Po.Lb.ag) <- c("Sample", "Po L-beta")
    
    At.Lb.cps <- subset(data$CPS, !(data$Energy < At.L[3]-0.02 | data$Energy > At.L[5]+0.02))
    At.file <- subset(data$Sample, !(data$Energy < At.L[3]-0.02 | data$Energy > At.L[5]+0.02))
    At.Lb.frame <- data.frame(is.0(At.Lb.cps, At.file))
    colnames(At.Lb.frame) <- c("Counts", "Sample")
    At.Lb.ag <- aggregate(list(At.Lb.frame$Counts), by=list(At.Lb.frame$Sample), FUN="sum")
    colnames(At.Lb.ag) <- c("Sample", "At L-beta")
    
    Rn.Lb.cps <- subset(data$CPS, !(data$Energy < Rn.L[3]-0.02 | data$Energy > Rn.L[5]+0.02))
    Rn.file <- subset(data$Sample, !(data$Energy < Rn.L[3]-0.02 | data$Energy > Rn.L[5]+0.02))
    Rn.Lb.frame <- data.frame(is.0(Rn.Lb.cps, Rn.file))
    colnames(Rn.Lb.frame) <- c("Counts", "Sample")
    Rn.Lb.ag <- aggregate(list(Rn.Lb.frame$Counts), by=list(Rn.Lb.frame$Sample), FUN="sum")
    colnames(Rn.Lb.ag) <- c("Sample", "Rn L-beta")
    
    Fr.Lb.cps <- subset(data$CPS, !(data$Energy < Fr.L[3]-0.02 | data$Energy > Fr.L[5]+0.02))
    Fr.file <- subset(data$Sample, !(data$Energy < Fr.L[3]-0.02 | data$Energy > Fr.L[5]+0.02))
    Fr.Lb.frame <- data.frame(is.0(Fr.Lb.cps, Fr.file))
    colnames(Fr.Lb.frame) <- c("Counts", "Sample")
    Fr.Lb.ag <- aggregate(list(Fr.Lb.frame$Counts), by=list(Fr.Lb.frame$Sample), FUN="sum")
    colnames(Fr.Lb.ag) <- c("Sample", "Fr L-beta")
    
    Ra.Lb.cps <- subset(data$CPS, !(data$Energy < Ra.L[3]-0.02 | data$Energy > Ra.L[5]+0.02))
    Ra.file <- subset(data$Sample, !(data$Energy < Ra.L[3]-0.02 | data$Energy > Ra.L[5]+0.02))
    Ra.Lb.frame <- data.frame(is.0(Ra.Lb.cps, Ra.file))
    colnames(Ra.Lb.frame) <- c("Counts", "Sample")
    Ra.Lb.ag <- aggregate(list(Ra.Lb.frame$Counts), by=list(Ra.Lb.frame$Sample), FUN="sum")
    colnames(Ra.Lb.ag) <- c("Sample", "Ra L-beta")
    
    Ac.Lb.cps <- subset(data$CPS, !(data$Energy < Ac.L[3]-0.02 | data$Energy > Ac.L[5]+0.02))
    Ac.file <- subset(data$Sample, !(data$Energy < Ac.L[3]-0.02 | data$Energy > Ac.L[5]+0.02))
    Ac.Lb.frame <- data.frame(is.0(Ac.Lb.cps, Ac.file))
    colnames(Ac.Lb.frame) <- c("Counts", "Sample")
    Ac.Lb.ag <- aggregate(list(Ac.Lb.frame$Counts), by=list(Ac.Lb.frame$Sample), FUN="sum")
    colnames(Ac.Lb.ag) <- c("Sample", "Ac L-beta")
    
    Th.Lb.cps <- subset(data$CPS, !(data$Energy < Th.L[3]-0.02 | data$Energy > Th.L[5]+0.02))
    Th.file <- subset(data$Sample, !(data$Energy < Th.L[3]-0.02 | data$Energy > Th.L[5]+0.02))
    Th.Lb.frame <- data.frame(is.0(Th.Lb.cps, Th.file))
    colnames(Th.Lb.frame) <- c("Counts", "Sample")
    Th.Lb.ag <- aggregate(list(Th.Lb.frame$Counts), by=list(Th.Lb.frame$Sample), FUN="sum")
    colnames(Th.Lb.ag) <- c("Sample", "Th L-beta")
    
    Pa.Lb.cps <- subset(data$CPS, !(data$Energy < Pa.L[3]-0.02 | data$Energy > Pa.L[5]+0.02))
    Pa.file <- subset(data$Sample, !(data$Energy < Pa.L[3]-0.02 | data$Energy > Pa.L[5]+0.02))
    Pa.Lb.frame <- data.frame(is.0(Pa.Lb.cps, Pa.file))
    colnames(Pa.Lb.frame) <- c("Counts", "Sample")
    Pa.Lb.ag <- aggregate(list(Pa.Lb.frame$Counts), by=list(Pa.Lb.frame$Sample), FUN="sum")
    colnames(Pa.Lb.ag) <- c("Sample", "Pa L-beta")
    
    U.Lb.cps <- subset(data$CPS, !(data$Energy < U.L[3]-0.02 | data$Energy > U.L[5]+0.02))
    U.file <- subset(data$Sample, !(data$Energy < U.L[3]-0.02 | data$Energy > U.L[5]+0.02))
    U.Lb.frame <- data.frame(is.0(U.Lb.cps, U.file))
    colnames(U.Lb.frame) <- c("Counts", "Sample")
    U.Lb.ag <- aggregate(list(U.Lb.frame$Counts), by=list(U.Lb.frame$Sample), FUN="sum")
    colnames(U.Lb.ag) <- c("Sample", "U L-beta")

spectra.lines <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Ne.Ka.ag, Na.Ka.ag, Mg.Ka.ag, Al.Ka.ag, Si.Ka.ag, P.Ka.ag, S.Ka.ag, Cl.Ka.ag, Ar.Ka.ag, K.Ka.ag, Ca.Ka.ag, Sc.Ka.ag, Ti.Ka.ag, V.Ka.ag, Cr.Ka.ag, Mn.Ka.ag, Fe.Ka.ag, Co.Ka.ag, Ni.Ka.ag, Cu.Ka.ag, Zn.Ka.ag, Ga.Ka.ag, Ge.Ka.ag, As.Ka.ag, Se.Ka.ag, Br.Ka.ag, Kr.Ka.ag, Rb.Ka.ag, Sr.Ka.ag, Y.Ka.ag, Zr.Ka.ag, Nb.Ka.ag, Mo.Ka.ag, Mo.La.ag, Mo.Lb.ag, Ru.Ka.ag, Ru.La.ag, Ru.Lb.ag, Rh.Ka.ag, Rh.La.ag, Rh.Lb.ag, Pd.Ka.ag, Pd.La.ag, Pd.Lb.ag, Ag.Ka.ag, Ag.La.ag, Ag.Lb.ag, Cd.Ka.ag,Cd.La.ag, Cd.Lb.ag,  In.Ka.ag, In.La.ag, Sn.Ka.ag, Sn.La.ag, Sn.Lb.ag, Sb.Ka.ag, Sb.La.ag, Sb.Lb.ag, Te.Ka.ag, Te.La.ag, Te.Lb.ag, I.Ka.ag, I.La.ag, I.Lb.ag, Xe.Ka.ag, Xe.La.ag, Xe.Lb.ag, Cs.Ka.ag, Cs.La.ag, Cs.Lb.ag, Ba.Ka.ag, Ba.La.ag, Ba.Lb.ag, La.Ka.ag, La.La.ag, La.Lb.ag, Ce.Ka.ag, Ce.La.ag, Ce.Lb.ag, Pr.Ka.ag, Pr.La.ag, Pr.Lb.ag, Nd.Ka.ag, Nd.La.ag, Nd.Lb.ag, Pm.La.ag, Pm.Lb.ag, Sm.La.ag, Sm.Lb.ag, Eu.La.ag, Eu.Lb.ag, Gd.La.ag, Gd.Lb.ag, Tb.La.ag, Tb.Lb.ag, Dy.La.ag, Dy.Lb.ag, Ho.La.ag, Ho.Lb.ag, Er.La.ag, Er.Lb.ag, Tm.La.ag, Tm.Lb.ag, Yb.La.ag, Yb.Lb.ag, Lu.La.ag, Lu.Lb.ag, Hf.La.ag, Hf.Lb.ag, Ta.La.ag, Ta.Lb.ag, W.La.ag, W.Lb.ag, Re.La.ag, Re.Lb.ag, Os.La.ag, Os.Lb.ag, Ir.La.ag, Ir.Lb.ag, Pt.La.ag, Pt.Lb.ag, Au.La.ag, Au.Lb.ag, Hg.La.ag, Hg.Lb.ag, Tl.La.ag, Tl.Lb.ag, Pb.La.ag, Pb.Lb.ag, Bi.La.ag, Bi.Lb.ag, Po.La.ag, Po.Lb.ag, At.La.ag, At.Lb.ag, Rn.La.ag, Rn.Lb.ag, Fr.La.ag, Fr.Lb.ag, Ra.La.ag, Ra.Lb.ag, Ac.La.ag, Ac.Lb.ag, Th.La.ag, Th.Lb.ag, Pa.La.ag, Pa.Lb.ag, U.La.ag, U.Lb.ag))

spectra.lines <- data.frame(spectra.lines)
return(spectra.lines)

}

data <- NULL
if (is.null(data)){ data <- black.diamond.melt}


spectra.line.table <- spectra.line.fn(data)
spectra.line.table
 unique.spec <- seq(1, length(spectra.line.table$Sample), 1)

null <- rep(1, length(spectra.line.table$Sample))

spectra.line.table.norm <- data.frame(null, spectra.line.table)
colnames(spectra.line.table.norm) <- c("None", names(spectra.line.table))
spectra.line.table.norm




standard <- c("Sample", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Pb.L.alpha")






spectralLines <- c("Ne.K.alpha", "Ne.K.beta", "Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Rh.L.alpha", "Rh.L.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", " In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "Ba.L.alpha", "Ba.L.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta")


spectralLinesLight <- c("Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "Rh.L.alpha", "Rh.L.Beta", "Ba.L.alpha", "Ba.L.beta")

spectralLinesTrace <- c("V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", " In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta")



elementGrabKalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.file <- subset(data$Sample, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Sample")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Sample), FUN="sum")
    colnames(hold.ag) <- c("Sample", paste(element, "K-alpha", sep=" "))
    
    hold.ag
    
}

elementGrabKbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    hold.file <- subset(data$Sample, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Sample")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Sample), FUN="sum")
    colnames(hold.ag) <- c("Sample", paste(element, "K-beta", sep=" "))
    
    hold.ag
    
}

elementGrabLalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][1,]+0.02))
    hold.file <- subset(data$Sample, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][,1]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Sample")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Sample), FUN="sum")
    colnames(hold.ag) <- c("Sample", paste(element, "L-alpha", sep=" "))
    
    hold.ag
    
}

elementGrabLbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))
    hold.file <- subset(data$Sample, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))
    
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Sample")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Sample), FUN="sum")
    colnames(hold.ag) <- c("Sample", paste(element, "L-beta", sep=" "))
    
    hold.ag
    
}

elementGrab <- function(element.line, data) {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    elementSelection <- if(destination=="K" && distance=="alpha"){
        elementGrabKalpha(element, data)
    } else if(destination=="K" && distance=="beta"){
        elementGrabKbeta(element, data)
    } else if(destination=="L" && distance=="alpha"){
        elementGrabLalpha(element, data)
    } else if (destination=="L" && distance=="beta"){
        elementGrabLbeta(element, data)
    }
    
    elementSelection
    
}


###############
###Prep Data###
###############


###############
###Raw Spectra##
###############


general.prep <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Sample, data=data, sum)
    colnames(total.counts) <- c("Sample", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Sample, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Sample")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Sample), FUN="sum")
    colnames(compton.frame.ag) <- c("Sample", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.frame.ag$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lukas.simp.prep <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept)-lukas.intercept/(intensity+lukas.intercept))),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Sample, data=data, sum)
    colnames(total.counts) <- c("Sample", "CPS")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    
    predict.intensity.luk.tc <- data.frame(((1+intensity/(intensity+lukas.intercept.tc)-lukas.intercept.tc/(intensity+lukas.intercept.tc))),lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    predict.intensity.luk.tc
}





lukas.comp.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Sample, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Sample")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Sample), FUN="sum")
    colnames(compton.frame.ag) <- c("Sample", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE)/compton.frame.ag$Compton)
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines]/compton.frame.ag$Compton)
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luk.comp <- data.frame(((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)-lukas.intercept.comp/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}




###############
###Prep Data###
###############


###############
###Net Counts##
###############


general.prep.net <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep.net <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
    total.counts <- data.frame(data$Sample, total.counts.net)
    colnames(total.counts) <- c("Sample", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep.net <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.ag.fake.Spectrum <- data$Sample
    compton.ag.fake.Compton <- rep(1, length(data$Sample))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Sample", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.ag.fake$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lukas.simp.prep.net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
    total.counts <- data.frame(data$Sample, total.counts.net)
    colnames(total.counts) <- c("Sample", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    predict.intensity.luk.tc <- data.frame(((1+intensity/(intensity+lukas.intercept.tc)-lukas.intercept.tc/(intensity+lukas.intercept.tc))),lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    predict.intensity.luk.tc
}


lukas.comp.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Sample
    compton.ag.fake.Compton <- rep(1, length(data$Sample))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Sample", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")], na.rm = TRUE))/compton.ag.fake$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.ag.fake$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    
    predict.frame.luk.comp <- data.frame(((1+predict.frame.comp$Intensity/(predict.frame.comp$Intensity+lukas.intercept.comp)-lukas.intercept.comp/(predict.frame.comp$Intensity+lukas.intercept.comp))),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}


####Function to organize plots in a window
layOut = function(...) {
    
    require(grid)
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
        layout.pos.col = x[[i]][[3]]))
    }
}


###Write Equation
lm_eqn = function(m) {
    
    a = format(coef(m)[1], digits = 2)
    b = format(abs(coef(m)[2]), digits = 2)
    
    eq <- substitute(italic(C)[i] == a + b %.% italic(I)[i])
    
    
    as.character(as.expression(eq));
}


lm.dat <- function (formula, data, subset, weights, na.action, method = "qr",
model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
contrasts = NULL, offset, ...)
{
    dat.fram <- data.frame(x, y)
    dat.fram <- dat.fram[complete.cases(dat.fram),]
    x <- dat.fram$x
    y <- dat.fram$y
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
    "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame")
    return(mf)
    else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
    method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y))
        stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
        length(offset), NROW(y)), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0,
        3) else numeric(), residuals = y, fitted.values = 0 *
        y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
        0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w))
        lm.fit(x, y, offset = offset, singular.ok = singular.ok,
        ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
        ...)
    }
    class(z) <- c(if (is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model)
    z$model <- mf
    if (ret.x)
    z$x <- x
    if (ret.y)
    z$y <- y
    if (!qr)
    z$qr <- NULL
    z
}

unlist.tree <- function(temp, myfiles){
    n <- length(temp)
    for (i in n) {
        temp[i] <- myfiles[[i]]
    }
}

ig.na <- function(x) {
    length(na.omit(x))
}

nonNAs <- function(x) {
    n <- as.vector(apply(x, 1, function(x) length(which(!is.na(x)))))
    return(n)
}

readRWL.simp <- function(file) {
    raw <- read.rwl(file)
    years <- rownames(raw)
    non.total <- data.frame(years, raw)
    colnames(non.total)[1] <- "Year"
    return(non.total)
}

readRWLArima <- function(file) {
    raw <- read.rwl(file)
    raw <- read.rwl(file)
    years <- rownames(raw)
    detrended <- data.frame(detrend(raw, method="Spline", nyrs=50))
    list.arima <- pbapply(X=detrended, MARGIN=2, FUN=auto.arima)
    arima.data <- data.frame(subListExtract(L=list.arima, name="residuals"))
    arima.total <- data.frame(years, arima.data)
    colnames(arima.total)[1] <- "Year"
    return(arima.total)
}

readDataArima <- function(file) {
    raw <- read.csv(file)
    n <- length(raw)
    years <- raw[,1]
    data <- raw[,2:n]
    detrended <- data.frame(detrend(data, method="Spline", nyrs=50))
    arima.data <- pbapply(X=detrended, MARGIN=2, function(x) FUN=auto.arima(x)$residuals)
    arima.total <- data.frame(years, arima.data)
    colnames(arima.total)[1] <- "Year"
    return(arima.total)
}


readDataArimaFit <- function(file) {
    raw <- read.csv(file)
    n <- length(raw)
    years <- raw[,1]
    data <- raw[,2:n]
    detrended <- data.frame(detrend(data, method="Spline", nyrs=50))
    list.arima <- pbapply(X=detrended, MARGIN=2, function(x) FUN=fitted(auto.arima(x)))
    arima.total <- data.frame(years, list.arima)
    colnames(arima.total)[1] <- "Year"
    return(arima.total)
}


readDataArima4 <- function(file) {
    raw <- read.csv(file)
    n <- length(raw)
    years <- raw[,1]
    data <- raw[,2:n]
    detrended <- data.frame(detrend(data, method="Spline"))
    list.arima <- pbapply(X=detrended, MARGIN=2, FUN=auto.arima)
    arima.data <- data.frame(subListExtract(L=list.arima, name="x"))
    arima.total <- data.frame(years, arima.data)
    colnames(arima.total)[1] <- "Year"
    return(arima.total)
}


meanSequence <- function(tree.dataframe, name) {
    n <- length(tree.dataframe)
    sequ <- rowMeans(tree.dataframe[2:n], na.rm=TRUE)
    results.frame <- data.frame(as.numeric(as.vector(tree.dataframe$Year)), sequ)
    colnames(results.frame) <- c("Year", name)
    return(results.frame)
}

treeHypothesis <- function(time, tree.dataframe.1, tree.dataframe.2) {
    
    
    
    tree.a <- tree.dataframe.1[match(time, tree.dataframe.1$Year, nomatch=0),]
    tree.b <- tree.dataframe.2[match(time, tree.dataframe.2$Year, nomatch=0),]
    
    
    
    df <- data.frame(time, tree.a$Mean, tree.b$Mean, tree.a$SD, tree.b$SD, tree.a$N, tree.b$N)
    colnames(df) <- c("Year", "FirstMean", "SecondMean", "FirstSD", "SecondSD", "FirstN", "SecondN")
    
    
    df$Ttest <- c(abs(df$FirstMean-df$SecondMean)/(sqrt((df$FirstSD^2)/df$FirstN + (df$SecondSD^2)/df$SecondN)))
    
    
    df$DF <- c(((((df$FirstSD^2)/df$FirstN) +  ((df$SecondSD^2)/df$SecondN))^2)/((df$FirstSD^4)/((df$FirstN^2)*(df$FirstN-1)) + (df$SecondSD^4)/((df$SecondN^2)*(df$SecondN-1))))
    
    
    df$pvalue <- c((2*pt(df$Ttest, df$DF, lower=FALSE)))
    df$Significant <- rep("Yes", length(df$Year))
    df <- transform(df, Significant = ifelse(pvalue > 0.05, "No", Significant))
    
    
    return(df)
}

sigCount <- function(tree.hypothesis.test.results) {
    Yes <- subset(tree.hypothesis.test.results$pvalue, tree.hypothesis.test.results$Significant=="Yes")
    No <- subset(tree.hypothesis.test.results$pvalue, tree.hypothesis.test.results$Significant=="No")
    
    Yess <- length(Yes)
    Nos <- length(No)
    
    results <- data.frame(mean(Yes), mean(No), Yess, Nos)
    colnames(results) <- c("p-value diff", "p-value same", "p < 0.05", "p > 0.05")
    return(results)
    
}


treeCorTest <- function(time, tree.object, tree.source){
    
    
    
    tree.a <- tree.object[match(time, tree.object$Year, nomatch=0),]
    tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
    
    tree.a <- tree.a[complete.cases(tree.a), ]
    tree.b <- tree.b[complete.cases(tree.b), ]
    
    tree.a.arima <-arima(tree.a[,2], order=c(1, 0 ,1))
    tree.a.n <- tree.a.arima$residuals
    
    #tree.b.arima <-arima(tree.b[,2], order=c(1, 0 ,1))
    #tree.b.n <- tree.b.arima$residuals
    
    tree.a.frame <- data.frame(tree.a$Year, tree.a.n)
    colnames(tree.a.frame) <- c("Year", "A")
    tree.b.frame <- data.frame(tree.b$Year, tree.b[,2])
    colnames(tree.b.frame) <- c("Year", "B")
    
    tree.a.re <- tree.a.frame$A[tree.a.frame$Year %in% tree.b$Year]
    tree.b.re <- tree.b.frame$B[tree.b.frame$Year %in% tree.a$Year]
    
    
    
    trees.grid <- data.frame(tree.a.re, tree.b.re)
    colnames(trees.grid) <- c("First", "Second")
    
    
    trees.lm <- lm(trees.grid$First~trees.grid$Second)
    trees.s.lm <- summary(trees.lm)
    
    turn.to.t <- function(x.lm) {
        x.s.lm <- summary(x.lm)
        r.sq <- x.s.lm$r.squared
        just.r <- sqrt(r.sq)
        t <- (just.r*sqrt(length(x.lm$residuals)-2))/sqrt(1-r.sq)
        return(t)
    }
    
    
    trees.t <- turn.to.t(trees.lm)
    
    result.frame <- data.frame(trees.t,sqrt(trees.s.lm$r.squared), length(trees.lm$residuals))
    colnames(result.frame) <- c("t", "r", "overlap")
    return(result.frame)
}


treeCorTestMultiple <- function(time, tree.object, tree.sources){
    
    tree.sources.n <- length(tree.sources)
    
    tree.a <- tree.object[match(time, tree.object$Year, nomatch=0),]
    tree.b <- tree.sources[match(time, tree.sources$Year, nomatch=0),]
    
    tree.a <- tree.a[complete.cases(tree.a), ]
    tree.b <- tree.b[complete.cases(tree.b), ]
    
    tree.a.arima <-arima(tree.a[,2], order=c(1, 0 ,1))
    tree.a.n <- tree.a.arima$residuals
    
    #tree.b.arima <-arima(tree.b[,2], order=c(1, 0 ,1))
    #tree.b.n <- tree.b.arima$residuals
    
    tree.a.frame <- data.frame(tree.a$Year, tree.a.n)
    colnames(tree.a.frame) <- c("Year", "A")
    tree.b.frame <- tree.b
    
    tree.a.re <- tree.a.frame$A[tree.a.frame$Year %in% tree.b$Year]
    tree.b.re <- semi_join(tree.b.frame, tree.a.frame, by="Year")
    
    
    tree.sources.frame <- tree.b.re[2:tree.sources.n]
    colnames(tree.sources.frame) <- names(tree.sources[2:tree.sources.n])
    tree.total.frame <- data.frame(tree.a.re, tree.sources.frame)
    colnames(tree.total.frame) <- c("to.test", names(tree.sources.frame))
    
    trees.r2 <- apply(tree.sources.frame, 2, function(x) summary(lm(x~tree.a.re))$r.squared)
    trees.n <- apply(tree.sources.frame, 2, function(x) length(summary(lm(x~tree.a.re))$residuals))
    
    
    turn.to.t <- function(trees.rsquared, trees.residual.n) {
        x.s.lm <- summary(x.lm)
        r.sq <- x.s.lm$r.squared
        just.r <- sqrt(r.sq)
        t <- (just.r*sqrt(length(x.lm$residuals)-2))/sqrt(1-r.sq)
        return(t)
    }
    
    trees.t <- sqrt(trees.r2)*sqrt(trees.n-2)/sqrt(1-trees.r2)
    trees.r <- sqrt(trees.r2)
    
    result.frame <- data.frame(trees.t, trees.r, trees.n)
    colnames(result.frame) <- c("t", "r", "overlap")
    return(format(result.frame, digits=3))
}


treeJackKnife <- function(time,  tree.dataframe, tree.source) {
    
    
    
    tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
    tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
    
    tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
    
    samp.n <- length(names(tree.a.mod))
    tree.names <- names(tree.a.mod[2:samp.n])
    
    tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
    tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
    
    tree.a.re.re <- tree.a.re[2:samp.n]
    
    source <- tree.b.re[,2]
    
    
    n <- length(ls(tree.a.re))
    
    
    group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
    
    group.lm.r <- sqrt(group.lm.r2)
    
    group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
    
    group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
    
    
    result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
    colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
    return(format(result.frame, digits=3))
    
    
}



obsidianJackKnifeMultipleSourceSig <- function(time, tree.dataframe, tree.source.list) {
    
    
    
    treeJackKnife <- function(time,  tree.dataframe, tree.source) {
        
        
        
        tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
        tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
        
        tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
        
        samp.n <- length(names(tree.a.mod))
        tree.names <- names(tree.a.mod[2:samp.n])
        
        tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
        tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
        
        tree.a.re.re <- tree.a.re[2:samp.n]
        
        source <- tree.b.re[,2]
        
        
        n <- length(ls(tree.a.re))
        
        
        group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
        
        group.lm.r <- sqrt(group.lm.r2)
        
        group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
        
        group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
        
        
        result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
        colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
        return(format(result.frame, digits=3))
        
        
    }
    
    all.tree.names <- names(tree.dataframe)
    all.tree.names <- all.tree.names[2:length(all.tree.names)]
    
    tree.subset <- subset(tree.dataframe, as.numeric(as.vector(Year)) < max(time))
    tree.subset <- subset(tree.subset, as.numeric(as.vector(Year)) > min(time))
    
    tree.time <- tree.subset[colSums(!is.na(tree.subset)) > 0]
    
    tree.names <- names(tree.time)[2:length(tree.time)]
    
    
    #cat(gettext(tree.names))
    
    source.name.list <- sapply(tree.source.list, names)
    source.names <- source.name.list[2,]
    source.names <- make.names(source.names, unique=TRUE)
    
    all.group.t <- pblapply(tree.source.list, function(tree.source.list) treeJackKnife(time, tree.dataframe, tree.source.list))
    
    t.value <- as.data.frame(subListExtract(L=all.group.t, name="t-value"), stringsAsFactors=TRUE)
    colnames(t.value) <- source.names
    n <- length(names(t.value))
    t.value <- as.data.frame(lapply(t.value, as.numeric))
    t.value <- t.value[,source.names]
    scaled.t <- t(apply(t.value, 1, function(x) scale(x)[,1]))
    t.value$Mean <- rowMeans(t.value)
    scaled.mean <- rowMeans(scaled.t)
    t.value$SD <- apply(t.value, 1, sd)
    scaled.sd <- apply(scaled.t, 1, sd)
    t.value$SourceValue <- apply(t.value, 1, max)
    scaled.max.value <- apply(scaled.t, 1, max)
    t.value$Source <- colnames(t.value)[apply(t.value,1,which.max)]
    
    
    
    t.value$ZScore <- (scaled.max.value-scaled.mean)/scaled.sd
    t.value$pvalue <- 1-pnorm(t.value$ZScore)
    
    t.value$Difference <- rep("Yes", length(t.value$Mean))
    t.value <- transform(t.value, Difference = ifelse(pvalue > 0.01, "No", Difference))
    t.value.names <- names(t.value)
    t.value <- data.frame(tree.names, t.value)
    colnames(t.value) <- c("Specimen", t.value.names)
    t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")] <- round(t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")], 2)
    
    
    
    r.value <- data.frame(tree.names, Biobase::subListExtract(L=all.group.t, name="r-value"))
    colnames(r.value) <- c("Specimen", source.names)
    r.value[,!names(r.value) %in% c("Specimen")] <- apply(r.value[,!names(r.value) %in% c("Specimen")], 2, as.numeric)
    r.value[,!names(r.value) %in% c("Specimen")] <- round(r.value[,!names(r.value) %in% c("Specimen")], 4)
    
    
    samp.over <- data.frame(tree.names, Biobase::subListExtract(L=all.group.t, name="Sample Overlap"))
    colnames(samp.over) <- c("Specimen", source.names)
    
    
    ####p-value table
    z.score.table <- apply(scaled.t, 2, function(x) (x-scaled.mean)/scaled.sd)
    p.value.table <- 1-apply(z.score.table, 2, function(x) pnorm(x))
    p.values <- data.frame(tree.names, p.value.table)
    colnames(p.values)[1] <- "Specimen"
    
    p.values[,!names(p.values) %in% c("Specimen", "Source")] <- round(p.values[,!names(p.values) %in% c("Specimen", "Source")], 4)
    
    p.values$SourceValue <- apply(p.values, 1, min)
    
    p.values$Source <- colnames(p.values[2:length(p.values)-1])[apply(p.values[2:length(p.values)-1],1,which.min)]
    
    
    
    
    posterior.prob.vector <- function(p.vector, strength.vector) {
        
        p.vector <- as.vector(p.vector)
        un.inf.prior <- 1/length(source.names)
        un.inf.strength <- un.inf.prior*strength.vector
        alt.poss <- (1-un.inf.strength)*p.vector
        all.poss <- alt.poss+un.inf.strength
        
        
        posteriors <- un.inf.strength/all.poss
        
        posteriors
    }
    
    p.value.list <- setNames(split(p.value.table, seq(nrow(p.value.table))), rownames(p.value.table))
    
    chaco.strength.vector <- 1/length(source.names)
    
    posterior.table.1 <- lapply(p.value.list,  function(x) posterior.prob.vector(x, chaco.strength.vector))
    
    posterior.table.2 <- do.call("rbind", posterior.table.1)
    colnames(posterior.table.2) <- source.names
    
    
    posterior.table <- data.frame(tree.names, posterior.table.2)
    colnames(posterior.table)[1] <- "Specimen"
    
    posterior.table$SourcePosterior <- apply(posterior.table[source.names], 1, max)
    posterior.table$Source <- colnames(posterior.table[2:length(posterior.table)-1])[apply(posterior.table[2:length(posterior.table)-1],1,which.max)]
    posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")] <- round(posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")], 4)
    
    
    
    
    
    
    
    
    
    result.list <- list(t.value, r.value, p.values, posterior.table)
    names(result.list) <- c("T-Value", "r-Value", "p-Value", "Posterior Probabilities")
    
    return(result.list)
    
    
}




obsidianJackKnifeMultipleSourceSigVerify <- function(time, tree.dataframe, tree.source.list) {
    
    
    treeJackKnife <- function(time,  tree.dataframe, tree.source) {
        
        
        
        tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
        tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
        
        tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
        
        samp.n <- length(names(tree.a.mod))
        tree.names <- names(tree.a.mod[2:samp.n])
        
        tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
        tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
        
        tree.a.re.re <- tree.a.re[2:samp.n]
        
        source <- tree.b.re[,2]
        
        
        n <- length(ls(tree.a.re))
        
        
        group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
        
        group.lm.r <- sqrt(group.lm.r2)
        
        group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
        
        group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
        
        
        result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
        colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
        return(format(result.frame, digits=3))
        
        
    }
    
    all.tree.names <- names(tree.dataframe)
    all.tree.names <- all.tree.names[2:length(all.tree.names)]
    
    tree.subset <- subset(tree.dataframe, as.numeric(as.vector(Year)) < max(time))
    tree.subset <- subset(tree.subset, as.numeric(as.vector(Year)) > min(time))
    
    tree.time <- tree.subset[colSums(!is.na(tree.subset)) > 0]
    
    tree.names <- names(tree.time)[2:length(tree.time)]
    
    
    #cat(gettext(tree.names))
    
    source.name.list <- sapply(tree.source.list, names)
    source.names <- source.name.list[2,]
    source.names <- make.names(source.names, unique=TRUE)
    
    all.group.t <- pblapply(tree.source.list, function(tree.source.list) treeJackKnife(time, tree.dataframe, tree.source.list))
    
    t.value <- as.data.frame(subListExtract(L=all.group.t, name="t-value"), stringsAsFactors=TRUE)
    colnames(t.value) <- source.names
    n <- length(names(t.value))
    t.value <- as.data.frame(lapply(t.value, as.numeric))
    t.value <- t.value[,source.names]
    scaled.t <- t(apply(t.value, 1, function(x) scale(x)[,1]))
    t.value$Mean <- rowMeans(t.value)
    scaled.mean <- rowMeans(scaled.t)
    t.value$SD <- apply(t.value, 1, sd)
    scaled.sd <- apply(scaled.t, 1, sd)
    t.value$SourceValue <- apply(t.value, 1, max)
    scaled.max.value <- apply(scaled.t, 1, max)
    t.value$Source <- colnames(t.value)[apply(t.value,1,which.max)]
    
    
    
    t.value$ZScore <- (scaled.max.value-scaled.mean)/scaled.sd
    t.value$pvalue <- 1-pnorm(t.value$ZScore)
    
    t.value$Difference <- rep("Yes", length(t.value$Mean))
    t.value <- transform(t.value, Difference = ifelse(pvalue > 0.01, "No", Difference))
    t.value.names <- names(t.value)
    t.value <- data.frame(tree.names, t.value)
    colnames(t.value) <- c("Specimen", t.value.names)
    t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")] <- round(t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")], 2)
    
    t.value$SourceMatch <- t.value$Source %in% t.value$Specimen
    
    r.value <- data.frame(tree.names, Biobase::subListExtract(L=all.group.t, name="r-value"))
    colnames(r.value) <- c("Specimen", source.names)
    r.value[,!names(r.value) %in% c("Specimen")] <- apply(r.value[,!names(r.value) %in% c("Specimen")], 2, as.numeric)
    r.value[,!names(r.value) %in% c("Specimen")] <- round(r.value[,!names(r.value) %in% c("Specimen")], 4)
    
    
    samp.over <- data.frame(tree.names, Biobase::subListExtract(L=all.group.t, name="Sample Overlap"))
    colnames(samp.over) <- c("Specimen", source.names)
    
    
    ####p-value table
    z.score.table <- apply(scaled.t, 2, function(x) (x-scaled.mean)/scaled.sd)
    p.value.table <- 1-apply(z.score.table, 2, function(x) pnorm(x))
    p.values <- data.frame(tree.names, p.value.table)
    colnames(p.values)[1] <- "Specimen"
    
    p.values[,!names(p.values) %in% c("Specimen", "Source")] <- round(p.values[,!names(p.values) %in% c("Specimen", "Source")], 4)
    
    p.values$SourceValue <- apply(p.values, 1, min)
    
    p.values$Source <- colnames(p.values[2:length(p.values)-1])[apply(p.values[2:length(p.values)-1],1,which.min)]
    
    
    
    
    posterior.prob.vector <- function(p.vector, strength.vector) {
        
        p.vector <- as.vector(p.vector)
        un.inf.prior <- 1/length(source.names)
        un.inf.strength <- un.inf.prior*strength.vector
        alt.poss <- (1-un.inf.strength)*p.vector
        all.poss <- alt.poss+un.inf.strength
        
        
        posteriors <- un.inf.strength/all.poss
        
        posteriors
    }
    
    p.value.list <- setNames(split(p.value.table, seq(nrow(p.value.table))), rownames(p.value.table))
    
    chaco.strength.vector <- 1/length(source.names)
    
    posterior.table.1 <- lapply(p.value.list,  function(x) posterior.prob.vector(x, chaco.strength.vector))
    
    posterior.table.2 <- do.call("rbind", posterior.table.1)
    colnames(posterior.table.2) <- source.names
    
    
    posterior.table <- data.frame(tree.names, posterior.table.2)
    colnames(posterior.table)[1] <- "Specimen"
    
    posterior.table$SourcePosterior <- apply(posterior.table[source.names], 1, max)
    posterior.table$Source <- colnames(posterior.table[2:length(posterior.table)-1])[apply(posterior.table[2:length(posterior.table)-1],1,which.max)]
    posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")] <- round(posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")], 4)
    
    
    result.list <- list(t.value, r.value, p.values, posterior.table)
    names(result.list) <- c("T-Value", "r-Value", "p-Value", "Posterior Probabilities")
    
    return(result.list)
    
    
}

treeJackKnifeOrigional <- function(time, tree.dataframe, tree.source) {
    
    tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
    tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
    
    tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
    
    samp.n <- length(names(tree.a.mod))
    tree.names <- names(tree.a.mod[2:samp.n])
    
    tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
    tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
    
    tree.a.re.re <- tree.a.re[2:samp.n]
    
    source <- tree.b.re[,2]
    
    
    n <- length(ls(tree.a.re))
    
    
    group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
    group.lm.r2[group.lm.r2==1] <- .99999
    
    
    group.lm.r <- sqrt(group.lm.r2)
    
    group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
    
    group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
    
    
    result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
    colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
    return(format(result.frame, digits=3))
}

in_interval_value <- function(x, mean, sensitivity){
    (mean-((mean*sensitivity))) < x & x < (mean+((mean*sensitivity)))
}

in_interval_vector <- function(vector, mean, sensitivity){
    year.s <- seq(1, length(vector), 1)
    the.match <- sapply(year.s,  function(x) in_interval_value(vector[x], mean[x,2], sensitivity=sensitivity))
    any(the.match)
}

treeJackKnife <- function(time,  tree.dataframe, tree.source) {
    
    in_interval_value <- function(x, mean, sensitivity){
        (mean-((mean*sensitivity))) < x & x < (mean+((mean*sensitivity)))
    }
    
    in_interval_vector <- function(vector, mean, sensitivity){
        year.s <- seq(1, length(vector), 1)
        the.match <- sapply(year.s,  function(x) in_interval_value(vector[x], mean[x,2], sensitivity=sensitivity))
        any(the.match)
    }
    
    #test <- lapply(names(source.list), function(x) in_interval_vector(vector=tree.frame[,2], mean=source.list[[x]]))
    
    
    tree.source.check <- in_interval_vector(vector=tree.dataframe[,2], mean=tree.source, sensitivity=sensitivity)
    
    
    treeJackKnifeOrigional <- function(time, tree.dataframe, tree.source) {
        
        tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
        tree.b <- tree.source[match(time, tree.source$Year, nomatch=0),]
        
        tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
        
        samp.n <- length(names(tree.a.mod))
        tree.names <- names(tree.a.mod[2:samp.n])
        
        tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
        tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
        
        tree.a.re.re <- tree.a.re[2:samp.n]
        
        source <- tree.b.re[,2]
        
        
        n <- length(ls(tree.a.re))
        
        
        group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
        group.lm.r2[group.lm.r2==1] <- .99999
        
        
        group.lm.r <- sqrt(group.lm.r2)
        
        group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
        
        group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
        
        
        result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
        colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
        return(format(result.frame, digits=3))
    }
    
    null.frame <- data.frame(NA, NA ,NA)
    colnames(null.frame) <- c("t-value", "r-value", "Sample Overlap")
    
    if(tree.source.check==TRUE){
        return(treeJackKnifeOrigional(time=time, tree.dataframe=tree.dataframe, tree.source=tree.source))
    }else if(tree.source.check==FALSE){
        return(null.frame)
    } else if(tree.source.check==NA){
        return(null.frame)
    }
}

apply_treeJackKnife <- function(time, tree.dataframe, tree.source){
    
    temp.frame <- tree.dataframe[,-1]
    
    
    temp.list <- apply(temp.frame, 2, function(x) list(as.vector(x)))
    temp.list <- lapply(temp.list, "[[", 1)
    temp.names <- colnames(temp.frame)
    tree.list <- lapply(temp.list, function(x) data.frame(Year, x))
    for(i in 1:length(tree.list)){
        colnames(tree.list[[i]]) <- c("Year", temp.names[i])
    }
    
    result.list <- lapply(tree.list, function(x) treeJackKnife(time=time, x, tree.source))
    
    result.frame <- do.call("rbind", result.list)
    result.frame <- apply(result.frame, 2, as.numeric)
    return(result.frame)
}

distance_function <- function(source.metadata, site.vector){
    
    sample.seq <- seq(1, length(source.metadata[,1]),1)
    
    result.vector <- sapply(sample.seq, function(x) distGeo(unlist(source.metadata[x, 2:3]), site.vector))
    
    result.frame <- data.frame(source.metadata[,1], result.vector)
    colnames(result.frame) <- c("Source", "Distance")
    
    return(result.frame)
}

prior_function_vector <- function(source.metadata, site.vector){
    
    uninformative.prior <- 1/length(source.metadata[,1])
    
    farthest.distance <- 12756000
    
    far.frame <- data.frame("Max", farthest.distance)
    colnames(far.frame) <- c("Source", "Distance")
    
    
    distance.frame <- distance_function(source.metadata, site.vector)
    
    prior.vector <- (((mean((1-distance.frame$Distance/farthest.distance), na.rm=TRUE)*uninformative.prior)-uninformative.prior)*-1)+((1-distance.frame$Distance/farthest.distance)*uninformative.prior)
    
    prior.vector[is.na(prior.vector)] <- uninformative.prior
    
    
    return(prior.vector)
    
}

prior_function_frame <- function(source.metadata, tree.metadata){
    
    prior.table <- apply(tree.metadata[,c("Longitude", "Latitude")], 1, function(x) prior_function_vector(source.metadata, x))
    
    rownames(prior.table) <- source.metadata$Sources
    colnames(prior.table) <- tree.metadata$Specimen
    
    return(prior.table)
}




obsidianJackKnifeMultipleSourceProb <- function(time, tree.dataframe, tree.source.list, tree.metadata, source.metadata, sensitivity, scale) {
    
    treeJackKnife <- function(tree.dataframe, tree.source) {
        
        weird.frame <- merge(x=tree.dataframe, y=tree.source, by.x="Year", by.y="Year")
        weirder.frame <- weird.frame[complete.cases(weird.frame),]
        
        tree.dataframe <- weirder.frame[, names(tree.dataframe)]
        tree.source <- weirder.frame[, names(tree.source)]
        
        time <- weirder.frame$Year
        
        
        in_interval_value <- function(x, mean, sensitivity){
            (mean-((mean*sensitivity))) < x & x < (mean+((mean*sensitivity)))
        }
        
        
        in_interval_vector <- function(vector, mean, sensitivity){
            
            sub_interval_vector <- function(vector, mean, sensitivity){
                year.s <- seq(1, length(vector), 1)
                the.match <- sapply(year.s,  function(x) in_interval_value(vector[x], mean[x,2], sensitivity=sensitivity))
                if(tolerance==TRUE){
                    any(the.match)
                } else if(tolerance==FALSE){
                    all(the.match)
                }
            }
            
            if(length(vector)!=0){
                return(sub_interval_vector(vector, mean, sensitivity))
            }else if(length(vector)==0){
                return(FALSE)
            }
            
            
        }
        
        #test <- lapply(names(source.list), function(x) in_interval_vector(vector=tree.frame[,2], mean=source.list[[x]]))
        
        
        tree.source.check <- in_interval_vector(vector=tree.dataframe[,2], mean=tree.source, sensitivity=sensitivity)
        
        
        treeJackKnifeOrigional <- function(tree.dataframe, tree.source, scale) {
            time <- tree.dataframe$Year
            
            tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
            tree.b<- tree.source[match(time, tree.source$Year, nomatch=0),]
            
            autoDivide <- function(vector) {
                vector[1]/vector[2]
            }
            
            #tree.a.ratios <- c(tree.a.first[,2], combn(tree.a.first[,2], 2, FUN=autoDivide))
            #tree.b.ratios <- c(tree.b.first[,2], combn(tree.b.first[,2], 2, FUN=autoDivide))
            
            
            #tree.a <- data.frame(seq(1, length(tree.a.ratios), 1), tree.a.ratios)
            #colnames(tree.a) <- colnames(tree.a.first)
            #tree.b <- data.frame(seq(1, length(tree.b.ratios), 1), tree.b.ratios)
            #colnames(tree.b) <- colnames(tree.b.first)
            
            
            
            tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
            
            samp.n <- length(names(tree.a.mod))
            tree.names <- names(tree.a.mod[2:samp.n])
            
            tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
            tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
            
            tree.a.re.re <- tree.a.re[2:samp.n]
            
            source <- tree.b.re[,2]
            
            
            n <- length(ls(tree.a.re))
            
            
            #group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(scale(x)[,1]~scale(source)[,1]))$r.squared))
            group.lm.r2 <- if(scale==FALSE){
                apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
            } else if(scale==TRUE){
                apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(scale(x)[,1]~scale(source)[,1]))$r.squared))
            }
            group.lm.r2[group.lm.r2==1] <- .99999
            
            
            group.lm.r <- sqrt(group.lm.r2)
            
            #group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(scale(x)[,1]~scale(source)[,1]))$residuals)))
            group.lm.res.n <- if(scale==FALSE){
                apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
            } else if(scale==TRUE){
                group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(scale(x)[,1]~scale(source)[,1]))$residuals)))
            }
            
            group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
            
            
            result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
            colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
            return(format(result.frame, digits=3))
        }
        
        null.frame <- data.frame(NA, NA ,NA)
        colnames(null.frame) <- c("t-value", "r-value", "Sample Overlap")
        
        if(tree.source.check==TRUE){
            return(treeJackKnifeOrigional(tree.dataframe=tree.dataframe, tree.source=tree.source, scale=scale))
        } else if(tree.source.check==FALSE){
            return(null.frame)
        } else if(is.na(tree.source.check)==TRUE){
            return(null.frame)
        } else if(is.null(tree.source.check)==TRUE){
            return(null.frame)
        }
    }
    
    all.tree.names <- names(tree.dataframe)
    all.tree.names <- all.tree.names[2:length(all.tree.names)]
    
    tree.subset <- tree.dataframe
    
    tree.time <- tree.subset[colSums(!is.na(tree.subset)) > 0]
    
    tree.names <- names(tree.time)[2:length(tree.time)]
    
    
    #cat(gettext(tree.names))
    
    source.name.list <- sapply(tree.source.list, names)
    source.names <- source.name.list[2,]
    source.names <- make.names(source.names, unique=TRUE)
    
    apply_treeJackKnife <- function(tree.dataframe, tree.source){
        
        temp.frame <- tree.dataframe[,-1]
        Year <- tree.dataframe$Year
        
        temp.list <- apply(temp.frame, 2, function(x) list(as.vector(x)))
        temp.list <- lapply(temp.list, "[[", 1)
        temp.names <- colnames(temp.frame)
        tree.list <- lapply(temp.list, function(x) data.frame(Year, x))
        for(i in 1:length(tree.list)){
            colnames(tree.list[[i]]) <- c("Year", temp.names[i])
        }
        
        result.list <- lapply(tree.list, function(x) treeJackKnife(x, tree.source))
        
        result.frame <- do.call("rbind", result.list)
        result.frame <- apply(result.frame, 2, as.numeric)
        return(result.frame)
    }
    
    
    
    #clusterExport(cl, library(parallel))
    
    all.group.t <- pblapply(names(tree.source.list), function(x) apply_treeJackKnife( tree.dataframe, tree.source.list[[x]]))
    names(all.group.t) <- names(tree.source.list)
    
    all.group <- ldply(all.group.t, data.frame)
    colnames(all.group) <- c("Source", "t-value", "r-value", "Sample Overlap")
    all.group$Sample <- rep(names(tree.dataframe[,-1]), length(all.group.t))
    
    
    t.value <- reshape2::dcast(data=all.group, formula=Sample~Source, value.var="t-value", fun.aggregate=mean)[,-1]
    rownames(t.value) <- names(tree.dataframe[,-1])
    n <- length(all.group.t)
    #t.value <- as.data.frame(lapply(t.value, as.numeric))
    #t.value <- t.value[,source.names]
    scaled.t <- t(apply(t.value, 1, function(x) scale(x)[,1]))
    t.value$Mean <- rowMeans(t.value, na.rm=TRUE)
    scaled.mean <- rowMeans(scaled.t, na.rm=TRUE)
    t.value$SD <- apply(t.value, 1, sd, na.rm=TRUE)
    scaled.sd <- apply(scaled.t, 1, sd, na.rm=TRUE)
    t.value$SourceValue <- apply(t.value, 1, max, na.rm=TRUE)
    scaled.max.value <- apply(scaled.t, 1, max, na.rm=TRUE)
    t.value$Source <- colnames(t.value)[apply(t.value,1,which.max)]
    t.value$Source[t.value$Source == "SourceValue"] <- ""

    
    
    t.value$ZScore <- (scaled.max.value-scaled.mean)/scaled.sd
    t.value$pvalue <- 1-pnorm(t.value$ZScore)
    
    t.value$Difference <- rep("Yes", length(t.value$Mean))
    t.value <- transform(t.value, Difference = ifelse(pvalue >= 0.05, "No", Difference))
    t.value.names <- names(t.value)
    t.value <- data.frame(tree.names, t.value)
    colnames(t.value) <- c("Specimen", t.value.names)
    
    t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")] <- round(t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")], 2)
    
    t.value$SourceMatch <- t.value$Source %in% t.value$Specimen
    
    r.value <- data.frame(tree.names, reshape2::dcast(data=all.group, formula=Sample~Source, value.var="r-value", fun.aggregate=mean)[,-1])
    colnames(r.value)[1] <- "Specimen"
    r.value[,!names(r.value) %in% c("Specimen")] <- apply(r.value[,!names(r.value) %in% c("Specimen")], 2, as.numeric)
    r.value[,!names(r.value) %in% c("Specimen")] <- round(r.value[,!names(r.value) %in% c("Specimen")], 4)
    
    
    samp.over <- data.frame(tree.names, reshape2::dcast(data=all.group, formula=Sample~Source, value.var="Sample Overlap", fun.aggregate=mean)[,-1])
    colnames(samp.over)[1] <- "Specimen"

    
    ####p-value table
    z.score.table <- apply(scaled.t, 2, function(x) (x-scaled.mean)/scaled.sd)
    p.value.table <- 1-apply(z.score.table, 2, function(x) pnorm(x))
    p.values <- data.frame(tree.names, p.value.table)
    colnames(p.values)[1] <- "Specimen"
    
    p.values[,!names(p.values) %in% c("Specimen", "Source", "SourceValue")] <- round(p.values[,!names(p.values) %in% c("Specimen", "Source", "SourceValue")], 4)
    
    
    
    temp.source <- colnames(p.values[,-1])[as.numeric(unname(as.vector(apply(p.values[,-1],1,which.min))))]
    
    is.sig <- function(p.value){
        
        if(length(p.value)==1 && !is.na(p.value)){
            p.value <0.05
        } else if(length(p.value)==1 && is.na(p.value)){
            FALSE
        } else if(length(p.value)!=1){
            FALSE
        }
    }
    
    
    
    is.sig.vec <- function(p.values){
        
        first <- as.vector(sapply(p.values, is.sig))
        first[is.na(first)] <- FALSE
        first
    }
    
    is.sig.col <- function(p.value.frame){
        
        p.value.frame[is.na(p.value.frame)] <- 1
        
        logic.frame <- apply(p.value.frame[,-1], 2, is.sig.vec)
        
        origional.columns <- colnames(p.value.frame[,-1])
        
        source.hold <- rep("", length(p.value.frame[,1]))
        
        for(i in 1:length(p.value.frame[,1])){
            source.hold[i] <- paste(origional.columns[logic.frame[i,, drop=FALSE]], collapse=", ")
        }
        
        source.hold[! source.hold %in% "SourceValue"]
        
    }
    
    temp.sources <- is.sig.col(p.values)
    
    temp.source.value <- apply(p.values[,-1],1,base::min, na.rm=TRUE)
    
    temp.source[temp.source == "SourceValue"] <- ""
    
    p.values$Source <- temp.source
    
    p.values$SourceValue <- temp.source.value
    
    p.values$SignificantMatches <- temp.sources
    
    
    
    distance_function <- function(source.metadata, site.vector){
        
        sample.seq <- seq(1, length(source.metadata[,1]),1)
        
        result.vector <- sapply(sample.seq, function(x) distGeo(unlist(source.metadata[x, 2:3]), site.vector))
        
        result.frame <- data.frame(source.metadata[,1], result.vector)
        colnames(result.frame) <- c("Source", "Distance")
        
        return(result.frame)
    }
    
    prior_function_vector <- function(source.metadata, site.vector){
        
        uninformative.prior <- 1/length(source.metadata[,1])
        
        farthest.distance <- 12756000
        
        far.frame <- data.frame("Max", farthest.distance)
        colnames(far.frame) <- c("Source", "Distance")
        
        
        distance.frame <- distance_function(source.metadata, site.vector)
        
        prior.vector <- (((mean((1-distance.frame$Distance/farthest.distance), na.rm=TRUE)*uninformative.prior)-uninformative.prior)*-1)+((1-distance.frame$Distance/farthest.distance)*uninformative.prior)
        
        prior.vector[is.na(prior.vector)] <- uninformative.prior
        
        
        return(prior.vector)
        
    }
    
    prior_function_frame <- function(source.metadata, tree.metadata){
        
        prior.table <- apply(tree.metadata[,c("Longitude", "Latitude")], 1, function(x) prior_function_vector(source.metadata, x))
        
        rownames(prior.table) <- source.metadata$Sources
        colnames(prior.table) <- tree.metadata$Specimen
        
        return(prior.table)
    }
    
    
    
    
    posterior.prob.vector <- function(p.vector, prior.vector) {
        
        p.vector <- as.vector(p.vector)
        
        alt.poss <- (1-prior.vector)*p.vector
        all.poss <- alt.poss+prior.vector
        
        
        posteriors <- prior.vector/all.poss
        
        posteriors
    }
    
    prior.table <- prior_function_frame(source.metadata, tree.metadata)
    prior.table.t <- t(prior.table)
    #prior.list <- setNames(split(prior.table.t, seq(nrow(prior.table.t))), rownames(prior.table.t))
    
    #p.value.list <- setNames(split(p.value.table, seq(nrow(p.value.table))), rownames(p.value.table))
    #chaco.strength.vector <- 1/length(source.names)
    
    posterior.table.prep <- posterior.prob.vector(p.value.table, prior.table.t)
    
    #posterior.table.1 <- lapply(p.value.list,  function(x) posterior.prob.vector(x, prior_function_frame(source.metadata, tree.metadata)))
    
    #posterior.table.2 <- do.call("rbind", posterior.table.1)
    #colnames(posterior.table.2) <- source.names
    
    
    posterior.table <- data.frame(tree.names, posterior.table.prep)
    colnames(posterior.table)[1] <- "Specimen"
    
    posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")] <- round(posterior.table[,!names(posterior.table) %in% c("Specimen", "Source")], 4)
    
    
    posterior.table$SourcePosterior <- apply(posterior.table[source.names], 1, max, na.rm=TRUE)
    #posterior.table$Source <- colnames(as.data.frame(posterior.table)[2:length(as.data.frame(posterior.table))-1])[apply(as.data.frame(posterior.table)[2:length(as.data.frame(posterior.table))-1],1,which.max)]
    posterior.table$Source <- colnames(posterior.table[,-1])[as.numeric(unname(as.vector(apply(posterior.table[,-1],1,which.max))))]
    
    posterior.table$Source[posterior.table$Source=="SourcePosterior"] <- "Unknown"
    #forsave <- data.frame(posterior.table["Specimen"], posterior.table["Source"], posterior.table["SourcePosterior"])
    result.summary <- as.data.frame(table(as.data.frame(posterior.table$Source)))
    
    parameter.table <- data.frame(length(tree.dataframe), length(tree.source.list), length(unique(posterior.table$Source)), 1/length(tree.source.list), sensitivity)
    colnames(parameter.table) <- c("# Samples", "# Possible Sources", "# Identified Sources", "Prior Probability", "Sensitivity")
    
    result.list <- list(result.summary, parameter.table, t.value, r.value, p.values, posterior.table)
    names(result.list) <- c("Summary", "Parameters", "T-Value", "r-Value", "p-Value", "Posterior Probabilities")
    
    return(result.list)
    
    
    
}


obsidianJackKnifeMultipleSourceSimp <- function(tree.dataframe, tree.source.list, sensitivity, tolerance, scale) {
    
    treeJackKnife <- function(tree.dataframe, tree.source) {
        
        weird.frame <- merge(x=tree.dataframe, y=tree.source, by.x="Year", by.y="Year")
        weirder.frame <- weird.frame[complete.cases(weird.frame),]
        
        tree.dataframe <- weirder.frame[, names(tree.dataframe)]
        tree.source <- weirder.frame[, names(tree.source)]
        
        time <- weirder.frame$Year
        
        
        in_interval_value <- function(x, mean, sensitivity){
            (mean-((mean*sensitivity))) < x & x < (mean+((mean*sensitivity)))
        }
        
        in_interval_vector <- function(vector, mean, sensitivity){
            
            sub_interval_vector <- function(vector, mean, sensitivity){
                year.s <- seq(1, length(vector), 1)
                the.match <- sapply(year.s,  function(x) in_interval_value(vector[x], mean[x,2], sensitivity=sensitivity))
                if(tolerance==TRUE){
                    any(the.match)
                } else if(tolerance==FALSE){
                    all(the.match)
                }
            }
            
            if(length(vector)!=0){
                return(sub_interval_vector(vector, mean, sensitivity))
            }else if(length(vector)==0){
                return(FALSE)
            }
            
            
        }
        
        #test <- lapply(names(source.list), function(x) in_interval_vector(vector=tree.frame[,2], mean=source.list[[x]]))
        
        
        tree.source.check <- in_interval_vector(vector=tree.dataframe[,2], mean=tree.source, sensitivity=sensitivity)
        
        
        treeJackKnifeOrigional <- function(tree.dataframe, tree.source, scale) {
            time <- tree.dataframe$Year
            
            tree.a <- tree.dataframe[match(time, tree.dataframe$Year, nomatch=0),]
            tree.b<- tree.source[match(time, tree.source$Year, nomatch=0),]
            
            autoDivide <- function(vector) {
                vector[1]/vector[2]
            }
            
            #tree.a.ratios <- c(tree.a.first[,2], combn(tree.a.first[,2], 2, FUN=autoDivide))
            #tree.b.ratios <- c(tree.b.first[,2], combn(tree.b.first[,2], 2, FUN=autoDivide))
            
            
            #tree.a <- data.frame(seq(1, length(tree.a.ratios), 1), tree.a.ratios)
            #colnames(tree.a) <- colnames(tree.a.first)
            #tree.b <- data.frame(seq(1, length(tree.b.ratios), 1), tree.b.ratios)
            #colnames(tree.b) <- colnames(tree.b.first)
            
            
            
            tree.a.mod <- tree.a[, colSums(is.na(tree.a)) != nrow(tree.a)]
            
            samp.n <- length(names(tree.a.mod))
            tree.names <- names(tree.a.mod[2:samp.n])
            
            tree.a.re <- tree.a.mod[tree.a.mod$Year %in% tree.b$Year, ]
            tree.b.re <- tree.b[tree.b$Year %in% tree.a.mod$Year, ]
            
            tree.a.re.re <- tree.a.re[2:samp.n]
            
            source <- tree.b.re[,2]
            
            
            n <- length(ls(tree.a.re))
            
            
            #group.lm.r2 <- apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(scale(x)[,1]~scale(source)[,1]))$r.squared))
            group.lm.r2 <- if(scale==FALSE){
                apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(x~source))$r.squared))
            } else if(scale==TRUE){
                apply(tree.a.re.re, 2, function(x) as.vector(summary(lm(scale(x)[,1]~scale(source)[,1]))$r.squared))
            }
            group.lm.r2[group.lm.r2==1] <- .99999
            
            
            group.lm.r <- sqrt(group.lm.r2)
            
            #group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(scale(x)[,1]~scale(source)[,1]))$residuals)))
            group.lm.res.n <- if(scale==FALSE){
                apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(x~source))$residuals)))
            } else if(scale==TRUE){
                group.lm.res.n <- apply(tree.a.re.re, 2, function(x) as.numeric(length(summary(lm(scale(x)[,1]~scale(source)[,1]))$residuals)))
            }

            group.t <- sqrt(group.lm.r2)*sqrt(group.lm.res.n-2)/sqrt(1-group.lm.r2)
            
            
            result.frame <- data.frame(group.t, group.lm.r, group.lm.res.n)
            colnames(result.frame) <- c("t-value", "r-value", "Sample Overlap")
            return(format(result.frame, digits=3))
        }
        
        null.frame <- data.frame(NA, NA ,NA)
        colnames(null.frame) <- c("t-value", "r-value", "Sample Overlap")
        
        if(tree.source.check==TRUE){
            return(treeJackKnifeOrigional(tree.dataframe=tree.dataframe, tree.source=tree.source, scale=scale))
        } else if(tree.source.check==FALSE){
            return(null.frame)
        } else if(is.na(tree.source.check)==TRUE){
            return(null.frame)
        } else if(is.null(tree.source.check)==TRUE){
            return(null.frame)
        }
    }
    
    all.tree.names <- names(tree.dataframe)
    all.tree.names <- all.tree.names[2:length(all.tree.names)]
    
    tree.subset <- tree.dataframe
    
    tree.time <- tree.subset[colSums(!is.na(tree.subset)) > 0]
    
    tree.names <- names(tree.time)[2:length(tree.time)]
    
    
    #cat(gettext(tree.names))
    
    source.name.list <- sapply(tree.source.list, names)
    source.names <- source.name.list[2,]
    source.names <- make.names(source.names, unique=TRUE)
    
    apply_treeJackKnife <- function(tree.dataframe, tree.source){
        
        temp.frame <- tree.dataframe[,-1]
        Year <- tree.dataframe$Year
        
        temp.list <- apply(temp.frame, 2, function(x) list(as.vector(x)))
        temp.list <- lapply(temp.list, "[[", 1)
        temp.names <- colnames(temp.frame)
        tree.list <- lapply(temp.list, function(x) data.frame(Year, x))
        for(i in 1:length(tree.list)){
            colnames(tree.list[[i]]) <- c("Year", temp.names[i])
        }
        
        result.list <- lapply(tree.list, function(x) treeJackKnife(x, tree.source))
        
        result.frame <- do.call("rbind", result.list)
        result.frame <- apply(result.frame, 2, as.numeric)
        return(result.frame)
    }
    
    
    
    #clusterExport(cl, library(parallel))
    
    all.group.t <- pblapply(names(tree.source.list), function(x) apply_treeJackKnife( tree.dataframe, tree.source.list[[x]]))
    names(all.group.t) <- names(tree.source.list)
    
    all.group <- ldply(all.group.t, data.frame)
    colnames(all.group) <- c("Source", "t-value", "r-value", "Sample Overlap")
    all.group$Sample <- rep(names(tree.dataframe[,-1]), length(all.group.t))
    
    
    t.value <- reshape2::dcast(data=all.group, formula=Sample~Source, value.var="t-value", fun.aggregate=mean)[,-1]
    rownames(t.value) <- names(tree.dataframe[,-1])
    n <- length(all.group.t)
    #t.value <- as.data.frame(lapply(t.value, as.numeric))
    #t.value <- t.value[,source.names]
    scaled.t <- t(apply(t.value, 1, function(x) scale(x)[,1]))
    t.value$Mean <- rowMeans(t.value, na.rm=TRUE)
    scaled.mean <- rowMeans(scaled.t, na.rm=TRUE)
    t.value$SD <- apply(t.value, 1, sd, na.rm=TRUE)
    scaled.sd <- apply(scaled.t, 1, sd, na.rm=TRUE)
    t.value$SourceValue <- apply(t.value, 1, max, na.rm=TRUE)
    scaled.max.value <- apply(scaled.t, 1, max, na.rm=TRUE)
    t.value$Source <- colnames(t.value)[apply(t.value,1,which.max)]
    t.value$Source[t.value$Source == "SourceValue"] <- ""

    
    
    t.value$ZScore <- (scaled.max.value-scaled.mean)/scaled.sd
    t.value$pvalue <- 1-pnorm(t.value$ZScore)
    
    t.value$Difference <- rep("Yes", length(t.value$Mean))
    t.value <- transform(t.value, Difference = ifelse(pvalue >= 0.05, "No", Difference))
    t.value.names <- names(t.value)
    t.value <- data.frame(tree.names, t.value)
    colnames(t.value) <- c("Specimen", t.value.names)
    
    t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")] <- round(t.value[,!names(t.value) %in% c("Specimen", "Source", "Difference")], 2)
    
    t.value$SourceMatch <- t.value$Source %in% t.value$Specimen
    
    r.value <- data.frame(tree.names, reshape2::dcast(data=all.group, formula=Sample~Source, value.var="r-value", fun.aggregate=mean)[,-1])
    colnames(r.value)[1] <- "Specimen"
    r.value[,!names(r.value) %in% c("Specimen")] <- apply(r.value[,!names(r.value) %in% c("Specimen")], 2, as.numeric)
    r.value[,!names(r.value) %in% c("Specimen")] <- round(r.value[,!names(r.value) %in% c("Specimen")], 4)
    
    
    samp.over <- data.frame(tree.names, reshape2::dcast(data=all.group, formula=Sample~Source, value.var="Sample Overlap", fun.aggregate=mean)[,-1])
    colnames(samp.over)[1] <- "Specimen"

    
    ####p-value table
    z.score.table <- apply(scaled.t, 2, function(x) (x-scaled.mean)/scaled.sd)
    p.value.table <- 1-apply(z.score.table, 2, function(x) pnorm(x))
    p.values <- data.frame(tree.names, p.value.table)
    colnames(p.values)[1] <- "Specimen"
    
    p.values[,!names(p.values) %in% c("Specimen", "Source", "SourceValue")] <- round(p.values[,!names(p.values) %in% c("Specimen", "Source", "SourceValue")], 4)
    
    
    
    temp.source <- colnames(p.values[,-1])[as.numeric(unname(as.vector(apply(p.values[,-1],1,which.min))))]
    
    is.sig <- function(p.value){
        
        if(length(p.value)==1 && !is.na(p.value)){
            p.value <0.05
        } else if(length(p.value)==1 && is.na(p.value)){
            FALSE
        } else if(length(p.value)!=1){
            FALSE
        }
    }
    
    
    
    is.sig.vec <- function(p.values){
        
        first <- as.vector(sapply(p.values, is.sig))
        first[is.na(first)] <- FALSE
        first
    }
    
    is.sig.col <- function(p.value.frame){
        
        p.value.frame[is.na(p.value.frame)] <- 1
        
        logic.frame <- apply(p.value.frame[,-1], 2, is.sig.vec)
        
        origional.columns <- colnames(p.value.frame[,-1])
        
        source.hold <- rep("", length(p.value.frame[,1]))
        
        for(i in 1:length(p.value.frame[,1])){
            source.hold[i] <- paste(origional.columns[logic.frame[i,, drop=FALSE]], collapse=", ")
        }
        
        source.hold[! source.hold %in% "SourceValue"]

    }
    
    temp.sources <- is.sig.col(p.values)
    
    temp.source.value <- apply(p.values[,-1],1,base::min, na.rm=TRUE)
    


    p.values$Source <- temp.source
    
    p.values$Source[p.values$Source == "SourceValue"] <- ""

    
    p.values$SourceValue <- temp.source.value
    
    p.values$SignificantMatches <- temp.sources

    
    
    #forsave <- data.frame(posterior.table["Specimen"], posterior.table["Source"], posterior.table["SourcePosterior"])
    result.summary <- as.data.frame(table(as.data.frame(t.value$Source)))
    
    parameter.table <- data.frame(length(tree.dataframe), length(tree.source.list), length(unique(t.value$Source)),  sensitivity)
    colnames(parameter.table) <- c("# Samples", "# Possible Sources", "# Identified Sources",  "Sensitivity")
    
    result.list <- list(result.summary, parameter.table, t.value, r.value, p.values, samp.over)
    names(result.list) <- c("Summary", "Parameters", "T-Value", "r-Value", "p-Value", "Overlap")
    
    return(result.list)
    
    
    
}

source.replicate <- function(source, data, elements){
    
    new.data <- dplyr::filter(data,
    Sources %in% source)
    
    sim.list <- lapply(elements, function(x) rnorm(100, mean=new.data[,paste0("Mean", x)], sd=new.data[,paste0("SD", x)]))
    names(sim.list) <- elements
    sim.frame <- data.frame(rep(source, 100), sim.list)
    colnames(sim.frame) <- c("Source", elements)
    return(sim.frame)
    
}


#####Read Obsidian Data

#source(file="data/globe/dataPrep.R")

#no_cores <- detectCores() - 2
#cl<-makeCluster(no_cores)



#clusterEvalQ(cl, sessionInfo())
