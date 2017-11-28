library(pbapply)
library(reshape2)
library(TTR)
library(plyr)
library(dplyr)
library(data.table)
library(shiny)
library(ggplot2)
library(ggtern)
library(random)
library(rhandsontable)
library(random)
library(Cairo)
library(gghighlight)
library(ggmap)





shinyServer(function(input, output, session) {
    
    output$file2gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('file2', 'Choose Spectra', multiple=TRUE,
            accept=c('text/csv',
            'text/comma-separated-values,text/plain',
            '.csv'))
        } else {
            p()
        }
        
    })
    
    
    output$calfile2gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('calfileinput2', 'Load Cal File', accept='.quant', multiple=FALSE)
        } else {
            p()
        }
        
    })
    
    
    output$space23gen <- renderUI({
        
        if(input$otherdata==TRUE){
            tags$hr()
        } else {
            p()
        }
        
    })
    
    
    output$file3gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('file3', 'Choose Spectra', multiple=TRUE,
            accept=c('text/csv',
            'text/comma-separated-values,text/plain',
            '.csv'))
        } else {
            p()
        }
        
    })
    
    
    output$calfile3gen <- renderUI({
        
        if(input$otherdata==TRUE){
            fileInput('calfileinput3', 'Load Cal File', accept='.quant', multiple=FALSE)
        } else {
            p()
        }
        
    })

    

    

    
    
    fullSpectra1 <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
            
            
            
            myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
            
            
            
            
            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
            
            
            xrf.x <- data.table(xrf.x)
            xrf.y <- data.table(xrf.y)
            
            
            energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            
            
            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Sample")
            data <- spectra.frame
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        data
    })
    
    
    netCounts1 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Sample <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Sample", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Sample))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    
    fullSpectra2 <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
            
            
            
            myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
            
            
            
            
            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
            
            
            xrf.x <- data.table(xrf.x)
            xrf.y <- data.table(xrf.y)
            
            
            energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            
            
            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Sample")
            data <- spectra.frame
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        data
    })
    
    
    netCounts2 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Sample <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Sample", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Sample))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    

    
    fullSpectra3 <- reactive({
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file3
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
            
            
            
            myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
            
            
            
            
            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
            
            
            xrf.x <- data.table(xrf.x)
            xrf.y <- data.table(xrf.y)
            
            
            energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            
            
            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Sample")
            data <- spectra.frame
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        data
    })
    
    
    netCounts3 <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file3
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Sample <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Sample", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Sample))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    
    
    
    importSpreadsheet <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        
        sheet <- read.csv(file=inFile$datapath, sep=",")
        sheet
        
    })
    
    observeEvent(is.null(input$file1)==FALSE, {
        
        
        myData1 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra1()
            } else if(input$filetype=="Net"){
                netCounts1()
            }
            
            data
            
            
        })
        
        myData2 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra2()
            } else if(input$filetype=="Net"){
                netCounts2()
            }
            
            data
            
            
        })
        
        myData3 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra3()
            } else if(input$filetype=="Net"){
                netCounts3()
            }
            
            data
            
            
        })
        
        
        

        
        
        calFileContents1 <- reactive({
            
            existingCalFile <- input$calfileinput1
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount1 <- reactive({
            inFile <- input$file1
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        
        
        
        
        myValData1 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra1()
            } else if(input$filetype=="Net"){
                netCounts1()
            }
            
            data
            
        })
        
        
        
        
        
        
        
        calValHold1 <- reactive({
            
            
            calFileContents1()[[6]]
            
            
            
            
            
        })
        
        calVariables1 <- reactive({
            
            
            calFileContents1()$Intensities
            
            
            
        })
        
        calValElements1 <- reactive({
            calList <- calValHold1()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements1 <- reactive({
            variables <- calVariables1()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts1 <- reactive({
            valelements <- calValElements1()
            variableelements <- calVariableElements1()
            val.data <- myValData1()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(valelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", valelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){spectra.line.frame}
            
            if(input$filetype=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Sample", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Sample", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts1 <- reactive({
            valelements <- calValElements1()
            variableelements <- calVariableElements1()
            val.data <- myValData1()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(variableelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", variableelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){val.line.table <- spectra.line.frame[c("Sample", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant1 <- reactive({
            
            count.table <- data.frame(fullInputValCounts1())
            the.cal <- calValHold1()
            elements <- calValElements1()
            variables <- calVariableElements1()
            valdata <- myValData1()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Sample), length(elements))
            
            predicted.frame <- data.frame(count.table$Sample, predicted.vector)
            
            colnames(predicted.frame) <- c("Sample",  gsub('\\.K.alpha', '', elements))

            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
        
        
        
        
        
        
        calFileContents2 <- reactive({
            
            existingCalFile <- input$calfileinput2
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount2 <- reactive({
            inFile <- input$file2
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents2()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        myValData2 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra2()
            } else if(input$filetype=="Net"){
                netCounts2()
            }
            
            data
            
        })
        
        
        
        
        
        calValHold2 <- reactive({
            
            
            calFileContents2()[[6]]
            
            
            
        })
        
        calVariables2 <- reactive({
            
            
            calFileContents2()$Intensities
            
            
            
        })
        
        calValElements2 <- reactive({
            calList <- calValHold2()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements2 <- reactive({
            variables <- calVariables2()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts2 <- reactive({
            valelements <- calValElements2()
            variableelements <- calVariableElements2()
            val.data <- myValData2()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(valelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", valelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){spectra.line.frame}
            
            if(input$filetype=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Sample", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Sample", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts2 <- reactive({
            valelements <- calValElements2()
            variableelements <- calVariableElements2()
            val.data <- myValData2()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(variableelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", variableelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){val.line.table <- spectra.line.frame[c("Sample", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant2 <- reactive({
            
            count.table <- data.frame(fullInputValCounts2())
            the.cal <- calValHold2()
            elements <- calValElements2()
            variables <- calVariableElements2()
            valdata <- myValData2()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Sample), length(elements))
            
            predicted.frame <- data.frame(count.table$Sample, predicted.vector)
            
            colnames(predicted.frame) <- c("Sample",  gsub('\\.K.alpha', '', elements))

            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
        
        
        

        
        
        calFileContents3 <- reactive({
            
            existingCalFile <- input$calfileinput3
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount3 <- reactive({
            inFile <- input$file3
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usecalfile==TRUE){
                length(calFileContents3()$Intensities)
            }
        })
        
        
        
        
        
        
        
        
        
        
        
        
        myValData3 <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra3()
            } else if(input$filetype=="Net"){
                netCounts3()
            }
            
            data
            
        })
        
        
        
        
        
        
        
        
        
        
        calValHold3 <- reactive({
            
            
            calFileContents3()[[6]]
            
            
            
            
            
        })
        
        calVariables3 <- reactive({
            
            
            calFileContents3()$Intensities
            
            
            
        })
        
        calValElements3 <- reactive({
            calList <- calValHold3()
            valelements <- ls(calList)
            valelements
        })
        
        calVariableElements3 <- reactive({
            variables <- calVariables3()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
        
        
        
        tableInputValCounts3 <- reactive({
            valelements <- calValElements3()
            variableelements <- calVariableElements3()
            val.data <- myValData3()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(valelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", valelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){spectra.line.frame}
            
            if(input$filetype=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Sample", valelements), drop = FALSE])}
            
            
            if(input$filetype=="Net"){val.line.table <- val.data[c("Sample", valelements), drop=FALSE]}
            
            
            val.line.table
            
            
        })
        
        
        
        fullInputValCounts3 <- reactive({
            valelements <- calValElements3()
            variableelements <- calVariableElements3()
            val.data <- myValData3()
            
            if(input$filetype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Sample), length(variableelements))}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Sample, spectra.line.vector)}
            
            if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Sample", variableelements)}
            
            if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$filetype=="Spectra"){val.line.table <- spectra.line.frame[c("Sample", variableelements)]}
            
            if(input$filetype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })
        
        
        
        
        
        
        
        tableInputValQuant3 <- reactive({
            
            count.table <- data.frame(fullInputValCounts3())
            the.cal <- calValHold3()
            elements <- calValElements3()
            variables <- calVariableElements3()
            valdata <- myValData3()
            
            
            
            
            
            predicted.list <- pblapply(elements, function (x)
            if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=general.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x)
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                object=the.cal[[x]][[2]],
                newdata=simple.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.simp.prep.net(
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.tc.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                )
                )
            } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                object=the.cal[[x]][[2]],
                newdata=lukas.comp.prep.net(
                data=valdata,
                spectra.line.table=as.data.frame(
                count.table
                ),
                element.line=x,
                slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                )
                )
            }
            
            
            
            )
            
            predicted.vector <- unlist(predicted.list)
            
            dim(predicted.vector) <- c(length(count.table$Sample), length(elements))
            
            predicted.frame <- data.frame(count.table$Sample, predicted.vector)
            
            colnames(predicted.frame) <- c("Sample",  gsub('\\.K.alpha', '', elements))

            #predicted.data.table <- data.table(predicted.frame)
            #predicted.values <- t(predicted.values)
            #predicted.data.table
            predicted.frame
            
        })
    

    
    
    
    
        
        myData <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra()
            } else if(input$filetype=="Net"){
                netCounts()
            }else if(input$filetype=="Spreadsheet"){
                importSpreadsheet()
            }
            
            data
            
            
        })
        
        
        calFileContents <- reactive({
            
            existingCalFile <- input$calfileinput
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        
        
        dataCount <- reactive({
            inFile <- input$file1
            
            if(input$usecalfile==FALSE){
                length(inFile$datapath)
            }else if(input$usefile==TRUE){
                length(calFileContents()$Intensities)
            }
        })
        
        
        
        
     





        
        # Return the requested dataset
        datasetInput <- reactive({
            switch(input$element,
            "H.table" = H.table,
            "He.table" = He.table,
            "Li.table" = Li.table,
            "Be.table" = Be.table,
            "B.table" = B.table,
            "C.table" = C.table,
            "N.table" = N.table,
            "O.table" = O.table,
            "F.table" = F.table,
            "Ne.table" = Ne.table,
            "Na.table" = Na.table,
            "Mg.table" = Mg.table,
            "Al.table" = Al.table,
            "Si.table" = Si.table,
            "P.table" = P.table,
            "S.table" = S.table,
            "Cl.table" = Cl.table,
            "Ar.table" = Ar.table,
            "K.table" = K.table,
            "Ca.table" = Ca.table,
            "Sc.table" = Sc.table,
            "Ti.table" = Ti.table,
            "V.table" = V.table,
            "Cr.table" = Cr.table,
            "Mn.table" = Mn.table,
            "Fe.table" = Fe.table,
            "Co.table" = Co.table,
            "Ni.table" = Ni.table,
            "Cu.table" = Cu.table,
            "Zn.table" = Zn.table,
            "Ga.table" = Ga.table,
            "Ge.table" = Ge.table,
            "As.table" = As.table,
            "Se.table" = Se.table,
            "Br.table" = Br.table,
            "Kr.table" = Kr.table,
            "Rb.table" = Rb.table,
            "Sr.table" = Sr.table,
            "Y.table" = Y.table,
            "Zr.table" = Zr.table,
            "Nb.table" = Nb.table,
            "Mo.table" = Mo.table,
            "Tc.table" = Tc.table,
            "Ru.table" = Ru.table,
            "Rh.table" = Rh.table,
            "Pd.table" = Pd.table,
            "Ag.table" = Ag.table,
            "Cd.table" = Cd.table,
            "In.table" = In.table,
            "Sn.table" = Sn.table,
            "Sb.table" = Sb.table,
            "Te.table" = Te.table,
            "I.table" = I.table,
            "Xe.table" = Xe.table,
            "Cs.table" = Cs.table,
            "Ba.table" = Ba.table,
            "La.table" = La.table,
            "Ce.table" = Ce.table,
            "Pr.table" = Pr.table,
            "Nd.table" = Nd.table,
            "Pm.table" = Pm.table,
            "Sm.table" = Sm.table,
            "Eu.table" = Eu.table,
            "Gd.table" = Gd.table,
            "Tb.table" = Tb.table,
            "Dy.table" = Dy.table,
            "Ho.table" = Ho.table,
            "Er.table" = Er.table,
            "Tm.table" = Tm.table,
            "Yb.table" = Yb.table,
            "Lu.table" = Lu.table,
            "Hf.table" = Hf.table,
            "Ta.table" = Ta.table,
            "W.table" = W.table,
            "Re.table" = Re.table,
            "Os.table" = Os.table,
            "Ir.table" = Ir.table,
            "Pt.table" = Pt.table,
            "Au.table" = Au.table,
            "Hg.table" = Hg.table,
            "Tl.table" = Tl.table,
            "Pb.table" = Pb.table,
            "Bi.table" = Bi.table,
            "Po.table" = Po.table,
            "At.table" = At.table,
            "Rn.table" = Rn.table,
            "Fr.table" = Fr.table,
            "Ra.table" = Ra.table,
            "Ac.table" = Ac.table,
            "Th.table" = Th.table,
            "Pa.table" = Pa.table,
            "U.table" = U.table)
        })
        
         observeEvent(input$actionplot, {
        
        # Expression that generates a histogram. The expression is
        # wrapped in a call to renderPlot to indicate that:
        #
        #  1) It is "reactive" and therefore should re-execute automatically
        #     when inputs change
        #  2) Its output type is a plot
        ranges <- reactiveValues(x = NULL, y = NULL)
        
        
        
         plotInput <- reactive({
             


             data <- myData()
             id.seq <- seq(1, 2048,1)
             
             n <- length(data$Energy)
             
             element <- datasetInput()
             intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
             intensity.base <- (element$Intensity/max(element$Intensity))
             
             
             
             spectral.plot <- qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Sample) +
             theme_light()+
             theme(legend.position="bottom") +
             geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             scale_colour_discrete("Sample") +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y)
             
             
             ###Background Subtraction
             # data.n <- as.data.frame(dcast(data=data, formula=Energy~Spectrum, fun.aggregate = sum,value.var = "CPS"))
             
             # background.subtracted <- pbapply(data.n, 2, Hodder.v)
             #background.subtracted <- as.data.frame(background.subtracted)
             # background.subtracted$Energy <- data.n$Energy
             
             # background.melt <- melt(background.subtracted, id="Energy")
             # colnames(background.melt) <- c("Energy", "Sample", "CPS")
             
             
             #transformed.spectral.plot <-  qplot(background.melt$Energy+1, SMA(background.melt$CPS, 10), xlab = "Energy (keV)", ylab = "CPS", geom="line", colour=background.melt$Sample)+
             # theme_light()+
             # theme(legend.position="bottom") +
             # geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             # scale_colour_discrete("Sample") +
             # coord_cartesian(xlim = ranges$x, ylim = ranges$y)


# if (input$backgroundsubtract == FALSE) {
#   spectral.plot

#} else if (input$backgroundsubtract == TRUE) {
#   transformed.spectral.plot
# }

spectral.plot
       

         })


        output$distPlot <- renderPlot({

print(plotInput())


        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$plot1_dblclick, {
            brush <- input$plot1_brush
            if (!is.null(brush)) {
                ranges$x <- c(brush$xmin*mean(data$Energy), brush$xmax*max(data$Energy))
                ranges$y <- c(brush$ymin*mean(data$CPS), brush$ymax*max(data$CPS))
                
            } else {
                ranges$x <- NULL
                ranges$y <- NULL
            }
            
            
            
        })
        
        output$downloadPlot <- downloadHandler(
        
        plotname <- "SpectraPlot",
        
    
        filename = function() { paste(paste(c(input$projectname, "-", plotname), collapse=''), '.tiff', sep='') },
        content = function(file) {
            ggsave(file,plotInput(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
        }
        )
        
        
        
         })
        
      
   
    
    
         
         dataMergeFirst <- reactive({
             
             if(input$filetype!="Spreadsheet"){
                 
                 first.instrument <- if(input$filetype=="Spectra" && input$usecalfile==FALSE){
                     spectra.line.fn(myData1())
                 } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                     myData1()
                 } else if(input$filetype=="Spectra" && input$usecalfile==TRUE) {
                     tableInputValQuant1()
                 } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                     tableInputValQuant1()
                 }
                 
                 if(is.null(input$file2)==FALSE){
                     second.instrument <- if(input$filetype=="Spectra" && input$usecalfile==FALSE){
                         spectra.line.fn(myData2())
                     } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                         myData2()
                     } else if(input$filetype=="Spectra" && input$usecalfile==TRUE) {
                         tableInputValQuant2()
                     } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                         tableInputValQuant2()
                     }
                 }
                 
                 if(is.null(input$file3)==FALSE){
                     third.instrument <- if(input$filetype=="Spectra" && input$usecalfile==FALSE){
                         spectra.line.fn(myData3())
                     } else if(input$filetype=="Net" && input$usecalfile==FALSE){
                         myData3()
                     } else if(input$filetype=="Spectra" && input$usecalfile==TRUE) {
                         tableInputValQuant3()
                     } else if(input$filetype=="Net" && input$usecalfile==TRUE){
                         tableInputValQuant3()
                     }
                 }
                 
                 
                 if(is.null(input$file2)==TRUE && is.null(input$file3)==TRUE){
                     first.instrument
                 }else if(is.null(input$file2)==FALSE && is.null(input$file3)==TRUE){
                     merge(first.instrument, second.instrument, all=TRUE)
                 }else if(is.null(input$file2)==FALSE && is.null(input$file3)==FALSE){
                     merge(merge(first.instrument, second.instrument, all=TRUE), third.instrument, all=TRUE)
                 }
             }  else if(input$filetype=="Spreadsheet"){
                 importSpreadsheet()
             }
             
         })
         
         
         dataMerge <- reactive({
             
             data <- dataMergeFirst()
             
             data.names <- colnames(data)
             data.names.pass.1 <- gsub("*.K.alpha", "", data.names)
             data.names.pass.2 <- gsub("*.K.beta", "", data.names.pass.1)
             data.names.pass.3 <- gsub("*.L.alpha", "", data.names.pass.2)
             data.names.pass.4 <- gsub("*.L.beta", "", data.names.pass.3)
             data.names.pass.5 <- gsub("*.K12", "", data.names.pass.4)
             data.names.pass.6 <- gsub("*.L1", "", data.names.pass.5)
             data.names.pass.7 <- gsub("*Ka1", "", data.names.pass.6)
             data.names.pass.8 <- gsub("*La1", "", data.names.pass.7)
             data.names.pass.9 <- gsub("*Lb1", "", data.names.pass.8)


             colnames(data) <- data.names.pass.9
             data
         })
         
         
         
         
         
         lineOptions <- reactive({
             
             spectra.line.table <- dataMerge()[ ,!(colnames(dataMerge()) == "Sample")]
             if(input$usecalfile==TRUE){
                 quant.frame <- dataMerge()[ ,!(colnames(dataMerge()) =="Sample")]
                 quantified <- colnames(quant.frame)
             }
             
             standard <- if(input$usecalfile==FALSE && input$filetype=="Spectra"){
                 spectralLines
             } else if(input$usecalfile==FALSE && input$filetype=="Net"){
                 colnames(spectra.line.table)
             } else if(input$usecalfile==TRUE && input$filetype=="Spectra"){
                 quantified
             }else if(input$usecalfile==TRUE && input$filetype=="Net"){
                 quantified
             } else if(input$filetype=="Spreadsheet"){
                 colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
             }
             
         })
         
         defaultLines <- reactive({
             
             spectra.line.table <- dataMerge()
             if(input$usecalfile==TRUE){quantified <- colnames(dataMerge()[ ,!(colnames(dataMerge()) =="Sample")])
             }
             
             standard <- if(input$usecalfile==FALSE && input$filetype=="Spectra"){
                 c("Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha", "Cu.K.alpha", "Zn.K.alpha")
             } else if(input$usecalfile==FALSE && input$filetype=="Net"){
                 colnames(spectra.line.table)
             } else if(input$usecalfile==TRUE && input$filetype=="Spectra"){
                 quantified
             }else if(input$usecalfile==TRUE && input$filetype=="Net"){
                 quantified
             } else if(input$filetype=="Spreadsheet"){
                 colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
             }
             
         })
         
        
         
         
         output$defaultlines <- renderUI({
             
             
             checkboxGroupInput('show_vars', 'Elemental lines to show:',
             choices=lineOptions(), selected = lineOptions())
         })
         


 tableInput <- reactive({
     spectra.line.table <- dataMerge()
     select.line.table <- spectra.line.table[, c("Sample", input$show_vars), drop = FALSE]
     select.line.table
 })


  output$mytable1 <- renderDataTable({
   
  data.table(tableInput())

  })
  

  
  
  hotableInput <- reactive({
      
      spectra.line.table <- dataMerge()
      spectra.line.vector <- spectra.line.table$Sample
      
      n <- length(spectra.line.vector)
      
      lin.vector <- seq(from = 1, to = n, by = 1)


      na.vector <- rep("HOLD", n)
     
      
      
      empty.line.table <- data.frame(spectra.line.vector, na.vector, na.vector, na.vector, na.vector, na.vector, na.vector)
      colnames(empty.line.table) <- c("Sample", "Longitude", "Latitude", "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4")
      
      empty.line.table$Quantitative <- lin.vector
      
      
      empty.line.table
      
  })
  
  values <- reactiveValues()
  
  observe({
      if (!is.null(input$hot)) {
          DF = hot_to_r(input$hot)
      } else {
          if (is.null(values[["DF"]]))
          DF <- hotableInput()
          else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
  })

  
  ## Handsontable
  
  output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
      rhandsontable(DF, useTypes = FALSE, stretchH = "all")
  })
  
  
  

  qualitativeSelect1 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative1)
      qual.options
      
  })
  
  qualitativeSelect2 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative2)
      qual.options
      
  })
  
  qualitativeSelect3 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative3)
      qual.options
      
  })
  
  qualitativeSelect4 <- reactive({
      
      qualitative.data <- values[["DF"]]
      qual.options <- unique(qualitative.data$Qualitative4)
      qual.options
      
  })
  

  
  
  output$qualSelect1a <- renderUI({
      
      
      checkboxGroupInput('qual_select1a', 'Select',
      choices=qualitativeSelect1(), selected = qualitativeSelect1())
  })
  
  output$qualSelect2a <- renderUI({
      
      
      checkboxGroupInput('qual_select2a', 'Select',
      choices=qualitativeSelect2(), selected = qualitativeSelect2())
  })
  
  output$qualSelect3a <- renderUI({
      
      
      checkboxGroupInput('qual_select3a', 'Select',
      choices=qualitativeSelect3(), selected = qualitativeSelect3())
  })
  
  output$qualSelect4a <- renderUI({
      
      
      checkboxGroupInput('qual_select4a', 'Select',
      choices=qualitativeSelect4(), selected = qualitativeSelect4())
  })
  
  

  



dataMerge1 <- reactive({
    
    spectra.line.table <- as.data.frame(tableInput())
    quality.table <- values[["DF"]]

    spectra.line.table$Qualitative1 <- quality.table$Qualitative1
    spectra.line.table$Qualitative2 <- quality.table$Qualitative2
    spectra.line.table$Qualitative3 <- quality.table$Qualitative3
    spectra.line.table$Qualitative4 <- quality.table$Qualitative4


    
    spectra.line.table$Quantitative <- quality.table$Quantitative
    


    
    filter(spectra.line.table,
    Qualitative1 %in% input$qual_select1a,
    Qualitative2 %in% input$qual_select2a,
    Qualitative3 %in% input$qual_select3a,
    Qualitative4 %in% input$qual_select4a
    )
    
})


mergedHold <- reactive({
    
    merged.table <- dataMerge1()
    merged.table$Sample
    
})

output$clipsubsetfinal <- renderUI({
    
    checkboxGroupInput("show_rows", label="Choose Samples", choices=mergedHold(), selected=mergedHold())
    
})


dataMerge2 <- reactive({
    
    merged.table <- dataMerge1()
    
    filter(merged.table,
    Sample %in% input$show_rows
    )
    

    
})







dataMerge2TableOutput <- reactive({
    spectra.line.table <- dataMerge2()
    select.line.table <- datatable(spectra.line.table)
    select.line.table
})

output$mydatamerge2 <- renderDataTable({
    
    dataMerge2TableOutput()
    
})

output$downloadsubsetfinal <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "Subset"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(dataMerge2(), file)
}
)






qualityTable <- reactive({
    spectra.line.table <- dataMerge2()
    quality.table <- values[["DF"]]
    
    partial.table <- spectra.line.table[,c("Sample", input$show_vars)]

    
    full.table <- merge(x=partial.table, y=quality.table, by.x="Sample", by.y="Sample")
    full.table <- full.table[complete.cases(full.table),]
    
    full.table[,ls(quality.table)]
    
})



choiceLines <- reactive({
    
    spectra.line.table <- dataMerge2()
    
    standard <- if(input$filetype=="Spectra"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
    } else if(input$filetype=="Net"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
    } else if(input$filetype=="Spreadsheet"){
        colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
    }
    
})
  

  
  output$downloadData <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "CountTable"), collapse=''), '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(tableInput(), file)
  }
  )
  
  
  
  #Source Manager
  
  output$minlat <- renderUI({
      if(input$regionselect=="Lat/Long")
      {numericInput('latmin', "Latitude Minimum", min=-90, max=90, step=.1, value=-90)
      }else{
          p()
      }
  })
  
  output$maxlat <- renderUI({
       if(input$regionselect=="Lat/Long"){
           numericInput('latmax', "Latitude Maximum", min=-90, max=90, step=.1, value=90)
       }else{
           p()
       }
  })
  
  output$minlong <- renderUI({
       if(input$regionselect=="Lat/Long"){
           numericInput('lonmin', "Longitude Minimum", min=-180, max=180, step=.1, value=-180)
       }else{
           p()
       }
  })
  
  
  output$maxlong <- renderUI({
       if(input$regionselect=="Lat/Long"){
           numericInput('lonmax', "Longitude Maximum", min=-180, max=180, step=.1, value=180)
       }else{
           p()
       }
  })
  
  output$choosecountry <- renderUI({
      if(input$regionselect=="Political"){
          selectInput('selectcountry', "Choose Country", choices=unique(all.data$Country), selected="USA", multiple=TRUE)
      }else{
          p()
      }
  })
  
  output$choosestate <- renderUI({
      if(input$regionselect=="Political"){
          
          subset.state <- dplyr::filter(all.data,
          Country %in% input$selectcountry
          )
          
          selectInput('selectstate', "Choose State/Provence", choices=unique(subset.state$State..or.the.like.), selected=unique(subset.state$State..or.the.like.), multiple=TRUE)
      }else{
          p()
      }
  })
  
  output$choosecontinent <- renderUI({
      if(input$regionselect=="Continent"){
          selectInput('selectcontinent', "Choose Continent", choices=unique(all.data$Continent), selected="North America", multiple=TRUE)
      }else{
          p()
      }
  })
  
  output$chooseregion <- renderUI({
       if(input$regionselect=="Region"){
           selectInput('selectregion', "Choose Region", choices=unique(all.data$Region.Plate), selected="Colorado Plateau", multiple=TRUE)
       }else{
           p()
       }
  })
  


yearSequence <- reactive({
    
    seq(1, length(input$show_vars), 1)
})
  
  ####Obsidian Sourcing
  
  sourcePrep <- reactive({
      Year <- yearSequence()
      
      
      all.data$Latitude <- as.numeric(as.character(all.data$Latitude))
      all.data$Longitude <- as.numeric(as.character(all.data$Longitude))
      all.data$Sources <- make.names(all.data$Source.Common.Name, unique=TRUE)
      
      region.data <- if(input$regionselect=="Lat/Long"){
          dplyr::filter(all.data,
          Latitude %between%  c(input$latmin, input$latmax),
          Longitude %between% c(input$lonmin, input$lonmax)
          )
      } else if(input$regionselect=="Political"){
          dplyr::filter(all.data,
          Country %in% input$selectcountry,
          State..or.the.like. %in% input$selectstate
          )
      }else if(input$regionselect=="Continent"){
          dplyr::filter(all.data,
          Continent %in% input$selectcontinent
          )
      }else if(input$regionselect=="Region"){
          dplyr::filter(all.data,
          Region.Plate %in% input$selectregion
          )
      }
      
      region.data
      
  })
  
  
  
  sourceList <- reactive({
      
      Year <- yearSequence()

      
      region.data <- sourcePrep()
     
      short.source <- region.data[, c("Source.Common.Name", paste0("Mean", input$show_vars))]
      colnames(short.source) <- c("Sources", input$show_vars)
      short.source$Sources <- make.names(short.source$Sources, unique=TRUE)
      short.source <- short.source[order(short.source$Sources),]
      short.source[short.source == 0] <- NA
      short.source <- short.source[complete.cases(short.source),]
      
      temp.source <- short.source[,c(input$show_vars)]
      
      temp.source.t <- t(temp.source)
      colnames(temp.source.t) <- short.source$Sources
      
      
      source.list <- apply(temp.source.t, 2, function(x) list(as.vector(x)))
      source.list <- lapply(source.list, "[[", 1)
      #names(source.list) <- as.vector(short.source$Sources)
      source.names <- colnames(temp.source.t)
      source.list <- lapply(source.list, function(x) data.frame(Year, x))
      for(i in 1:length(source.names)){
          colnames(source.list[[i]]) <- c("Year", source.names[i])
      }
      
      source.list
      
  })
  
  sourceMetadata <- reactive({
      
      Year <- yearSequence()

      
      region.data <- sourcePrep()

      
      source.metadata <- region.data[,c("Source.Common.Name", "Longitude", "Latitude", paste0("Mean", input$show_vars))]
      colnames(source.metadata)[1] <- "Sources"
      source.metadata <- source.metadata[order(source.metadata$Sources),]
      source.metadata <- source.metadata[complete.cases(source.metadata),]
      source.metadata <- source.metadata[,c("Sources", "Longitude", "Latitude")]
      source.metadata[, 2:3] <- sapply(source.metadata[, 2:3], function(x) as.numeric(as.character(x)))
      
      source.metadata
  })
  

  
  sourceLength <- reactive({
      
      length(sourceList())
      
  })
  
  output$source.number <- renderUI({
      
      
      
      tagList(sourceLength())
      
  })
  
  sampleData <- reactive({
      
      Year <- yearSequence()

      
      element.vector <- gsub('\\Mean', '', input$show_vars)
      
      user.data <- dataMerge2()
      user.data <- user.data[,c("Sample", input$show_vars)]
      user.data <- user.data[complete.cases(user.data),]

      
      short.data.t <- t(user.data[,-1])
      colnames(short.data.t) <- user.data$Sample
      Year <- seq(1, length(short.data.t[,1]), 1)
      
      tree.frame <- data.frame(Year, short.data.t)
      
      tree.frame
      
  })
  
  
  
  
  
  obsidianSource <- reactive({
      
      Year <- yearSequence()
      time <- Year
      tree.dataframe <- sampleData()
      tree.source.list <- sourceList()
      if(input$bayesian==TRUE){tree.metadata <- values[["DF"]]}
      if(input$bayesian==TRUE){source.metadata <- sourceMetadata()}
      sensitivity <- input$sensitivity
      
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
      

      
      #if(input$bayesian==TRUE){
      #clusterExport(cl, varlist= c("time", "tree.dataframe", "tree.source.list", "tree.metadata", "source.metadata", "sensitivity", "apply_treeJackKnife", "obsidianJackKnifeMultipleSourceProb"), envir=environment())
      #} else if(input$bayesian==FALSE){
      #clusterExport(cl, varlist= c("time", "tree.dataframe", "tree.source.list", "sensitivity", "apply_treeJackKnife", "obsidianJackKnifeMultipleSourceSimp"), envir=environment())
      #}
      
      withProgress(message = 'Processing Data', value = 0, {
          n <- length(tree.source.list)

      
       result.list <- if(input$bayesian==TRUE){
           obsidianJackKnifeMultipleSourceProb(time=time, tree.dataframe=tree.dataframe, tree.source.list=tree.source.list, tree.metadata=tree.metadata, source.metadata=source.metadata, sensitivity=sensitivity)
       } else if(input$bayesian==FALSE){
            obsidianJackKnifeMultipleSourceSimp(time=time, tree.dataframe=tree.dataframe, tree.source.list=tree.source.list, sensitivity=sensitivity)
       }
       
       incProgress(1/n)
       Sys.sleep(0.1)
      })
      result.list
  })
  

  prepMap <- reactive({
      
      region.data <- sourcePrep()
      
      summary.frame <- if(input$bayesian==TRUE){
          as.data.frame(table(obsidianSource()[["Posterior Probabilities"]]["Source"]))
      } else if(input$bayesian==FALSE){
          as.data.frame(table(obsidianSource()[["T-Value"]]["Source"]))
      }
      
      colnames(summary.frame) <- c("Source", "Total")
      summary.frame$Percent <- summary.frame$Total/sum(summary.frame$Total)
      
      
      
      
      small.region.frame <- region.data[,c("Source.Common.Name", "Latitude", "Longitude")]
      small.region.frame$Source.Common.Name <- make.names(small.region.frame$Source.Common.Name)
      colnames(small.region.frame)[1] <- "Source"
      small.region.frame$Latitude <- round(as.numeric(as.character(small.region.frame$Latitude)), 3)
      small.region.frame$Longitude <- round(as.numeric(as.character(small.region.frame$Longitude)), 3)
      
      
      small.region.frame$Description <- noquote(do.call(paste, c(region.data[,c("Geology", "Ethnominerology")], sep=" ")))
      
      small.region.frame$References <- noquote(region.data$Bibliographies)
      
      
      
      for.maps <- merge(x=summary.frame, y=small.region.frame, by.x="Source", by.y="Source")
      for.maps <- for.maps[order(-for.maps$Percent),]

      
      data.frame(for.maps)
      
  })
  
  obsidianSourceResults <- reactive({
      
      result.list <- obsidianSource()
      result.list[["Summary"]] <- prepMap()
      result.list
      
  })
  
  output$sourcelistresults <- downloadHandler(
  filename <- function(){
      paste(input$projectname, "xlsx", sep=".")
  },
  
  content = function(file) {
      openxlsx::write.xlsx(obsidianSourceResults(), file=file, colWidths = c(NA, "auto", "auto"))
      
  }
  )
  
  output$sourcelistresultsraw <- downloadHandler(
  filename <- function(){
      paste(input$projectname, "source", sep=".")
  },
  
  content = function(file) {
      saveRDS(obsidianSourceResults(), file=file, compress="xz")
      
  }
  )
  
  
  output$summarytable <- renderDataTable({
      
      data <- prepMap()
      data$Percent <- percent(data$Percent)
      
      data.table(data[,c("Source", "Total", "Percent", "Latitude", "Longitude", "Description")])
      
  })
  
  
  sourceMap <- reactive({
      
      tree.metadata.frame <- values[["DF"]]
      tree.metadata.frame$Latitude <- as.numeric(as.character(tree.metadata.frame$Latitude))
      tree.metadata.frame$Longitude <- as.numeric(as.character(tree.metadata.frame$Longitude))
      
      for.maps <- prepMap()
      
      latmin <-  as.numeric(as.character(if(input$adjustcoordinates==TRUE){
          input$latminmap
      }else if(input$adjustcoordinates==FALSE){
          min(for.maps$Latitude)
      }))
      
      latmax <-  as.numeric(as.character(if(input$adjustcoordinates==TRUE){
          input$latmaxmap
      }else if(input$adjustcoordinates==FALSE){
          max(for.maps$Latitude)
      }))
      
      longmin <-  as.numeric(as.character(if(input$adjustcoordinates==TRUE){
          input$lonminmap
      }else if(input$adjustcoordinates==FALSE){
          min(for.maps$Longitude)
      }))
      
      longmax <-  as.numeric(as.character(if(input$adjustcoordinates==TRUE){
          input$lonmaxmap
      }else if(input$adjustcoordinates==FALSE){
          max(for.maps$Longitude)
      }))
      
      source.area <- get_map(location=c(lon=weighted.mean(for.maps$Longitude, n=for.maps$Total), lat=weighted.mean(for.maps$Latitude, n=for.maps.total)), zoom=5, maptype="terrain")
      
      test.map <- ggmap(source.area) +
      geom_rect(xmin=-180, xmax=180, ymin=-90, ymax=90, alpha=0.1, fill="white")+
      geom_point(data=for.maps, aes(x=Longitude, y=Latitude, size=Percent*3), shape=13) +
      geom_point(data=for.maps, aes(x=Longitude, y=Latitude, colour=Source, size=Percent*3), alpha=0.7) +
      geom_point(data=tree.metadata.frame, aes(x=Longitude, y=Latitude), shape=17) +
      scale_y_continuous("Latitude", limits=c(latmin, latmax)) +
      scale_x_continuous("Longitude", limits=c(longmin, longmax)) +
      #scale_shape_manual("Source", values=1:nlevels(as.factor(for.maps$Source))) +
      guides(size=FALSE) +
      coord_equal() +
      theme_classic()
      test.map
      
      
  })
  
  
  
  output$minlatmap <- renderUI({
      if(input$adjustcoordinates==TRUE){
          numericInput('latminmap', "Latitude Minimum", min=-90, max=90, step=.1, value=min(prepMap()$Latitude))
      }else{
          p()
      }
  })
  
  output$maxlatmap <- renderUI({
      if(input$adjustcoordinates==TRUE){
          numericInput('latmaxmap', "Latitude Maximum", min=-90, max=90, step=.1, value=max(prepMap()$Latitude))
      }else{
          p()
      }
  })
  
  output$minlongmap <- renderUI({
      if(input$adjustcoordinates==TRUE){
          numericInput('lonminmap', "Longitude Minimum", min=-180, max=180, step=.1, value=min(prepMap()$Longitude))
      }else{
          p()
      }
  })
  
  
  output$maxlongmap <- renderUI({
      if(input$adjustcoordinates==TRUE){
          numericInput('lonmaxmap', "Longitude Maximum", min=-180, max=180, step=.1, value=max(prepMap()$Longitude))
      }else{
          p()
      }
  })
  
  
  output$sourcedMap <- renderPlot({
      
      sourceMap()
      
  })
  
  
  output$downloadsourcemap <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "Map"), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,sourceMap(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
  }
  )
  
  
  
  
  dataMerge3 <- reactive({
      
      origional.data <- dataMerge2()
      
      #origional.data$Serial <- obsidianSource()[["Posterior Probabilities"]]["Serial"]
      if(input$bayesian==TRUE){
          origional.data$Source <- unlist(obsidianSource()[["Posterior Probabilities"]]["Source"])
      } else if(input$bayesian==FALSE){
          origional.data$Source <- unlist(obsidianSource()[["T-Value"]]["Source"])
      }
      as.data.frame(origional.data)

  })
  
  output$sourcedTable <- renderDataTable({
      
      data.table(dataMerge3()[,!names(dataMerge3()) %in% c("Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative")])
      
  })
  
  

  
  output$pcaellipsesources <- renderUI({

      if(input$elipseplot1==TRUE){
          selectInput('pcasources', label="Sources to Plot", choices=names(sourceList()), selected=prepMap()$Source, multiple=TRUE)
      }else{
          p()
      }
      
  })
  
  
  
  
  sourceDefinitions <- reactive({
      
      region.data <- sourcePrep()
      
      short.source <- region.data[, c("Source.Common.Name", paste0("Mean", input$show_vars), paste0("SD", input$show_vars))]
      colnames(short.source)[1] <- "Sources"
      short.source$Sources <- make.names(short.source$Sources, unique=TRUE)

      
      #shorter.source <- dplyr::filter(short.source,
      #Sources %in% input$pcasources)
      
      

      
      source.sim.list <- pblapply(input$pcasources, function(x) source.replicate(source=x, data=short.source, elements=input$show_vars))
      
      source.sim.frame <- as.data.frame(do.call("rbind", source.sim.list))
      source.sim.frame
      
  })
  
  

  #####PCA Analysis
  

  
  xrfKReactive <- reactive({
         spectra.line.table <- dataMerge3()
         
         xrf.pca.frame <- spectra.line.table[,input$show_vars]
         xrf.pca.frame <- xrf.pca.frame[complete.cases(xrf.pca.frame),]
         xrf.pca <- prcomp(xrf.pca.frame, scale.=FALSE)
         xrf.k <- kmeans(xrf.pca.frame, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))

         xrf.scores <- as.data.frame(xrf.pca$x)

        cluster.frame <- data.frame(spectra.line.table$Sample, spectra.line.table$Source, xrf.k$cluster, xrf.scores)

        colnames(cluster.frame) <- c("Assay", "Source", "Cluster", names(xrf.scores))
      
  
          cluster.frame
      
      
  })
  
  
  xrfkReactivePrep <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table <- spectra.line.table[,c("Sample", "Source", input$show_vars)]
      spectra.line.table$Type <- rep("Sample", length(spectra.line.table[,1]))
      
      simulation.table <- sourceDefinitions()
      simulation.table <- simulation.table[,c("Source", input$show_vars)]
      simulation.table$Sample <- simulation.table$Source
      simulation.table$Type <- rep("Simulation", length(simulation.table[,1]))
      
      spectra.line.table.sim <- rbind(spectra.line.table, simulation.table)
      
      
      xrf.pca.frame.sim <- spectra.line.table.sim[,input$show_vars]
      xrf.pca.frame.sim <- xrf.pca.frame.sim[complete.cases(xrf.pca.frame.sim),]
      
      
      xrf.k.sim <- kmeans(xrf.pca.frame.sim, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      xrf.pca.sim <- prcomp(xrf.pca.frame.sim, scale.=FALSE)
      
      xrf.scores.sim <- as.data.frame(xrf.pca.sim$x)
      
      cluster.frame.sim <- data.frame(spectra.line.table.sim$Sample, spectra.line.table.sim$Source, spectra.line.table.sim$Type, xrf.k.sim$cluster, xrf.scores.sim)
      
      colnames(cluster.frame.sim) <- c("Assay", "Source", "Type", "Cluster", names(xrf.scores.sim))
      
      cluster.list <- list(xrfKReactive(), cluster.frame.sim, spectra.line.table.sim)
      names(cluster.list) <- c("Samples", "Simulations", "RawSimulations")
      
      cluster.list
      
      
  })

  
  xrfPCAReactive <- reactive({
      
      spectra.line.table <- dataMerge3()
      
     

      
      xrf.clusters <- xrfKReactive()
      
      element.counts <- spectra.line.table[,input$show_vars]

      
      
      
      xrf.pca.results <- data.frame(xrf.clusters, element.counts)
      
      
      xrf.pca.results
  })
  
  

  

  
  xMinPCA <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      
      xrf.k <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["Simulations"]]
        }else{
          xrfKReactive()
        }

      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      
      
      
      round(min(spectra.line.table$PC1), 2)
      
      
  })
  
  xMaxPCA <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      
      xrf.k <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }

      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      
     
      
      round(max(spectra.line.table$PC1), 2)
      
      
  })
  
  
  
  yMinPCA <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      
      xrf.k <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }

      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      
    
      round(min(spectra.line.table$PC2), 2)
      
      
  })
  
  yMaxPCA <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      
      xrf.k <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }

      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      

 
      
      round(max(spectra.line.table$PC2), 2)
      
      
  })
  
  output$inxlimrangepca <- renderUI({
      
      
      sliderInput("xlimrangepca", "X axis", min=xMinPCA(), max=xMaxPCA(), value=c(xMinPCA(), xMaxPCA()), round=FALSE)
  })
  
  output$inylimrangepca <- renderUI({
      
      
      sliderInput("ylimrangepca", "Y axis", min=yMinPCA(), max=yMaxPCA(), value=c(yMinPCA(), yMaxPCA()), round=FALSE)
  })
  
  focusTable <- reactive({
      
      focus.frame <- data.frame(values[["DF"]], dataMerge3()$Source)
      colnames(focus.frame) <- c(names(values[["DF"]]), "Source")
      focus.frame
    
  })
  
  
  output$pcaFocusVariable <- renderUI({
      
      if(input$pcacolour=="Focus"){selectInput('pcafocusvariable', "Choose Variable", choices=names(focusTable()), selected="Source")} else {
          p()
      }
      
  })
  
  output$pcaFocusUI <- renderUI({
      
      if(input$pcacolour=="Focus"){selectInput('pcafocuschoice', "Choose Focus", choices=unique(focusTable()[,input$pcafocusvariable]), selected=NULL, multiple=TRUE)} else {
          p()
      }
      
  })
  
  output$pcaFocusLabel <- renderUI({
      if(input$pcacolour=="Focus"){selectInput('pcafocuslabel', "Choose Label", choices=c("None", names(focusTable())), selected="None")} else {
          p()
      }
      
  })
  
  

  plotInput2 <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      

      
      xrf.k <- if(input$elipseplot1==TRUE){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }
  
  quality.table <-qualityTable()
  
  colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
  colnames(colour.table) <- c("Cluster", names(spectra.line.table))
  
  
  
  
  unique.spec <- seq(1, length(colour.table$Sample), 1)
  null <- rep(1, length(unique.spec))
  
  spectra.line.table$Cluster <- xrf.k$Cluster
  spectra.line.table$PC1 <- xrf.k$PC1
  spectra.line.table$PC2 <- xrf.k$PC2
  
  
  spectra.line.table <- subset(spectra.line.table, !(spectra.line.table$PC1 < input$xlimrangepca[1] | spectra.line.table$PC1 > input$xlimrangepca[2]))
  
  spectra.line.table <- subset(spectra.line.table, !(spectra.line.table$PC2 < input$ylimrangepca[1] | spectra.line.table$PC2 > input$ylimrangepca[2]))
  
  if(input$elipseplot1==TRUE){just.samples <- dplyr::filter(spectra.line.table, Type %in% "Sample")}
  
  if(input$elipseplot1==TRUE){just.simulations <- dplyr::filter(spectra.line.table, Type %in% "Simulation")}


  
  
  basic <- ggplot(data=spectra.line.table, aes(PC1, PC2)) +
  geom_point(aes(PC1, PC2), size = input$spotsize) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  
  regular <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Cluster), shape=as.factor(Cluster)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster))) +
  scale_colour_discrete("Cluster") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)

if(input$elipseplot1==TRUE && input$pcacolour == "Cluster"){
  ellipse <- ggplot(data= spectra.line.table)+
  geom_point(data=just.samples, aes(PC1, PC2, colour=as.factor(Cluster), shape=as.factor(Cluster)), size = input$spotsize+1) +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster))) +
  scale_colour_discrete("Cluster") +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
}
  
  source.regular <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Source), shape=as.factor(Source)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Source", values=1:nlevels(as.factor(spectra.line.table$Source))) +
  scale_colour_discrete("Source") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  if(input$elipseplot1==TRUE && input$pcacolour == "Source"){
  source.ellipse <- ggplot(data=spectra.line.table, aes(PC1, PC2))+
  geom_point(data=just.samples, aes(PC1, PC2, colour=as.factor(Source), shape=as.factor(Source)), size = input$spotsize+1) +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Source", values=1:nlevels(as.factor(spectra.line.table$Source))) +
  scale_colour_discrete("Source") +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  }


  qual.regular.1 <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Qualitative1), shape=as.factor(Qualitative1)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Qualitative1", values=1:nlevels(as.factor(spectra.line.table$Qualitative1))) +
  scale_colour_discrete("Qualitative1") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  if(input$elipseplot1==TRUE && input$pcacolour == "Qualitative1"){
  qual.ellipse.1 <- ggplot(data= spectra.line.table)+
  geom_point(data=just.samples, aes(PC1, PC2, colour=as.factor(Qualitative1), shape=as.factor(Qualitative1)), size = input$spotsize+1) +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Qualitative1", values=1:nlevels(as.factor(spectra.line.table$Qualitative1))) +
  scale_colour_discrete("Qualitative1") +
  geom_point(data=just.samples, aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  }
  
  qual.regular.2 <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Qualitative2), shape=as.factor(Qualitative2)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Qualitative2", values=1:nlevels(as.factor(spectra.line.table$Qualitative2))) +
  scale_colour_discrete("Qualitative2") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  if(input$elipseplot1==TRUE && input$pcacolour == "Qualitative2"){
  qual.ellipse.2 <- ggplot(data= spectra.line.table)+
  geom_point(data=just.samples, aes(PC1, PC2, colour=as.factor(Qualitative2), shape=as.factor(Qualitative2)), size = input$spotsize+1) +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Qualitative2", values=1:nlevels(as.factor(spectra.line.table$Qualitative2))) +
  scale_colour_discrete("Qualitative2") +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  }
  
  if(input$elipseplot1==TRUE && input$pcacolour == "Qualitative3"){
  qual.ellipse.3 <- ggplot(data= spectra.line.table)+
  geom_point(data=just.samples,aes(PC1, PC2, colour=as.factor(Qualitative3), shape=as.factor(Qualitative3)), size = input$spotsize+1) +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Qualitative3", values=1:nlevels(as.factor(spectra.line.table$Qualitative3))) +
  scale_colour_discrete("Qualitative3") +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  }
  
  qual.regular.3 <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Qualitative3), shape=as.factor(Qualitative3)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Qualitative3", values=1:nlevels(as.factor(spectra.line.table$Qualitative3))) +
  scale_colour_discrete("Qualitative3") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  if(input$elipseplot1==TRUE && input$pcacolour == "Qualitative4"){
  qual.ellipse.4 <- ggplot(data= spectra.line.table)+
  geom_point(data=just.samples,aes(PC1, PC2, colour=as.factor(Qualitative4), shape=as.factor(Qualitative4)), size = input$spotsize+1) +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(linetype=FALSE) +
  scale_shape_manual("Qualitative4", values=1:nlevels(as.factor(spectra.line.table$Qualitative4))) +
  scale_colour_discrete("Qualitative4") +
  geom_point(data=just.samples,aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  }
  
  qual.regular.4 <- ggplot(data= spectra.line.table) +
  geom_point(aes(PC1, PC2, colour=as.factor(Qualitative4), shape=as.factor(Qualitative4)), size = input$spotsize+1) +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  scale_shape_manual("Qualitative4", values=1:nlevels(as.factor(spectra.line.table$Qualitative4))) +
  scale_colour_discrete("Qualitative4") +
  geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel=="None" && input$elipseplot1 == FALSE) {new.spectra.line.table <- spectra.line.table[,c("Sample", "PC1", "PC2", input$pcafocusvariable)]}
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel=="None" && input$elipseplot1 == FALSE) {colnames(new.spectra.line.table) <- c("Sample", "PC1", "PC2", "Selected")}
  
  if (input$elipseplot1 == FALSE && input$pcacolour == "Focus" && input$pcafocuslabel=="None") {
      select.plot <- gghighlight_point(new.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize,  use_group_by=FALSE, use_direct_label=FALSE) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      theme_light()
      
  }
  
  #if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel=="None") {select.plot.ellipse <- gghighlight_point(new.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, use_group_by=FALSE, use_direct_label=FALSE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel!="None" && input$elipseplot1 == FALSE) {newer.spectra.line.table <- spectra.line.table[,c("Sample", "PC1", "PC2", input$pcafocusvariable, input$pcafocuslabel)]}
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel!="None" && input$elipseplot1 == FALSE) {colnames(newer.spectra.line.table) <- c("Sample", "PC1", "PC2", "Selected", "Label")}
  
  if (input$elipseplot1 == FALSE && input$pcacolour == "Focus" && input$pcafocuslabel!="None") {
      select.plot <- gghighlight_point(newer.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme_light()
      
  }
  
  #if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel!="None") {select.plot.ellipse <- gghighlight_point(newer.spectra.line.table, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + scale_x_continuous("Principle Component 1") + scale_y_continuous("Principle Component 2") + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
  
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel=="None" && input$elipseplot1 == TRUE) {new.spectra.line.table.source <- just.samples[,c("Sample", "PC1", "PC2", input$pcafocusvariable)]}
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel=="None" && input$elipseplot1 == TRUE) {colnames(new.spectra.line.table.source) <- c("Sample", "PC1", "PC2", "Selected")}
  
  
  if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel=="None") {
      select.plot.ellipse <- gghighlight_point(new.spectra.line.table.source, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, use_group_by=FALSE, use_direct_label=FALSE) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
      theme_light()
      
  }
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel!="None" && input$elipseplot1 == TRUE) {newer.spectra.line.table.source <- just.samples[,c("Sample", "PC1", "PC2", input$pcafocusvariable, input$pcafocuslabel)]}
  
  if (input$pcacolour == "Focus" && input$pcafocuslabel!="None" && input$elipseplot1 == TRUE) {colnames(newer.spectra.line.table.source) <- c("Sample", "PC1", "PC2", "Selected", "Label")}
  
  if (input$elipseplot1 == TRUE && input$pcacolour == "Focus"  && input$pcafocuslabel!="None") {
      select.plot.ellipse <- gghighlight_point(newer.spectra.line.table.source, aes(PC1, PC2, colour=Selected), Selected %in% c(input$pcafocuschoice), size=input$spotsize, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      stat_ellipse(data=just.simulations, aes(PC1, PC2, colour=as.factor(Source), linetype=as.factor(Source))) +
      theme_light()
      
  }
  
  
  #quant.regular <- ggplot(data= spectra.line.table) +
  #geom_point(aes(PC1, PC2, colour=Quantitative), size = input$spotsize) +
  #scale_x_continuous("Principle Component 1") +
  #scale_y_continuous("Principle Component 2") +
  #theme_light() +
  #theme(axis.text.x = element_text(size=15)) +
  #theme(axis.text.y = element_text(size=15)) +
  #theme(axis.title.x = element_text(size=15)) +
  #theme(axis.title.y = element_text(size=15, angle=90)) +
  #theme(plot.title=element_text(size=20)) +
  #theme(legend.title=element_text(size=15)) +
  #theme(legend.text=element_text(size=15)) +
  #scale_colour_gradientn("Quantitative", colours=rainbow(length(spectra.line.table$Quantitative))) +
  #geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2)


  if (input$elipseplot1 == FALSE && input$pcacolour == "black") {
      basic
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Cluster") {
      ellipse
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Cluster") {
      regular
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Source") {
      source.ellipse
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Source") {
      source.regular
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Qualitative1") {
      qual.ellipse.1
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Qualitative1") {
      qual.regular.1
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Qualitative2") {
      qual.ellipse.2
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Qualitative2") {
      qual.regular.2
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Qualitative3") {
      qual.ellipse.3
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Qualitative3") {
      qual.regular.3
  } else if (input$elipseplot1 == TRUE && input$pcacolour == "Qualitative4") {
      qual.ellipse.4
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Qualitative4") {
      qual.regular.4
  } else if (input$elipseplot1 == FALSE && input$pcacolour == "Focus") {
      select.plot
  }  else if (input$elipseplot1 == TRUE && input$pcacolour == "Focus") {
      select.plot.ellipse
  }



  })
  
  
  output$xrfpcaplot <- renderPlot({
      plotInput2()
      
  })
  
  hoverHold <- reactive({
      
      spectra.line.table <- if(input$elipseplot1==TRUE && input$pcacolour == "Source"){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      xrf.k <- if(input$elipseplot1==TRUE && input$pcacolour == "Source"){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }
      
      
      colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
      colnames(colour.table) <- c("Cluster", names(spectra.line.table))
      
      
      
      
      unique.spec <- seq(1, length(colour.table$Sample), 1)
      null <- rep(1, length(unique.spec))
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      
      
      spectra.line.table <- subset(spectra.line.table, !(spectra.line.table$PC1 < input$xlimrangepca[1] | spectra.line.table$PC1 > input$xlimrangepca[2]))
      
      spectra.line.table <- subset(spectra.line.table, !(spectra.line.table$PC2 < input$ylimrangepca[1] | spectra.line.table$PC2 > input$ylimrangepca[2]))
      
      point.table <- spectra.line.table[,c("Sample", "Source", "PC1", "PC2")]
      
      point.table
      
  })
  


  
  output$hover_infopca <- renderUI({
      
      point.table <- hoverHold()
      
      hover <- input$plot_hoverpca
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      


      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Sample: </b>", point$Sample, "<br/>",
      "<b> Source: </b>", point$Source, "<br/>",
      "<b> PC1: </b>", point$PC1, "<br/>",
      "<b> PC2: </b>", point$PC2, "<br/>"

      )))
      )
  })

  
  
  output$downloadPlot2 <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "PCAPlot"), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,plotInput2(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
  }
  )
  
  
  
  pcaTableInputFull <- reactive({
      # xrf.pca.results <- xrfPCAReactive()

#xrf.pca.results

sourceDefinitions()
 
      
  })
  
  
  
  
  output$xrfpcatable <- DT::renderDataTable({
      
    
      
      df <- xrfkReactivePrep()[["Simulations"]]
      
      

      DT::datatable(df)
      
  })




output$xrfpcatablefull <- DT::renderDataTable({
    
    df <- pcaTableInputFull()
    DT::datatable(df)
    
})




output$xrfpcatablefulldownload <- downloadHandler(
filename = function() { paste(paste(c(input$projectname, "_", "PCATable"), collapse=''), '.csv', sep=',') },
content = function(file
) {
    write.csv(pcaTableInputFull(), file)
}
)





outApp <- reactive({
    
    xrf.k <- xrfKReactive()
    
    quality.table <-qualityTable()
    
    colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
    colnames(colour.table) <- c("Cluster", names(spectra.line.table))
    
    names(colour.table)
    
    
})



output$inApp <- renderUI({
    selectInput(inputId = "app", label = h4("Application"), choices =  outApp())
})


dataDefaultSelect <- reactive({
    
    data.options <-defaultLines()
    data.selected <- data.options[5]
    data.selected
    
})

secondDefaultSelect <- reactive({
    
    data.options <-defaultLines()
    data.selected <- data.options[6]
    data.selected
    
})


  
  choiceLinesRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))
      
      standard <- if(input$filetype=="Spectra"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
      } else if(input$filetype=="Net"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
      } else if(input$filetype=="Spreadsheet"){
          colnames(spectra.line.table[ ,!(colnames(spectra.line.table) == "Sample")])
      }
      
  })
  
  

  
  
  output$ratioellipsesources <- renderUI({
      
      used.sources <- unique(xrfkReactivePrep()[["RawSimulations"]]["Source"])
      
      if(input$elipseplot2==TRUE){
          selectInput('ratiosources', label="Sources to Plot", choices=used.sources, selected=used.sources, multiple=TRUE)
      }else{
          p()
      }
      
  })
  
  
  ratioChooseA <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- spectra.line.names[2]
      standard
      
  })
  
  
  
  ratioChooseB <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- "None"
      
      
      standard
      
  })
  
  
  ratioChooseC <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      
      standard <- spectra.line.names[4]
      
      
      standard
      
  })
  
  
  ratioChooseD <- reactive({
      spectra.line.table <- dataMerge3()
      spectra.line.names <- colnames(spectra.line.table)
      
      standard <- "None"
      
      
      standard
      
  })
  
  
  output$inelementratioa <- renderUI({
      selectInput("elementratioa", "Element A", choices=choiceLinesRatio(), selected=ratioChooseA())
  })
  
  output$inelementratiob <- renderUI({
      selectInput("elementratiob", "Element B", choices=choiceLinesRatio(), selected=ratioChooseB())
  })
  
  output$inelementratioc <- renderUI({
      selectInput("elementratioc", "Element C", choices=choiceLinesRatio(), selected=ratioChooseC())
  })
  
  output$inelementratiod <- renderUI({
      selectInput("elementratiod", "Element D", choices=choiceLinesRatio(), selected=ratioChooseD())
  })
  
  
  xMinRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio,  spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative"))
      
      round(min((ratio.frame[,1]/ratio.frame[,2])), 2)
      
      
  })
  
  xMaxRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

     
      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio,  spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative"))
  
      
      round(max((ratio.frame[,1]/ratio.frame[,2])), 2)
      
      
  })
  
  
  
  yMinRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2),  "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative"))
      
      round(min((ratio.frame[,3]/ratio.frame[,4])), 2)
      
      
  })
  
  yMaxRatio <- reactive({
      
      spectra.line.table <- dataMerge3()
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio,  spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2),  "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative"))
   
      
      round(max((ratio.frame[,3]/ratio.frame[,4])), 2)
      
      
  })
  
  output$inxlimrangeratio <- renderUI({
      
      
      sliderInput("xlimrangeratio", "X axis", min=xMinRatio(), max=xMaxRatio(), value=c(xMinRatio(), xMaxRatio()), round=TRUE)
  })
  
  output$inylimrangeratio <- renderUI({
      
      
      sliderInput("ylimrangeratio", "Y axis", min=yMinRatio(), max=yMaxRatio(), value=c(yMinRatio(), yMaxRatio()), round=TRUE)
  })
  
  

  
  
  output$ratioFocusVariable <- renderUI({
      
      if(input$ratiocolour=="Focus"){selectInput('ratiofocusvariable', "Choose Variable", choices=names(values[["DF"]]), selected="Qualitative1")} else {
          p()
      }
      
  })
  
  output$ratioFocusUI <- renderUI({
      
      if(input$ratiocolour=="Focus"){selectInput('ratiofocuschoice', "Choose Focus", choices=unique(values[["DF"]][input$ratiofocusvariable]), selected="Qualitative1", multiple=TRUE)} else {
          p()
      }
      
  })
  
  output$ratioFocusLabel <- renderUI({
      if(input$ratiocolour=="Focus"){selectInput('ratiofocuslabel', "Choose Label", choices=c("None", names(values[["DF"]])), selected="None")} else {
          p()
      }
      
  })
  
  
  
  plotInput4 <- reactive({
      
      
      spectra.line.table <- if(input$elipseplot2==TRUE){
          xrfkReactivePrep()[["RawSimulations"]]
      }else{
          dataMerge3()
      }
      
      spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

      
      xrf.k <- if(input$elipseplot2==TRUE){
          xrfkReactivePrep()[["Simulations"]]
      }else{
          xrfKReactive()
      }
      
      quality.table <-qualityTable()
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$Type <- xrf.k$Type
      #spectra.line.table$Source <- xrf.k$Source






      first.ratio <-spectra.line.table[,input$elementratioa]
      second.ratio <- spectra.line.table[,input$elementratiob]
      third.ratio <- spectra.line.table[,input$elementratioc]
      fourth.ratio <- spectra.line.table[,input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      if(input$elipseplot2==FALSE){
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Cluster, spectra.line.table$Source, spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative, spectra.line.table$Sample)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster", "Source", "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative", "Sample"))
      }
      
      
      if(input$elipseplot2==TRUE){
          ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Cluster, spectra.line.table$Source, spectra.line.table$Sample, spectra.line.table$Type)
          colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster", "Source", "Sample", "Type"))
      }
      
      
            if(input$elementratiob!="None"){ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))}
            if(input$elementratiod!="None"){ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))}
            
            if(input$elementratiob=="None"){ratio.names.x <- c(names(ratio.frame[1]))}
            if(input$elementratiod=="None"){ratio.names.y <- c(names(ratio.frame[3]))}
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      
      if(input$elementratiob!="None"){ratio.frame$X <- ratio.frame[,1]/ratio.frame[,2]}
      if(input$elementratiod!="None"){ratio.frame$Y <- ratio.frame[,3]/ratio.frame[,4]}
      
      if(input$elementratiob=="None"){ratio.frame$X <- ratio.frame[,1]}
      if(input$elementratiod=="None"){ratio.frame$Y <- ratio.frame[,3]}
      
      
      if(input$elipseplot2==TRUE){just.samples <- subset(ratio.frame, Type=="Sample")}
      if(input$elipseplot2==TRUE){just.samples$Qualitative1 <- dataMerge3()$Qualitative1}
      if(input$elipseplot2==TRUE){just.samples$Qualitative2 <- dataMerge3()$Qualitative2}
      if(input$elipseplot2==TRUE){just.samples$Qualitative3 <- dataMerge3()$Qualitative3}
      if(input$elipseplot2==TRUE){just.samples$Qualitative4 <- dataMerge3()$Qualitative4}
      if(input$elipseplot2==TRUE){just.samples$Quantitative <- dataMerge3()$Quantitative}
      
      
      if(input$elipseplot2==TRUE){just.simulations <- subset(ratio.frame, Type=="Simulation")}
      if(input$elipseplot2==TRUE){just.simulations <- subset(just.simulations, Source==input$ratiosources)}

      
      ratio.frame <- subset(ratio.frame, !((ratio.frame[,1]/ratio.frame[,2]) <= input$xlimrangeratio[1] | (ratio.frame[,1]/ratio.frame[,2]) >= input$xlimrangeratio[2]))
      
      ratio.frame <- subset(ratio.frame, !((ratio.frame[,3]/ratio.frame[,4]) <= input$ylimrangeratio[1] | (ratio.frame[,3]/ratio.frame[,4]) >= input$ylimrangeratio[2]))
      
      
      black.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(lwd=input$spotsize2) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      cluster.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Cluster"){
      cluster.ratio.ellipse.plot <- ggplot(aes(X, Y), data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Cluster), shape=as.factor(Cluster)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(just.samples$Cluster))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      guides(linetype=FALSE) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      source.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Source), shape=as.factor(ratio.frame$Source)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Source", values=1:nlevels(ratio.frame$Source)) +
      scale_colour_discrete("Source") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Source"){
      source.ratio.ellipse.plot <- ggplot(aes(X, Y), data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Source), shape=as.factor(Source)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Source", values=1:nlevels(just.samples$Source)) +
      scale_colour_discrete("Source") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      guides(linetype=FALSE) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      qualitative.ratio.plot.1 <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative1), shape=as.factor(ratio.frame$Qualitative1)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative1", values=1:nlevels(ratio.frame$Qualitative1)) +
      scale_colour_discrete("Qualitative1") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Qualitative1"){
      qualitative.ratio.ellipse.plot.1 <- ggplot(aes(X, Y), data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Qualitative1), shape=as.factor(Qualitative1)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative1", values=1:nlevels(just.samples$Qualitative1)) +
      scale_colour_discrete("Qualitative1") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      guides(linetype=FALSE) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      
      qualitative.ratio.plot.2 <- qplot(X, Y, data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative2), shape=as.factor(ratio.frame$Qualitative2)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative2", values=1:nlevels(ratio.frame$Qualitative2)) +
      scale_colour_discrete("Qualitative2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Qualitative2"){
      qualitative.ratio.ellipse.plot.2 <- ggplot(aes(X, Y), data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Qualitative2), shape=as.factor(Qualitative2)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative2", values=1:nlevels(just.samples$Qualitative2)) +
      scale_colour_discrete("Qualitative2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      qualitative.ratio.plot.3 <- qplot(X, Y, data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative3), shape=as.factor(ratio.frame$Qualitative3)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative3", values=1:nlevels(ratio.frame$Qualitative3)) +
      scale_colour_discrete("Qualitative3") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      guides(linetype=FALSE) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Qualitative3"){
      qualitative.ratio.ellipse.plot.3 <- ggplot(aes(X, Y), data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Qualitative3), shape=as.factor(Qualitative3)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative3", values=1:nlevels(just.samples$Qualitative3)) +
      scale_colour_discrete("Qualitative3") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      qualitative.ratio.plot.4 <- qplot(X, Y, data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative4), shape=as.factor(ratio.frame$Qualitative4)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative4", values=1:nlevels(ratio.frame$Qualitative4)) +
      scale_colour_discrete("Qualitative4") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      geom_point(colour="grey30", size=input$spotsize2-2, alpha=0.01)
      
      if(input$elipseplot2==TRUE && input$ratiocolour == "Qualitative4"){
      qualitative.ratio.ellipse.plot.4 <- ggplot(aes(X, Y), data=ratio.frame,  xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
      geom_point(data=just.samples, aes(colour=as.factor(Qualitative4), shape=as.factor(Qualitative4)), size=input$spotsize2+1) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative4", values=1:nlevels(just.samples$Qualitative4)) +
      scale_colour_discrete("Qualitative4") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x) +
      scale_y_continuous(ratio.names.y) +
      guides(linetype=FALSE) +
      geom_point(data=just.samples, colour="grey30", size=input$spotsize2-2, alpha=0.01)
      }
      
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {new.ratio.table <- ratio.frame[,c("Sample", "X", "Y", input$ratiofocusvariable)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {colnames(new.ratio.table) <- c("Sample", "X", "Y", "Selected")}
      
      if (input$elipseplot2 == FALSE && input$ratiocolour == "Focus" && input$ratiofocuslabel=="None") {select.plot <- gghighlight_point(new.ratio.table, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, use_group_by=FALSE, use_direct_label=FALSE) +
          scale_x_continuous(ratio.names.x) +
          scale_y_continuous(ratio.names.y) +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme_light()
          
      }
      
      #if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel=="None") {select.plot.ellipse <- gghighlight_point(new.ratio.table, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, use_group_by=FALSE, use_direct_label=FALSE) + scale_x_continuous(ratio.names.x) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {newer.ratio.table <- ratio.frame[,c("Sample", "X", "Y", input$ratiofocusvariable, input$ratiofocuslabel)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {colnames(newer.ratio.table) <- c("Sample", "X", "Y", "Selected", "Label")}
      
      if (input$elipseplot2 == FALSE && input$ratiocolour == "Focus" && input$ratiofocuslabel!="None") {
          select.plot <- gghighlight_point(newer.ratio.table, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) +
          scale_x_continuous(ratio.names.x) +
          scale_y_continuous(ratio.names.y) +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          theme_light()
          
      }
      
      #if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel!="None") {select.plot.ellipse <- gghighlight_point(newer.ratio.table, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) + scale_x_continuous(ratio.names.x) + scale_y_continuous(ratio.names.y) + theme(axis.text.x = element_text(size=15)) + theme(axis.text.y = element_text(size=15)) + theme(axis.title.x = element_text(size=15)) + theme(axis.title.y = element_text(size=15, angle=90)) + theme(plot.title=element_text(size=20)) + theme(legend.title=element_text(size=15)) + stat_ellipse() + theme_light()}
      
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None" && input$elipseplot2 == TRUE) {new.spectra.line.table.source <- just.samples[,c("Sample", "X", "Y", input$pcafocusvariable)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel=="None" && input$elipseplot2 == TRUE) {colnames(new.spectra.line.table.source) <- c("Sample", "X", "Y", "Selected")}
      
      
      if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel=="None") {
          select.plot.ellipse <- gghighlight_point(new.spectra.line.table.source, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, use_group_by=FALSE, use_direct_label=FALSE) +
          scale_x_continuous(ratio.names.x) +
          scale_y_continuous(ratio.names.y) +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
          theme_light()
          
      }
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None" && input$elipseplot2 == TRUE) {newer.spectra.line.table.source <- just.samples[,c("Sample", "X", "Y", input$pcafocusvariable, input$pcafocuslabel)]}
      
      if (input$ratiocolour == "Focus" && input$ratiofocuslabel!="None" && input$elipseplot2 == TRUE) {colnames(newer.spectra.line.table.source) <- c("Sample", "X", "Y", "Selected", "Label")}
      
      if (input$elipseplot2 == TRUE && input$ratiocolour == "Focus"  && input$ratiofocuslabel!="None") {
          select.plot.ellipse <- gghighlight_point(newer.spectra.line.table.source, aes(X, Y, colour=Selected), Selected %in% c(input$ratiofocuschoice), size=input$spotsize2, label_key=Label, use_group_by=FALSE, use_direct_label=TRUE) +
          scale_x_continuous(ratio.names.x) +
          scale_y_continuous(ratio.names.y) +
          theme(axis.text.x = element_text(size=15)) +
          theme(axis.text.y = element_text(size=15)) +
          theme(axis.title.x = element_text(size=15)) +
          theme(axis.title.y = element_text(size=15, angle=90)) +
          theme(plot.title=element_text(size=20)) +
          theme(legend.title=element_text(size=15)) +
          stat_ellipse(data=just.simulations, aes(X, Y, colour=as.factor(Source), linetype=as.factor(Source))) +
          theme_light()
          
      }

      
      


      
      
      if (input$ratiocolour == "Black" && input$elipseplotnorm==FALSE) {
          black.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==FALSE) {
          cluster.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==TRUE) {
          cluster.ratio.ellipse.plot
      } else if (input$ratiocolour == "Source" && input$elipseplot2==FALSE) {
          source.ratio.plot
      } else if (input$ratiocolour == "Source" && input$elipseplot2==TRUE ) {
          source.ratio.ellipse.plot
      } else if (input$ratiocolour == "Qualitative1" && input$elipseplot2==FALSE) {
          qualitative.ratio.plot.1
      } else if (input$ratiocolour == "Qualitative1" && input$elipseplot2==TRUE ) {
          qualitative.ratio.ellipse.plot.1
      } else if (input$ratiocolour == "Qualitative2" && input$elipseplot2==FALSE) {
          qualitative.ratio.plot.2
      } else if (input$ratiocolour == "Qualitative2" && input$elipseplot2==TRUE ) {
          qualitative.ratio.ellipse.plot.2
      } else if (input$ratiocolour == "Qualitative3" && input$elipseplot2==FALSE) {
          qualitative.ratio.plot.3
      } else if (input$ratiocolour == "Qualitative3" && input$elipseplot2==TRUE ) {
          qualitative.ratio.ellipse.plot.3
      } else if (input$ratiocolour == "Qualitative4" && input$elipseplot2==FALSE) {
          qualitative.ratio.plot.4
      } else if (input$ratiocolour == "Qualitative4" && input$elipseplot2==TRUE ) {
          qualitative.ratio.ellipse.plot.4
      } else if (input$ratiocolour == "Quantitative" && input$elipseplot2==FALSE ) {
          quanitative.ratio.plot
      } else if (input$ratiocolour == "Quantitative" && input$elipseplot2==TRUE) {
          quantitative.ratio.plot
      } else if (input$ratiocolour == "Focus" && input$elipseplot2==FALSE) {
          select.plot
      } else if (input$ratiocolour == "Focus" && input$elipseplot2==TRUE) {
          select.plot.ellipse
      }

  })


   output$elementratiotimeseries <- renderPlot({
       plotInput4()


    })
   
   
   
   hoverHoldRatio <- reactive({
       
       spectra.line.table <- dataMerge3()
       spectra.line.table$None <- rep(1, length(spectra.line.table$Sample))

       
       
       xrf.k <- xrfKReactive()
       
       colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
       colnames(colour.table) <- c("Cluster", names(spectra.line.table))
       
       
       
       
       unique.spec <- seq(1, length(colour.table$Sample), 1)
       null <- rep(1, length(unique.spec))
       
       spectra.line.table$Cluster <- xrf.k$Cluster
       
       
       
       
       
       first.ratio <-spectra.line.table[,input$elementratioa]
       second.ratio <- spectra.line.table[,input$elementratiob]
       third.ratio <- spectra.line.table[,input$elementratioc]
       fourth.ratio <- spectra.line.table[,input$elementratiod]
       
       first.ratio.norm <- first.ratio/sum(first.ratio)
       second.ratio.norm <- second.ratio/sum(second.ratio)
       third.ratio.norm <- third.ratio/sum(third.ratio)
       fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
       
       
       ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Source, spectra.line.table$Cluster, spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative, spectra.line.table$Sample)
       colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Source", "Cluster", "Qualitative1", "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative", "Sample"))
       
       
       if(input$elementratiob!="None"){ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))}
       if(input$elementratiod!="None"){ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))}
       
       if(input$elementratiob=="None"){ratio.names.x <- c(names(ratio.frame[1]))}
       if(input$elementratiod=="None"){ratio.names.y <- c(names(ratio.frame[3]))}
       
       ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
       ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
       
       #ratio.frame <- subset(ratio.frame, !((ratio.frame[,1]/ratio.frame[,2]) < input$xlimrangeratio[1] | (ratio.frame[,1]/ratio.frame[,2]) > input$xlimrangeratio[2]))
       
       #ratio.frame <- subset(ratio.frame, !((ratio.frame[,3]/ratio.frame[,4]) < input$ylimrangeratio[1] | (ratio.frame[,3]/ratio.frame[,4]) > input$ylimrangeratio[2]))
       
       ratio.frame$X <- ratio.frame[,1]/ratio.frame[,2]
       ratio.frame$Y <- ratio.frame[,3]/ratio.frame[,4]
       
       ratio.frame$X <- ratio.frame[,1]
       ratio.frame$Y <- ratio.frame[,3]
       
       ratio.frame
       
   })
   
   
   
   
   output$hover_inforatio <- renderUI({
       
       point.table <- hoverHoldRatio()
       
       hover <- input$plot_hoverratio
       point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
       if (nrow(point) == 0) return(NULL)
       
       
       # calculate point position INSIDE the image as percent of total dimensions
       # from left (horizontal) and from top (vertical)
       left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
       
       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
       
       
       
       
       
       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
       "left:", left_px + 2, "px; top:", top_px + 2, "px;")
       
       # actual tooltip created as wellPanel
       wellPanel(
       style = style,
       p(HTML(paste0("<b> Sample: </b>", point$Sample, "<br/>",
       "<b> Source: </b>", point$Source, "<br/>"

       
       
       )))
       )
   })
   
   ratioTerm <- reactive({
       
          ratio.names <- paste(c(c(substr(input$elementratioa, 1,2), "-", substr(input$elementratiob, 1, 2)), "_", c(substr(input$elementratioc,1,2), "-", substr(input$elementratiod,1,2), "_RatioPlot")), collapse="")
          ratio.label <- paste(c(input$projectname, "_", ratio.names), collapse='')
          ratio.label
   })
   
   output$downloadPlot4 <- downloadHandler(

   
   filename = function() { paste(ratioTerm(), '.tiff', sep='') },
   content = function(file) {
       ggsave(file,plotInput4(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
   }
   )




ternaryChooseA <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(input$filetype=="Spectra"){
        "Al.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[2]
    } else if (input$filetype=="Spreadsheet"){
        spectra.line.names[2]
    }
    
    standard
    
})

ternaryChooseB <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(input$filetype=="Spectra"){
        "Si.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[3]
    } else if (input$filetype=="Spreadsheet"){
        spectra.line.names[3]
    }
    
    standard
    
})

ternaryChooseC <- reactive({
    spectra.line.table <- dataMerge3()
    spectra.line.names <- colnames(spectra.line.table)
    
    
    standard <- if(input$filetype=="Spectra"){
        "Ca.K.alpha"
    } else if(input$filetype=="Net"){
        spectra.line.names[4]
    } else if (input$filetype=="Spreadsheet"){
        spectra.line.names[4]
    }
    
    standard
    
})

output$inaxisa <- renderUI({
    selectInput("axisa", "Axis A", choices=choiceLines(), selected=ternaryChooseA())
})

output$inaxisb <- renderUI({
    selectInput("axisb", "Axis B", choices=choiceLines(), selected=ternaryChooseB())
})

output$inaxisc <- renderUI({
    selectInput("axisc", "Axis C", choices=choiceLines(), selected=ternaryChooseC())
})


plotInput5 <- reactive({
    
    spectra.line.table <- dataMerge3()

    
    xrf.k <- xrfKReactive()
    
    quality.table <-qualityTable()
    
    
    colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
    colnames(colour.table) <- c("Cluster", names(spectra.line.table))
    
    
    
    
    unique.spec <- seq(1, length(colour.table$Sample), 1)
    null <- rep(1, length(unique.spec))
    
    spectra.line.table$Cluster <- xrf.k$Cluster



    
    first.axis <- spectra.line.table[input$axisa]
    second.axis <- spectra.line.table[input$axisb]
    third.axis <- spectra.line.table[input$axisc]
    
    first.axis.norm <- first.axis/sum(first.axis)
    second.axis.norm <- second.axis/sum(second.axis)
    third.axis.norm <- third.axis/sum(third.axis)
    
    axis.frame <- data.frame(first.axis, second.axis, third.axis, spectra.line.table$Cluster, spectra.line.table$Source, spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
    colnames(axis.frame) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Source", "Qualitative1",  "Qualitative2", "Qualitative3", "Qualitative4","Quantitative"))
    
    axis.frame.norm <- data.frame(first.axis.norm, second.axis.norm, third.axis.norm, spectra.line.table$Cluster, spectra.line.table$Source, spectra.line.table$Qualitative1, spectra.line.table$Qualitative2, spectra.line.table$Qualitative3, spectra.line.table$Qualitative4, spectra.line.table$Quantitative)
    colnames(axis.frame.norm) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Source", "Qualitative1",  "Qualitative2", "Qualitative3", "Qualitative4", "Quantitative"))
    
    ternaryplot1 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(size=input$ternpointsize) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplot2 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(size=input$ternpointsize) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotcluster <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15)) 
    
    
    ternaryplotclusterellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotsource <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Source), shape=as.factor(Source)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Source", values=1:nlevels(axis.frame$Source)) +
    scale_colour_discrete("Source") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotsourceellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Source), shape=as.factor(Source)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Source", values=1:nlevels(axis.frame$Source)) +
    scale_colour_discrete("Source") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.1 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Qualitative1), shape=as.factor(Qualitative1)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative1", values=1:nlevels(axis.frame$Qualitative1)) +
    scale_colour_discrete("Qualitative1") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.1 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative1), shape=as.factor(Qualitative1)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative1", values=1:nlevels(axis.frame$Qualitative1)) +
    scale_colour_discrete("Qualitative1") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.2 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Qualitative2), shape=as.factor(Qualitative2)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative2", values=1:nlevels(axis.frame$Qualitative2)) +
    scale_colour_discrete("Qualitative2") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.2 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative2), shape=as.factor(Qualitative2)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative2", values=1:nlevels(axis.frame$Qualitative2)) +
    scale_colour_discrete("Qualitative2") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.3 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Qualitative3), shape=as.factor(Qualitative3)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative3", values=1:nlevels(axis.frame$Qualitative3)) +
    scale_colour_discrete("Qualitative3") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.3 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative3), shape=as.factor(Qualitative3)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative3", values=1:nlevels(axis.frame$Qualitative3)) +
    scale_colour_discrete("Qualitative3") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.4 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = as.factor(Qualitative4), shape=as.factor(Qualitative4)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative4", values=1:nlevels(axis.frame$Qualitative4)) +
    scale_colour_discrete("Qualitative4") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.4 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative4), shape=as.factor(Qualitative4)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative4", values=1:nlevels(axis.frame$Qualitative4)) +
    scale_colour_discrete("Qualitative4") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotquantitative <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes(colour = Quantitative), size=input$ternpointsize+1) +
    geom_point(size=input$ternpointsize-2) +
    scale_colour_gradientn("Quantitative", colours=rainbow(length(axis.frame$Quantitative))) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotquanitativeellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = Quantitative), size=input$ternpointsize) +
    scale_colour_gradientn("Quantitative", colours=rainbow(length(axis.frame$Quantitative))) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    
    
    #####Normalization
    
    ternaryplot1.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(size=input$ternpointsize) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplot2.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(size=input$ternpointsize) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotcluster.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotclusterellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
    scale_colour_discrete("Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotsource.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Source), shape=as.factor(Source)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Source", values=1:nlevels(axis.frame$Source)) +
    scale_colour_discrete("Source") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotsourceellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Source), shape=as.factor(Source)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Source", values=1:nlevels(axis.frame$Source)) +
    scale_colour_discrete("Source") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.norm.1 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Qualitative1), shape=as.factor(Qualitative1)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative1", values=1:nlevels(axis.frame$Qualitative1)) +
    scale_colour_discrete("Qualitative1") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.norm.1 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative1), shape=as.factor(Qualitative1)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative1", values=1:nlevels(axis.frame$Qualitative1)) +
    scale_colour_discrete("Qualitative1") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.norm.2 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Qualitative2), shape=as.factor(Qualitative2)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative2", values=1:nlevels(axis.frame$Qualitative2)) +
    scale_colour_discrete("Qualitative2") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.norm.2 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative2), shape=as.factor(Qualitative2)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative2", values=1:nlevels(axis.frame$Qualitative2)) +
    scale_colour_discrete("Qualitative2") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.norm.3 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Qualitative3), shape=as.factor(Qualitative3)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative3", values=1:nlevels(axis.frame$Qualitative3)) +
    scale_colour_discrete("Qualitative3") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.norm.3 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative3), shape=as.factor(Qualitative3)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative3", values=1:nlevels(axis.frame$Qualitative3)) +
    scale_colour_discrete("Qualitative3") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitative.norm.4 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = as.factor(Qualitative4), shape=as.factor(Qualitative4)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative4", values=1:nlevels(axis.frame$Qualitative4)) +
    scale_colour_discrete("Qualitative4") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotqualitativeellipse.norm.4 <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = as.factor(Qualitative4), shape=as.factor(Qualitative4)), size=input$ternpointsize+1) +
    geom_point(colour="grey30", size=input$ternpointsize-2) +
    scale_shape_manual("Qualitative4", values=1:nlevels(axis.frame$Qualitative4)) +
    scale_colour_discrete("Qualitative4") +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotquantitative.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_point(aes(colour = Quantitative), size=input$ternpointsize+1) +
    geom_point(size=input$ternpointsize-2) +
    scale_colour_gradientn("Quantitative", colours=rainbow(length(axis.frame$Quantitative))) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))
    
    ternaryplotquanitativeellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
    geom_density_tern() +
    geom_point(aes(colour = Quantitative), size=input$ternpointsize) +
    scale_colour_gradientn("Quantitative", colours=rainbow(length(axis.frame$Quantitative))) +
    theme_light() +
    theme(axis.text.x = element_text(size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    theme(axis.title.x = element_text(size=15)) +
    theme(axis.title.y = element_text(size=15, angle=90)) +
    theme(plot.title=element_text(size=20)) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=15))


    if (input$ternarycolour == "black" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplot1
    } else if (input$ternarycolour == "black" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplot2
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotcluster
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplotclusterellipse
    } else if (input$ternarycolour == "Source" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotsource
    } else if (input$ternarycolour == "Source" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplotsourceellipse
    } else if (input$ternarycolour == "Qualitative1" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotqualitative.1
    } else if (input$ternarycolour == "Qualitative1" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
                ternaryplotqualitativeellipse.1
    } else if (input$ternarycolour == "Qualitative2" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotqualitative.2
    } else if (input$ternarycolour == "Qualitative2" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplotqualitativeellipse.2
    } else if (input$ternarycolour == "Qualitative3" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotqualitative.3
    } else if (input$ternarycolour == "Qualitative3" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplotqualitativeellipse.3
    } else if (input$ternarycolour == "Qualitative4" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotqualitative.4
    } else if (input$ternarycolour == "Qualitative4" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
        ternaryplotqualitativeellipse.4
    } else if (input$ternarycolour == "Quantitative" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
        ternaryplotquantitative
    } else if (input$ternarycolour == "Quantitative" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
            ternaryplotquanitativeellipse
    } else if (input$ternarycolour == "black" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplot1.norm
    } else if (input$ternarycolour == "black" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplot2.norm
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotcluster.norm
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotclusterellipse.norm
    } else if (input$ternarycolour == "Source" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotsource.norm
    } else if (input$ternarycolour == "Source" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotsourceellipse.norm
    } else if (input$ternarycolour == "Qualitative1" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotqualitative.norm.1
    } else if (input$ternarycolour == "Qualitative1" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotqualitativeellipse.norm.1
    } else if (input$ternarycolour == "Qualitative2" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotqualitative.norm.2
    } else if (input$ternarycolour == "Qualitative2" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotqualitativeellipse.norm.2
    } else if (input$ternarycolour == "Qualitative3" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotqualitative.norm.3
    } else if (input$ternarycolour == "Qualitative3" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotqualitativeellipse.norm.3
    } else if (input$ternarycolour == "Qualitative4" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotqualitative.norm.4
    } else if (input$ternarycolour == "Qualitative4" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotqualitativeellipse.norm.4
    } else if (input$ternarycolour == "Quantitative" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
        ternaryplotquantitative.norm
    } else if (input$ternarycolour == "Quantitative" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
        ternaryplotquanitativeellipse.norm
    }


})

output$ternaryplot <- renderPlot({
    
    print(plotInput5())
    
})


hoverHoldTern <- reactive({
    
    spectra.line.table <- dataMerge3()
    
    
    xrf.k <- xrfKReactive()
    
    quality.table <-qualityTable()
    
    colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
    colnames(colour.table) <- c("Cluster", names(spectra.line.table))
    
    
    
    
    unique.spec <- seq(1, length(colour.table$Sample), 1)
    null <- rep(1, length(unique.spec))
    
    spectra.line.table$Cluster <- xrf.k$Cluster
    
    
    
    
    first.axis <- spectra.line.table[input$axisa]
    second.axis <- spectra.line.table[input$axisb]
    third.axis <- spectra.line.table[input$axisc]
    
    first.axis.norm <- first.axis/sum(first.axis)
    second.axis.norm <- second.axis/sum(second.axis)
    third.axis.norm <- third.axis/sum(third.axis)
    
    axis.frame <- data.frame(first.axis, second.axis, third.axis, spectra.line.table$Sample)
    colnames(axis.frame) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Sample"))
    
    axis.frame
    
})


hoverHoldTernNorm <- reactive({
    
    spectra.line.table <- dataMerge3()
    
    
    xrf.k <- xrfKReactive()
    
    quality.table <-qualityTable()
    
    colour.table <- data.frame(xrf.k$Cluster, spectra.line.table)
    colnames(colour.table) <- c("Cluster", names(spectra.line.table))
    
    
    
    
    unique.spec <- seq(1, length(colour.table$Sample), 1)
    null <- rep(1, length(unique.spec))
    
    spectra.line.table$Cluster <- xrf.k$Cluster
    
    
    
    
    first.axis <- spectra.line.table[input$axisa]
    second.axis <- spectra.line.table[input$axisb]
    third.axis <- spectra.line.table[input$axisc]
    
    first.axis.norm <- first.axis/sum(first.axis)
    second.axis.norm <- second.axis/sum(second.axis)
    third.axis.norm <- third.axis/sum(third.axis)
    
    
    axis.frame.norm <- data.frame(first.axis.norm, second.axis.norm, third.axis.norm, spectra.line.table$Sample)
    colnames(axis.frame.norm) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Sample"))
    
    axis.frame.norm
    
})





axisTerm <- reactive({
    axis.names.tern <- paste(gsub("[.]", "-", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Ternary")), collapse='')
    axis.labels <- paste(c(input$projectname, "_", axis.names.tern), collapse='')
    axis.labels
})

output$downloadPlot5 <- downloadHandler(



filename = function() { paste(axisTerm(), '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInput5(), device="tiff", compression="lzw", type="cairo",  dpi=300, width=12, height=7)
}
)



})

})

