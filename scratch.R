proto.fish <- openxlsx::loadWorkbook(file="~/Desktop/ObsidianSourcing (2).xlsx")
        just.fish <- openxlsx::readWorkbook(proto.fish, sheet=5)
        p.values <- just.fish
        
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
        
        source.hold
        
    }
    
temp.sources <- is.sig.col(p.values)

temp.list <- lapply(temp.sources, function(x) strsplit(x=x, split="\\__")[[1]])


    
