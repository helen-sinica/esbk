library(XML)
library(stringr)

# set working directory according to the OS
is.windows <- function() .Platform$OS.type=="windows"
ifelse(is.windows(), setwd("D:/E-sun"), setwd('/home/helen/esbk'))
#xxx getwd()

# read 22 htmls (loop by names)
keywd <- LETTERS[-c(12, 18, 19, 25)]
count <- 1
for(c in keywd){
    file <- sprintf("Income_tax_2012/101_165-%s.html", c)
    doc <- htmlParse(file)
    # get info by table rows
    tr <- xpathSApply(doc, '//tr', xmlValue)
    
    # set city or county name for later append
    cty <- gsub(" ", "", substr(tr[5], 8, 12))
    
    # leave only necessary rows
    # ex. "\n  \n  自強里\n  3108\n  3924749\n  1263\n  793\n  397\n  1563\n  2219.35\n  175.75\n  \n"
    tr <- tr[grep("里|村", tr)]
    tr <- tr[-grep("鄉鎮村里", tr)]
    
    # set rows to correct matrix by splitting and replacing "\n"
    # leave out first and last blank columns
    tmp <- matrix(unlist(strsplit(tr, "\n  ")), ncol=12, byrow=T)
    tmp <- gsub("\n", "", tmp)
    tmp <- tmp[,-c(1,12)]
    
    # append cty name
    tmp <- cbind(cty, tmp)
        
    # fill in blank district (must by looping)
    x <- tmp[,2]
    for(i in 1:length(x)){
        if(x[i]==""){
            x[i] <- x[(i-1)]
        }
    }
    tmp[,2] <- x
    
    # set column names of the matrix
    dimnames(tmp)[[2]] <- c("cty", "dist", "vil", "fam.num", "t.pay", "vil.pay", 
                            "med.pay", "Q1.pay", "Q3.pay", "sd.pay", "var.pay")
    # re-order the columns
    tmp <- tmp[,c(1:3, 6, 4:5, 7:11)]
    
    # change column class
    for(cl in 4:11){
        tmp[,cl] <- as.numeric(tmp[,cl])
    }
    
    # save the data to one big table
    # P.S. *.pay: unit = 10k NTD
    if(count==1){
        write.csv(tmp, "district_pay_raw.csv", row.names=F)
    }else{
        write.table(tmp, "district_pay_raw.csv", row.names=F, sep=",", col.names=F, append=T)
    }
    count <- count + 1
#是否必須統一臺或台?
}

# x <- fread("district_pay_raw.csv", data.table=F)
# x <- x[,1:4]
# x[,4] <- x[,4]/10
# write.csv(x, "district_pay.csv", row.names=F)
