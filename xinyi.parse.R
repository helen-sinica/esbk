library(XML)
library(data.table)
library(plyr)

# setting
# is.windows <- function() .Platform$OS.type=="windows"
# ifelse(is.windows(), setwd("D:/E-sun"), setwd('/home/esunb/Helen/'))
# getwd()
setwd("D:/E-sun")

brh.geo <- fread("brh.addr.csv", data.table=F, colClasses = c("character", "character", 
                                                              "character", "character", 
                                                              "numeric", "numeric"))

x <- fread("brh.price.raw.csv")
i <- x[,.(brh.id, unit.10k.pr)]
# calcuate the mean price without outliers(1.5*IQR)
y <- i[,.(brh.hm.pri=mean(unit.10k.pr[!unit.10k.pr %in% boxplot.stats(unit.10k.pr)])), by=brh.id]
y <- join(brh.geo, y, by="brh.id")
write.csv(y, "branch_home_price.csv", row.names=F)

# none: 1034 Peng-Hu
brh <- brh.geo$brh
#----
# table <- matrix()
# for(brh.code in brh){
#     file <- sprintf("rEst_price/%s.html", brh.code)
#     doc <- htmlParse(file)
#     td <- xpathSApply(doc, '//td', xmlValue)
#     tb <- data.frame(matrix(td, ncol = 12, byrow=T))    
#     colnames(tb) <- c("yr.mth", "obj.type", "addr", "obj.age", "layout", "fl/t.fl", 
#                       "obj.ping", "landd.ping", "park.ping", "park.10k.pr", 
#                       "unit.10k.pr", "total.10k.pr")
#     
#     for(c in c("park.10k.pr", "unit.10k.pr", "total.10k.pr")){
#         tb[, c] <- gsub("萬", "", tb[, c])
#         tb[, c] <- as.numeric(gsub("--", "0", tb[, c]))
#     }
#         
#     
#     o.file <- paste0(brh.code, "_price_raw.csv")
#     write.table(tb, file = o.file, fileEncoding = "UTF-8", row.names=F, sep=",")
# }
#----
o.file <- "brh.price.raw.csv"
count <- 1
for(brh.code in brh){
    file <- sprintf("rEst_price/%s_price_raw.csv", brh.code)
    tb <- fread(file, data.table=F)
    tb$brh.id <- brh.code
    
    if(count==1){
        write.table(tb, file = o.file, row.names=F, sep=",", 
                    col.names=T)
    }else{
        write.table(tb, file = o.file, row.names=F, sep=",",
                    append=T, col.names=F)
    }
    count <- count + 1
}


# set of address (136 branches)
# cookie setting??

library(stringr)

brh.geo <- fread("brh.addr.csv", data.table=F, colClasses = c("character", "character", 
                                                           "character", "character", 
                                                           "numeric", "numeric"))
# brh$brh.add <- str_trim(brh$brh.add)
# colnames(brh.geo) <- c("brh", "brh.name", "brh.zip", "brh.addr", "brh.lat", "brh.lng")
# write.table(brh.geo, "brh.addr.csv", row.names=F, sep=",")


addr <- brh.geo$brh.addr[1:3]
URL <- vector()
for(i in add){
    URL[i] <- sprintf("http://tradeinfo.sinyi.com.tw/itemList.html?k1=%s&c8=700&s1=1&s2=10310_10403", i)
}
