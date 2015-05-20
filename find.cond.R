tmp <- etab[etab$serv.sec<0|etab$wait.sec<0,]
cond <- unique(tmp[,c("brh", "in.dt")])

range <- etab[(etab$brh %in% cond$brh[1] & etab$in.dt %in% cond$in.dt[1] | 
               etab$brh %in% cond$brh[2] & etab$in.dt %in% cond$in.dt[2] |
               etab$brh %in% cond$brh[3] & etab$in.dt %in% cond$in.dt[3] | 
               etab$brh %in% cond$brh[4] & etab$in.dt %in% cond$in.dt[4] | 
               etab$brh %in% cond$brh[5] & etab$in.dt %in% cond$in.dt[5] | 
               etab$brh %in% cond$brh[6] & etab$in.dt %in% cond$in.dt[6]), ]
# brh_1104 <- etab[etab$brh=="1104",]
write.csv(range, "range.csv", fileEncoding="UTF-8", row.names=F)



# check the mean service time of each branch
x <- aggregate(serv.sec~brh, data = etab, FUN="mean")


p.serv.sec <- function(brh.code="0679", date="2014-10-01"){
    y <- etab[etab$brh==brh.code&etab$in.dt==date, c("tkt.no", "serv.sec")]    
    y$tkt.no <- as.integer(y$tkt.no)
    y <- y[order(y$tkt.no),]
    y <- y[y$tkt.no<700,]
    main <- paste("brh", brh.code, date, sep="_")
    pdf(file = paste0(main, ".pdf"), width = 10, height=5)
    with(y, plot(serv.sec~tkt.no, pch=1, cex=.6, main=main))
    dev.off()
}

p.serv.sec(brh.code="1399")
p.serv.sec(brh.code="1399", date="2014-12-15")

brh.code="0912"
date="2015-02-13"

# 備註
# >- A = 一般收付 + VVIP + VIP;  
# 1~700 (GRP1) + 901~950 (GRP4) + 951~999 (GRP5)
# >- B = 外匯服務：701~800 (GRP2);  
# >- C = 服務中心：801~900 (GRP3)

View(etab[etab$tkt.no=="0700",])
View(etab[etab$tkt.no=="0800",])
View(etab[etab$tkt.no=="0900",])
View(etab[etab$tkt.no=="1000",])

seg.etab <- function(brh.code="0912", date="2015-02-16"){
    y <- etab[etab$brh==brh.code&etab$in.dt==date,] 
    y$tkt.no <- as.integer(y$tkt.no)
    y <- y[order(y$tkt.no),]
    View(y)
}

seg.etab(brh.code="0912", date="2014-12-15")
seg.etab(brh.code="0440", date="2015-02-16")