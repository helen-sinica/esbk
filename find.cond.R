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
x <- x[order(x$serv.sec),]


# 分行當天的服務時間(<700為主)
p.serv.sec <- function(brh.code="0679", date="2014-10-01", i=10){
    y <- etab[etab$brh==brh.code&etab$in.dt==date, c("tkt.no", "serv.sec")]    
    y$tkt.no <- as.integer(y$tkt.no)
    y <- y[order(y$tkt.no),]
    y <- y[y$tkt.no<700,]
    main <- paste("brh", brh.code, date, sep="_")
    
    pdf(file = paste0(i, main, ".pdf"), width = 10, height=5)
    with(y, plot(serv.sec~tkt.no, pch=1, cex=.6, main=main))
    dev.off()
}

for(x in 51:136){
    y <- brh.serv.se$brh[x]
    p.serv.sec(brh.code=y, date = "2014-12-15", i = x+10)
}
seg.etab(brh.code="0222", date = "2014-12-15")

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
View(etab[etab$tkt.no=="0950",])
View(etab[etab$tkt.no=="1000",])

# 分行當天的叫號情況（表）
#.. seg.etab <- function(brh.code="0912", date="2015-02-16", i=10){
seg.etab <- function(brh.code="0912", date="2015-02-16"){
    y <- etab[etab$brh==brh.code&etab$in.dt==date,] 
    y$tkt.no <- as.integer(y$tkt.no)
    y <- y[order(y$tkt.no),]
    View(y)
    #.. add i for looping
    #.. write.csv(y, paste0(i, ".csv"))
}
# 
# seg.etab(brh.code="0912", date="2014-12-15")
# seg.etab(brh.code="0440", date="2015-02-16")

for(x in 0:3){
    y <- brh.serv.se$brh[x]
    seg.etab(brh.code=y, date = "2014-12-15", i = x+10)
}



# Branch summary (not all 136 branches, least-20 ignored)
c.brh <- count(etab.f, vars=c("brh", "in.dt", "cu.grp"))
m.w.num <- aggregate(wait.num~brh+in.dt+cu.grp, data=etab.f, mean)
m.serv <- aggregate(serv.sec~brh+in.dt+cu.grp, data=etab.f, mean)
m.wait <- aggregate(wait.sec~brh+in.dt+cu.grp, data=etab.f, mean)

clerk <- aggregate(clerk.card~brh+in.dt+cu.grp, data=etab.f, unique)
clerk$clerk.num <- unlist(lapply(clerk$clerk.card, length))
clerk <- clerk[,-4]

brh.info.dt <- data.frame(clerk, m.serv=m.serv$serv.sec, m.wait=m.wait$wait.sec, 
                  tkt.num = c.brh$freq, wait.num =m.w.num$wait.num)

write.csv(brh.info.dt, "brh.info.dt.csv", row.names=F)

brh.info <- brh.info.dt[,-2]
t <- aggregate(.~brh+cu.grp, data=brh.info, mean)
t.sd <- aggregate(.~brh+cu.grp, data=df, t.sd)
colnames(t.sd) <- c("brh", "cu.grp", "sd.clerk.num", "sd.m.serv", "sd.m.wait", 
                  "sd.tkt.num", "sd.wait.num")

brh.info <- join(t, t.sd)

write.csv(brh.info, "brh.info.csv", row.names=F)


# call.opr summary
c.opr <- count(call, vars=c("opr.id.md5", "call.dt"))
c.opr$dt <- as.Date(c.opr$call.dt)

opr.calls <- aggregate(freq~opr.id.md5, data=c.opr, mean)
colnames(opr.calls)[2] <- "m.call"
call.sd <- aggregate(freq~opr.id.md5, data=c.opr, sd)
min.dt <- aggregate(dt~opr.id.md5, data=c.opr, min)
max.dt <- aggregate(dt~opr.id.md5, data=c.opr, max)
days <- aggregate(dt~opr.id.md5, data=c.opr, length)

opr.info <- data.frame(opr.calls, call.sd=call.sd$freq, start.dt=min.dt$dt, 
                       last.dt=max.dt$dt, days=days$dt)

opr.info$l_s <- opr.info$last.dt - opr.info$start.dt + 1

opr.info <- opr.info[order(opr.info$m.call, decreasing=T), ]

write.csv(opr.info, "opr.info.csv", row.names=F)

setnames(ss, c("brh", "cu.grp", "m.clerk.num", "m.serv", "m.wait", "m.tkt.num", 
                  "m.wait.num", "sd.clerk.num", "sd.serv", "sd.wait", "sd.tkt.num", 
                  "sd.wait.num" ))
write.csv(ss, "brh.info.csv", row.names=F)

setnames(opr, c("opr.id.md5", "m.call", "call.sd", "start.dt", "last.dt", "days", "l_s"))
write.csv(opr, "opr.info.csv", row.names=F)

