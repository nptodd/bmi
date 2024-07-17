
####### change of ethnic composition by cohort



res <- lapply(nhanes_descr_l,
              function(x){
                
                res <- svyby(~b, by = ~ bc + sex + aboveAgeMin, 
                             svymean, design = x, na.rm=T, 
                             keep.names = F)
                setDT(res)
                setnames(res, c("bTRUE", "se.bTRUE"), c("b", "se.b"))
                res[,`:=`(b=100*b,  se.b=100*se.b,
                          bFALSE=NULL, se.bFALSE=NULL)]
                
                sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
                
                res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
                
                res = res[sample_size>15]   # se is only estimated. Pb with estimation with very low sample size
                
                res <- res[(aboveAgeMin)]
                
                return(res) })

res <- rbindlist(res, idcol = "cycle")

res[, pre.b:=1/(se.b^2)] # precision (inverse variance)
res <- res[, .(b=weighted.mean(b, pre.b), se.b=sqrt(1/sum(pre.b))), 
           keyby=.(sex, bc)]


res2 <- lapply(nhanes_descr_l,
              function(x){
                
                res <- svyby(~bo, by = ~ bc + sex + aboveAgeMin, 
                             svymean, design = x, na.rm=T, 
                             keep.names = F)
                setDT(res)
                setnames(res, c("boTRUE", "se.boTRUE"), c("bo", "se.bo"))
                res[,`:=`(bo=100*bo,  se.bo=100*se.bo,
                          boFALSE=NULL, se.boFALSE=NULL)]
                
                sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
                
                res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
                
                res = res[sample_size>15]   # se is only estimated. Pb with estimation with very low sample size
                
                res <- res[(aboveAgeMin)]
                
                return(res) })

res2 <- rbindlist(res2, idcol = "cycle")

res2[, pre.bo:=1/(se.bo^2)] # precision (inverse variance)
res2 <- res2[, .(bo=weighted.mean(bo, pre.bo), se.bo=sqrt(1/sum(pre.bo))), 
           keyby=.(sex, bc)]



res3 <- lapply(nhanes_descr_l,
               function(x){
                 
                 res <- svyby(~boh, by = ~ bc + sex + aboveAgeMin, 
                              svymean, design = x, na.rm=T, 
                              keep.names = F)
                 setDT(res)
                 setnames(res, c("bohTRUE", "se.bohTRUE"), c("boh", "se.boh"))
                 res[,`:=`(boh=100*boh,  se.boh=100*se.boh,
                           bohFALSE=NULL, se.bohFALSE=NULL)]
                 
                 sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
                 
                 res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
                 
                 res = res[sample_size>15]   # se is only estimated. Pb with estimation with very low sample size
                 
                 res <- res[(aboveAgeMin)]
                 
                 return(res) })

res3 <- rbindlist(res3, idcol = "cycle")

res3[, pre.boh:=1/(se.boh^2)] # precision (inverse variance)
res3 <- res3[, .(boh=weighted.mean(boh, pre.boh), se.boh=sqrt(1/sum(pre.boh))), 
             keyby=.(sex, bc)]


res[res2, on=.(bc, sex), `:=`(bo=bo,
                              se.bo=se.bo) ]
res[res3, on=.(bc, sex), `:=`(boh=boh,
                              se.boh=se.boh) ]

res <- res[ bc %in% cohorts_study & 
              (bc+4) %in% cohorts_study]
res[,`:=`(all=100,none=0)]
rm(res2); rm(res3)

plot_race_f <- function(data_a){
  
  pal <- RColorBrewer::brewer.pal(4, "Blues")
  
  plot(data_a[, .(bc, b)], type="l", 
       axes=F, xlab="Birth cohort", cex.lab=2, cex.axis=1.4,
       ylab="Proportion (%)", ylim=c(0, 50))
  lines(data_a[, .(bc, bo)])
  lines(data_a[, .(bc, boh)])
  
  X <- data_a[,bc]
  polygon(c(X,rev(X)),
          c(data_a[,none],rev(data_a[,b])),col=pal[4])
  polygon(c(X,rev(X)),
          c(data_a[,b],rev(data_a[,bo])),col=pal[3])
  polygon(c(X,rev(X)),
          c(data_a[,bo],rev(data_a[,boh])),col=pal[2])
  # polygon(c(X,rev(X)),
  #         c(data_a[,boh],rev(data_a[,all])),col=pal[1])
  
  
  Hmisc::errbar(x = X, 
                y = data_a[,b], 
                yplus = data_a[,b+se.b],
                yminus = data_a[,b-se.b], add=T)
  Hmisc::errbar(x = X, 
                y = data_a[,bo], 
                yplus = data_a[,bo+se.bo],
                yminus = data_a[,bo-se.bo], add=T)
  Hmisc::errbar(x = X, 
                y = data_a[,boh], 
                yplus = data_a[,boh+se.boh],
                yminus = data_a[,boh-se.boh], add=T)
  
  axis(1, cex.axis=1.3,
       at=unique(data_a[,bc]), 
       labels = paste(unique(data_a[,bc]), 
                      substr(unique(data_a[,bc])+4, 4, 4), sep="-") , cex.axis=1.5 )
  axis(2, las=2, tck=0.02, cex.axis=1.3)
  # abline(v=seq(1945, 1990, 5), lty=2, col="darkgrey", lwd=0.5)
  
  text(x = 1969, y=48, pos = 4, labels =  "Non-Hispanic White", font = 2, cex = 1.8)
  text(x = 1975, y=32, pos = 4, labels =  "Hispanic", font = 2, cex = 1.8)
  text(x = 1976, y=16.5, pos = 4, labels =  "Other Race", font = 2, cex = 1.8)
  text(x = 1969, y=5, pos = 4, labels =  "Non-Hispanic Black", font = 2, cex = 1.8)
  
  if(unique(data_a[, sex])=="m"){LABEL <- "Males"}
  if(unique(data_a[, sex])=="f"){LABEL <- "Females"}
  
  text(x=1952, y=43, pos=4, labels=LABEL, font=2, cex=2.5)
}

png( "../ms/figs_supApp/change_ethnicity.png",
     width = 8, height = 5,
     units = "in", res    = 1200, pointsize = 5)
par( mfrow=c(1, 2), mar=c(6, 5, 2, 1) ) 
plot_race_f(res[sex=="f"])
plot_race_f(res[sex=="m"])
dev.off()

rm(res)

