
####### change over cohorts of educational attainment


res <- lapply(nhanes_descr_l, 
       function(x){
         
         res <- svyby(~is_hsl, by = ~ bc + sex + aboveAgeMin, 
                                svymean, design = x, na.rm=T, 
                                keep.names = F)
       setDT(res)
       setnames(res, c("is_hslTRUE", "se.is_hslTRUE"), c("hsl", "se.hsl"))
       res[,`:=`(hsl=100*hsl,  se.hsl=100*se.hsl,
                 is_hslFALSE=NULL, se.is_hslFALSE=NULL)]
       
       sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
       
       res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
       
       res = res[sample_size>15]   # se is only estimated. Pb with estimation with very low sample size
       
       res <- res[(aboveAgeMin)]
       
       return(res) })

res <- rbindlist(res, idcol = "cycle")

nhanes_descr[bc==1990 & sex=="m" & (aboveAgeMin), table(cycle)]
res[bc==1990 & sex=="m"] # test
# res[bc==1990 & sex=="m", weighted.mean(hsl, 1/se.hsl^2)] # test
res[, pre.hsl:=1/(se.hsl^2)] # precision (inverse variance)
res <- res[, .(hsl=weighted.mean(hsl, pre.hsl), se.hsl=sqrt(1/sum(pre.hsl))), 
           keyby=.(sex, bc)]
# res[bc==1990 & sex=="m"]  # test


res2 <- lapply(nhanes_descr_l, 
              function(x){res <- svyby(~is_hslscd, by = ~ bc + sex + aboveAgeMin, 
                                       svymean, design = x, na.rm=T, 
                                       keep.names = F)
              setDT(res)
              setnames(res, c("is_hslscdTRUE", "se.is_hslscdTRUE"), c("hslscd", "se.hslscd"))
              res[,`:=`(hslscd=100*hslscd,  se.hslscd=100*se.hslscd,
                        is_hslscdFALSE=NULL, se.is_hslscdFALSE=NULL)]

              sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
              
              res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
              
              res = res[sample_size>15]   # se is only estimated. Pb with estimation with very low sample size
              
              res <- res[(aboveAgeMin)]
              
              return(res) })

res2 <- rbindlist(res2, idcol = "cycle")

res2[bc==1990 & sex=="m"]  # test

res2[, pre.hslscd:=1/(se.hslscd^2)] # precision (inverse variance)
res2 <- res2[, .(hslscd=weighted.mean(hslscd, pre.hslscd), se.hslscd=sqrt(1/sum(pre.hslscd))),
             keyby=.(sex, bc)]
res2[bc==1990 & sex=="m"]  # test

res[res2, on=.(bc, sex), `:=`(hslscd=hslscd,
                              se.hslscd=se.hslscd) ]

# res <- res[ bc %in% cohorts_study & 
#               (bc+4) %in% cohorts_study ]
res <- res[ bc %in% cohorts_study ]
res[,`:=`(all=100,none=0)]
rm(res2)

plot_educ_f <- function(data_a){
  
  pal <- RColorBrewer::brewer.pal(3, "Blues")
  
  plot(data_a[, .(bc, hsl)], type="l", 
       axes=F, xlab="Birth cohort", 
       cex.lab=2, cex.axis=1.4,
       ylab="Proportion (%)", ylim=c(0, 100))
  lines(data_a[, .(bc, hslscd)])
  
  polygon(c(data_a[,bc],rev(data_a[,bc])),
          c(data_a[,hsl],rev(data_a[,hslscd])),col=pal[2])
  polygon(c(data_a[,bc],rev(data_a[,bc])),
          c(data_a[,hslscd],rev(data_a[,all])),col=pal[3])
  polygon(c(data_a[,bc],rev(data_a[,bc])),
          c(data_a[,none],rev(data_a[,hsl])),col=pal[1])
  
  Hmisc::errbar(x = data_a[,bc], 
                y = data_a[,hsl], 
                yplus = data_a[,hsl+se.hsl],
                yminus = data_a[,hsl-se.hsl], add=T)
  Hmisc::errbar(x = data_a[,bc], 
                y = data_a[,hslscd], 
                yplus = data_a[,hslscd+se.hslscd],
                yminus = data_a[,hslscd-se.hslscd], add=T)
  
  axis(1, cex.axis=1.3,
       at=unique(data_a[,bc]), 
       labels = paste(unique(data_a[,bc]), 
                      substr(unique(data_a[,bc])+4, 4, 4), sep="-") , cex.axis=1.5 )
  axis(2, las=2, tck=0.02, cex.axis=1.3)
  # abline(v=seq(1945, 1990, 5), lty=2, col="darkgrey", lwd=0.5)
  
  text(x = 1953, y=80, pos = 4, labels =  "College graduates", font = 2, cex = 1.8)
  text(x = 1953, y=55, pos = 4, labels =  "Some college degree", font = 2, cex = 1.8)
  text(x = 1953, y=20, pos = 4, labels =  "High school or less", font = 2, cex = 1.8)
  
  if(unique(data_a[, sex])=="m"){LABEL <- "Males"}
  if(unique(data_a[, sex])=="f"){LABEL <- "Females"}
  
  text(x=1953, y=95, pos=4, labels=LABEL, font=2, cex=2.5)
}

png( "../ms/figs_supApp/change_education.png",
     width = 8, height = 5,
     units = "in", res    = 1200, pointsize = 5)
par( mfrow=c(1, 2), mar=c(6, 5, 2, 1) ) 
plot_educ_f(res[sex=="f"])
plot_educ_f(res[sex=="m"])
dev.off()

rm(res)