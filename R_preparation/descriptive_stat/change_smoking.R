####### change over cohorts of smoking at ageMin

res <- lapply(nhanes_descr_l,
              function(x){
                      
                      res <- svyby(~smokerAgeMin, by = ~ bc + sex + aboveAgeMin, 
                                   svymean, design = x, na.rm=T, 
                                   keep.names = F)
                      setDT(res)
                      setnames(res, c("smokerAgeMin"), c("mean"))
                      res[,`:=`(mean=100*mean,  se=100*se)]
                      
                      sample_size_dt = x$variables[, .N, by=.(bc, sex, aboveAgeMin)]
                      
                      res[sample_size_dt, on=.(bc, sex, aboveAgeMin), sample_size:=N]
                      
                      res = res[sample_size>15]  # se is only estimated. Pb with estimation with very low sample size
                      
                      res <- res[(aboveAgeMin)]
                      
                      return(res) })

res <- rbindlist(res, idcol = "cycle")

res[, pre:=1/(se^2)] # precision (inverse variance)
res <- res[, .(mean=weighted.mean(mean, pre), se=sqrt(1/sum(pre))), 
           keyby=.(sex, bc)]


res <- res[ bc %in% cohorts_study & (bc+4) %in% cohorts_study]

png( "../ms/figs_supApp/change_smoking.png",
     width = 4, height = 4,
     units = "in", res    = 1200, pointsize = 5)
par(mar = c(6, 9, 2, 2) )

plot(res[sex=="m", .(bc, mean)], 
     pch=20, cex=1.5, col="red",
     axes=F,
     xlab="Birth cohort", cex.lab=2, cex.axis=1.4,
     ylab=paste("Smokers at ", min(ages_study), " (%)"), 
     ylim=c(10, 60))
abline(h=seq(10, 70, 10), lty=2, col="darkgrey", lwd=0.5)
axis(1, cex.axis=1.3,
     at=unique(res[,bc]), 
     labels = paste(unique(res[,bc]), 
                    substr(unique(res[,bc])+4, 4, 4), sep="-")   )
axis(2, las=2, tck=0.02, cex.axis=1.3)
# abline(v=seq(1945, 1990, 5), lty=2, col="darkgrey", lwd=0.5)
segments(x0=res[sex=="m",bc],
         x1=res[sex=="m",bc],
         y0=res[sex=="m", (mean + se)],
         y1=res[sex=="m", (mean - se)],
         col="red" )
points(res[sex=="f", .(bc, mean)], 
       pch=20, cex=1.5, col="forestgreen" )
segments(x0=res[sex=="f",bc],
         x1=res[sex=="f",bc],
         y0=res[sex=="f", (mean + se)],
         y1=res[sex=="f", (mean - se)],
         col="forestgreen" )
segments(x0=res[sex=="m",bc],
         x1=res[sex=="m",bc],
         y0=res[sex=="m", (mean + se)],
         y1=res[sex=="m", (mean - se)],
         col="red", lty=3)
legend(x=1975, y=57, 
       legend = c("Female", "Male"),
       col=c("forestgreen", "red"),
       pch=20, cex=2, pt.cex=3,
       bty="n")
dev.off()
