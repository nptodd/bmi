####### smoking at ageMin - at around last age analyzed association
# summarize the association between smoking at AgeMin and current 
# smoking for those with 'completed' smoking history

res <- lapply(nhanes_descr_l, 
              function(x){
                 
                 res <- svyby(~smok_curr_log, by = ~ end_age + nonSmokerAgeMin + sex, 
                              svymean, design = x, na.rm=T, 
                              keep.names = F)
                 setDT(res)
                 setnames(res, old = c("smok_curr_logTRUE", "se.smok_curr_logTRUE"),
                          new= c("mean", "se"))
                 res[,`:=`(mean=100*mean,  se=100*se)]

                 res <- res[(end_age)]
                 
                 return(res) })

res <- rbindlist(res, idcol = "cycle")

res[, pre:=1/(se^2)] # precision (inverse variance)
res <- res[, .(mean=weighted.mean(mean, pre), se=sqrt(1/sum(pre))), 
           keyby=.(sex, nonSmokerAgeMin)]


res[, legend := paste0(ifelse(nonSmokerAgeMin==F, 
                              "Smok. at ", "Non-smok. at "), 
                       min(ages_study) )]


png( "../ms/figs_supApp/smoking_continuation.png",
     width = 4, height = 4,
     units = "in", res    = 1200, pointsize = 5)
par(mar = c(6, 9, 2, 2) )

barCenters <- barplot(res[,mean], 
                      ylim = c(0,70),
                      ylab = "",
                      axes=F, 
                      cex.names = 1.3, 
                      names.arg = res[,legend])
axis(2, las=2, tck=0.02, line = 3, cex.axis=1.4)

text(x = c(mean(barCenters[1:2]), mean(barCenters[3:4])), 
     y = 65, pos = 4,
     labels = c("Females", "Males"), font=2, cex=2 )

mtext(paste0("Smokers at ",  
             max(ages_study)-4, "-", 
             max(ages_study), " (%)"),
      side = 2, line = 7, cex=2)
segments(x0 = barCenters, 
         y0 = res$mean - res$se,
         x1 = barCenters,
         y1 = res$mean + res$se, lwd = 1.5)
arrows(barCenters, res$mean - res$se, barCenters,
       res$mean + res$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()


rm(res)

