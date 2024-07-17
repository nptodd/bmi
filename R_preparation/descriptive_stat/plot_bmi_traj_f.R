
###################### plot individual bmi trajectories of individuals interviewed at age in ages_study

plot_bmi_traj_f=function(n_a=5, cycle_a="A", deb_type_a=c("bmi"), max_bmi_a=F, 
                         sex_a=c("m", "f"), return_a=F, verbose_a=T, lty_a=1, lwd_a=1.5,
                         ylim_a=c(16,43), add_legend_a=F){
  
  ### check arguments
  if(! prod(deb_type_a %in% c("bmi", "bmi_deb1", "bmi_deb2"))){ stop("Incorrect BMI type")  }
     
     if( (! cycle_a %in% names(nhanes_l)) | (length(cycle_a)>1)){ stop("Incorrect wave number")  }
     
     if( !sex_a %in% c("m", "f") ){  stop("Incorrect sex argument")  }
     
     ### graphical parameters
     col_dt = data.table(deb_type = c("bmi", "bmi_deb1", "bmi_deb2"), 
                         name     = c("Reported", "Debiased", "Debiased 2"),
                         colour   = c("gray55",  "gray40", "gray25" ),
                         pch      = c(17, 17, 17))
     col_dt=col_dt[deb_type %in% deb_type_a]
     
     
     nhanes_loc = melt_nhanes_f(input_data_a = nhanes_l[[cycle_a]], 
                                verbose_a = F, bmis_a = "all")
     
     nhanes_loc = nhanes_loc[(age_screening %in% ages_study) &
                             (examined==1)]
     
     # sample of individuals to graph
     rdm_sample = sample(nhanes_loc[sex %in% sex_a, unique(seqn)], 
                         size = n_a, 
                         replace = F)
     
     # restrict to selected rows. Removing NAs is important to avoid breaks in lines
     graph_data = nhanes_loc[seqn %in% rdm_sample &
                             !is.na(age) &
                             !is.na(bmi_deb1)  ]
     
     graph_data2 =melt(graph_data, id.vars = c("seqn", "age", "type_recall"), value.name = "bmi",
                       measure.vars = c("bmi", "bmi_deb1", "bmi_deb2"), variable.name = "deb_type")
     
     # only keep series of deb_type_a
     graph_data2=graph_data2[deb_type %in% deb_type_a]
     
     # create identity variable of each curve to plot (seqn x deb_type)
     graph_data2[,id2 := paste(seqn, gsub("bmi_", "", deb_type), sep="_" )]
     
     # add graphical parameters (col and pch) to graph_data2
     graph_data2[col_dt, on=.(deb_type), COL:= colour]
     graph_data2[, PCH:=ifelse(type_recall!="whichmax", 17, 20)]
     
     
     if(!max_bmi_a){  # remove max bmi measure
       graph_data2 = graph_data2[type_recall!="whichmax"]
     }
     
     # order by indiv. and age
     setkey(graph_data2, id2, age)
     
     plot_line_f=function(data_a, first_a, type_a=1){
       
       if(first_a){ 
         
         XLIM = c(min(ages_study)-1, max(ages_study)+1)
         YLIM = ylim_a
         
         plot(data_a[,.(age, bmi)], 
              type = "b", 
              xlab="", ylab="", 
              lty=lty_a, lwd=lwd_a,
              axes=F,
              pch = data_a[,PCH], col=data_a[,COL],
              xlim = XLIM, ylim = YLIM)
         mtext("Age", side = 1, line = 2.4, cex=1.2)
         axis(1, at=seq(min(ages_study), max(ages_study), by = 5))
         axis(2, las=2)
         
       } else {
         lines(data_a[,.(age, bmi)], 
               type="b", lty=lty_a, lwd=lwd_a,
               pch= data_a[,PCH], col=data_a[,COL])                          
       }
     }
     
     first_draw=T
     
     for(I in graph_data2[, unique(id2)]){
       
       if(verbose_a){ print(graph_data2[id2==I]) }
       
       plot_line_f(graph_data2[id2==I], first_draw, 1)
       
       if(first_draw){ abline(h = c(18.5, 25, 30, 40), lty = 2, lwd=1.5, col = "darkgrey")  }
       first_draw=F    
     }
     
     if(add_legend_a){
     legend("top", legend = col_dt[,name], xpd=T,
            col=col_dt[, colour], pt.cex=2,
            ncol=length(deb_type_a), bty='n', x.intersp=0.9, pch=col_dt[,pch]) }
     
     if(sex_a=="m"){
       txt_title = "Male           "
     }else if (sex_a=="f"){
       txt_title = "Female         " } 
     else{
       txt_title = "" }
     mtext(txt_title, side = 3, line = -5, adj = 1,  font = 2)
     
     if(return_a){return(graph_data)}
}

