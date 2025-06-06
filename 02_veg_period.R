# 
# read meteo data
# loop plots
# get phaeno phasis
# chart phasis
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","mgcv","RPostgreSQL","dotenv",
       "vegperiod","trend","berryFunctions")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}
citation("vegperiod")

# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
G$n_project <-bb[length(bb)-1]
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in);
cc <-unlist(str_split(G$d_in,G$n_project))[1];
G$d_in1 <-file.path(cc,"icpf_mm/output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
### end
print(G)

# LOAD data ---------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"07_model_day_Mm.rda"));
list.files(G$d_out1);
load(file.path(G$d_out1,"01_icpf_ph-ph_phi.rda"));
load(file.path(G$d_out1,"01_icpf_ph-ph_plp.rda"));


# LOOP plot ------------------------------------------------------------------
ll <-names(Mm); VEG <-list(); veg <-data.frame(matrix(NA,0,0));
ii <-4;
for(ii in 1:length(ll))
{
  aa <-Mm[[ll[ii]]];
  id <-str_sub(ll[ii],1,-4);
  ### veg period
  bb <-data.frame( # data(goe)
    date=aa$date,
    year=as.integer(format(aa$date,"%Y")),
    month=as.integer(format(aa$date,"%m")),
    day=as.integer(format(aa$date,"%d")),
    doy=as.integer(format(aa$date,"%j")),
    t=aa$at
  )
  bb <-bb[is.na(bb$t)==F,]; #summary(bb$t)
  bb <-bb[!bb$year%in%G$t_year,];
  ### tree species
  bart <-"Pinus sylvestris";
  if(id%in%c(1207)){bart <-"Fagus sylvatica";}
  if(id%in%c(1208,1209)){bart <-"Quercus robur";}
  ### vegperiod
  cc <-vegperiod(dates=bb$date, Tavg=bb$t, 
            start.method="Menzel", end.method="LWF-BROOK90", 
            species=bart, est.prev=3)
  VEG[[id]] <-cc;
  ### plot
  {
    G$d_temp <-file.path(G$d_out2,"veg_start"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
    ### window
    graphics.off();
    nam <-paste("veg_start",id,sep="_");
    out <-file.path(G$d_temp,paste(nam,".png",sep="_"));
    png(out, units="mm", width=160, height=100, res=300);
    par(mar=c(3,3,2,1),mgp=c(2,1,0),lab=c(12,5,7)); # par()
    ### base
    plot(cc$start~cc$year,type="l",ylim=c(120,170),
         ylab="doy",xlab="year",col="black",main=nam);
    grid(nx = NA, ny=NULL); # par()$usr
    lines(cc$start~cc$year,col="green3");
    ### loess
    dd <-loess(start ~ year, data=cc, span=0.80);
    lines(predict(dd)~cc$year,col="black"); 
    text(1961,164,"LOESS Trendlinie",cex=.8,col="black",adj=0);
    ### Calculate Kendall's tau and its associated p-value
    ee <- cor.test(cc$start, cc$year, method = "kendall")
    ### mann-kendall
    ff <-mk.test(cc$start,"less"); ff$p.value;
    text(1961,168,paste0("Mann-Kendall Trend Test p-Wert = ",
                            round(ff$p.value,4)),cex=.8,col="black",adj=0);
    ### save
    graphics.off();  
  }
  ### budding
  {
    aa <-ph_plp[ph_plp$code_plot%in%id,];
    bb <-ph_phi[ph_phi$code_plot%in%id,];
    bb <-bb[bb$code_event%in%1,];
    cc <-cc[cc$year%in%bb$survey_year,];
    if(nrow(cc)==0){next}
  }
  ### plot subset
  {
    G$d_temp <-file.path(G$d_out2,"veg_start"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
    ### window
    graphics.off();
    nam <-paste("veg_start",id,"subset",sep="_");
    out <-file.path(G$d_temp,paste(nam,".png",sep="_"));
    png(out, units="mm", width=160, height=100, res=300);
    par(mar=c(3,3,2,1),mgp=c(2,1,0),lab=c(12,5,7)); # par()
    ### base
    plot(cc$start~cc$year,type="l",ylim=c(120,170),
         ylab="doy",xlab="year",col="black",main=nam);
    grid(nx = NA, ny=NULL); # par()$usr
    lines(cc$start~cc$year,col="green3");
    ### loess
    dd <-loess(start ~ year, data=cc, span=0.80);
    lines(predict(dd)~cc$year,col="black"); 
    text(min(cc$year),164,"LOESS Trendlinie",cex=.8,col="black",adj=0);
    ### mann-kendall
    ff <-mk.test(cc$start,"less"); ff$p.value;
    text(min(cc$year),168,paste0("Mann-Kendall Trend Test p-Wert = ",
                         round(ff$p.value,4)),cex=.8,col="black",adj=0);
    ### save
    graphics.off();  
  }
  ### event_score 1.5 (this is ugly)
  {
    aa <-bb[bb$code_event_score%in%1,];
    qq <-levels(as.factor(aa$tree_number));
    jj <-2; b6 <-NULL;
    for(jj in 1:length(qq))
    {
      b1 <-aa[aa$tree_number%in%qq[jj],]; 
      b1 <-b1[order(b1$date_observation,decreasing = T),];
      b1 <-b1[duplicated(b1$survey_year)==F,];
      b2 <-bb[bb$code_event_score%in%2 & bb$tree_number%in%qq[jj],];
      b2 <-b2[order(b2$date_observation,decreasing = F),];
      b2 <-b2[duplicated(b2$survey_year)==F,];
      b3 <-merge(b1,b2,by="survey_year");
      if(nrow(b3)==0){next};
      b4 <-b3$date_observation.y-b3$date_observation.x;
      b5 <-b1[paste(b1$survey_year,b1$tree_number)%in%paste(b3$survey_year,b3$tree_number.x),];
      b5$code_event_score <-1.5;
      b5$date_observation <-b5$date_observation+b4;
      b6 <-rbind(b6,b5);
    }
    bb <-rbind(bb,b6);
  }
  
  ### loop score
  {
    G$d_temp <-file.path(G$d_out2,"lm_flushing_score"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
    pp <-paste0(c(1:5),".0");
    pp <-c(1:5,1.5); # scorte as integer
    jj <-6;
    for(jj in 1:length(pp))
    {
      aa <-bb[bb$code_event_score%in%pp[jj],];
      dd <-tapply(as.integer(format(aa$date_observation,"%j")), aa$survey_year, mean)
      ee <-data.frame(year=names(dd),var=dd);
      ff <-merge(cc,ee,by="year")
      ### plot
      {
        ### window
        graphics.off();
        nam <-paste(id,"flushing_score",pp[jj],"veg.start",sep="_");
        out <-file.path(G$d_temp,paste(nam,".png",sep="_"));
        png(out, units="mm", width=160, height=160, res=300);
        par(mar=c(3,3,2,1),mgp=c(2,1,0),lab=c(12,5,7)); # par()
        ### base
        plot(ff$var~ff$start,type="p",xlim=c(100,170),ylim=c(100,170),
             ylab="doy - flushing",xlab="doy - veg.start",col="black",main=nam);
        grid(nx = NA, ny=NULL); # par()$usr
        points(ff$var~ff$start)
        ### lm
        hh <-lm(ff$var~ff$start); rr <-summary(hh);
        gg <-cor.test(ff$var,ff$start, method = "pearson")
        abline(hh,col="red3");
        text(100,165,paste0("p-Wert = ",round(gg$p.value,4)),cex=1.5,col="black",adj=0);
        text(100,160,paste0("R² = ",round(rr$r.squared,4)),cex=1.5,col="black",adj=0);
        text(100,155,paste0("RMSE = ",round(rmse(ff$var,ff$start),1)),cex=1.5,col="black",adj=0);
        ### save
        graphics.off();  
        ### table
        veg[nrow(veg)+1,] <-NA;
        veg[nrow(veg),"plot"] <-id;
        veg[nrow(veg),"score"] <-pp[jj];
        veg[nrow(veg),"p_value"] <-gg$p.value;
        veg[nrow(veg),"r_squre"] <-rr$r.squared;
        veg[nrow(veg),"rmse"] <-rmse(ff$var,ff$start);
      }
    }
  }
  ### end ii
}


# SAVE --------------------------------------------------------
out <-paste(G$n_script,"VEG.rda",sep="-");
save(VEG,file = file.path(G$d_out1,out));

out <-paste(G$n_script,"veg.csv",sep="-");
write.table(veg[order(veg$score),],file = file.path(G$d_out2,out),col.names = T,row.names = F,sep=";",dec=".");

# SAVE -----------------------------------------------------------
out <-paste(G$n_script,"_image.rda",sep="-");
save.image(file = file.path(G$d_out1,out));

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
