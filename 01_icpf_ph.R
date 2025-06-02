# 
# read phaeno data
# loop plots
# chart phasis
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","mgcv","RPostgreSQL","dotenv")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}

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
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
### end
print(G)


# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();

# LOAD data ---------------------------------------------------------------------
list.files(G$d_in1);
# load(file.path(G$d_in1,"07_model_day_Mm.rda")); list.files(G$d_in1);

# CONNECT PG -------------------------------------------------------------
C <-list();

### CONNECT
aa <-E[["sys_env"]]
C$host <-aa[names(aa)%in%"FUK_PG_HOST"]; C$port <-aa[names(aa)%in%"FUK_PG_PORT"];
C$user <-aa[names(aa)%in%"FUK_PG_USER"]; C$pw <-aa[names(aa)%in%"FUK_PG_PW"]; 
C$db <-aa[names(aa)%in%"FUK_PG_DB"]; 
C$pg <- dbConnect(PostgreSQL(),host=C$host,user=C$user,password=C$pw,port=C$port,dbname=C$db);

### pg/icp_download
C$s1 <-"icp_download";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
ph_plp <-dbReadTable(C$pg, "ph_plp"); 
ph_phi <-dbReadTable(C$pg, "ph_phi"); 

### pg/icp_download
C$s1 <-"icp_dictionaries";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
d_event <-dbReadTable(C$pg, "d_event"); 
d_event_score <-dbReadTable(C$pg, "d_event_score"); 

# LOOP plot ------------------------------------------------------------------
ll <-levels(as.factor(ph_plp$code_plot))
ii <-3;
for(ii in 1:length(ll))
{
  aa <-ph_plp[ph_plp$code_plot%in%ll[ii],];
  bb <-ph_phi[ph_phi$code_plot%in%ll[ii],];
  ### event 1 flushing
  {
    cc <-bb[bb$code_event%in%1,];
    nam <-"flushing";
    ### loop score
    pp <-paste0(c(1:5),".0");
    pp <-c(1:5); # scorte as integer
    jj <-1;
    for(jj in 1:length(pp))
    {
      dd <-cc[cc$code_event_score%in%pp[jj],];
      ### ex duplicated
      dd <-dd[order(dd$date_observation,decreasing = F),];
      dd <-dd[duplicated(paste0(dd$survey_year,dd$tree_number,dd$code_event_score))==F,];
      ### y
      dd$y <-as.integer(format(dd$date_observation,"%j")); 
      y_nam <-"doy"; 
      y_min <-min(as.integer(format(cc$date_observation,"%j")));
      y_max <-max(as.integer(format(cc$date_observation,"%j"))); 
      ### x
      dd$x <-dd$survey_year; 
      x_nam <-"year";
      ### boxplot
      {
        out <-paste(G$d_out2,paste(nam,ll[ii],pp[jj],".png",sep="_"),sep="/");
        png(out, width=160,height=120,units="mm",res=300,pointsize=5); 
        par(cex=1.5,cex.lab=2,cex.axis=2,pch=16, mar=c(2,5,1,1)); # par()
        ee <-boxplot(dd$y~dd$x,ylim=c(y_min,y_max),ylab=y_nam,xlab=x_nam,pch=16);
        grid();
        boxplot(dd$y~dd$x,col="green3",add=T);
        text(x=0.5,y_min+y_min*0.1,"number of trees",col="red3",cex=2,pos=4);
        text(x=1:length(ee$names),y_min+y_min*0.05,ee$n,col="red3",cex=2,pos=1);
        text(x=0.5,y_max-y_max*0.025,paste(ll[ii]," - ",nam,"score",pp[jj]),col="blue3",cex=2,pos=4);
        graphics.off();
      }
      ### plot
      {
        out <-paste(G$d_out2,paste(nam,"trend",ll[ii],pp[jj],".png",sep="_"),sep="/");
        png(out, width=160,height=120,units="mm",res=300,pointsize=5); 
        par(cex=1.5,cex.lab=2,cex.axis=2,pch=16, mar=c(2,5,1,1)); # par()
        ee <-tapply(dd$y, dd$x, mean)
        plot(ee,type="l",ylim=c(y_min,y_max),ylab=y_nam,xlab=x_nam,pch=16,xaxt="n");
        axis(1,at=1:length(ee),labels=names(ee))
        grid();
        lines(ee,col="green3")
        ###
        text(x=0.5,y_max-y_max*0.02,paste(ll[ii]," - ",nam,"score",pp[jj]),col="blue3",cex=2,pos=4);
        ### loess
        ff <-data.frame(var=ee,year=names(ee))
        gg <-loess(var ~ year, data=ff, span=2);
        lines(predict(gg)~ c(1:nrow(ff)),col="black"); 
        text(1,y_max-y_max*0.075,"LOESS Trendlinie",cex=1.5,col="black",adj=0);
        ### Calculate Kendall's tau and its associated p-value
        # ee <- cor.test(cc$var, as.numeric(cc$year), method = "kendall")
        ### mann-kendall
        gg <-mk.test(ff$var,"two.sided"); gg$p.value;
        text(1,y_max-y_max*0.05,paste0("Mann-Kendall Trend Test p-Wert = ",
                             round(gg$p.value,4)),cex=1.5,col="black",adj=0);
        ###
        graphics.off();
      }
    }
  }
  ### end ii
}


# SAVE rda --------------------------------------------------------
out <-paste(G$n_script,"ph_plp.rda",sep="-");
save(ph_plp,file = file.path(G$d_out1,out));

out <-paste(G$n_script,"ph_phi.rda",sep="-");
save(ph_phi,file = file.path(G$d_out1,out));

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
