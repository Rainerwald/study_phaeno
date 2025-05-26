# 
# read meteo data
# loop plots
# get phaeno phasis
# chart phasis
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","mgcv","RPostgreSQL","dotenv","vegperiod")
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
mm_plm <-dbReadTable(C$pg, "mm_plm"); 
mm_mem <-dbReadTable(C$pg, "mm_mem"); 

### pg/icp_upload
C$s1 <-"icp_upload";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
mm_plm2 <-dbReadTable(C$pg, "mm_plm"); 
mm_mem2 <-dbReadTable(C$pg, "mm_mem"); 

### rbind (thos is ugly)
cc <-c("partner_code","q_flag","change_date","code_line","line_nr");
mm_plm <-mm_plm[,!colnames(mm_plm)%in%cc]; colnames(mm_plm); 
mm_plm2 <-mm_plm2[,!colnames(mm_plm2)%in%c("sequence")]; colnames(mm_plm2);
mm_plm2 <-data.frame(survey_year=mm_plm2[,"survey_year"],mm_plm2[,!colnames(mm_plm2)%in%"survey_year"]);
colnames(mm_plm2) <-colnames(mm_plm);
mm_plm <-rbind(mm_plm,mm_plm2);
cc <-c("code_country","survey_year","partner_code","q_flag","change_date","code_line","line_nr");
mm_mem <-mm_mem[,!colnames(mm_mem)%in%cc]; colnames(mm_mem); 
mm_mem2 <-mm_mem2[,!colnames(mm_mem2)%in%c("sequence")]; colnames(mm_mem2);
colnames(mm_mem2) <-colnames(mm_mem);
mm_mem2$date_observation <-as.Date(mm_mem2$date_observation,"%d%m%Y");
mm_mem <-rbind(mm_mem,mm_mem2);
mm_mem <-data.frame(survey_year=format(mm_mem$date_observation,"%Y"),mm_mem);


### pg/icp_download
C$s1 <-"icp_dictionaries";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
d_variable <-dbReadTable(C$pg, "d_variable"); 

# LOOP plot ------------------------------------------------------------------
ll <-levels(as.factor(mm_plm$code_plot))
ii <-6;
for(ii in 1:length(ll))
{
  aa <-mm_plm[mm_plm$code_plot%in%ll[ii],];
  aa <-aa[aa$code_location%in%"F",];
  bb <-mm_mem[mm_mem$code_plot%in%ll[ii],];
  bb <-bb[paste(bb$survey_year,bb$code_variable,bb$instrument_seq_nr)%in%paste(aa$survey_year,aa$code_variable,aa$instrument_seq_nr),];
  ### veg period
  cc <-bb[bb$code_variable%in%"AT",];
  dd <-data.frame( # data(goe)
    date=cc$date_observation,
    year=as.integer(cc$survey_year),
    month=as.integer(format(cc$date_observation,"%m")),
    day=as.integer(format(cc$date_observation,"%d")),
    doy=as.integer(format(cc$date_observation,"%j")),
    t=cc$daily_mean
  )
  bart <-"Pinus sylvestris";
  if(ll[ii]%in%c(1207)){bart <-"Fagus sylvatica";}
  if(ll[ii]%in%c(1208,1209)){bart <-"Quercus robur";}
  summary(dd$t)
  vegperiod(dates=dd$date, Tavg=dd$t, 
            start.method="Menzel", end.method="LWF-BROOK90", 
            species=bart, est.prev=3)
  
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
      dd <-dd[order(dd$date_observation),];
      dd <-dd[duplicated(paste0(dd$survey_year,dd$tree_number,dd$code_event_score))==F,];
      ### y
      dd$y <-as.integer(format(dd$date_observation,"%j")); 
      y_nam <-"doy"; 
      y_min <-min(as.integer(format(cc$date_observation,"%j")));
      y_max <-max(as.integer(format(cc$date_observation,"%j")));
      ### x
      dd$x <-dd$survey_year; 
      x_nam <-"year";
      ### plot
      {
        out <-paste(G$d_out2,paste(nam,ll[ii],pp[jj],".png",sep="_"),sep="/");
        png(out, width=160,height=120,units="mm",res=300,pointsize=5); 
        par(cex=1.5,cex.lab=2,pch=16, mar=c(2,5,1,1)); # par()
        ee <-boxplot(dd$y~dd$x,ylim=c(y_min,y_max),ylab=y_nam,xlab=x_nam,pch=16,);
        grid();
        boxplot(dd$y~dd$x,col="green3",add=T);
        text(x=1,y_min+y_min*0.05,"number of trees",col="red3",pos=4);
        text(x=1:length(ee$names),y_min+y_min*0.01,ee$n,col="red3");
        text(x=1,y_max,paste(ll[ii]," - ",nam,"score",pp[jj]),col="blue3",cex=2,pos=4);
        graphics.off();
      }
    }
  }
  ### end ii
}


# SAVE rda --------------------------------------------------------
out <-paste(G$n_script,"mm_plm.rda",sep="-");
save(mm_plm,file = file.path(G$d_out1,out));

out <-paste(G$n_script,"mm_mem.rda",sep="-");
save(mm_mem,file = file.path(G$d_out1,out));

# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
