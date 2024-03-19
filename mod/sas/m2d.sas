proc import datafile='H:\trigat\dat\dat_twisk_l.csv' out=dat_twisk_l replace; run;

data dat_twisk_l;
 set dat_twisk_l;
 if AVISIT = "Week 12" then Week_12 = 1; else Week_12 = 0;
 if AVISIT = "Week 24" then Week_24 = 1; else Week_24 = 0;
run;
run;

ods html file='H:\trigat\mod\sas\m2d.html' style=HTMLEncore;

proc glimmix data=dat_twisk_l noclprint = 10;
 class USUBJID TRT01P (ref = first);
 model AVAL = Week_12 Week_24 Week_12*TRT01P Week_24*TRT01P / solution ddfm=satterthwaite;
 random _residual_ / subject=USUBJID type=un;
run;

ods html close;

