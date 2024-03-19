proc import datafile='H:\trigat\dat\dat_twisk_l.csv' out=dat_twisk_l replace; run;

ods html file='H:\trigat\mod\sas\m1a.html' style=HTMLEncore;

proc glimmix data=dat_twisk_l noclprint = 10;
 where AVISITN ^= 0;
 class USUBJID AVISIT (ref = first) TRT01P (ref = first);
 model AVAL = TRT01P BASE / solution ddfm=satterthwaite;
 random _residual_ / subject=USUBJID type=un;
 lsmeans TRT01P / diff cl;
run;

ods html close;