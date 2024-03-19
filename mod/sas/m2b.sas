
proc import datafile='H:\trigat\dat\dat_twisk_l.csv' out=dat_twisk_l replace; run;

ods html file='H:\trigat\mod\sas\m2b.html' style=HTMLEncore;

proc glimmix data=dat_twisk_l noclprint = 10;
 class USUBJID AVISIT (ref = first) TRT01P (ref = first);
 model AVAL =  TRT01P AVISIT AVISIT*TRT01P  / solution ddfm=satterthwaite;
 random _residual_ / subject=USUBJID type=un;
 lsmeans TRT01P*AVISIT / slicediff=AVISIT diff cl;
run;

ods html close;

