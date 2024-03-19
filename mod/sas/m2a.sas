
proc import datafile='H:\trigat\dat\dat_twisk_l.csv' out=dat_twisk_l replace; run;

ods html file='H:\trigat\mod\sas\m2a.html' style=HTMLEncore;

proc glimmix data=dat_twisk_l noclprint = 10;
 class USUBJID AVISIT (ref = first) ABLFL (ref = first) TRT01P (ref = first);
 model AVAL =  TRT01P ABLFL ABLFL*TRT01P  / solution ddfm=satterthwaite;
 random _residual_ / subject=USUBJID type=un;
 lsmeans TRT01P*ABLFL / slicediff=ABLFL diff cl;
run;

ods html close;

