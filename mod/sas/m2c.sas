proc import datafile='H:\trigat\dat\dat_twisk_l.csv' out=dat_twisk_l replace; run;

data dat_twisk_l;
 set dat_twisk_l;
 if ABLFL = 'Y' then ABLFLN = 0;
 else if ABLFL = 'N' then ABLFLN = 1;
run;

ods html file='H:\trigat\mod\sas\m2c.html' style=HTMLEncore;

proc glimmix data=dat_twisk_l noclprint = 10;
 class USUBJID TRT01P (ref = first);
 model AVAL =  ABLFLN ABLFLN*TRT01P  / solution ddfm=satterthwaite;
 random _residual_ / subject=USUBJID type=un;
run;

ods html close;

