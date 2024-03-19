* analysis of covariance
use "\\vumc.nl\home$\store4ever\JWR.Twisk\JWR.Twisk\Desktop\artikelen\artikel trial analyses\trial paper\final dataset.dta", clear
rename syst0 baseline
reshape long syst, i(id) j(time)
mixed syst treatment baseline || id: // model 1a

mixed syst treatment##i.time baseline || id: // model 1b
mixed syst treatment##ib2.time baseline || id: // model 1b

* repeated measures

use "\\vumc.nl\home$\store4ever\JWR.Twisk\JWR.Twisk\Desktop\artikelen\artikel trial analyses\trial paper\final dataset.dta", clear
reshape long syst, i(id) j(time)
gen time_rec = time
recode time_rec (0=0) (1=1) (2=1)
mixed syst treatment##i.time_rec || id: // model 2a

mixed syst treatment##ib1.time || id: // model 2b
mixed syst treatment##ib2.time || id: // model 2b

gen int_time_rec_tr = time_rec*treatment
mixed syst time_rec int_time_rec_tr || id: // model 2c

gen dummytime1 = 0
replace dummytime1 = 1 if time==1
gen dummytime2 = 0
replace dummytime2 = 1 if time==2
gen int_time1_tr = dummytime1*treatment  
gen int_time2_tr = dummytime2*treatment
mixed syst dummytime1 dummytime2 int_time1_tr int_time2_tr || id: // model 2d


* analysis of changes
use "\\vumc.nl\home$\store4ever\JWR.Twisk\JWR.Twisk\Desktop\artikelen\artikel trial analyses\trial paper\final dataset.dta", clear
gen sys_change1 = syst1 - syst0
gen sys_change2 = syst2 - syst0
reshape long sys_change, i(id) j(time)
mixed sys_change treatment || id: // model 3a
mixed sys_change treatment syst0 || id: // model 3b

mixed sys_change treatment##i.time || id: // model 3c
mixed sys_change treatment##ib2.time || id: // model 3c
mixed sys_change treatment##i.time syst0 || id: // model 3d
mixed sys_change treatment##ib2.time syst0 || id: // model 3d
