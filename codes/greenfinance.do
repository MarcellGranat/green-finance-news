use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\nyse_nasdaq.dta", clear


************************************************************** DATA PREP **************************************************************


*replacing NAs with ., converting strings to numeric
replace turnover="." if turnover=="NA"
destring turnover, replace
rename turnover volume
replace price="." if price=="NA"
destring price, replace
replace eps="." if eps=="NA"
destring eps, replace

*converting date
gen yearhelp=substr(date, 1, 4)
gen monthhelp=substr(date, 6, 2)
gen dayhelp=substr(date, 9, 2)
destring yearhelp, replace
destring monthhelp, replace
destring dayhelp, replace
gen date2=mdy(monthhelp, dayhelp, yearhelp) 
format date2 %d
gen year=yofd(date2)
gen month=mofd(date2)
rename date old_date
drop monthhelp yearhelp dayhelp
renam date2 date
order date, before(permno)
drop old_date

*generating returns
sort permno month
by permno: gen ret=(totalreturn[_n+1]/totalreturn)-1 if month[_n+1]==month+1 
by permno: gen retmonth1=ret[_n+1] if month[_n+1]==month+1
by permno: gen retmonth2=ret[_n+2] if month[_n+2]==month+2
by permno: gen retmonth3=ret[_n+3] if month[_n+3]==month+3

*replacing return and total return index outliers
replace ret=. if ret>2
replace retmonth1=. if retmonth1>2
replace retmonth2=. if retmonth2>2
replace retmonth3=. if retmonth3>2
replace ret=. if totalreturn<0.01
replace retmonth1=. if totalreturn<0.01
replace retmonth2=. if totalreturn<0.01
replace retmonth3=. if totalreturn<0.01

*generating size, book to market, turnover, momentum and long-term reversal

gen size=ln(price[_n+1]*outstanding)
gen btm=ln((bookvalue*outstanding)/exp(size))
gen turnover=volume/(outstanding)
gen rev=ret
by permno: gen mom=exp(log(1+ret[_n-1])+log(1+ret[_n-2])+log(1+ret[_n-3])+log(1+ret[_n-4])+log(1+ret[_n-5])+log(1+ret[_n-6])+log(1+ret[_n-7])+log(1+ret[_n-8])+log(1+ret[_n-9])+log(1+ret[_n-10])+log(1+ret[_n-11]))-1
by permno: gen ltrev=exp(log(1+ret[_n-12])+log(1+ret[_n-13])+log(1+ret[_n-14])+log(1+ret[_n-15])+log(1+ret[_n-16])+log(1+ret[_n-17])+log(1+ret[_n-18])+log(1+ret[_n-19])+log(1+ret[_n-20])+log(1+ret[_n-21])+log(1+ret[_n-22])+log(1+ret[_n-23])+log(1+ret[_n-24])+log(1+ret[_n-25])+log(1+ret[_n-26])+log(1+ret[_n-27])+log(1+ret[_n-28])+log(1+ret[_n-29])+log(1+ret[_n-30])+log(1+ret[_n-31])+log(1+ret[_n-32])+log(1+ret[_n-33])+log(1+ret[_n-34])+log(1+ret[_n-35])+log(1+ret[_n-36])+log(1+ret[_n-37])+log(1+ret[_n-38])+log(1+ret[_n-39])+log(1+ret[_n-40])+log(1+ret[_n-41])+log(1+ret[_n-42])+log(1+ret[_n-43])+log(1+ret[_n-44])+log(1+ret[_n-45])+log(1+ret[_n-46])+log(1+ret[_n-47])+log(1+ret[_n-48])+log(1+ret[_n-49])+log(1+ret[_n-50])+log(1+ret[_n-51])+log(1+ret[_n-52])+log(1+ret[_n-53])+log(1+ret[_n-54])+log(1+ret[_n-55])+log(1+ret[_n-56])+log(1+ret[_n-57])+log(1+ret[_n-58])+log(1+ret[_n-59]))-1

*generate market return - value weighted return
bysort month: egen totalmarket=total(exp(size))
gen value=ret*exp(size)/totalmarket
bysort month: egen vwret=total(value)

*merge with risk-free rate
merge n:1 date using "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\riskfree_monthly.dta", keepusing(rf)
drop if _merge==2
drop _merge
drop if date==.

*beta monthly (3 year)
gen retpr=ret-rf
gen marketpr=vwret-rf
ssc install rangestat
rangestat (reg) retpr marketpr, by(permno) interval(month -35 0)
*dropping if number of observation is less than 30
replace b_marketpr=. if reg_nobs<30
rename b_marketpr betamonthly
drop reg_nobs reg_r2 reg_adj_r2 b_cons se_marketpr se_cons
sort permno month

*merge with industry classification
merge n:1 ticker using "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\industry_classification.dta"
sort permno date
tabulate industry, missing
tabulate general_industry_classification, missing
codebook permno
*drop if _merge==2
drop _merge

*merge with monthly gamma
merge n:1 month using "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\monthly_gamma.dta", keepusing(weighted_gamma gamma n_word n_document mean_gamma median_gamma mean_wgamma median_wgamma gamma_lag1 gamma_lag2 gamma_lag3 wgamma_lag1 wgamma_lag2 wgamma_lag3)
sort permno date
*drop if _merge==2
drop _merge
summarize gamma, detail
summarize weighted_gamma, detail

************************************************************** CLEANING **************************************************************

drop if size==.
drop if ret==.
drop if turnover==.
drop if btm==.
drop if betamonthly==.
drop if social==.
drop if governance==.
drop if environment==.
drop if gamma==.
drop if weighted_gamma==.

*delete 10% most illiquid permno per month

bysort month: egen pc10=pctile(turnover), p(10)
bysort month: egen pc20=pctile(turnover), p(20)
bysort month: egen pc30=pctile(turnover), p(30)
bysort month: egen pc40=pctile(turnover), p(40)
bysort month: egen pc50=pctile(turnover), p(50)
bysort month: egen pc60=pctile(turnover), p(60)
bysort month: egen pc70=pctile(turnover), p(70)
bysort month: egen pc80=pctile(turnover), p(80)
bysort month: egen pc90=pctile(turnover), p(90)

gen turnoverdecile=1 if turnover<pc10 & pc10!=. & turnover!=.
replace turnoverdecile=2 if turnover>=pc10 & turnover<=pc20 & pc10!=. & pc20!=. & turnover!=.
replace turnoverdecile=3 if turnover>=pc20 & turnover<=pc30 & pc20!=. & pc30!=. & turnover!=.
replace turnoverdecile=4 if turnover>=pc30 & turnover<=pc40 & pc30!=. & pc40!=. & turnover!=.
replace turnoverdecile=5 if turnover>=pc40 & turnover<=pc50 & pc40!=. & pc50!=. & turnover!=.
replace turnoverdecile=6 if turnover>=pc50 & turnover<=pc60 & pc50!=. & pc60!=. & turnover!=.
replace turnoverdecile=7 if turnover>=pc60 & turnover<=pc70 & pc60!=. & pc70!=. & turnover!=.
replace turnoverdecile=8 if turnover>=pc70 & turnover<=pc80 & pc70!=. & pc80!=. & turnover!=.
replace turnoverdecile=9 if turnover>=pc80 & turnover<=pc90 & pc80!=. & pc90!=. & turnover!=.
replace turnoverdecile=10 if  turnover>=pc90 & pc90!=. & turnover!=.

drop pc10 pc20 pc30 pc40 pc50 pc60 pc70 pc80 pc90

drop if turnoverdecile==1

*delete permno if less than 50 obs/month

bysort month: egen nbr=count(date)
drop if nbr<50
drop if retmonth1==0

kdensity size if price<10, addplot(kdensity size if price>=10) legend(label(1 "Firms with price below 10 USD") label(2 "Firms with price above 10 USD"))
summarize size, detail
drop if price<10
summarize size, detail

hist date, color(gray)

ssc install distplot
distplot date


**********************************************************************************************************************************************************
*************************************************         SUMMARY STATISTICS 		**********************************************************************
**********************************************************************************************************************************************************
********************************************************** DESCRIPTIVE STATISTICS **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if year<2010

kdensity gamma, addplot(kdensity weighted_gamma) legend(label(1 "gamma") label(2 "weighted_gamma"))

hist gamma, color(gray) addplot(hist weighted_gamma) legend(label(1 "gamma") label(2 "weighted_gamma"))

line gamma mean_gamma median_gamma date
line weighted_gamma mean_wgamma median_wgamma date
	
	
*Descriptive statistics
codebook date
codebook permno
summarize month

summarize environment, detail
summarize social, detail
summarize governance, detail


kdensity environment, addplot(kdensity social||kdensity governance) legend(ring(0) pos(2) label(1 "environmental score") label(2 "social score") label(3 "governance score"))

summarize environment social governance, detail

gen t_entry_norm1 = entry_norm1 * 100
label var t_entry_norm1	"Firm entry rate (\%)"

gen t_frac = frac * 100
label var t_frac	"Frac (\%)"

gen t_chHPI = chHPI * 100
label var t_chHPI	"House price index change (\%)"

eststo clear
eststo: quietly estpost summarize	t_entry_norm1 ///
									t_frac lnpop lnpercap lnvc t_chHPI ///
								if ${SAMPLEIF} & (age_buckets == 1) & (pa > 0), detail

esttab using "${OUTPATH}summstat_bds_sy.tex", replace ///
	cells("mean(fmt(2)) sd(fmt(2)) p50(fmt(2)) p25(fmt(2)) p75(fmt(2))") label booktab nonumber nomtitles
eststo clear


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* ORIGINAL, 2010-2020, no gaps ************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************


********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma mean *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma mean ***********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force



********************************************************** FM regressions using observations over weighted gamma mean ************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma <= mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma mean ***************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma > mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* ORIGINAL, retmonth2 2010-2020, no gaps ************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************


********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma mean *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma mean ***********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force



********************************************************** FM regressions using observations over weighted gamma mean ************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma <= mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma mean ***************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma > mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth2 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* ORIGINAL, 2008-2020 but with gaps  *******************************************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************



********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma mean *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= mean_gamma 


sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma mean ***********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > mean_gamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force



********************************************************** FM regressions using observations over weighted gamma mean ************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma <= mean_wgamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma mean ***************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if weighted_gamma > mean_wgamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_social, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force

*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* GAMMA LAG1, 2010-2020 no gaps ***********************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************

********************************************************** FM regressions using observations over gamma mean *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma_lag1 <= mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma mean ***********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma_lag1 > mean_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over weighted gamma mean ************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if wgamma_lag1 <= mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma mean ***************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear


drop if wgamma_lag1 > mean_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma_lag1 <= median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma_lag1 > median_gamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if wgamma_lag1 <= median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if wgamma_lag1 > median_wgamma 
drop if year<2010

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* environment score affect on green stocks (SIC CODE) *************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************

*The interaction term between green dummy and environment is insignificant, showing there is no additional affect of environmental score on green stocks.

********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

tabulate general_industry_classification, missing
tabulate industry, missing

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_environment = green*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_environment green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_environment, lag(12) force
newey b_green, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_environment = green*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_environment green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_environment, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_environment = green*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_environment green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_environment, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_environment = green*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_environment green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_environment, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_environment = green*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_environment green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_environment, lag(12) force
newey b_green, lag(12) force


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* social score affect on green stocks (SIC CODE)  ************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************



********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

tabulate general_industry_classification, missing
tabulate industry, missing

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_social = green*social

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_social, lag(12) force
newey b_green, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_social = green*social

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_social, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_social = green*social

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_social, lag(12) force
newey b_green, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_social = green*social

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_social, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""
gen green_social = green*social

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green_social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green_social, lag(12) force
newey b_green, lag(12) force


*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* green stock affect on return (SIC CODE)  ************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************



********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

tabulate general_industry_classification, missing
tabulate industry, missing

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""


sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green, lag(12) force



********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

gen bank=1 if general_industry_classification=="Bank/Savings & Loan"
replace bank = 0 if bank==.
gen industrial=1 if general_industry_classification=="Industrial"
replace industrial = 0 if industrial==. 
gen insurance=1 if general_industry_classification=="Insurance"
replace insurance = 0 if insurance==. 
gen financial=1 if general_industry_classification=="Other Financial"
replace financial = 0 if financial==. 
gen transportation=1 if general_industry_classification=="Transportation"
replace transportation = 0 if transportation==. 
gen utility=1 if general_industry_classification=="Utility"
replace utility = 0 if utility==. 
gen missing=1 if general_industry_classification==""
replace missing = 0 if missing==. 

drop if industry==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green, lag(12) force

*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* environment score affect on green stocks (GIC CODE)  ************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************



********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

tabulate general_industry_classification, missing
tabulate industry, missing

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""
gen green1_environmental = green1*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1 green1_environmental, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1_environmental, lag(12) force
newey b_green1, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""
gen green1_environmental = green1*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1 green1_environmental, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1_environmental, lag(12) force
newey b_green1, lag(12) force



********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""
gen green1_environmental = green1*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1 green1_environmental, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1_environmental, lag(12) force
newey b_green1, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""
gen green1_environmental = green1*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1 green1_environmental, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1_environmental, lag(12) force
newey b_green1, lag(12) force



********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""
gen green1_environmental = green1*environment

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1 green1_environmental, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1_environmental, lag(12) force
newey b_green1, lag(12) force



*******************************************************************************************************************************************
*******************************************************************************************************************************************
************************************************* green stock affect on return (GIC CODE)  ************************************************
*******************************************************************************************************************************************
*******************************************************************************************************************************************



********************************************************** FM regressions **********************************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

tabulate general_industry_classification, missing
tabulate industry, missing

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1, lag(12) force


********************************************************** FM regressions using observations over gamma median *********************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma <= median_gamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1, lag(12) force



********************************************************** FM regression using observations under gamma median *****************************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if gamma > median_gamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1, lag(12) force


********************************************************** FM regressions using observations over weighted gamma median **********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma <= median_wgamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1, lag(12) force



********************************************************** FM regression using observations under weighted gamma median ***********************************
use "C:\Users\Lenovo ideapad 320S\Desktop\Balazs\munka\BCE\GREEN_FINANCE\data\masterdata_v1.dta", clear

drop if weighted_gamma > median_wgamma 

gen brown1 = 1 if general_industry_classification=="Industrial" | general_industry_classification=="Transportation"
replace brown1 = 0 if brown1==.
gen green1 = 1-brown1

drop if general_industry_classification==""

sort month permno
rangestat (reg) retmonth1 betamonthly size btm mom environment governance social green1, interval(month 0 0)
duplicates drop month, force
tsset month
newey b_betamonthly, lag(12) force
newey b_size, lag(12) force
newey b_btm, lag(12) force
newey b_mom, lag(12) force
newey b_environment, lag(12) force
newey b_social, lag(12) force
newey b_governance, lag(12) force
newey b_green1, lag(12) force