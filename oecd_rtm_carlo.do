************************
* OECD RTM Data Analysis
************************

set more off
version 13.1

cap cd "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Dofiles/Analysis_Carlo"

cap use "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Data/OECD_RTM_2020_Public_Use_Microdata/OECD_RTM_2020_Public_Use_Microdata.dta", clear


* Subsetting (adapt as needed)
******************************
keep q31* id ctrcode weight q4 s27 q3d s30 q27* q11* q12* q13 q14 ///
	s2 s3_age s6 s8_dec s25 s35 

* svyset-ing
************
svyset id [pweight=weight], strata(ctrcode)

* Removing 'can't choose' from concern vars
for var q11a-q12f q13 q14: replace X =.a if X==97 | X==-77

* Same for others (demographics)
recode s2 (3 = .a), copyrest gen(gender)
	label values gender s2
	
rename s3_age age
	rename s6 edu
	rename s8_dec inc
	
recode s25 (6 7 = 6) (9 10 11 12 = 7), copyrest gen(empstat)
	label define empstat 1 "Employee working full-time  (≥30 hrs/week)" ///
		2 "Self-employed working full-time (≥30 hrs/week)" ///
		3 "Employee working part-time (<30 hrs/week)" ///
		4 "Self-employed working part-time (<30 hrs/week)" ///
		5 "Unemployed" 6 "Student or apprentice/intern" ///
		7 "Other" 8 "Retired"
	label values empstat empstat
	
rename s35 covid_inc

* Dummy coding of main DVs
**************************
ta q31a, gen(edupref)
	ta q31b, gen(vocpref)
	ta q31c, gen(infrapref)
	ta q31d, gen(robtax)
	ta q31e, gen(wrkhrs)
	ta q31f, gen(socprot)	
	ta q31g, gen(ubi)
	ta q31h, gen(mig)
	
* RTI & offsharability scores
*****************************
gen isco=s27
	replace isco=. if isco==10 | isco==99
	label define isco 1 "Manager or senior official" ///
		2 "Professional" 3 "Technical or associate prof." ///
		4 "Clerical support worker" 5 "Service or sales worker" ///
		6 "Skilled agricultural worker" 7 "Craft or trade worker" ///
		8 "Plant or machine operator" 9 "Elementary occ. (e.g. cleaner)"
	label values isco isco
	
	
merge m:1 isco using rti_scores.dta
	ta s27 if _merge==1 // all non-matched are missing on ISCO/S27
	drop _merge
	
tabstat rti_score offs_score, by(isco)


* Government measures to cope with digitalization
gr bar, over(q31a, label(angle(15))) /// education
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f))
		gr export edu.pdf, replace
	gr bar, over(q31b, label(angle(15))) /// vocational training
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export voc.pdf, replace
	gr bar, over(q31c, label(angle(15))) /// infrastructure
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export infra.pdf, replace
	gr bar, over(q31d, label(angle(15))) /// robot tax
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export robot.pdf, replace
	gr bar, over(q31e, label(angle(15))) /// work hour sharing
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export hrs.pdf, replace
	gr bar, over(q31f, label(angle(15))) /// more generous social safety net
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export prot.pdf, replace
	gr bar, over(q31g, label(angle(15))) /// universal basic income
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export ubi.pdf, replace
	gr bar, over(q31h, label(angle(15))) /// migration
		ytitle("Percent") ylabel(0(10)50) blabel(bar, format(%9.1f)) 
		gr export mig.pdf, replace
		
		
* Factor analysis

for var q31a-q31h: replace X =.a if X==97 | X==-77 // Removing 'can't choose'

factor q31a-q31h, pcf 
	rotate
	
mat A = e(r_L)
	mat colnames A = Factor~1 Factor~2
	
esttab matrix(A)
	
esttab matrix(A, fmt(3)) using factor.tex, replace nomtitles ///
		varlabels(q31a "Education and vocational training" q31b "Re-training" ///
			q31c "Digital infrastructure" q31d "Robot taxes" q31e "Work-sharing" ///
			q31f "Social protection" q31g "Universal basic income" ///
			q31h "Skilled migration") varwidth(25) ///
		order(q31a q31b q31c q31h) booktabs b(3) width(0.8\hsize)  
	
	
* Generating outcome variables
factor q31a-q31g, pcf // note: without migration
	rotate
	predict passive active		
	
hist passive
	hist active
	
recode q27a (-77 97 =.a), copyrest gen(skillrisk) // self-assessed risk
	label values skillrisk q27a
	
tabstat rti_score, by(s27) // closer look at occupations
	
* Output
gr hbar rti_score, over(isco, sort(rti_score) descending) ///
	ytitle("Routine-task intensity score") ///
	blabel(bar, format(%9.2f)) ylabel(-1.5(.5)2.5)
	gr export rti.pdf, replace
	
* Relation RTI to policy preferences
gen pos=3
	replace pos=9 if isco==4

preserve
collapse passive rti_score pos, by(isco)
	gr tw (scatter passive rti_score, mlabel(isco) ms(+) mlabv(pos)) ///
		(lfit passive rti_score, lp(dash)), ///
		legend(off) xtitle("Routine-task intensity score") ///
		ytitle("Support for passive measures")
		gr export rti_passive.pdf, replace
restore		
		
replace pos=9 if isco==5	
preserve
collapse active rti_score pos, by(isco)
	gr tw (scatter active rti_score, mlabel(isco) ms(+) mlabv(pos)) ///
		(lfit active rti_score, lp(dash)), ///
		legend(off) xtitle("Routine-task intensity score") ///
		ytitle("Support for active measures")
		gr export rti_active.pdf, replace
restore
	drop pos				
	
	
* By-country
************

sort ctrcode
	encode ctrcode, gen(cntry)

putexcel set cntryeffects.xlsx, modify
	putexcel A1 = "Country"
	putexcel B1 = "beta_passive"
	putexcel C1 = "rho_passive"
	putexcel D1 = "beta_active"
	putexcel E1 = "rho_active"	
	
qui su cntry, meanonly // stores r(max)	
forvalues i = 1/`r(max)'{
	local c=`i'+1 // cell number
	di `c'
	
	qui levelsof ctrcode if cntry==`i', local(levl)
	di `levl' // country label
	putexcel A`c' = `levl'
	
	preserve
	collapse passive active rti_score if cntry==`i', by(isco)
	
	qui reg passive rti_score // regression, passive
		putexcel B`c' = _b[rti_score]
	
	qui corr passive rti_score // correlate, passive
		local rho = r(rho)
		putexcel C`c'= `rho'
	
	qui reg active rti_score // regression, active
		putexcel D`c' = _b[rti_score]
	
	qui corr active rti_score // correlate, active
		local rho = r(rho)
		putexcel E`c'= `rho'
		
	gr tw (scatter passive rti_score, mlabel(isco) ms(+)) ///
		(lfit passive rti_score, lp(dash)), ///
		legend(off) xtitle("Routine-task intensity score") ///
		ytitle("Support for passive measures") ///
		title(`levl')
		gr export "compa_graphs/rti_passive_`i'.pdf", replace	
		
	gr tw (scatter active rti_score, mlabel(isco) ms(+)) ///
		(lfit active rti_score, lp(dash)), ///
		legend(off) xtitle("Routine-task intensity score") ///
		ytitle("Support for active measures") ///
		title(`levl')
		gr export "compa_graphs/rti_active_`i'.pdf", replace
	
	restore
}


* Overall support by country
****************************
gr bar active, over(ctrcode, sort(active) descending ///
	label(angle(15) labs(vsmall)))
	
gr bar passive, over(ctrcode, sort(passive) descending ///
	label(angle(15) labs(vsmall)))
	
	
* Variation in support, by ISCO & country
*****************************************

gr hbar passive if ctrcode=="AUT", over(isco, sort(passive) desc)

gr hbar active if ctrcode=="KOR", over(isco, sort(active) desc)


* Correlation analysis - controls
*********************************

pwcorr rti_score gender age edu inc covid_inc empstat // looks ok


* Individual-level models of preferences
****************************************
svyset, clear // check how weighting affects results first

* Passive measures
xtmixed passive || ctrcode:, mle

xtmixed passive rti_score || ctrcode:, mle // baseline

xtmixed passive rti_score i.gender age inc || ctrcode:, mle // basic demographics

xtmixed passive rti_score i.gender age inc i.covid_inc i.empstat || ctrcode:, mle // extended demographics

xtmixed passive rti_score i.gender age inc i.covid_inc i.empstat i.edu || ctrcode:, mle // adding education

* Active measures
xtmixed active || ctrcode:, mle

xtmixed active rti_score || ctrcode:, mle // baseline

xtmixed active rti_score i.gender age inc || ctrcode:, mle // basic demographics

xtmixed active rti_score i.gender age inc i.covid_inc i.empstat || ctrcode:, mle // extended demographics

xtmixed active rti_score i.gender age inc i.covid_inc i.empstat i.edu || ctrcode:, mle // adding education


* By country
************
gen k = . in 1/24
	gen b = . in 1/24
	gen tval = . in 1/24
	gen pval = . in 1/24
	gen ul = . in 1/24
	gen ll = . in 1/24
	gen omega = . in 1/24


{ // graph for RTI betas on active by country
qui su cntry, meanonly // stores r(max)	
forvalues i = 1/`r(max)'{	
	qui reg active rti_score i.gender age inc i.covid_inc i.empstat if cntry==`i' [pweight=weight], vce(robust)
	replace k = `i' in `i'
	replace b = _b[rti_score] in `i'
	replace ul = _b[rti_score] + 1.96*_se[rti_score] in `i'
	replace ll = _b[rti_score] - 1.96*_se[rti_score] in `i'
	replace tval = _b[rti_score]/_se[rti_score] in `i'
		local t = _b[rti_score]/_se[rti_score]
		local df = e(df_r)	
	replace pval = 2*ttail(`df',abs(`t')) in `i'
	replace omega = (_se[rti_score])^2
}
	label values k cntry
	
gr tw (bar b k) (rcap ul ll k) , ///
	xlabel(1 "AUT" 2 "BEL" 3 "CAN" 4 "CHE" 5 "CHL" 6 "DEU" 7 "DNK" 8 "ESP" ///
		9 "EST" 10 "FIN" 11 "FRA" 12 "GRC" 13 "IRL" 14 "ITA" 15 "KOR" 16 "LTU" ///
		17 "MEX" 18 "NLD" 19 "NOR" 20 "POL" 21 "PRT" 22 "SVN" 23 "TUR" 24 "USA", ///
		labs(vsmall)) ///
	ytitle("{&beta}{sup:^} RTI Score") xtitle("") legend(off) ///
	note("Spikes indicate 95% confidence intervals.", size(vsmall))
	gr export beta_rti-active_cn.pdf, replace
	
* Export to R
preserve
	keep k b tval pval ul ll
	decode k, gen(country)
		drop k
	compress
	export delimited using betas_active.csv, replace

restore	
}	

{ // graph for RTI beta on passive by country
qui su cntry, meanonly // stores r(max)	
forvalues i = 1/`r(max)'{	
	qui reg passive rti_score i.gender age inc i.covid_inc i.empstat if cntry==`i' [pweight=weight], vce(robust)
	replace k = `i' in `i'
	replace b = _b[rti_score] in `i'
	replace ul = _b[rti_score] + 1.96*_se[rti_score] in `i'
	replace ll = _b[rti_score] - 1.96*_se[rti_score] in `i'
	replace tval = _b[rti_score]/_se[rti_score] in `i'
		local t = _b[rti_score]/_se[rti_score]
		local df = e(df_r)	
	replace pval = 2*ttail(`df',abs(`t')) in `i'
	replace omega = (_se[rti_score])^2
}
	label values k cntry
	
gr tw (bar b k) (rcap ul ll k) , ///
	xlabel(1 "AUT" 2 "BEL" 3 "CAN" 4 "CHE" 5 "CHL" 6 "DEU" 7 "DNK" 8 "ESP" ///
		9 "EST" 10 "FIN" 11 "FRA" 12 "GRC" 13 "IRL" 14 "ITA" 15 "KOR" 16 "LTU" ///
		17 "MEX" 18 "NLD" 19 "NOR" 20 "POL" 21 "PRT" 22 "SVN" 23 "TUR" 24 "USA", ///
		labs(vsmall)) ///
	ytitle("{&beta}{sup:^} RTI Score") xtitle("") legend(off) ///
	note("Spikes indicate 95% confidence intervals.", size(vsmall))
	gr export beta_rti-passive_cn.pdf, replace	
	
* Export to R
preserve
	keep k b tval pval ul ll omega
	decode k, gen(country)
		drop k
	compress
	export delimited using betas_passive.csv, replace

restore	
	drop k b tval pval ul ll omega
} 


* Interactive models
********************

* Passive
xtmixed passive c.rti_score##c.age i.gender inc i.covid_inc i.empstat || ctrcode:, mle
	margins, dydx(rti_score) at(age=(20(5)65))
	marginsplot
	
xtmixed passive c.rti_score##c.inc i.gender age i.covid_inc i.empstat || ctrcode:, mle
	margins, dydx(rti_score) over(inc) // Thewissen/Rueda
	marginsplot
	
* Active
xtmixed active c.rti_score##c.age i.gender inc i.covid_inc i.empstat || ctrcode:, mle
	margins, dydx(rti_score) at(age=(20(5)65))
	marginsplot
	
xtmixed active c.rti_score##c.inc i.gender age i.covid_inc i.empstat || ctrcode:, mle
	margins, dydx(rti_score) over(inc) // Thewissen/Rueda
	marginsplot

	
	
reg passive rti_score if ctrcode=="NOR"	
	local omega = (_se[rti_score])^2
	di `omega'
	
	
	
	
* Concerns over access to social protection, by country
*******************************************************

gr bar q11a, over(ctrcode, sort(q11a) label(angle(15) labs(vsmall))) // family
	gr bar q11b, over(ctrcode, sort(q11b) label(angle(15) labs(vsmall))) // education
	gr bar q11c, over(ctrcode, sort(q11c) label(angle(15) labs(vsmall))) // employment
	gr bar q11d, over(ctrcode, sort(q11d) label(angle(15) labs(vsmall))) // housing
	gr bar q11e, over(ctrcode, sort(q11e) label(angle(15) labs(vsmall))) // health
	gr bar q11f, over(ctrcode, sort(q11f) label(angle(15) labs(vsmall))) // incapacity
	gr bar q11g, over(ctrcode, sort(q11g) label(angle(15) labs(vsmall))) // long-term care
	gr bar q11h, over(ctrcode, sort(q11h) label(angle(15) labs(vsmall))) // public safety
	
gr bar q12a, over(ctrcode, sort(q12a) label(angle(15) labs(vsmall))) // unemployment benefits
	gr bar q12b, over(ctrcode, sort(q12b) label(angle(15) labs(vsmall))) // illness/disability
	gr bar q12c, over(ctrcode, sort(q12c) label(angle(15) labs(vsmall))) // parental benefits
	gr bar q12d, over(ctrcode, sort(q12d) label(angle(15) labs(vsmall))) // elderly care
	gr bar q12e, over(ctrcode, sort(q12e) label(angle(15) labs(vsmall))) // retirement pensions
	gr bar q12f, over(ctrcode, sort(q12f) label(angle(15) labs(vsmall))) // widow(er)hood
	
egen accserv = rowmean(q11a-q11h) // overall Q11
	gr bar accserv, over(ctrcode, sort(accserv) label(angle(15) labs(vsmall)))
	
egen accben = rowmean(q12a-q12f) // overall Q12
	gr bar accben, over(ctrcode, sort(accben) label(angle(15) labs(vsmall)))
	
egen access = rowmean(accserv accben)
	gr bar access, over(ctrcode, sort(access) label(angle(15) labs(vsmall)))
