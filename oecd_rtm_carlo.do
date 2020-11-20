*************************
* Analyis - OECD RTM Data
*************************

* Carlo Knotz

set more off
version 13.1

cap cd "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Dofiles/Analysis_Carlo"

cap use "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Data/OECD_RTM_2020_Public_Use_Microdata/OECD_RTM_2020_Public_Use_Microdata.dta", clear


* Subsetting (adapt as needed)
******************************
keep q31* s6 id ctrcode weight s3_agegroup q4 s27 q3d s30 q27*

* svyset-ing
************
svyset id [pweight=weight], strata(ctrcode)

* Removing 'can't choose'
for var q31a-q31h: replace X =.a if X==97 | X==-77

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
	
merge m:1 isco using rti_scores.dta
	ta s27 if _merge==1 // all non-matched are missing on ISCO/S27
	drop _merge
	
tabstat rti_score offs_score, by(isco)
	

* Descriptives
**************

* Government measures to cope with digitalization
gr bar, over(q31a, label(angle(15))) // education
	gr bar, over(q31b, label(angle(15))) // vocational training
	gr bar, over(q31c, label(angle(15))) // infrastructure
	gr bar, over(q31d, label(angle(15))) // robot tax
	gr bar, over(q31e, label(angle(15))) // work hour sharing
	gr bar, over(q31f, label(angle(15))) // more generous social safety net
	gr bar, over(q31g, label(angle(15))) // universal basic income
	gr bar, over(q31h, label(angle(15))) // migration
	
	
* Data reduction
****************

pca q31*
	pca q27* // all reduce to one main component	
	
	
* Education
gr bar, over(s6, label(angle(25)))


* Bivariate patterns
********************

* Education
gr hbar edupref1-edupref5, over(s6) stack percent /// weak pos
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(s6) stack percent /// weak pos
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(s6) stack percent /// stronger pos
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(s6) stack percent /// weak neg
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(s6) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(s6) stack percent /// weak neg
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(s6) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(s6) stack percent /// weak pos
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
	
* Age
gr hbar edupref1-edupref5, over(s3_agegroup) stack percent /// unclear
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(s3_agegroup) stack percent /// weak pos (?)
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(s3_agegroup) stack percent /// weak pos (?)
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(s3_agegroup) stack percent /// weak pos
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(s3_agegroup) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(s3_agegroup) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(s3_agegroup) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(s3_agegroup) stack percent /// negative
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

* Occupation
recode s27 (10 99 = .a), copyrest gen(occ)
	label values occ s27
	
gr hbar edupref1-edupref5, over(occ) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(occ) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(occ) stack percent /// stronger among lower
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(occ) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(occ) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(occ) stack percent /// stronger among lower
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(occ) stack percent /// stronger among lower
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(occ) stack percent /// stronger among higher
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
		
		
* Exposure to technology at work
recode s30 (99=.a), copyrest gen(exp)
	label values exp s30

gr hbar edupref1-edupref5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(exp) stack percent /// weak negative (?)
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(exp) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(exp) stack percent /// weak negative (?)
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))


		
* Self-assessed automation risk
gr hbar edupref1-edupref5, over(q27a) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(q27a) stack percent /// none
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(q27a) stack percent /// only for those very strongly affected
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(q27a) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(q27a) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(q27a) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(q27a) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(q27a) stack percent /// affected support (?)
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
		
		

* Long-term risk of insufficient skills
recode q3d (-77 97 =.a), copyrest gen(skillrisk)
	label values skillrisk q3d
	
gr hbar edupref1-edupref5, over(skillrisk) stack percent /// curved
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar vocpref1-vocpref5, over(skillrisk) stack percent /// curved
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar infrapref1-infrapref5, over(skillrisk) stack percent /// curved
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar robtax1-robtax5, over(skillrisk) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar wrkhrs1-wrkhrs5, over(skillrisk) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))
	
gr hbar socprot1-socprot5, over(skillrisk) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar ubi1-ubi5, over(skillrisk) stack percent /// in line: affected support
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))

gr hbar mig1-mig5, over(skillrisk) stack percent /// unclear
	ytitle("Percent") ///
	legend(order(1 "Strongly oppose" 2 "Oppose" ///
		3 "Neither" 4 "Support" 5 "Strongly support"))	
		
		
		
		
* Who is most concerned about long-term skill insufficiencies?
**************************************************************
ta skillrisk, gen(skillrisk)

gr hbar skillrisk1-skillrisk4, over(s6) stack percent /// education
	ytitle("Percent") ///
	legend(order(1 "Not at all concerned" 2 "Not so concerned" /// curved
		3 "Somewhat concerned" 4 "Very concerned"))
		
		
gr hbar skillrisk1-skillrisk4, over(q4) stack percent /// perceived job security
	ytitle("Percent") ///
	legend(order(1 "Not at all concerned" 2 "Not so concerned" /// strong (!)
		3 "Somewhat concerned" 4 "Very concerned")) 
		
		
gr hbar skillrisk1-skillrisk4, over(s3_agegroup) stack percent /// age
	ytitle("Percent") ///
	legend(order(1 "Not at all concerned" 2 "Not so concerned" /// mostly young!
		3 "Somewhat concerned" 4 "Very concerned")) 
		
		
gr hbar skillrisk1-skillrisk4, over(s27) stack percent /// occupation
	ytitle("Percent") ///
	legend(order(1 "Not at all concerned" 2 "Not so concerned" /// strong (!)
		3 "Somewhat concerned" 4 "Very concerned")) 
		
		
* RTI, offsharability & risk perceptions
****************************************

gr hbar rti_score offs_score, over(skillrisk) // stark!!

gr hbar rti_score offs_score, over(q27a) // self-assessed automation risk

gr hbar rti_score offs_score, over(exp) // exposure to technology at work
	
	
* Country-variation in policy preferences
*****************************************

gr bar, over(q31a, label(angle(15))) by(ctrcode) // education
	gr bar, over(q31b, label(angle(15))) by(ctrcode) // voc training
	gr bar, over(q31c, label(angle(15))) by(ctrcode) // infrastructure
	gr bar, over(q31d, label(angle(15))) by(ctrcode) // robot tax; looks interesting
	gr bar, over(q31e, label(angle(15))) by(ctrcode) // shared hours
	gr bar, over(q31f, label(angle(15))) by(ctrcode) // benefits & services
	gr bar, over(q31g, label(angle(15))) by(ctrcode) // UBI - looks very similar!
	gr bar, over(q31h, label(angle(15))) by(ctrcode) // skilled migration; looks interesting
	
