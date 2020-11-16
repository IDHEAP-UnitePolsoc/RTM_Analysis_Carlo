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

* Descriptives
**************

* Government measures to cope with digitalization
ta q31a // education
	ta q31b // vocational training
	ta q31c // infrastructure
	ta q31d // robot tax
	ta q31e // work hour sharing
	ta q31f // more generous social safety net
	ta q31g // universal basic income
	ta q31h // migration
	
	
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
		
		
gr hbar skillrisk1-skillrisk4, over(s27) stack percent /// perceived job security
	ytitle("Percent") ///
	legend(order(1 "Not at all concerned" 2 "Not so concerned" /// strong (!)
		3 "Somewhat concerned" 4 "Very concerned")) 
