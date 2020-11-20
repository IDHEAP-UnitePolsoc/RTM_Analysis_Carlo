**********************
* Routine-task indices
**********************

* Carlo Knotz

clear
version 13.1
set more off

cap cd "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Dofiles/Analysis_Carlo"

* From Mahutga et al. (2019, IJCS)
**********************************

cap use "/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/OECD_module/Data/RTI_OFFSH/lis_rti_offs.dta"


keep dname isco_88_r rti_score offs_score // Subset
	collapse rti_score offs_score, by(isco_88_r) // collapse to ISCO-88 Level 2
	drop if isco_88_r==. // remove missings
	

tostring isco_88_r, gen(isco_str) // Aggregate to ISCO-major
	gen isco_maj=substr(isco_str,1,1)
	destring isco_maj, replace
	collapse rti_score offs_score, by(isco_maj)
	
* Saving
sort isco_maj
	compress
	save rti_scores.dta, replace
