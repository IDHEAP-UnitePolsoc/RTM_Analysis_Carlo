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






* Aggregate to ISCO-major
tostring isco, gen(isco_str)
	gen isco_maj=substr(isco_str,1,1)
	destring isco_maj, replace
	
collapse rti offsh, by(isco_maj)
