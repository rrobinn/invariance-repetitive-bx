Feb 2020
MNLFA for the RBS-EC was performed using the aMNLFA package in R (https://cran.r-project.org/web/packages/aMNLFA/index.html).  

The authors made slight modifications to the package functions (e.g. allowed user to enter specific calibration sample for aMNLFA_sample.R) which are included in this folder.  

The steps for MNLFA are outlined in <b>aMNLFA_RBS.R</b>. Broadly, they are as follows:  
<b> 1. Define MNLFA object </b>. For each subscale, state which indicators are included, which moderators to  test, etc.  
<b> 2. Generate item plots as a function of potential moderators. </b>  
<b> 3. Draw a calibratoin sample </b>.  The authors selected calibration samples to ensure similar age distributions across both samples (see find_balanced_sample.R).  
<b> 4. Create Mplus input files for mean impact, variance impact, and item-by-item measurement non-invariance (aMNLFA.initial). </b> To allow model to converge, authors manually constrained variance of cohort and sex to sample variance. 
<b>5. Create script that includes (marginally) significant terms </b>. *See aMNLFA_simultaneous*  
<b>6. Trim n.s. terms and create final model </b>. Authors applied BH-family wise error correction to all terms, and included terms significant at p<.05. Manual changes to MPlus code made when necessary.  
<b>7. Generate the final scores </b>. 


 



