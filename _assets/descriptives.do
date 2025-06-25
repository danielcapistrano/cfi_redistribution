
// Read file in the sub-folder "data" with name containing "ESS" (only works with single file)
local ess_list : dir "data" file "*ESS*", respectcase
frame create ess_frame
frame change ess_frame
local ess_file : word 1 of `ess_list'
use "data/`ess_file'", clear

// Switch back to the default frame
frame change default

// Read file in the root of sub-folder "data" with name containing "VS" (only works with single file)
local vs_list : dir "data" file "*VS*", respectcase
frame create vs_frame
frame change vs_frame
local vs_file : word 1 of `vs_list'
use "data/`vs_file'", clear

// Switch back to the default frame
frame change default



frame change ess_frame

// Convert essround to a factor variable
egen round = group(essround)

// Calculate the weighted mean and standard error
collapse (mean) mean = gincdif [pw = pspwght], by(round)
gen se = sqrt((r(sd)^2) / _N)

// Create the plot
twoway (rcap mean-2*se mean+2*se round, lcolor(black)) ///
       (scatter mean round, mcolor(black)) ///
       , ytitle("Mean") xtitle("Round") ///
       ylabel(1(1)5) ///
       yscale(range(1 5))

// Switch back to the default frame
frame change default