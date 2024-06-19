*** Let's make some data! 
set seed 824
set obs 1000

* Generate covariates
generate x = rnormal()
gen z = rbinomial(1, 0.25)

drawnorm error
generate ystar = 2 + 2*x + 3*z + error

* generate our cutpoints 
gen y=0
replace y = 1 if ystar < -1
replace y = 2 if -1 < ystar & ystar < 1
replace y = 3 if 1 < ystar & ystar < 4
replace y = 4 if 4 < ystar & ystar < 10 
replace y = 5 if 10 < ystar 

oprobit y x i.z 

*** Predicted Probability Plots
margins z, atmeans 

marginsplot, title("Predicted Probability of Y") ///
ytitle(P(Observing Y)) xtitle(Levels of Z) plot1opts(mc(red) lcolor(red)) ///
plot2opts(mc(blue) lcolor(blue)) plot3opts(mc(green)lcolor(green)) ///
plot4opts(mc(black)lcolor(black)) ci1opts(color(red%20)) ci2opts(color(blue%20)) ci3opts(color(green%20)) ci4opts(color(black%20)) recastci(rarea)

*** Contrasts Plots 
margins z, contrast

marginsplot, title("Effect of Change Between Levels") ///
ytitle(Effect on Probability per Category) xtitle(Change from Each Category) plot1opts(mc(black) lcolor(black)) ci1opts(color(black%20)) recastci(rarea)

*** Marginal Effects Plots
oprobit y x i.z 
* Average Marginal Effects
margins, dydx(z) 
marginsplot, title("Average Marginal Effect of Z") ///
ytitle(Marginal Effect of Z on P(Y)) xtitle(Level of Outcome)  ylab(-0.5(0.25)1) ///
plotopts(mc(black)lcolor(black)) ciopts(color(black%20)) recastci(rarea)