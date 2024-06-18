*** Let's make some data! ***
set seed 33
set obs 1000

gen y = rbinomial(1, 0.33)

* Generate our independent variables 
gen x = rnormal(0,1)
gen z = rbinomial(1, 0.5)

* Generate an error term
drawnorm error

* Generate our dependent variable 
gen y_star = 1 + 2*x + .5*z + error

gen y_logit = 0
replace y_logit = 1 if y_star>0

/* It's important to note that our z-term must have a 
i.z in order for Stata to recognize the variable is
a factor variable (cf. as.factor(z)) */

logit y_logit x i.z

*** Predicted Probabilites ***

* Predicted Probabilites for each individual observation 
predict y_hat // must happen after the regression model (cf. predictions)

* Average Predicted Probabilities conditional on a particular variable 
margins z, atmeans over(y) // N.B. we cannot get pred. probs. for x this way in Stata 
//(cf. avg_predictions(model, variables = 'z'))

* Predictive Margins (cf. avg_predictions(model, variables = 'z', type = "response"))
margins z
/* The margins z command can be replicated using the predict function values
mean y_hat if z == 0 
mean y_hat if z == 1 
should generate the same values as the margins z command 
 */ 

* Once the margins command has been done, we can use the marginsplot command
marginsplot, title("Predicted Probability of Y at Mean") ///
ytitle(P(Observing Y)) xtitle(Levels of Z) plotopts(lcolor(black) mcolor(black)) ciopts(color(black%20)) recastci(rarea)

** We can also look at the predicted probability of z conditional on levels of x

* Let's look at the range of our variable x: 
summarize x 
* x_min = -2.78 and x_max = 3.45
 
margins z, at(x=(-3(0.5)3.5)) // note that we set x_min at -3 and x_max 3.5 
                             // going up in increments of 0.5 
marginsplot, title("Predicted Probability of Y") ///
ytitle(P(Observing Y)) xtitle(Levels of X) plot1opts(lcolor(red) mcolor(red)) ci1opts(color(red%20)) recastci(rarea) plot2opts(lcolor(blue) mcolor(blue)) ci2opts(color(blue%20))

** Contrasts
* We can also see if the effect of a change in one value to another in a factor 
* variable is statistically signficant such as below 

margins z, contrast // cf. avg_comparisons

*** Marginal Effects ***

* Using the same regression
logit y_logit x i.z

*Average Marginal Effects of z (cf. avg_slopes)
margins, dydx(z)
marginsplot, title("Average Marginal Effect of Z on Y") ///
ytitle(Marginal Effect on Y) xtitle(Change in Z) plotopts(lcolor(black) mcolor(black)) ciopts(color(black)) ylab(0(0.05)0.25)

* Average Marginal Effects for all variables 
margins, dydx(*) over(y) 
marginsplot, title("Average Marginal Effect") ///
ytitle(Marginal Effect) xtitle(Values of Y) plot1opts(lcolor(red) mcolor(red)) ci1opts(color(red%20)) recastci(rarea) plot2opts(lcolor(blue) mcolor(blue)) ci2opts(color(blue%20))

*** Interaction Models ***

* Let's generate a new model
gen y_star1 = 1 + 2*x + .5*z + 2*x*.5*z + error

gen y_logit1 = 0
replace y_logit1 = 1 if y_star1>0

* Run our new regression
logit y_logit1 c.x##i.z // N.B. the use of c.x is essential as c denotes continous

* Let's get our marginal effects of x on y, conditional on z
margins, dydx(x) at(z=0) at(z=1)
marginsplot, title("Marginal Effect of X on Y") ///
ytitle(Marginal Effect) xtitle(Levels of Z) plotopts(lcolor(black) mcolor(black)) ciopts(color(black%20)) recastci(rarea)