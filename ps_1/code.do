cd ../../../all/study/NES/module_04/econometrics/problem_sets/ps_1 // to change w.d.

ls 						// to see content of w.d.

log using mylog.log		// to log history of state commands

use "Training Metrics.dta"

ds						// lists variable names of the dataset


/* Question 1 */
drop if in_model_2 != 1     // consider rows only with this condition for the 1st Question


tabulate metrics_assigned       // see the distribution
summarize metrics_assigned      // to see basic statistics


/* 
birthpcapital income age dedu4 out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden - "all other available individual characteristics obtained from administrative data"

metrics_selected - "The controls include Metrics Chosen (how much they were interested beforehand (before random assignment))"

gender_encode land fbr fsp - from the 1st Section
*/

local same_controls metrics_selected gender_encode land fbr fsp

// to get the "Birth in political capitals" column
reg birthpcapital metrics_assigned income age dedu4 out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m1
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize birthpcapital
estadd local mymean = round(r(mean), 0.001)


// to get the "Income" column
reg income metrics_assigned birthpcapital income age dedu4 out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m2
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize income
estadd local mymean = round(r(mean), 0.001)


// to get the "Age" column
reg age metrics_assigned birthpcapital income dedu4 out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m3
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize age
estadd local mymean = round(r(mean), 0.001)


// to get the "Education" column
reg dedu4 metrics_assigned birthpcapital income age out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m4
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize dedu4
estadd local mymean = round(r(mean), 0.001)


// to get the "Visited Foreign Country" column
reg out metrics_assigned birthpcapital income age dedu4 pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m5
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize out
estadd local mymean = round(r(mean), 0.001)


// to get the "PAS" column
reg pas metrics_assigned birthpcapital income age dedu4 out psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m6
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize pas
estadd local mymean = round(r(mean), 0.001)


// to get the "PSP" column
reg psp metrics_assigned birthpcapital income age dedu4 out pas othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m7
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize birthpcapital
estadd local mymean = round(r(mean), 0.001)


// to get the "Other groups" column
reg othergroups metrics_assigned birthpcapital income age dedu4 out pas psp writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m8
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize othergroups
estadd local mymean = round(r(mean), 0.001)


// to get the "Pre-Treatment Written Assessment" column
reg writtenmarks metrics_assigned birthpcapital income age dedu4 out pas psp othergroups writtenmarks interviewmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m9
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize writtenmarks
estadd local mymean = round(r(mean), 0.001)


// to get the "Pre-Treatment Interview Assessment" column
reg interviewmarks metrics_assigned birthpcapital income age dedu4 out pas psp othergroups writtenmarks PreTreatmentQuantitaiveEviden $same_controls, vce(cluster participantid_in_session)
estimates store m10
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize interviewmarks
estadd local mymean = round(r(mean), 0.001)


// to get the "Pre-Treatment Mathematics Assessment" column
reg PreTreatmentQuantitaiveEviden metrics_assigned birthpcapital income age dedu4 out pas psp othergroups writtenmarks interviewmarks $same_controls, vce(cluster participantid_in_session)
estimates store m11
estadd local controls Yes
estadd local N_ = round(e(N), 1)
summarize PreTreatmentQuantitaiveEviden
estadd local mymean = round(r(mean), 0.001)


local title "Deputy Minister - Balance of Treatment on Individual Characteristics"

local notes "Robust standard errors appear in brackets (clustered at the individual level). Metrics assigned is a dummy variable that switches on when a causal inference book is randomly assigned to participants. The causal inference book is randomly assigned conditional on the book being chosen. The controls include Metrics Chosen (a dummy variable that switches on when causal inference book is chosen by the participants), and all other available individual characteristics obtained from administrative data (i.e. all remaining column dependent variable except the dependent variable used in the respective column).  \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)"


esttab m* using task1.tex, replace b(3) se(3) mtitle("Birth in political capitals" "Income" "Age" "Education" "Visited foreign country" "PAS" "PSP" "Other groups" "Pre-Treatment Written Assignment" "Pre-Treatment Interview Assessment" "Pre-Treatment Mathematics Assessment")  star(* 0.10 ** 0.05 *** 0.01) varlabels(metrics_assigned "Metrics Assigned") keep(metrics_assigned) stats(controls mymean  N_, labels("Controls" "Mean Dep. Var." "Observations"))  title(`title') nonotes postfoot(\hline\hline \end{tabular} \medskip \caption*{\footnotesize  `notes'}\end{table})  sfmt(%12.2fc) tex



/* Question 3 */
clear
eststo clear
estimates clear

use "Training Metrics.dta"


// Found a better way to do wast amounts of regressions via loops

local columns LetterNo1 AmountforDeworming LetterNumber2 OrphanageRenovation LetterNumber3 SchoolRenovation

local control metrics_selected writtenmarks interviewmarks gender_encode birthpcapital land income age dedu4 out pas psp fbr fsp othersgroup


local i = 1
foreach col in $columns {
    regress $col metrics_assigned $control if in_model_2 == 1 , vce(cluster participantid_in_session)
    estimates store m$i
    estadd local control Yes
    estadd local N_ = round(e(N), 1)
    estadd local r2_ = round(e(r2), 0.1)
    summarize $col if metrics_assigned == 0 & in_model_2 == 0
    estadd local mymean = round(r(mean), 0.1) 	

    local i = $i + 1
}


local title "Effect of Metrics Training on Policy"

local notes "Robust standard errors appear in brackets (clustered at the individual level). The dependent variables are letters sent and funds recommended to government division in Pakistan (in Pakistani Rupees) for budget allocation for Deworming, Orphanage and School renovations, respectively. Metrics Assigned is a dummy variable that switches on when a causal inference book is randomly assigned to participants. Consistent with all our earlier regressions, we aways control for the metrics chosen. The causal inference book, Mastering Metrics, is randomly assigned conditional on the book being chosen. The estimations obtained from OLS regressions include the following controls: metrics chosen, written test scores, interview test scores, gender, birth in political capitals, asset ownership, income before joining public service, age, education, foreign visits and occupational group dummies.  \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)"


esttab m* using task3.tex, replace b(3) se(3) mtitle("Letter Sent" "Funds Recommended" "Letter Sent " "Funds Recommended" "Letter Sent" "Funds Recommended")  star(* 0.10 ** 0.05 *** 0.01) varlabels(metrics_assigned "Metrics Assigned") keep(metrics_assigned) stats(controls  N_ R_sqr mymean, labels("Individual Controls" "Observations" "R-squared" "Mean of dep. var. (placebo)"))  title(`title') nonotes postfoot(\hline\hline \end{tabular} \medskip \caption*{\footnotesize  `notes'}\end{table})  sfmt(%12.2fc) tex



/* Question 4 */
use "Training Metrics.dta", clear

hist Prior_Belief if metrics_selected == 0, discrete barwidth(1) fintensity(90) color(ltblue) title(" Histogram for Prior Beliefs with Metrics Chosen = 0") subtitle("Only for 1981") name("histogram")

