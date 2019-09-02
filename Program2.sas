/* Graded Assignment */

proc import datafile='/home/varunkaru0/T10_Logistic Regression Models/Datasets/Patient Data.csv'
dbms=csv
out=patient replace;
run;

/* Building Logistic Regression Model */

/* 1. Problem Statement:
Identify the factors that influence the heartfailure (Yes/No - 1/0) 
*/

/* 2. Solution Approach:
Create logistic regression model of form 
log(p/1-p)  f(CHOLESTEROL	BMI	AGE	SEX	FAMILYHISTORY	SMOKERLAST5YRS	EXERCISEMINPERWEEK)
where p = probability of causing heart failure 
*/

/* 3. Solution Implementation: */

/* 3(a) Data Exploration */

proc contents data = patient;
run;

/* 3(a) Step 1: Transforming Qualitative to Quantitaive Data */
/* Variables are : SEX	FAMILYHISTORY SMOKERLAST5YRS */

Data patient_1 replace;
set patient;
if sex = 'F' then gender = 0;
else gender = 1;
if FAMILYHISTORY = 'N' then famhist = 0;
else famhist = 1;
if SMOKERLAST5YRS = 'N' then smoke = 0;
else smoke = 1;
run;

Data patient_2(rename = (HEARTFAILURE = y) drop = SEX FAMILYHISTORY SMOKERLAST5YRS) replace;
set patient_1;
run;

/* 3(a) Step 2: Generate Required Derived variables - Not Required*/

/* 3(a) Step 3: Generate summary statistics*/

/* No Missing Values */
proc means data = patient_2 ;
/* class y; */
run;

/* No Outliers */
proc means data = patient_2;
run;

/* Freq Distribution */
proc freq data = patient_2;
run;

/* 3(a) Step 4: Cross Tabulation*/
proc freq data = patient_2 ;                                                     ;
tables y*(AVGHEARTBEATSPERMIN	PALPITATIONSPERDAY	CHOLESTEROL	BMI	 AGE gender famhist smoke EXERCISEMINPERWEEK)/nocol norow nocum;
run;

/* 3(a) Step 5: Graphical analysis*/

PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR AVGHEARTBEATSPERMIN;
QQPLOT AVGHEARTBEATSPERMIN /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN; 
/* AVGHEARTBEATSPERMIN is +Skewed 0.5
About 99% of data is below 141 */

PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR PALPITATIONSPERDAY;
QQPLOT PALPITATIONSPERDAY /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN;
/* PALPITATIONSPERDAY skewness 0.01423684  */

PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR CHOLESTEROL;
QQPLOT CHOLESTEROL /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN; 
/* CHOLESTEROL skewness -0.0119801  */

PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR BMI;
QQPLOT BMI /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN; 
/* BMI skewness 0.00299454  */


PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR EXERCISEMINPERWEEK;
QQPLOT EXERCISEMINPERWEEK /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN; 
/* EXERCISEMINPERWEEK skewness 0.06491171 */

PROC UNIVARIATE DATA=patient_2 NORMAL PLOT;
VAR AGE;
QQPLOT AGE /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
RUN; 
/* AGE skewness -0.0057656 */


/* Proportion of Data for heartfailure */

proc freq data = patient_2;
run;

/* 
y	Frequency	Percent	Cumulative
Frequency	Cumulative
Percent
0	9012	83.44	9012	83.44
1	1788	16.56	10800	100.00
*/

/* Checking collinearity */

proc logistic data=patient_2 descending;
model y = CHOLESTEROL	BMI	AGE	gender	famhist	smoke	EXERCISEMINPERWEEK
/selection=forward stop=10 include=2 sle=0.01;
run;

proc reg data=patient_2;
model y= CHOLESTEROL	BMI	AGE	gender	famhist	smoke	EXERCISEMINPERWEEK/ vif; 
run;

/* VIF is very less hence there exists no collinearity*/
/*
Parameter Estimates
Variable	DF	Parameter
Estimate	Standard
Error	t Value	Pr > |t|	Variance
Inflation
Intercept	1	-0.13901	0.03611	-3.85	0.0001	0
CHOLESTEROL	1	0.00016979	0.00012417	1.37	0.1716	1.00042
BMI	1	0.00972	0.00085317	11.39	<.0001	1.00890
AGE	1	-0.00033553	0.00024816	-1.35	0.1764	1.00061
gender	1	-0.00463	0.00649	-0.71	0.4756	1.00039
famhist	1	0.36833	0.00843	43.71	<.0001	1.01345
smoke	1	0.11452	0.01677	6.83	<.0001	1.00304
EXERCISEMINPERWEEK	1	-0.00031741	0.00004571	-6.94	<.0001	1.00431
*/

/* 3(c) Model Building */
/* 3(c)i : Split the data into training and validation datasets */

proc surveyselect data = patient_2
method=srs out = samp1 samprate=0.5 outall;
run;

data train validate replace;
set samp1;
if selected = 0 then output train;
else if selected = 1 then output validate;
run;


/* Iteration 1 */
proc logistic data = train descending; /*Interested in probability y = 0 hence descending*/
model y = CHOLESTEROL	BMI	AGE	gender	famhist	smoke	EXERCISEMINPERWEEK;
run;

/* Iteration 2: Creating age buckets for Age as it is Insignificant
<25  - ag25
>25 and <=45 - ag45
>45 and <=60 - ag60
>60 - ag61
*/

Data train1 replace;
set train;
if AGE lt 25 then ag25 = 1; else ag25 = 0;
if 25 le AGE lt 45 then ag45 = 1; else ag45 = 0;
if 45 le AGE lt 60 then ag60 = 1; else ag60 = 0;
if AGE>=60 then ag61 = 1;else ag61 = 0;
run;

proc logistic data = train1 descending; 
model y = CHOLESTEROL BMI ag45 /*ag60*/ ag61 gender famhist smoke EXERCISEMINPERWEEK;
run;
/*Creating age buckets shows that age variable is not significant*/

/*Iteration 3: Droping all the insignificant variables*/

proc logistic data = train1 descending; 
model y = /*CHOLESTEROL*/ BMI /*ag45 ag60 ag61*/ /*gender*/ famhist smoke EXERCISEMINPERWEEK;
run;

/*Classification table */

proc logistic data = train1 descending; 
model y = /*CHOLESTEROL*/ BMI /*ag45 ag60 ag61*/ /*gender*/ famhist smoke EXERCISEMINPERWEEK/ctable;
output out = test p = prob xbeta = logit;
run;


/* Validating the model on Validate dataset */

proc logistic data = train1 descending outmodel=dmm;
model y =  BMI famhist smoke EXERCISEMINPERWEEK/lackfit ctable;
score out = dmp;
run;

proc logistic data = validate descending outmodel=dmm;
model y =  BMI famhist smoke EXERCISEMINPERWEEK/lackfit ctable;
score out = dmp;
run;

/* Gain Chart */
proc rank data = dmp out = decile group = 10 ties = mean;
var p_1;
ranks decile;
run;

proc sort data = decile;
by descending p_1;
run;

proc export data = decile (keep = y p_1 decile)
outfile = "/home/varunkaru0/T10_Logistic Regression Models/Output/gains.csv"
dbms= csv replace;
run;


