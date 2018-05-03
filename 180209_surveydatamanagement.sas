* Program to upload SAS datasets, extract and modify the relevant survey variables for study III and export them as 
  CSV files prior to analysis in R statistical software

  Author: Johanna Holm
  Date of creation: 15.11.05
  Edited 15.11.17, 15.11.24, 15.11.25 (Added education and soc_born variables) 15.11.16 (Investigated how many children were born after a cancer diagnosis and if they were nursed)
  15.12.17 (Reran script with new input data received from Mikael Eriksson whith matching of cases after excluding first BC diagnoses)
  15.12.18 (Added bodyshape and aafftp variables to the output dataset)
  16.01.14 (Added the survey variables for breastfeeding per child to the output dataset, added variables nbf ncbf fcbf lcbf
  16.02.09-11 (Added the survey variables for age , age_five, postmenopausal, postmenopausal_before_bc, HRT use, age at menarche, bmi, brca mutation, contraception ever and european ancestry to the output dataset
  16.02.12 (Added mammographic density data)
  (160502  (Changed input datasets to the latest update from Mikael Eriksson after adding Karma Skåne cases (160429 update -erronous-)))
  16.05.11 (Changed input datasets to the latest update from Mikael Eriksson after adding Karma Skåne cases, added sympathydata code)
  16.05.25 (Added variables for physical activity lifetime from survey data. Updated the BBD coding after adding skåne cases)
  16.06.08 (Added corevariables yearofbirth, birth_times and age_at_firstbirth);

* Open connection to datasets;
libname rawdata "Z:\PhD\TNBC\Data\Raw Data\";
libname offline "P:\LIBRO_1\ResearchPlatform_karmastudy.org\Offline_Datasets";

* Create modifiable copies of survey datasets ;

* 1. 15.11.05. Datasets restricted to my study participants (This corresponds to survey_s1_prework for the libro1 women diagnosed 2005-2008, and karma survey for cases in sthlm 2005-2015;
data surveylibro1;
set rawdata.Surveylibro1;
run;
data surveykarma;
set rawdata.Surveykarma;
run;

* 2. 15.11.11. For trouble-shooting: Datasets for the full libro1 cohort;
data survey_original_libro1;
set offline.Survey_s1_prework_orig;
run;

data offlinesurvey_libro1;
set offline.Survey_s1_prework;
run;

proc freq data=offlinesurvey_libro1;
table can_bre_family_mot can_ova_family_mot can_bre_family_SIS can_ova_family_SIS/missing;
run;

proc freq data=surveylibro1;
table can_bre_family_mot can_ova_family_mot can_bre_family_SIS can_ova_family_SIS/missing;
run;


proc freq data=offlinesurvey_libro1;
table wom2_child_nursed_1*wom_birth_times/missing;
run;

proc freq data=surveylibro1;
table wom2_child_nursed_1*wom_birth_times/missing;
run;

data check; set offlinesurvey_libro1; if wom2_child_nursed_1>. and wom_birth_times='';run;
* Start recoding study III participant survey data;

* Variables on family history of bc and ovarian cancer;
* In Libro1- the survey only asked about mothers and sisters for family history ;
* In Karma, there is also information on children, fathers and brothers ;
* The limiting factor is therefore Libro1, so do not use information on children or paternal relatives from Karma as this would introduce systematic bias; 

data surveylibro1;
     set surveylibro1;
	     if can_bre_family_mot=1 or 10>can_bre_family_sis>0 then x_famhist_bre=1;/* If either mother or any sister have had, then 1 */
		 else if can_bre_family_mot=0 and can_bre_family_sis=0 then x_famhist_bre=0; /* If neither mother nor sister have had, then 0 */
		 else if can_bre_family_mot=0 and can_bre_family_sis=996 then x_famhist_bre=0;/* If mother haven't had, and she has no sister, then 0 */
         else x_famhist_bre=.;/* If unknown/refuse, then missing */
	     if can_ova_family_mot=1 or 10>can_ova_family_sis>0 then x_famhist_ova=1; /* If either mother or any sister have had, then 1 */
		 else if can_ova_family_mot=0 and can_ova_family_sis=0 then x_famhist_ova=0; /* If neither mother nor sister have had, then 0 */
		 else if can_ova_family_mot=0 and can_ova_family_sis=996 then x_famhist_ova=0;/* If mother haven't had, and she has no sister, then 0 */
         else x_famhist_ova=.;/* If unknown/refuse, then missing */
	     if sum(x_famhist_bre,x_famhist_ova)>0 then x_famhist=1;
	     else if X_famhist_bre=0 and x_famhist_ova=0 then x_famhist=0;
		 else x_famhist=.;
		 /* Create categorical variable of 0, 1, >1 first degree relative */
		 if can_bre_family_mot=1 and 10>can_bre_family_sis>0 then x_famhist_brecat=2;
		 else if 10>can_bre_family_sis>1 then x_famhist_brecat=2;
		 else x_famhist_brecat=x_famhist_bre;
		 if can_ova_family_mot=1 and 10>can_ova_family_sis>0 then x_famhist_ovacat=2;
		 else if 10>can_ova_family_sis>1 then x_famhist_ovacat=2;
		 else x_famhist_ovacat=x_famhist_ova;
run;

data surveykarma;
     set surveykarma;
         * Calculate number of paternal relatives with BC;
	     can_bre_family_paternal=sum(can_bre_family_whc_18,can_bre_family_whc_19,can_bre_family_whc_20,can_bre_family_whc_21,can_bre_family_whc_22,can_bre_family_whc_23,can_bre_family_whc_24,can_bre_family_whc_25,can_bre_family_whc_26,can_bre_family_whc_27,can_bre_family_whc_28,can_bre_family_whc_29,can_bre_family_whc_30,can_bre_family_whc_31,can_bre_family_whc_32,can_bre_family_whc_33,can_bre_family_whc_34, can_bre_family_36);
	     * Calculate number of maternal relatives with BC, omitting daughters;
	     can_bre_family_maternal=sum(can_bre_family_whc_1,can_bre_family_whc_2,can_bre_family_whc_3,can_bre_family_whc_4,can_bre_family_whc_5,can_bre_family_whc_6,can_bre_family_whc_7,can_bre_family_whc_8,can_bre_family_whc_9,can_bre_family_whc_10,can_bre_family_whc_11,can_bre_family_whc_12,can_bre_family_whc_13,can_bre_family_whc_14,can_bre_family_whc_15,can_bre_family_whc_16,can_bre_family_whc_17);
	     if can_bre_family = 2 then x_famhist_bre=0;
	     else if can_bre_family=1 and can_bre_family_maternal>0 then x_famhist_bre=1;/* If either mother or any full- or halfsister have had, then 1 */
		 else if can_bre_family_maternal=0  then x_famhist_bre=0; /* If paternal relatives or daughters have had, then 0 - NOTE! This is to make the variable comparable with the libro1 definition*/
         else x_famhist_bre=.;/* If unknown/refuse, then missing */

         * Calculate number of paternal relatives with ovarian cancer;
 	     can_ova_family_paternal=sum(can_ova_family_whc_18,can_ova_family_whc_19,can_ova_family_whc_20,can_ova_family_whc_21,can_ova_family_whc_22,can_ova_family_whc_23,can_ova_family_whc_24,can_ova_family_whc_25,can_ova_family_whc_26,can_ova_family_whc_27,can_ova_family_whc_28,can_ova_family_whc_29,can_ova_family_whc_30,can_ova_family_whc_31,can_ova_family_whc_32,can_ova_family_whc_33,can_ova_family_whc_34,can_ova_family_whc_36);
	     * Calculate number of maternal relatives with ovarian cancer, omitting daughters;
	     can_ova_family_maternal=sum(can_ova_family_whc_1,can_ova_family_whc_2,can_ova_family_whc_3,can_ova_family_whc_4,can_ova_family_whc_5,can_ova_family_whc_6,can_ova_family_whc_7,can_ova_family_whc_8,can_ova_family_whc_9,can_ova_family_whc_10,can_ova_family_whc_11,can_ova_family_whc_12,can_ova_family_whc_13,can_ova_family_whc_14,can_ova_family_whc_15,can_ova_family_whc_16,can_ova_family_whc_17);
	     if can_ova_family = 2 then x_famhist_ova=0;
	     else if can_ova_family=1 and can_ova_family_maternal>0 then x_famhist_ova=1;/* If either mother or any full- or halfsister have had, then 1 */
		 else if can_ova_family_maternal=0 then x_famhist_ova=0; /* If paternal relatives or daughters have had, then 0 - NOTE! This is to make the variable comparable with the libro1 definition*/
         else x_famhist_ova=.;/* If unknown/refuse, then missing */

	     if sum(x_famhist_bre,x_famhist_ova)>0 then x_famhist=1;
	     else if X_famhist_bre=0 and x_famhist_ova=0 then x_famhist=0;
		 else x_famhist=.;
run;

* Investigate impact of not knowing paternal or daughter/son history of breast cancer;
proc freq data=surveykarma;
table can_bre_family can_bre_family_maternal can_bre_family_whc_35 can_bre_family_paternal can_ova_family_maternal can_ova_family_whc_35 can_ova_family_paternal ;
run;

proc freq data=surveykarma;
table can_bre_family_maternal*can_bre_family_whc_35 can_ova_family_maternal*can_ova_family_whc_35;
run;
* 41 out of 48 who have a daughter with BC did not have any other first-degree maternal relative with BC, hence 41 out of 1830 = 2% of women with maternal first-degree relatives would be missed in libro1. That is reassuring! ;

proc freq data=surveykarma;
table can_bre_family_maternal*can_bre_family can_bre_family_paternal*can_bre_family;
run;
* In total, of 2505 with a firstdegree relative with bc, 6.96% would be restricted to daughters or paternal relatives 
  and hence missed if a libro1 questionnaire had been employed. Not an alarming rate. We still capture 93 % of first degree relatives in libro1;

proc freq data=surveylibro1;
table x_famhist x_famhist_bre x_famhist_ova/missing;
run;
proc freq data=surveykarma (where=(match_casestatus=1));
table x_famhist x_famhist_bre x_famhist_ova/missing;
run;
* Similar frequency of familial history of BC between libro1 and karma cases, 
  but libro1 cases appears to have more information on family history;

proc freq data=surveykarma;
table x_famhist x_famhist_bre x_famhist_ova/missing;
run;
* Similar frequencies are missing information on family history in karma and libro1.;



* Variables on parity;

proc freq data=surveylibro1;
table wom_birth_times/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table wom_birth_times*wom_pregn_times/missing;
run;
proc freq data=surveykarma;
table wom_birth_times*wom_pregn_times/missing;
run;

data surveylibro1;
set surveylibro1;
	 if wom_birth_times='no' then x_parity=0;
	 else if wom_birth_times='998' then x_parity=.;
	 else if wom_birth_times^='' 
     then do;
	     x_parity=input(wom_birth_times, best4.);
	 end;
run;

data surveykarma;
set surveykarma;
     if wom_pregn_times='no' then x_parity=0;
	 else if wom_birth_times='no' then x_parity=0;
	 else if wom_birth_times='998' then x_parity=.;
	 else if wom_birth_times^='' 
     then do;
	     x_parity=input(wom_birth_times, best4.);
	 end;
run;

proc freq data=surveylibro1;
table x_parity/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_parity/missing;
run;
proc freq data=surveykarma;
table x_parity/missing;
run;
* In Karma, cases are missing information on parity to a higher extent than controls.;


proc freq data=surveylibro1;
table x_parity;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_parity;
run;
proc freq data=surveykarma;
table x_parity;
run;
* More libro1 cases than karma cases and controls were nullipara - a selection bias in Karma or a rural vs urban effect?;

* Create own parity variable of parity before cancer diagnosis ;



* Variables on breast feeding;
* The information for breast feeding is based per child born in both libro1 and Karma surveys;
* However, for Libro1 the limit for number of children assessed is six, whereas Karma goes up to 15 children;
* Another important difference is that Libro1 and Karma has different categories of months breastfeeding;
* Both have options 0 months, 7-12 months, and >=12 months, but they differ for the cutoff defining the two categories covering 1-6 months;
* Therefor we should collaps these categories into one of >0-6 months. This is step two, where we create the variable x_nursedcat. ;
* First we create a continuous measure x_nursed of total months breastfed, by replacing each categorical interval with the mean of that category and summing it up across all children;

data surveylibro1;
set surveylibro1;
         if wom2_child_nursed_1>5 then nursed_1=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_2>5 then nursed_2=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_3>5 then nursed_3=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_4>5 then nursed_4=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_5>5 then nursed_5=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_6>5 then nursed_6=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_1=0 then nursed_1=0;/* Never nursed */
         if wom2_child_nursed_2=0 then nursed_2=0;
         if wom2_child_nursed_3=0 then nursed_3=0;
         if wom2_child_nursed_4=0 then nursed_4=0;
         if wom2_child_nursed_5=0 then nursed_5=0;
         if wom2_child_nursed_6=0 then nursed_6=0;
         if wom2_child_nursed_1=1 then nursed_1=2;/* Mean of 1-3 is 2 */
         if wom2_child_nursed_2=1 then nursed_2=2;
         if wom2_child_nursed_3=1 then nursed_3=2;
         if wom2_child_nursed_4=1 then nursed_4=2;
         if wom2_child_nursed_5=1 then nursed_5=2;
         if wom2_child_nursed_6=1 then nursed_6=2;
         if wom2_child_nursed_1=2 then nursed_1=5;/* Mean of 4-6 is 5 */
         if wom2_child_nursed_2=2 then nursed_2=5;
         if wom2_child_nursed_3=2 then nursed_3=5;
         if wom2_child_nursed_4=2 then nursed_4=5;
         if wom2_child_nursed_5=2 then nursed_5=5;
         if wom2_child_nursed_6=2 then nursed_6=5;
         if wom2_child_nursed_1=3 then nursed_1=9.5;/* Mean of 7-12 is 9.5 */
         if wom2_child_nursed_2=3 then nursed_2=9.5;
         if wom2_child_nursed_3=3 then nursed_3=9.5;
         if wom2_child_nursed_4=3 then nursed_4=9.5;
         if wom2_child_nursed_5=3 then nursed_5=9.5;
         if wom2_child_nursed_6=3 then nursed_6=9.5;
         if wom2_child_nursed_1=4 then nursed_1=18;/* Mean of 12-infinity is fixed at 18 (Arbitrary)*/
         if wom2_child_nursed_2=4 then nursed_2=18;
         if wom2_child_nursed_3=4 then nursed_3=18;
         if wom2_child_nursed_4=4 then nursed_4=18;
         if wom2_child_nursed_5=4 then nursed_5=18;
         if wom2_child_nursed_6=4 then nursed_6=18;
         x_nursed=sum(nursed_6,nursed_5,nursed_4,nursed_3,nursed_2,nursed_1);/* Sum across children */
run;


data surveykarma;
set surveykarma;
         if wom_child_nursed_1>5 then nursed_1=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_2>5 then nursed_2=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_3>5 then nursed_3=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_4>5 then nursed_4=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_5>5 then nursed_5=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_6>5 then nursed_6=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_7>5 then nursed_7=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_8>5 then nursed_8=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_9>5 then nursed_9=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_10>5 then nursed_10=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_11>5 then nursed_11=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_12>5 then nursed_12=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_13>5 then nursed_13=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_14>5 then nursed_14=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_15>5 then nursed_15=.;/* Set '998' to missing prior calculations */


         if wom_child_nursed_1=1 then nursed_1=0;/* Never nursed */
         if wom_child_nursed_2=1 then nursed_2=0;
         if wom_child_nursed_3=1 then nursed_3=0;
         if wom_child_nursed_4=1 then nursed_4=0;
         if wom_child_nursed_5=1 then nursed_5=0;
         if wom_child_nursed_6=1 then nursed_6=0;
         if wom_child_nursed_7=1 then nursed_7=0;
         if wom_child_nursed_8=1 then nursed_8=0;
         if wom_child_nursed_9=1 then nursed_9=0;
         if wom_child_nursed_10=1 then nursed_10=0;
         if wom_child_nursed_11=1 then nursed_11=0;
         if wom_child_nursed_12=1 then nursed_12=0;
         if wom_child_nursed_13=1 then nursed_13=0;
		 if wom_child_nursed_14=1 then nursed_14=0;
         if wom_child_nursed_15=1 then nursed_15=0;

         if wom_child_nursed_1=2 then nursed_1=0.5;/* Mean of <1 is 0.5*/
         if wom_child_nursed_2=2 then nursed_2=0.5;
         if wom_child_nursed_3=2 then nursed_3=0.5;
         if wom_child_nursed_4=2 then nursed_4=0.5;
         if wom_child_nursed_5=2 then nursed_5=0.5;
         if wom_child_nursed_6=2 then nursed_6=0.5;
         if wom_child_nursed_7=2 then nursed_7=0.5;
         if wom_child_nursed_8=2 then nursed_8=0.5;
         if wom_child_nursed_9=2 then nursed_9=0.5;
         if wom_child_nursed_10=2 then nursed_10=0.5;
         if wom_child_nursed_11=2 then nursed_11=0.5;
         if wom_child_nursed_12=2 then nursed_12=0.5;
         if wom_child_nursed_13=2 then nursed_13=0.5;
		 if wom_child_nursed_14=2 then nursed_14=0.5;
         if wom_child_nursed_15=2 then nursed_15=0.5;

         if wom_child_nursed_1=3 then nursed_1=3;/* Mean of 1-6 is 3 */
         if wom_child_nursed_2=3 then nursed_2=3;
         if wom_child_nursed_3=3 then nursed_3=3;
         if wom_child_nursed_4=3 then nursed_4=3;
         if wom_child_nursed_5=3 then nursed_5=3;
         if wom_child_nursed_6=3 then nursed_6=3;
         if wom_child_nursed_7=3 then nursed_7=3;
         if wom_child_nursed_8=3 then nursed_8=3;
         if wom_child_nursed_9=3 then nursed_9=3;
         if wom_child_nursed_10=3 then nursed_10=3;
         if wom_child_nursed_11=3 then nursed_11=3;
         if wom_child_nursed_12=3 then nursed_12=3;
         if wom_child_nursed_13=3 then nursed_13=3;
		 if wom_child_nursed_14=3 then nursed_14=3;
         if wom_child_nursed_15=3 then nursed_15=3;

         if wom_child_nursed_1=4 then nursed_1=9.5;/* Mean of 7-12 is 9.5 */
         if wom_child_nursed_2=4 then nursed_2=9.5;
         if wom_child_nursed_3=4 then nursed_3=9.5;
         if wom_child_nursed_4=4 then nursed_4=9.5;
         if wom_child_nursed_5=4 then nursed_5=9.5;
         if wom_child_nursed_6=4 then nursed_6=9.5;
         if wom_child_nursed_7=4 then nursed_7=9.5;
         if wom_child_nursed_8=4 then nursed_8=9.5;
         if wom_child_nursed_9=4 then nursed_9=9.5;
         if wom_child_nursed_10=4 then nursed_10=9.5;
         if wom_child_nursed_11=4 then nursed_11=9.5;
         if wom_child_nursed_12=4 then nursed_12=9.5;
         if wom_child_nursed_13=4 then nursed_13=9.5;
		 if wom_child_nursed_14=4 then nursed_14=9.5;
         if wom_child_nursed_15=4 then nursed_15=9.5;

         if wom_child_nursed_1=5 then nursed_1=18;/* Mean of 12-infinity is fixed at 18 (Arbitrary)*/
         if wom_child_nursed_2=5 then nursed_2=18;
         if wom_child_nursed_3=5 then nursed_3=18;
         if wom_child_nursed_4=5 then nursed_4=18;
         if wom_child_nursed_5=5 then nursed_5=18;
         if wom_child_nursed_6=5 then nursed_6=18;
         if wom_child_nursed_7=5 then nursed_7=18;
         if wom_child_nursed_8=5 then nursed_8=18;
         if wom_child_nursed_9=5 then nursed_9=18;
         if wom_child_nursed_10=5 then nursed_10=18;
         if wom_child_nursed_11=5 then nursed_11=18;
         if wom_child_nursed_12=5 then nursed_12=18;
         if wom_child_nursed_13=5 then nursed_13=18;
		 if wom_child_nursed_14=5 then nursed_14=18;
         if wom_child_nursed_15=5 then nursed_15=18;
         x_nursed=sum(nursed_15,nursed_14,nursed_13,nursed_12,nursed_11,nursed_10,nursed_9,nursed_8,nursed_7,nursed_6,nursed_5,nursed_4,nursed_3,nursed_2,nursed_1);/* Sum across children */
run;

proc univariate data=surveylibro1;
var x_nursed;
run;
proc univariate data=surveykarma(where=(match_casestatus=1));
var x_nursed;
run;
proc univariate data=surveykarma;
var x_nursed;
run;
* Despite more information on more children in karma, very similar range and median/mean breastfeeding in the two cohorts;

* Variable x_breastfeed and bfcat are categorical for total lifetime of breastfeeding and based on x_nursed;
* Note: Since x_nursed is a continuous measure based on a categorical variable using the mean, 
  it is only an approximation of lifetime breastfeeding and there will be noise introduced;

data surveylibro1;
set surveylibro1;
         if x_nursed=. then x_breastfeed=.;
         else if x_nursed=0 then x_breastfeed=0; /* No breast feeding */
         else if 0<x_nursed<=6 then x_breastfeed=1; /* 1-6 months */
         else if 6<x_nursed<13 then x_breastfeed=2; /* 7-12 months */
         else if 12<x_nursed<=18 then x_breastfeed=3; /* 1-1½ years */
         else if 18<x_nursed<=24 then x_breastfeed=4; /* 1½-2 years */
         else if 24<x_nursed<=36 then x_breastfeed=5; /* 2-3 years */
         else if 36<x_nursed<=48 then x_breastfeed=6; /* 3-4 years */
         else if 48<x_nursed<=60 then x_breastfeed=7; /* 4-5 years */
         else if 60<x_nursed then x_breastfeed=8; /* More than 5 years */
         * Collapse into categories of 0, >0-1,5 and >1,5 years;
         if x_breastfeed=. then bfcat=.;
         else if x_breastfeed=0 then bfcat=0;
         else if 0<x_breastfeed<=3 then bfcat=1;
         else if 3<x_breastfeed then bfcat=2;
         else bfcat=.;
run;

data surveykarma;
set surveykarma;
         if x_nursed=. then x_breastfeed=.;
        else if x_nursed=0 then x_breastfeed=0; /* No breast feeding */
         else if 0<x_nursed<=6 then x_breastfeed=1; /* 1-6 months */
         else if 6<x_nursed<13 then x_breastfeed=2; /* 7-12 months */
         else if 12<x_nursed<=18 then x_breastfeed=3; /* 1-1½ years */
         else if 18<x_nursed<=24 then x_breastfeed=4; /* 1½-2 years */
         else if 24<x_nursed<=36 then x_breastfeed=5; /* 2-3 years */
         else if 36<x_nursed<=48 then x_breastfeed=6; /* 3-4 years */
         else if 48<x_nursed<=60 then x_breastfeed=7; /* 4-5 years */
         else if 60<x_nursed then x_breastfeed=8; /* More than 5 years */
         * Collapse into categories of 0, >0-1,5 and >1,5 years;
         if x_breastfeed=. then bfcat=.;
         else if x_breastfeed=0 then bfcat=0;
         else if 0<x_breastfeed<=3 then bfcat=1;
         else if 3<x_breastfeed then bfcat=2;
         else bfcat=.;
run;

proc freq data=surveylibro1;
table x_breastfeed bfcat/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_breastfeed bfcat/missing;
run;
proc freq data=surveykarma;
table x_breastfeed bfcat/missing;
run;

proc freq data=surveylibro1;
table x_breastfeed bfcat;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_breastfeed bfcat;
run;
proc freq data=surveykarma;
table x_breastfeed bfcat;
run;
* More libro1 women never breastfed - a sthlm vs rural effect?;

* New variables nursedcat_(n child) harmonizing the categories of months breast feeding per child for Libro1 and Karma;

data surveylibro1;
set surveylibro1;
         if wom2_child_nursed_1>5 then nursedcat_1=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_2>5 then nursedcat_2=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_3>5 then nursedcat_3=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_4>5 then nursedcat_4=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_5>5 then nursedcat_5=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_6>5 then nursedcat_6=.;/* Set '998' to missing prior calculations */
         if wom2_child_nursed_1=0 then nursedcat_1=0;/* Never nursed */
         if wom2_child_nursed_2=0 then nursedcat_2=0;
         if wom2_child_nursed_3=0 then nursedcat_3=0;
         if wom2_child_nursed_4=0 then nursedcat_4=0;
         if wom2_child_nursed_5=0 then nursedcat_5=0;
         if wom2_child_nursed_6=0 then nursedcat_6=0;
         if wom2_child_nursed_1=1 then nursedcat_1=3;/* Nursed <3mo set to 3 (>0-6mo) */
         if wom2_child_nursed_2=1 then nursedcat_2=3;
         if wom2_child_nursed_3=1 then nursedcat_3=3;
         if wom2_child_nursed_4=1 then nursedcat_4=3;
         if wom2_child_nursed_5=1 then nursedcat_5=3;
         if wom2_child_nursed_6=1 then nursedcat_6=3;
         if wom2_child_nursed_1=2 then nursedcat_1=3;/* Nursed 4-6mo set to 3 (>0-6mo) */
         if wom2_child_nursed_2=2 then nursedcat_2=3;
         if wom2_child_nursed_3=2 then nursedcat_3=3;
         if wom2_child_nursed_4=2 then nursedcat_4=3;
         if wom2_child_nursed_5=2 then nursedcat_5=3;
         if wom2_child_nursed_6=2 then nursedcat_6=3;
         if wom2_child_nursed_1=3 then nursedcat_1=9.5;/* Nursed 7-12 set to category 9.5 (7-12mo) */
         if wom2_child_nursed_2=3 then nursedcat_2=9.5;
         if wom2_child_nursed_3=3 then nursedcat_3=9.5;
         if wom2_child_nursed_4=3 then nursedcat_4=9.5;
         if wom2_child_nursed_5=3 then nursedcat_5=9.5;
         if wom2_child_nursed_6=3 then nursedcat_6=9.5;
         if wom2_child_nursed_1=4 then nursedcat_1=18;/* Nursed >12mo set to category 18 (<12mo)*/
         if wom2_child_nursed_2=4 then nursedcat_2=18;
         if wom2_child_nursed_3=4 then nursedcat_3=18;
         if wom2_child_nursed_4=4 then nursedcat_4=18;
         if wom2_child_nursed_5=4 then nursedcat_5=18;
         if wom2_child_nursed_6=4 then nursedcat_6=18;
         x_nursedcat=sum(nursedcat_6,nursedcat_5,nursedcat_4,nursedcat_3,nursedcat_2,nursedcat_1);/* Dont use breastfeed until redone! */
         x_mean_nursed_child=x_nursed/x_parity;
run;


data surveykarma;
set surveykarma;
         if wom_child_nursed_1>5 then nursedcat_1=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_2>5 then nursedcat_2=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_3>5 then nursedcat_3=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_4>5 then nursedcat_4=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_5>5 then nursedcat_5=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_6>5 then nursedcat_6=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_7>5 then nursedcat_7=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_8>5 then nursedcat_8=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_9>5 then nursedcat_9=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_10>5 then nursedcat_10=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_11>5 then nursedcat_11=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_12>5 then nursedcat_12=.;/* Set '998' to missing prior calculations */
		 if wom_child_nursed_13>5 then nursedcat_13=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_14>5 then nursedcat_14=.;/* Set '998' to missing prior calculations */
         if wom_child_nursed_15>5 then nursedcat_15=.;/* Set '998' to missing prior calculations */


         if wom_child_nursed_1=1 then nursedcat_1=0;/* Never nursed */
         if wom_child_nursed_2=1 then nursedcat_2=0;
         if wom_child_nursed_3=1 then nursedcat_3=0;
         if wom_child_nursed_4=1 then nursedcat_4=0;
         if wom_child_nursed_5=1 then nursedcat_5=0;
         if wom_child_nursed_6=1 then nursedcat_6=0;
         if wom_child_nursed_7=1 then nursedcat_7=0;
         if wom_child_nursed_8=1 then nursedcat_8=0;
         if wom_child_nursed_9=1 then nursedcat_9=0;
         if wom_child_nursed_10=1 then nursedcat_10=0;
         if wom_child_nursed_11=1 then nursedcat_11=0;
         if wom_child_nursed_12=1 then nursedcat_12=0;
         if wom_child_nursed_13=1 then nursedcat_13=0;
		 if wom_child_nursed_14=1 then nursedcat_14=0;
         if wom_child_nursed_15=1 then nursedcat_15=0;

         if wom_child_nursed_1=2 then nursedcat_1=3;/* Nursed <1 set to 3 (>0-6mo)*/
         if wom_child_nursed_2=2 then nursedcat_2=3;
         if wom_child_nursed_3=2 then nursedcat_3=3;
         if wom_child_nursed_4=2 then nursedcat_4=3;
         if wom_child_nursed_5=2 then nursedcat_5=3;
         if wom_child_nursed_6=2 then nursedcat_6=3;
         if wom_child_nursed_7=2 then nursedcat_7=3;
         if wom_child_nursed_8=2 then nursedcat_8=3;
         if wom_child_nursed_9=2 then nursedcat_9=3;
         if wom_child_nursed_10=2 then nursedcat_10=3;
         if wom_child_nursed_11=2 then nursedcat_11=3;
         if wom_child_nursed_12=2 then nursedcat_12=3;
         if wom_child_nursed_13=2 then nursedcat_13=3;
		 if wom_child_nursed_14=2 then nursedcat_14=3;
         if wom_child_nursed_15=2 then nursedcat_15=3;

         if wom_child_nursed_1=3 then nursedcat_1=3;/* Nursed 1-6 set to 3 (>0-6mo) */
         if wom_child_nursed_2=3 then nursedcat_2=3;
         if wom_child_nursed_3=3 then nursedcat_3=3;
         if wom_child_nursed_4=3 then nursedcat_4=3;
         if wom_child_nursed_5=3 then nursedcat_5=3;
         if wom_child_nursed_6=3 then nursedcat_6=3;
         if wom_child_nursed_7=3 then nursedcat_7=3;
         if wom_child_nursed_8=3 then nursedcat_8=3;
         if wom_child_nursed_9=3 then nursedcat_9=3;
         if wom_child_nursed_10=3 then nursedcat_10=3;
         if wom_child_nursed_11=3 then nursedcat_11=3;
         if wom_child_nursed_12=3 then nursedcat_12=3;
         if wom_child_nursed_13=3 then nursedcat_13=3;
		 if wom_child_nursed_14=3 then nursedcat_14=3;
         if wom_child_nursed_15=3 then nursedcat_15=3;

         if wom_child_nursed_1=4 then nursedcat_1=9.5;/*Nursed  7-12 set to 9.5 (7-12mo) */
         if wom_child_nursed_2=4 then nursedcat_2=9.5;
         if wom_child_nursed_3=4 then nursedcat_3=9.5;
         if wom_child_nursed_4=4 then nursedcat_4=9.5;
         if wom_child_nursed_5=4 then nursedcat_5=9.5;
         if wom_child_nursed_6=4 then nursedcat_6=9.5;
         if wom_child_nursed_7=4 then nursedcat_7=9.5;
         if wom_child_nursed_8=4 then nursedcat_8=9.5;
         if wom_child_nursed_9=4 then nursedcat_9=9.5;
         if wom_child_nursed_10=4 then nursedcat_10=9.5;
         if wom_child_nursed_11=4 then nursedcat_11=9.5;
         if wom_child_nursed_12=4 then nursedcat_12=9.5;
         if wom_child_nursed_13=4 then nursedcat_13=9.5;
		 if wom_child_nursed_14=4 then nursedcat_14=9.5;
         if wom_child_nursed_15=4 then nursedcat_15=9.5;

         if wom_child_nursed_1=5 then nursedcat_1=18;/* Nursed <12 set to 18 (<12mo)*/
         if wom_child_nursed_2=5 then nursedcat_2=18;
         if wom_child_nursed_3=5 then nursedcat_3=18;
         if wom_child_nursed_4=5 then nursedcat_4=18;
         if wom_child_nursed_5=5 then nursedcat_5=18;
         if wom_child_nursed_6=5 then nursedcat_6=18;
         if wom_child_nursed_7=5 then nursedcat_7=18;
         if wom_child_nursed_8=5 then nursedcat_8=18;
         if wom_child_nursed_9=5 then nursedcat_9=18;
         if wom_child_nursed_10=5 then nursedcat_10=18;
         if wom_child_nursed_11=5 then nursedcat_11=18;
         if wom_child_nursed_12=5 then nursedcat_12=18;
         if wom_child_nursed_13=5 then nursedcat_13=18;
		 if wom_child_nursed_14=5 then nursedcat_14=18;
         if wom_child_nursed_15=5 then nursedcat_15=18;
         x_nursedcat=sum(nursedcat_15,nursedcat_14,nursedcat_13,nursedcat_12,nursedcat_11,nursedcat_10,nursedcat_9,nursedcat_8,nursedcat_7,nursedcat_6,nursedcat_5,nursedcat_4,nursedcat_3,nursedcat_2,nursedcat_1);/* Dont use breastfeed until redone! */
         x_mean_nursed_child=x_nursed/x_parity;
run;

* Tabulate the proxy for total months breastfed and and mean months per child breastfed;
proc freq data=surveylibro1;
table x_nursedcat x_mean_nursed_child/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_nursedcat x_mean_nursed_child/missing;
run;
proc freq data=surveykarma;
table x_nursedcat x_mean_nursed_child/missing;
run;


* Variable x_breastfeed_har and bfcat_har are categorical for total lifetime of breastfeeding and based on x_nursedcat;
* Note: Since x_nursedcat is a continuous measure based on a categorical variable using the mean, 
  it is only an approximation of lifetime breastfeeding and there will be noise introduced;

data surveylibro1;
set surveylibro1;
         if x_nursedcat=. then x_breastfeed_har=.;
         else if x_nursedcat=0 then x_breastfeed_har=0; /* No breast feeding */
         else if 0<x_nursedcat<=6 then x_breastfeed_har=1; /* 1-6 months */
         else if 6<x_nursedcat<13 then x_breastfeed_har=2; /* 7-12 months */
         else if 12<x_nursedcat<=18 then x_breastfeed_har=3; /* 1-1½ years */
         else if 18<x_nursedcat<=24 then x_breastfeed_har=4; /* 1½-2 years */
         else if 24<x_nursedcat<=36 then x_breastfeed_har=5; /* 2-3 years */
         else if 36<x_nursedcat<=48 then x_breastfeed_har=6; /* 3-4 years */
         else if 48<x_nursedcat<=60 then x_breastfeed_har=7; /* 4-5 years */
         else if 60<x_nursedcat then x_breastfeed_har=8; /* More than 5 years */
         * Collapse into categories of 0, >0-1,5 and >1,5 years;
         if x_breastfeed_har=. then bfcat_har=.;
         else if x_breastfeed_har=0 then bfcat_har=0;
         else if 0<x_breastfeed_har<=3 then bfcat_har=1;
         else if 3<x_breastfeed_har then bfcat_har=2;
         else bfcat_har=.;
run;

data surveykarma;
set surveykarma;
         if x_nursedcat=. then x_breastfeed_har=.;
        else if x_nursedcat=0 then x_breastfeed_har=0; /* No breast feeding */
         else if 0<x_nursedcat<=6 then x_breastfeed_har=1; /* 1-6 months */
         else if 6<x_nursedcat<13 then x_breastfeed_har=2; /* 7-12 months */
         else if 12<x_nursedcat<=18 then x_breastfeed_har=3; /* 1-1½ years */
         else if 18<x_nursedcat<=24 then x_breastfeed_har=4; /* 1½-2 years */
         else if 24<x_nursedcat<=36 then x_breastfeed_har=5; /* 2-3 years */
         else if 36<x_nursedcat<=48 then x_breastfeed_har=6; /* 3-4 years */
         else if 48<x_nursedcat<=60 then x_breastfeed_har=7; /* 4-5 years */
         else if 60<x_nursedcat then x_breastfeed_har=8; /* More than 5 years */
         * Collapse into categories of 0, >0-1,5 and >1,5 years;
         if x_breastfeed_har=. then bfcat_har=.;
         else if x_breastfeed_har=0 then bfcat_har=0;
         else if 0<x_breastfeed_har<=3 then bfcat_har=1;
         else if 3<x_breastfeed_har then bfcat_har=2;
         else bfcat_har=.;
run;
proc freq data=surveylibro1;
table x_breastfeed*x_parity wom2_child_nursed_1*wom_birth_times/missing;
run;
proc freq data=surveykarma;
table x_breastfeed*x_parity wom_child_nursed_1*wom_birth_times/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_breastfeed*x_parity/missing;
run;
proc freq data=surveykarma;
table x_breastfeed*x_parity/missing;
run;
* Women who had no children have missing information on breastfeeding in both libro1 and karma;
* Hence analysis on breastfeeding is restricted to parous women by default;

proc freq data=surveylibro1;
table bfcat bfcat_har;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table bfcat bfcat_har;
run;
proc freq data=surveykarma;
table bfcat bfcat_har;
run;


proc freq data=surveylibro1;
table x_breastfeed*x_parity;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table x_breastfeed*x_parity;
run;
proc freq data=surveykarma;
table x_breastfeed*x_parity;
run;

* Create a categorical variable which compares breast feeding to nullipasous women;
data surveylibro1;
set surveylibro1;
    if x_parity=0 then bfvspar=0;
    else if bfcat=0 then bfvspar=1;
    else if bfcat=1 then bfvspar=2;
    else if bfcat=2 then bfvspar=3;
    else bfvspar=.; 

    if x_parity=0 then bfvspar_har=0;
    else if bfcat_har=0 then bfvspar_har=1;
    else if bfcat_har=1 then bfvspar_har=2;
    else if bfcat_har=2 then bfvspar_har=3;
    else bfvspar_har=.;
run;


data surveykarma;
set surveykarma;
    if x_parity=0 then bfvspar=0;
    else if bfcat=0 then bfvspar=1;
    else if bfcat=1 then bfvspar=2;
    else if bfcat=2 then bfvspar=3;
    else bfvspar=.; 

    if x_parity=0 then bfvspar_har=0;
    else if bfcat_har=0 then bfvspar_har=1;
    else if bfcat_har=1 then bfvspar_har=2;
    else if bfcat_har=2 then bfvspar_har=3;
    else bfvspar_har=.;
run;

proc freq data=surveylibro1;
table x_parity bfcat bfcat_har bfvspar bfvspar_har/missing;
run;

proc freq data=surveykarma;
table x_parity bfcat  bfcat_har bfvspar bfvspar_har/missing;
run;


proc freq data=surveylibro1;
table  bfcat  bfcat_har bfvspar bfvspar_har/missing;
run;

proc freq data=surveykarma(where=(match_casestatus=1));
table bfcat  bfcat_har bfvspar bfvspar_har/missing;
run;

proc freq data=surveykarma;
table bfcat  bfcat_har bfvspar bfvspar_har/missing;
run;
* bfcat is more similar in distribution between karma and libro1 than bfcat_har...;

* Create variable nbf of being parous but not breast feeding at least one child ;
data surveylibro1;
set surveylibro1;
    if x_parity>0 then do;
        if nursedcat_1=0 or nursedcat_2=0 or nursedcat_3=0 or nursedcat_4=0 or nursedcat_5=0 or nursedcat_6=0 then nbf=1;
        else nbf=0;
    end;
run;

proc freq data=surveylibro1;
table nbf*x_parity;
run;

* Create variable ncbf of number of children breastfed;
data surveylibro1;
set surveylibro1;
    if nursedcat_1>0  then bf1=1;
    else if nursedcat_1=. then bf1=.;
    else bf1=0;
    if nursedcat_2>0  then bf2=1;
    else if nursedcat_2=. then bf2=.;
    else bf2=0;
    if nursedcat_3>0  then bf3=1;
    else if nursedcat_3=. then bf3=.;
    else bf3=0;
    if nursedcat_4>0  then bf4=1;
    else if nursedcat_4=. then bf4=.;
    else bf4=0;
    if nursedcat_5>0  then bf5=1;
    else if nursedcat_5=. then bf5=.;
    else bf5=0;
    if nursedcat_6>0  then bf6=1;
    else if nursedcat_6=. then bf6=.;
    else bf6=0;
    ncbf=sum(bf1,bf2,bf3,bf4,bf5,bf6);
run;


proc freq data=surveylibro1;
table ncbf*x_parity/missing;
run;

proc freq data=surveylibro1;
table ncbf/missing;
run;

* Create variable fcbf of fraction children breastfed;
data surveylibro1;
set surveylibro1;
    if x_parity >0 then do;
       fcbf= round(ncbf/x_parity, .01);
    end;
run;

proc freq data=surveylibro1;
table fcbf*x_parity/missing;
run;

proc freq data=surveylibro1;
table fcbf*x_parity;
run;
/* Most libro1 women (89%) breastfed all their children at least 0-6 months. 5% never breastfed any child. */

* Create a new variable of breastfeeding of last born child;
data surveylibro1;
set surveylibro1;
    if x_parity >0 then do;
        if x_parity=6 and nursedcat_6>0  then lcbf=1;
        else if x_parity=5 and nursedcat_5>0  then lcbf=1;
        else if x_parity=4 and nursedcat_4>0  then lcbf=1;
        else if x_parity=3 and nursedcat_3>0  then lcbf=1;
        else if x_parity=2 and nursedcat_2>0  then lcbf=1;
        else if x_parity=1 and nursedcat_1>0  then lcbf=1;
        else if x_parity>6 then lcbf=.;
        else lcbf=0;
    end;
run;

proc freq data=surveylibro1;
table lcbf*x_parity/missing;
run;



* Create variable nbf of not breast feeding at least one child ;
data surveykarma;
set surveykarma;
    if x_parity>0 then do;
        if nursedcat_1=0 or nursedcat_2=0 or nursedcat_3=0 or nursedcat_4=0 or nursedcat_5=0 or nursedcat_6=0 then nbf=1;
        else nbf=0;
    end;
run;

proc freq data=surveykarma(where=(match_casestatus=1));
table nbf*x_parity;
run;

proc freq data=surveykarma;
table nbf*x_parity;
run;

* Create variable ncbf of number of children breastfed;
data surveykarma;
set surveykarma;
    if nursedcat_1>0  then bf1=1;
    else if nursedcat_1=. then bf1=.;
    else bf1=0;
    if nursedcat_2>0  then bf2=1;
    else if nursedcat_2=. then bf2=.;
    else bf2=0;
    if nursedcat_3>0  then bf3=1;
    else if nursedcat_3=. then bf3=.;
    else bf3=0;
    if nursedcat_4>0  then bf4=1;
    else if nursedcat_4=. then bf4=.;
    else bf4=0;
    if nursedcat_5>0  then bf5=1;
    else if nursedcat_5=. then bf5=.;
    else bf5=0;
    if nursedcat_6>0  then bf6=1;
    else if nursedcat_6=. then bf6=.;
    else bf6=0;
    ncbf=sum(bf1,bf2,bf3,bf4,bf5,bf6);
run;


proc freq data=surveykarma;;
table ncbf*x_parity/missing;
run;

* Create variable fcbf of fraction children breastfed;
data surveykarma;
set surveykarma;
    if 0<x_parity<6 then do;
       fcbf= round(ncbf/x_parity, .01);
    end;
run;


proc freq data=surveykarma(where=(match_casestatus=1));
table fcbf*x_parity/missing;
run;
proc freq data=surveykarma;
table fcbf*x_parity/missing;
run;

proc freq data=surveykarma(where=(ipt_karmaunit="Södersjukhuset"));
table fcbf*x_parity/missing;
run;

proc freq data=surveykarma(where=(match_casestatus=1));
table fcbf*x_parity;
run;
proc freq data=surveykarma;
table fcbf*x_parity;
run;

proc freq data=surveykarma(where=(ipt_karmaunit="Södersjukhuset"));
table fcbf*x_parity;
run;
* 3 % never breastfed any of their children. 92 % breastfed all their children. ;

* Create a new variable of breastfeeding of last born child;
data surveykarma;
set surveykarma;
    if x_parity >0 then do;
        if x_parity=6 and nursedcat_6>0  then lcbf=1;
        else if x_parity=5 and nursedcat_5>0  then lcbf=1;
        else if x_parity=4 and nursedcat_4>0  then lcbf=1;
        else if x_parity=3 and nursedcat_3>0  then lcbf=1;
        else if x_parity=2 and nursedcat_2>0  then lcbf=1;
        else if x_parity=1 and nursedcat_1>0  then lcbf=1;
        else if x_parity>6 then lcbf=.;
        else lcbf=0;
    end;
run;

proc freq data=surveykarma;;
table lcbf*x_parity/missing;
run;


* Create a new variable - breastfeeding before cancer diagnosis ;


proc freq data=surveykarma(where=(match_casestatus=1));
table wom_child_birthyear_1 wom_child_birthyear_2 wom_child_birthyear_3 wom_child_birthyear_4 wom_child_birthyear_5 wom_child_birthyear_6; run;
* Children number 1-4 MAY have been born after cancer diagnosis - total 78 births;
proc freq data=surveylibro1;
table wom_child_birthyear_1 wom_child_birthyear_2 wom_child_birthyear_3 wom_child_birthyear_4 wom_child_birthyear_5 wom_child_birthyear_6; run;
* Children number 1-4 MAY have been born after cancer diagnosis - total 37 births;

proc sql; create table karma as 
          select surveykarma.*,  corekarma.age, corekarma.age_five, corekarma.yearofbirth, corekarma.birth_times, corekarma.age_at_firstbirth, corekarma.postmenopausal, corekarma.postmenopausal_before_bc, corekarma.bc_1stdiagdate, corekarma.brca_mutation, corekarma.european_ancestry_iso, corekarma.menarche_age, 
		  corekarma.hrt_status, corekarma.hrt_status_at_bc, corekarma.bmi, corekarma.contraception_ever, corekarma.bc_1stdiagage, corekarma.alcohol_gram_week, corekarma.smoking_status, corekarma.smoking_status_bc, corekarma.smoking_packyears, corekarma.ipt_karma_unit
          from work.surveykarma A left join rawdata.corekarma B 
          on a.studieid=b.studieid; 
quit;

data karma;
set karma;
    if ipt_karma_unit="Södersjukhuset" then urban=1;
	else urban=0;
run;

* Check for rural vs urban effects on parity;
proc freq data=karma(where=(match_casestatus=1));
table urban*x_parity;
run;
proc freq data=karma;
table urban*x_parity;
run;

* Check for rural vs urban effects on bfcat;
proc freq data=karma(where=(match_casestatus=1));
table urban*bfcat;
run;
proc freq data=karma;
table urban*bfcat;
run;

* Check for rural vs urban effects on nbf;
proc freq data=karma(where=(match_casestatus=1));
table urban*nbf;
run;
proc freq data=karma;
table urban*nbf;
run;

data surveykarma;
set karma;
if bc_1stdiagdate ne . then do;
x_diagyr=year(bc_1stdiagdate);
end;
run;

proc freq data=surveykarma; table x_diagyr/missing;run;

data surveykarma_case;
set surveykarma(where=(match_casestatus=1));
if x_diagyr<=wom_child_birthyear_1 then do;wom_child_birthyear_1=1;end;
if x_diagyr<=wom_child_birthyear_6 then do;wom_child_birthyear_6=1;end;
if x_diagyr<=wom_child_birthyear_5 then do;wom_child_birthyear_5=1;end;
if x_diagyr<=wom_child_birthyear_4 then do;wom_child_birthyear_4=1;end;
if x_diagyr<=wom_child_birthyear_3 then do;wom_child_birthyear_3=1;end;
if x_diagyr<=wom_child_birthyear_2 then do;wom_child_birthyear_2=1;end;
run;

proc freq data=surveykarma_case;
table wom_child_birthyear_1 wom_child_birthyear_2 wom_child_birthyear_3 wom_child_birthyear_4 wom_child_birthyear_5 wom_child_birthyear_6; run;
* only three observations had children born after cancer diagnosis;

data odd; set surveykarma_case; if wom_child_birthyear_1=1 or wom_child_birthyear_2=1 or wom_child_birthyear_3=1 or wom_child_birthyear_4=1 or wom_child_birthyear_5=1; run;


* Check how much they nursed these kids;

proc freq data=odd;
table x_diagyr wom_child_birthyear_1*wom_child_nursed_1 wom_child_birthyear_2*wom_child_nursed_2; run;
* Two were nursed, one was not ;

* Libro1;


proc sql; create table libro1 as 
          select surveylibro1.*, corelibro1.age, corelibro1.age_five, corelibro1.yearofbirth, corelibro1.birth_times, corelibro1.age_at_firstbirth,corelibro1.postmenopausal, corelibro1.postmenopausal_before_bc,  corelibro1.bc_1stdiagdate, corelibro1.brca_mutation, corelibro1.european_ancestry_iso, corelibro1.menarche_age, 
		  corelibro1.hrt_status, corelibro1.hrt_status_at_bc, corelibro1.bmi, corelibro1.contraception_ever, corelibro1.bc_1stdiagage, corelibro1.alcohol_gram_week, corelibro1.smoking_status, corelibro1.smoking_status_bc, corelibro1.smoking_packyears
          from work.surveylibro1 A left join rawdata.corelibro1 B 
          on a.studiepersonid=b.studiepersonid; 
quit;

data surveylibro1;
set libro1;
if bc_1stdiagdate ne . then do;
x_diagyr=year(bc_1stdiagdate);
end;
run;

proc freq data=surveylibro1; table x_diagyr wom_child_birthyear_6;run;

data surveylibro1;
set surveylibro1;
distance1=x_diagyr-wom_child_birthyear_1;
distance2=x_diagyr-wom_child_birthyear_2;
distance3=x_diagyr-wom_child_birthyear_3;
distance4=x_diagyr-wom_child_birthyear_4;
distance6=x_diagyr-wom_child_birthyear_6;
distance5=x_diagyr-wom_child_birthyear_5;
run;

proc freq data=surveylibro1;
table distance1 distance2 distance3 distance4 distance5 distance6;
run;


data surveylibro1;
set surveylibro1;

if .<x_diagyr<=wom_child_birthyear_1 then do;wom_child_birthyear_1=1;end;
if .<x_diagyr<=wom_child_birthyear_6 then do;wom_child_birthyear_6=1;end;
if .<x_diagyr<=wom_child_birthyear_5 then do;wom_child_birthyear_5=1;end;
if .<_diagyr<=wom_child_birthyear_4 then do;wom_child_birthyear_4=1;end;
if .<x_diagyr<=wom_child_birthyear_3 then do;wom_child_birthyear_3=1;end;
if .<x_diagyr<=wom_child_birthyear_2 then do;wom_child_birthyear_2=1;end;
run;

proc freq data=surveylibro1;
table wom_child_birthyear_1 wom_child_birthyear_2 wom_child_birthyear_3 wom_child_birthyear_4 wom_child_birthyear_5 wom_child_birthyear_6; run;


data odd; set surveylibro1; if wom_child_birthyear_1=1 or wom_child_birthyear_2=1 or wom_child_birthyear_3=1 or wom_child_birthyear_4=1 or wom_child_birthyear_5=1 or wom_child_birthyear_6=1; run;
* 14 women had children born same year or after bc_1stdiagdate cancer diagnosis;

proc freq data=odd;
table x_diagyr wom_child_birthyear_1 wom_child_birthyear_2 wom_child_birthyear_3 wom_child_birthyear_4 wom_child_birthyear_5 wom_child_birthyear_6; run;

* Check how much they nursed these kids;

proc freq data=odd;
table x_diagyr wom_child_birthyear_1*wom2_child_nursed_1 wom_child_birthyear_2*wom2_child_nursed_2 wom_child_birthyear_3*wom2_child_nursed_3 wom_child_birthyear_4*wom2_child_nursed_4 wom_child_birthyear_5*wom2_child_nursed_5 wom_child_birthyear_6*wom2_child_nursed_6; run;


*  They seem to have nursed them anyways. 4 out of 14 observations did not nurse the child.;


proc freq data=odd;
table x_diagyr distance1*wom2_child_nursed_1 distance2*wom2_child_nursed_2 distance3*wom2_child_nursed_3 distance4*wom2_child_nursed_4 distance5*wom2_child_nursed_5 distance6*wom2_child_nursed_6; run;




data odd; 
set surveylibro1; 
    if  0<distance1<6 or  0<distance2<6  
    or  0<distance3<6 or  0<distance4<6  
    or  0<distance5<6 or  0<distance6<6; 
run;
* 66 women had children born 1-5 years prior to a bc_1stdiagdate cancer diagnosis;


proc freq data=odd;
table x_diagyr distance1*wom2_child_nursed_1 distance2*wom2_child_nursed_2 distance3*wom2_child_nursed_3 distance4*wom2_child_nursed_4 distance5*wom2_child_nursed_5 distance6*wom2_child_nursed_6; run;

* Only 3 of these 66 wmen did not breastfeed the child; 


* (...) Continuing with the other variables to check;

* Variables on age at first birth;

proc freq data=surveylibro1;
table wom_firstchild_age/missing;
run;

data surveykarma;
set surveykarma;
    if wom_child_birthyear_1 >998 then do;
        wom_firstchild_age=(wom_child_birthyear_1-yearofbirth);
    end;
    else wom_firstchild_age=.;
run;

proc freq data=surveykarma;
table wom_firstchild_age/missing;
run;


data check; 
set surveykarma;
if wom_firstchild_age>50;
run;

proc freq data=surveykarma;
table age_at_firstbirth/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table age_at_firstbirth/missing;
run;

proc freq data=surveylibro1;
table age_at_firstbirth/missing;
run;


proc freq data=surveykarma;
table birth_times/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table birth_times/missing;
run;

proc freq data=surveylibro1;
table birth_times/missing;
run;

* Variables on age at menarche;

proc freq data=surveylibro1;
table wom_mensfirst menarche_age/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table wom_mensfirst menarche_age/missing;
run;
proc freq data=surveykarma;
table wom_mensfirst menarche_age/missing;
run;



* Variables on body shape;

proc freq data=surveylibro1;
table phs_bodyshape_1/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table phs_bodyshape_1/missing;
run;
proc freq data=surveykarma;
table phs_bodyshape_1/missing;
run;


proc freq data=surveylibro1;
table phs_bodyshape_2/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table phs_bodyshape_2/missing;
run;
proc freq data=surveykarma;
table phs_bodyshape_2/missing;
run;


proc freq data=surveylibro1;
table phs_bodyshape_3/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table phs_bodyshape_3/missing;
run;
proc freq data=surveykarma;
table phs_bodyshape_3/missing;
run;
* phs_bodyshape_3 is only available in libro1 - because it represents bodyshape one year before cancer diagnosis 
  and was not included in the prospective karma study;

proc freq data=surveylibro1;
table phs_bodyshape_4/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table phs_bodyshape_4/missing;
run;
proc freq data=surveykarma;
table phs_bodyshape_4/missing;
run;

* Physical activity self-reported ;

proc freq data=surveylibro1;
table phy_lifetime_4 phy_lifetime_5 phy_lifetime_6 phy_lifetime_7/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table phy_lifetime_3 phy_lifetime_2 phy_lifetime_1/missing;
run;
proc freq data=surveykarma;
table phy_lifetime_3 phy_lifetime_2 phy_lifetime_1/missing;
run;


* Hormonal contraceptives ;

* Check core variable contraceptives_ever;

proc freq data=surveylibro1;
table contraception_ever/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table contraception_ever/missing;
run;
proc freq data=surveykarma;
table contraception_ever/missing;
run;

proc freq data=surveylibro1;
table contraception_ever;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table contraception_ever;
run;
proc freq data=surveykarma;
table contraception_ever;
run;


* Education level;

proc freq data=surveykarma;
table soc_education;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table soc_education;
run;
proc freq data=surveylibro1;
table soc_education;
run;
* Appears similar in education level but there are no libro1 women with 5 or 6 (folkskola or realskola);

* Collapse categories 1, 5 and 6 into one category of up to 8-10 years education;
data surveykarma;
set surveykarma;
if soc_education=5 then soc_edu=1;
else if soc_education=6 then soc_edu=1;
else soc_edu=soc_education;
run;

data surveylibro1;
set surveylibro1;
soc_edu=soc_education;
run;

proc freq data=surveykarma;
table soc_education soc_edu;
run;

proc freq data=surveylibro1;
table soc_education_4_other;
run;

proc freq data=surveykarma;
table soc_education_4_other;
run;

proc freq data=surveykarma;
table soc_edu;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table soc_edu;
run;
proc freq data=surveylibro1;
table soc_edu;
run;

* Born country;

proc freq data=surveykarma;
table soc_born soc_born_mother soc_born_father;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table soc_born soc_born_mother soc_born_father;
run;
proc freq data=surveylibro1;
table soc_born soc_born_mother soc_born_father;
run;
* Appears similar for all cases but higher than for controls;

* European ancestry;
proc freq data=surveykarma;
table european_ancestry_iso;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table european_ancestry_iso;
run;
proc freq data=surveylibro1;
table european_ancestry_iso;
run;
* More karma women are of european ancestry. A selection bias;

proc freq data=surveykarma;
table european_ancestry_iso*urban;
run;

proc freq data=surveykarma;
table soc_born*urban;
run;


* HRT use;

proc freq data=surveykarma;
table hrt_status hrt_status_at_bc/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table hrt_status hrt_status_at_bc/missing;
run;
proc freq data=surveylibro1;
table hrt_status hrt_status_at_bc/missing;
run;


proc freq data=surveykarma;
table hrt_status hrt_status_at_bc;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table hrt_status hrt_status_at_bc;
run;
proc freq data=surveylibro1;
table hrt_status hrt_status_at_bc;
run;

proc freq data=surveykarma(where=(match_casestatus=1));
table hrt_status_at_bc*urban;
run;
* I see varying frequencies of usage between libro1 and karma cases. Find out why.;
* There is also some weird lack of data on hrt_status_at_bc which is not there at hrt_status.;

* BMI ;
proc univariate data=surveykarma;
var bmi;
run;
proc univariate data=surveykarma(where=(match_casestatus=1));
var bmi;
run;
proc univariate data=surveylibro1;
var bmi;
run;

* BRCA mutation;

proc freq data=surveykarma;
table brca_mutation/missing;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table brca_mutation/missing;
run;

proc freq data=surveylibro1;
table brca_mutation/missing;
run;
* Very differing percentages of missing data for karma and libro1. Find out why.;
* The question was not included in the first round of the karma survey. Source: Mikael Eriksson. ;

proc freq data=surveykarma;
table brca_mutation;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table brca_mutation;
run;
proc freq data=surveylibro1;
table brca_mutation;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table brca_mutation*urban;
run;

proc univariate data=surveykarma;
var alcohol_gram_week;
run;
proc univariate data=surveykarma(where=(match_casestatus=1));
var alcohol_gram_week;
run;
proc univariate data=surveylibro1;
var alcohol_gram_week;
run;
proc univariate data=surveykarma(where=(urban=1));
var alcohol_gram_week;
run;

proc univariate data=surveykarma(where=(urban=0));
var alcohol_gram_week;
run;

proc freq data=surveykarma;
table smoking_status;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table smoking_status;
run;
proc freq data=surveylibro1;
table smoking_status;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table smoking_status*urban;
run;

proc freq data=surveykarma;
table smoking_status*urban;
run;
proc freq data=surveykarma;
table smoking_status_bc;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table smoking_status_bc;
run;
proc freq data=surveylibro1;
table smoking_status_bc;
run;
proc freq data=surveykarma(where=(match_casestatus=1));
table smoking_status_at_bc*urban;
run;

proc univariate data=surveykarma;
var smoking_packyears;
run;
proc univariate  data=surveykarma(where=(match_casestatus=1));
var smoking_packyears;
run;
proc univariate  data=surveylibro1;
var smoking_packyears;
run;
proc univariate  data=surveykarma(where=(urban=1));
var smoking_packyears;
run;
proc univariate  data=surveykarma(where=(urban=0));
var smoking_packyears;
run;

* Prepare for export: Append the data together ;

data surveylibro1;
set surveylibro1;
studieid=studiepersonid;
match_casestatus=1;
phy_lifetime_3 = .;
phy_lifetime_2 = .;
phy_lifetime_1 = .;
run;

data surveykarma;
set surveykarma;
phy_lifetime_4 = .;
phy_lifetime_5 = .;
phy_lifetime_6 = .;
phy_lifetime_7 = .;
run;

data surveylibro1;
retain studieid match_casestatus matchid age age_five yearofbirth birth_times age_at_firstbirth postmenopausal postmenopausal_before_bc bc_1stdiagage bc_1stdiagdate wom_firstchild_age soc_edu soc_born  soc_education x_famhist_bre x_famhist_ova x_famhist x_parity menarche menarche_age bfcat bfvspar  soc_born_country european_ancestry_iso bmi hrt_status hrt_status_at_bc contraception_ever brca_mutation phs_bodyshape_1 phs_bodyshape_2 phs_bodyshape_4 bf1 bf2 bf3 bf4 bf5 bf6 nbf ncbf fcbf lcbf alcohol_gram_week smoking_status phy_lifetime_7 phy_lifetime_6 phy_lifetime_5  phy_lifetime_4 phy_lifetime_3 phy_lifetime_2 phy_lifetime_1;
set surveylibro1;
keep studieid match_casestatus matchid age age_five yearofbirth birth_times age_at_firstbirth postmenopausal postmenopausal_before_bc bc_1stdiagage bc_1stdiagdate wom_firstchild_age soc_edu soc_born  soc_education x_famhist_bre x_famhist_ova x_famhist x_parity menarche menarche_age bfcat bfvspar  soc_born_country european_ancestry_iso bmi hrt_status hrt_status_at_bc contraception_ever brca_mutation phs_bodyshape_1 phs_bodyshape_2 phs_bodyshape_4 bf1 bf2 bf3 bf4 bf5 bf6 nbf ncbf fcbf lcbf alcohol_gram_week smoking_status phy_lifetime_7 phy_lifetime_6 phy_lifetime_5  phy_lifetime_4 phy_lifetime_3 phy_lifetime_2 phy_lifetime_1;
run;

data surveykarma;
retain studieid match_casestatus matchid age age_five yearofbirth birth_times age_at_firstbirth postmenopausal postmenopausal_before_bc bc_1stdiagage bc_1stdiagdate wom_firstchild_age soc_edu soc_born  soc_education x_famhist_bre x_famhist_ova x_famhist x_parity menarche menarche_age bfcat bfvspar  soc_born_country european_ancestry_iso bmi hrt_status hrt_status_at_bc contraception_ever brca_mutation phs_bodyshape_1 phs_bodyshape_2 phs_bodyshape_4 bf1 bf2 bf3 bf4 bf5 bf6 nbf ncbf fcbf lcbf alcohol_gram_week smoking_status phy_lifetime_7 phy_lifetime_6 phy_lifetime_5  phy_lifetime_4 phy_lifetime_3 phy_lifetime_2 phy_lifetime_1;
set surveykarma;
keep studieid match_casestatus matchid age age_five yearofbirth birth_times age_at_firstbirth postmenopausal postmenopausal_before_bc bc_1stdiagage bc_1stdiagdate wom_firstchild_age soc_edu soc_born  soc_education x_famhist_bre x_famhist_ova x_famhist x_parity menarche menarche_age bfcat bfvspar  soc_born_country european_ancestry_iso bmi hrt_status hrt_status_at_bc contraception_ever brca_mutation phs_bodyshape_1 phs_bodyshape_2 phs_bodyshape_4 bf1 bf2 bf3 bf4 bf5 bf6 nbf ncbf fcbf lcbf alcohol_gram_week smoking_status phy_lifetime_7 phy_lifetime_6 phy_lifetime_5  phy_lifetime_4 phy_lifetime_3 phy_lifetime_2 phy_lifetime_1;
run;

proc sql; create table analysis as 
            select * from work.surveylibro1
			union all
			select * from work.surveykarma;
quit;


* Merge on data on MD;

data md;
set rawdata.tnbc_mammographicdensity;
run;

data md;
set md;
if studieid=. then do;
lopnr=studiepersonid;
end;
else if studieid ne . then do;
lopnr=studieid;
end;
run;

proc sql; create table analysis2 as
          select analysis.*, md.mammography_date, md.stratus_pd_mlo_l, md.stratus_pd_mlo_r, md.stratus_densearea_cm2_mlo_l, md.stratus_densearea_cm2_mlo_r
		  from work.analysis A left join work.md B
          on A.studieid=B.lopnr;
quit;

* Add on sympathy data for benign breast disease;

data libro1_sympathy;
set rawdata.sympathylibro1;
run;

data karma_sympathy;
set rawdata.sympathykarma;
run;


proc sql; create table sympathy1 as select
          corevariables.bc_1stdiagdate, libro1_sympathy.*
		  from offline.corevariables A right join work.libro1_sympathy B
		  on A.studiepersonid = B.studiepersonid;
quit;

proc sql; create table sympathy as select
          corekarma.bc_1stdiagdate, karma_sympathy.*
		  from rawdata.corekarma A right join work.karma_sympathy B
		  on A.studieid = B.studieid;
quit;

data sympathy1;
set sympathy1;
drop studieid;
rename studiepersonid=studieid;
run;
data sympathy1;
set sympathy1;
label studieid="studieid";
format studieid 12.;
run;

proc append base=sympathy data=sympathy1;
run;

data sympathy;
set sympathy;
diagyr=year(bc_1stdiagdate);
run;

* Delete diagnoses same year as cancer diagnoses or post diagnostic for cases, and any diagnoses after 2012 for controls;
data sympathy;
set sympathy;
if match_casestatus=0 then do;
if referral_year<2013;
end;
if match_casestatus=1 then do;
if diagyr = . then delete;
if diagyr<=referral_year then delete;
end;
run;

proc freq data=sympathy;
table histology referral_txt diagnoskod_txt;
run;

proc sort data=sympathy;
by diagnoskod_txt;
run;

* Delete non-morphologically examined samples and normal findings;
data sympathy;
set sympathy;
if diagnoskod ne 'M00020';
if diagnoskod ne 'M00000';
if diagnoskod ne 'M09010';
if diagnoskod ne 'M00120';
if diagnoskod ne 'M0011';
if diagnoskod ne 'M09030';
if diagnoskod ne 'M00100';
if diagnoskod ne 'M00110';
if diagnoskod ne 'M06000';
run;

proc freq data=sympathy;
table diagnoskod;
run;

* Follow division from Guray et al, 2006, of benign breast diseases;
data sympathy;
set sympathy;
* Create variable for inflammatory breast disease;
if diagnoskod = 'M41000' then IBD=1; * inflammation akut;
else if diagnoskod = 'M41' then IBD=1; * inflammation akut;
else if diagnoskod = 'M42000' then IBD=1; * inflammation subakut;
else if diagnoskod = 'M417' then IBD=1; * inflammation akut nektrot;
else if diagnoskod = 'M41700' then IBD=1; * inflammation nektrot akut - abscess;
else if diagnoskod = 'M40000' then IBD=1; * Inflammation ospecifik;
else if diagnoskod = 'M4' then IBD=1; * Inflammation ospecifik;
else if diagnoskod = 'M4046' then IBD=1; * Inflammation ospecifik varig;
else if diagnoskod = 'M421' then IBD=1; * akut och kronisk inflammation;
else if diagnoskod = 'M42100' then IBD=1; * akut och kronisk inflammation;
else if diagnoskod = 'M41740' then IBD=1; * abscess;
else if diagnoskod = 'M43000' then IBD=1; * inflammation kronisk;
else if diagnoskod = 'M43' then IBD=1; * inflammation kronisk;
else if diagnoskod = 'M47700' then IBD=1;
else if diagnoskod = 'M44700' then IBD=1;
else if diagnoskod = 'M44000' then IBD=1; * iflammation granulomatös;
else if diagnoskod = 'M44200' then IBD=1; * iflammation icke-nekrotisk granulomatös;
else if diagnoskod = 'M54110' then IBD=1; * fettvävsnekros;
else if diagnoskod = 'M5411' then IBD=1; * fettvävsnekros;
else if diagnoskod = 'M541' then IBD=1; * fettvävsnekros;
else if diagnoskod = 'M54100' then IBD=1; * fettvävsnekros;
else IBD = 0;
* Create variable for non-proliferative fibrocystic changes;
if diagnoskod = 'M33410' then FCC = 1; *skivepitelcysta;
else if diagnoskod = 'M3341' then FCC = 1; *atherom/skivepitelcysta;
else if diagnoskod = 'M334' then FCC = 1; *cysta;
else if diagnoskod = 'M55400' then FCC = 1; *Calcifications;
else if diagnoskod = 'M3500' then FCC = 1; *Cyst unspecified;
else if diagnoskod =  'M33400' then FCC = 1; *Cyst;
else if diagnoskod = 'M49000' then FCC = 1; *Fibros;
else if diagnoskod = 'M49' then FCC = 1; *Fibros;
else if diagnoskod = 'M74200' then FCC = 1; *Adenos;
else if diagnoskod = 'M74320' then FCC = 1; *Cystic fibromatos sthlm kodning;
else if diagnoskod = 'M7432' then FCC = 1; *Cystic fibroadenos skåne kodning;
else if diagnoskod = 'M74300' then FCC = 1; *Cystic fibroadenosis sthlm kodning;
else if diagnoskod = 'M742' then FCC = 1; *Fibroadenos;
else if diagnoskod = 'M743' then FCC = 1; *Cystic dysplasia/fibroadenosis;
else FCC = 0;
* Create variable for proliferative fibrocystic change non-atypic;
if diagnoskod = 'M80500' then PFCCNA = 1; *Papilloma;
else if diagnoskod = 'M85030' then PFCCNA = 1; *Intraductal Papilloma;
else if diagnoskod = 'M8503' then PFCCNA = 1; *Intraductal Papilloma;
else if diagnoskod = 'M85050' then PFCCNA = 1; *Papillomatosis;
else if diagnoskod = 'M74220' then PFCCNA = 1; *Sclerosing adenosis;
else if diagnoskod = 'M7422' then PFCCNA = 1; *Sclerosing adenosis;
else if diagnoskod =  'M49060' then PFCCNA = 1; *Scar;
else if diagnoskod =  'M49251' then PFCCNA = 1; * Stråligt ärr;
else if diagnoskod = 'M72000' then PFCCNA = 1; *Hyperplasia;
else if diagnoskod = 'M72' then PFCCNA = 1; *Hyperplasia;
else if diagnoskod = 'M72100' then PFCCNA = 1; *Lobular Hyperplasia;
else if diagnoskod = 'M721' then PFCCNA = 1; *Lobular Hyperplasia;
else if diagnoskod = 'M72175' then PFCCNA = 1; *Ductal Hyperplasia;
else if diagnoskod = 'M76080' then PFCCNA = 1; *Epithelial proliferation; 
else PFCCNA = 0;
* Create variable for atypic hyperplasia;
if diagnoskod = 'M69700' then AHP = 1; *Atypia unspecified;
else if diagnoskod = 'M697' then AHP = 1; * Körtelcellsatypi;
else if diagnoskod = 'M69720' then AHP = 1; *Atypia;
else if diagnoskod = 'M69708' then AHP = 1; *Atypia grav;
else if diagnoskod = 'M69707' then AHP = 1; *Atypia måttlig;
else if diagnoskod = 'M69706' then AHP = 1; *Atypia lätt;
else if diagnoskod = 'M72005' then AHP = 1; *Atypical Hyperplasia;
else if diagnoskod = 'M72105' then AHP = 1; *Atypical Loular Hyperplasia;
else if diagnoskod = 'M72200' then AHP = 1; *Atypical Lymphoid Hyperplasia;
else if diagnoskod = 'M76085' then AHP = 1; *Epitelproliferation atypisk;
else AHP = 0;
* Create variable for fibroadenoma;
if diagnoskod = 'M90100' then FIBROADENOMA = 1; *Fibroadenoma;
else if diagnoskod = 'M90200' then FIBROADENOMA = 1; * Giant Fibroadenoma;
else FIBROADENOMA = 0;
* Create variable for Lipoma;
if diagnoskod = 'M88500' then LIPOMA = 1; *Lipoma;
else LIPOMA = 0;
* Create variable for Adenoma;
if diagnoskod = 'M81400' then ADENOMA = 1; *Adenom;
else ADENOMA = 0;
run;


proc freq data=sympathy(where=(ADENOMA=0 & LIPOMA=0 & FIBROADENOMA=0 & AHP=0 & PFCCNA=0 & FCC=0 & IBD=0));
table diagnoskod*diagnoskod_txt;
run;

* Inspect numer of unique observations by referral year and diagnosis;
data sub;
set sympathy;
if ADENOMA=1 OR LIPOMA=1 OR FIBROADENOMA=1 OR AHP=1 OR PFCCNA=1 OR FCC=1 OR IBD=1;
run;

proc sort data=sub NODUPKEY;
by studieid referral_year ADENOMA LIPOMA FIBROADENOMA AHP PFCCNA FCC IBD;
run;
* 2491 unique observations by year, id and diagnosis;

* Create dataset with only variables referral_year diagnosis date ADENOMA LIPOMA FIBROADENOMA AHP PFCCNA FCC IBD;
data sub;
set sub;
keep studieid bc_1stdiagdate referral_year ADENOMA LIPOMA FIBROADENOMA AHP PFCCNA FCC IBD;
run;

* Sum the number of diagnoses in each category by studyid;
data sub;
set sub;
    by studieid; 
    row+1; 
    if first.studieid then row=1; 
    if first.studieid then sumFCC=0;
    sumFCC+FCC;
	if first.studieid then sumLIPOMA=0;
    sumLIPOMA+LIPOMA;
	if first.studieid then sumADENOMA=0;
    sumADENOMA+ADENOMA;
	if first.studieid then sumFIBROADENOMA=0;
    sumFIBROADENOMA+FIBROADENOMA;
	if first.studieid then sumAHP=0; 
	sumAHP+AHP;
	if first.studieid then sumPFCCNA=0; 
	sumPFCCNA+PFCCNA;
	if first.studieid then sumIBD=0;
	sumIBD+IBD;
run;

proc sort data=sub;
    by studieid descending row;
run;

* Keep only one observation per woman;
proc sort data=sub NODUPKEY;
by Studieid;
run;
* 1634 observations ;

proc freq data=sub;
table sumADENOMA sumLIPOMA sumFIBROADENOMA sumAHP sumPFCCNA sumIBD sumFCC/missing;
run;

* Merge to surveydata;
proc sql; create table surveydata as select
          analysis2.*, sub.sumADENOMA, sub.sumLIPOMA, sub.sumFIBROADENOMA, sub.sumAHP, sub.sumPFCCNA, sub.sumIBD, sub.sumFCC
		  from work.analysis2 A left join work.sub B
		  on A.studieid = B.studieid;
quit;

proc freq data=surveydata;
table sumADENOMA sumLIPOMA sumFIBROADENOMA sumAHP sumPFCCNA sumIBD/missing;
run;

data surveydata;
set surveydata;
if sumADENOMA=. then sumADENOMA=0;
else if sumADENOMA>0 then sumADENOMA=1;
if sumFIBROADENOMA=. then sumFIBROADENOMA=0;
else if sumFIBROADENOMA>0 then sumFIBROADENOMA=1;
if sumLIPOMA=. then sumLIPOMA=0;
else if sumLIPOMA>0 then sumLIPOMA=1;
if sumAHP=. then sumAHP=0;
else if sumAHP>0 then sumAHP=1;
if sumPFCCNA=. then sumPFCCNA=0;
else if sumPFCCNA>0 then sumPFCCNA=1;
if sumIBD=. then sumIBD=0;
else if sumIBD>0 then sumIBD=1;
if sumFCC=. then sumFCC=0;
else if sumFCC>0 then sumFCC=1;
run;

proc freq data=surveydata;
table sumADENOMA sumLIPOMA sumFIBROADENOMA sumAHP sumPFCCNA sumIBD/missing;
run;

* Merge on follow-up variables for breast cancer specific survival ;
data libro1;
set surveydata;
if studieid>10000000;
run;

proc sql; create table libro1cases as select
          libro1.*, corevariables.studyentry_date, corevariables.emigration_date, corevariables.deathdate, corevariables.deathcause, corevariables.deathcause_bc, corevariables.deathcause_any
		  from work.libro1 A left join offline.corevariables B
		  on A.studieid = B.studiepersonid;
quit;

/* Export as csv file 
PROC EXPORT DATA= WORK.surveydata 
            OUTFILE= "Z:\PhD\TNBC\Data\Data cleaning\160608_casecontrolsurveydata.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

/* 	Recode Overall survival (OS) correctly */
data libro1cases;
	set libro1cases;
	OS=.;
	IF deathdate ^=. THEN OS=1;
	else IF deathdate =. THEN OS=0;
	BCS=.;
	IF (deathcause=:'C50') THEN BCS=1;
	else if (deathcause^=:'C50') THEN BCS=0;
	label OS="Overall survival";
	label BCS="Breast cancer survival";
run;

data libro1cases;
set libro1cases;
if emigration_date<bc_1stdiagdate then do;
emigration_date=.;
end;
run;

/* Create variable of end of follow-up for Survival: Censoring at death date or 2017-07-01 whichever comes first */
data libro1cases ;
set libro1cases ;
if emigration_date ne . then EndFU=emigration_date;
else if OS=1 then EndFU=deathdate;
else EndFU=INPUT('20170701',yymmdd8.);
format EndFU DATE9.;
run;

/* Export as csv file */
PROC EXPORT DATA= WORK.libro1cases
            OUTFILE= "Z:\PhD\TNBC\Data\Data cleaning\180209_casecontrolsurveydata.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

