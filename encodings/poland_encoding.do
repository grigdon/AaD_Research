
****PL Data: Do file for pre-processing endorsement experiment data

***March 6, 2025


*Load Data PL

PLDataMOD1.dta

*********DV preprocessing q10 militias***********

*questions <- c("id", "Q10A", "Q10B", "Q10C", "Q10D", "Q10E", "Q10F")

*label_map <- c("Q10A" = "Spending on Roma_control",
               "Q10D" = "Spending on Roma_experiment",
               
			   "Q10B" = "Euroscepticism_control",
               "Q10E" = "Euroscepticism_experiment",
               
			   "Q10C" = "Laxer Gun Control_control",
               "Q10F" = "Laxer Gun Control_experiment")

*Roma	Some people think that the government of [] should spend less money on Roma (1-4)		   


*recode the direction (1=4, etc), so 4 is more support (strongly agree) for the policy and 1 and less support (strongly disagree)

*create duplicate (=_reversed) of each policy question (control and experiment) in the ee

*hard to say, 9, recoded as middle category

*control Roma

codebook Q10A, d 

recode Q10A (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10A_control_reversed)

label define Q10A_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10A_control_reversed Q10A_control_reversed_lbla

codebook Q10A_control_reversed, d


*"Spending on Roma_experiment"

codebook Q10D, d

recode Q10D (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10D_experiment_reversed)

label define Q10D_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10D_experiment_reversed Q10D_experiment_reversed_lbla

codebook Q10D_experiment_reversed


****************

			   
	
* EU control  

codebook Q10B, d

recode Q10B (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10B_control_reversed)

label define Q10B_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10B_control_reversed Q10B_control_reversed_lbla

codebook Q10B_control_reversed, d



**EU experiment

codebook Q10E, d

recode Q10E (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10E_experiment_reversed)

label define Q10E_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10E_experiment_reversed Q10E_experiment_reversed_lbla

codebook Q10E_experiment_reversed, d


	
*****************


*Laxer Guns - control 
			   
codebook  Q10C, d 	

recode Q10C (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10C_control_reversed)

label define Q10C_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10C_control_reversed Q10C_control_reversed_lbla

codebook Q10C_control_reversed, d



	
*Laxer Guns exp

codebook Q10F, d 

recode Q10F (1=5) (2=4) (3=2) (4=1) (9=3), gen(Q10F_experiment_reversed)

label define Q10F_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values Q10F_experiment_reversed Q10F_experiment_reversed_lbla

codebook Q10F_experiment_reversed, d 


****************end of recoding of DV 
****************	
	
**************
			   
****************
			   
****************
****************


*t-test for differences, independennt t test with unequal variances


ttest Q10A == Q10D, unpaired


ttest Q10A_control_reversed == Q10D_experiment_reversed, unpaired


ttest Q10B_control_reversed == Q10D_experiment_reversed, unpaired


ttest Q10C_control_reversed == Q10F_experiment_reversed, unpaired


* first one (roma) marginal backlash; EU huge positive treatment effect (support for militia), guns sig backlash effect  



*********DV2 preprocessing q6 NGOs (not militias, skip for now)***********

*This section for Q6 needs to be redone with proper labels 

*q6c control

codebook q6c_control

recode q6c_control (1=4) (4=1) (2=3) (3=2), gen(q6c_control_reversed)

label define q6c_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6c_control_reversed q6c_control_reversed_lbla

tab q6c_control_reversed

tab q6c_control


*q6c experiment

codebook q6c_experiment

recode q6c_experiment (1=4) (4=1) (2=3) (3=2), gen(q6c_experiment_reversed)

label define q6c_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6c_experiment_reversed q6c_experiment_reversed_lbla

tab q6c_experiment_reversed

tab q6c_experiment


***policy B


codebook q6b_control

recode q6b_control (1=4) (4=1) (2=3) (3=2), gen(q6b_control_reversed)

label define q6b_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6b_control_reversed q6b_control_reversed_lbla

tab q6b_control_reversed

tab q6b_control


*q6c experiment

codebook q6b_experiment

recode q6b_experiment (1=4) (4=1) (2=3) (3=2), gen(q6b_experiment_reversed)

label define q6b_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6b_experiment_reversed q6b_experiment_reversed_lbla

tab q6b_experiment_reversed

tab q6b_experiment


***policy A


codebook q6a_control

recode q6a_control (1=4) (4=1) (2=3) (3=2), gen(q6a_control_reversed)

label define q6a_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6a_control_reversed q6a_control_reversed_lbla

tab q6a_control_reversed

tab q6a_control


*q6c experiment

codebook q6a_experiment

recode q6a_experiment (1=4) (4=1) (2=3) (3=2), gen(q6a_experiment_reversed)

label define q6a_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q6a_experiment_reversed q6a_experiment_reversed_lbla

tab q6a_experiment_reversed

tab q6a_experiment



*t-test for differences, independentn t test with unequal variances


ttest q6a_control_reversed == q6a_experiment_reversed, unpaired


ttest q6b_control_reversed == q6b_experiment_reversed, unpaired


ttest q6c_control_reversed == q6c_experiment_reversed, unpaired



*gen index of control and experiment separately


generate q6_control_index = q6a_control_reversed + q6b_control_reversed + q6c_control_reversed

generate q6_experiment_index = q6a_experiment_reversed + q6b_experiment_reversed + q6c_experiment_reversed

ttest q6_control_index == q6_experiment_index, unpaired

**end section that needs to be redone


*********************************************
*********Predictors preprocessing***********

******SES, Demographics and Ideology*********

*rename and recode (if in the right direction) or just rename IVs (if in the right direction already)


*****just a list of the original variable names in HU to facilitate recoding

# Select relevant columns for analysis
data_hu_vars <-HUDataMOD1 %>%
  select(
    id, Male, Age, Education, Capital, IdeologyLR, FamIncome, DemPolGrievance, PolicyPolGrievance,
    EconGrievanceRetro, EconGrievanceProspInd, EconGrievanceProspAgg, EconGrievanceProspMostFams,
    GayNeighbor, GayFamily, GayLesRights, ForNeighbor, ForPartner, Ukraine,
    NativeJobs, NativeRights, DemonstrateNational, Religiosity, VoteFarRight
  )
				  
				  
	*Remove Income due to missingness
	
				  > ci$variables <- c("Age", "Male", "Education", "Capital", "Conservative ideology", "Individual income", 
+                   "Political grievance (democracy)", "Political grievance (policy)", 
+                   "Economic grievance (retrospective-ind)", "Economic grievance (prospective-ind)", 
+                   "Economic grievance (prospective-agg)", "Native rights", "Native jobs", 
+                   "Demonstrated for national values", "Anti-Gay neighbor", "Anti-Gay family member", 
+                   "Anti-Foreigner neighbor", "Anti-Foreigner partner", "Anti-Ukrainian refugee", 
+                   "Far Right Voter", "Family income", "Religiosity")
**

data_pl_vars <- mutate(data_pl,
                  
                  age = AGE,
                  education = recode(as.integer(EDU),
                                   `1` = 1L,
                                   `2` = 1L,
                                   `3` = 1L,
                                   `4` = 1L,
                                   `5` = 1L,
                                   `7` = 1L,
                                   `6` = 2L,
                                   `8` = 2L,
                                   `9` = 3L,
                                   `10` = 3L,
                                   `11` = 3L,
                                   `12` = 3L),                                 
                  is_capital = recode(as.integer(REG),
                                   `7` = 1L,
                                   .default = 0L),
                  ideology = recode(as.integer(Q03),
                                  `1` = 1L,
                                  `2` = 2L,
                                  `3` = 3L,
                                  `4` = 4L,
                                  `5` = 5L,
                                  `9` = 99L),
                  income = recode(as.integer(PINC),
                                  `1` = 1L,
                                  `2` = 2L,
                                  `3` = 3L,
                                  `4` = 4L,
                                  `5` = 5L,
                                  `7` = 7L,
                                  `8` = 8L,
                                  `9` = 9L,
                                  `10` = 10L,
                                  `11` = 11L,
                                  `12` = 12L,
                                  `13` = 13L,
                                  `14` = 14L,
                                  `15` = 99L,
                                  `16` = 99L),
                  DemPolGrievence = recode(as.integer(Q04_01),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  PolicyPolGrievence = recode(as.integer(Q04_02),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  EUPolGrievence = recode(as.integer(Q04_03),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  EconGrievenceRetro = recode(as.integer(Q05A),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  EconGrievenceProspInd = recode(as.integer(Q05B),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  EconGrievenceProspAgg = recode(as.integer(Q05C),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  NatPride = recode(as.integer(Q07),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  NativeRights = recode(as.integer(Q09A_01),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  NativeJobs = recode(as.integer(Q09A_13),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  DemonstrateNational = recode(as.integer(Q04_03),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  LawOrder = recode(as.integer(Q09A_05),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  Chauvinism = recode(as.integer(Q09A_04),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  ChristianSchool = recode(as.integer(Q09A_03),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  DemonstrateTrad = recode(as.integer(Q12A),
                                          `1` = 1L,
                                          `2` = 2L,
                                          `3` = 3L,
                                          `4` = 4L,
                                          `9` = 99L),
                  GayNeighbor = recode(as.integer(Q08_03),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  GayPartner =  recode(as.integer(Q08_04),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  ForNeighbor = recode(as.integer(Q08_05),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  ForPartner = recode(as.integer(Q08_06),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  Ukraine = recode(as.integer(Q08_07),
                                          `1` = 4L,
                                          `2` = 3L,
                                          `3` = 2L,
                                          `4` = 1L,
                                          `9` = 99L),
                  )





***

*Poland

****************************************
SES and demographics
******************************************


*GENDER

codebook SEX, d

recode SEX (1=1) (2=2), gen(Male)

label define Male_lbla 1 "woman" 2 "male" 

label values Male Male_lbla

codebook Male


* AGE

recode AGE (18/26 = 1 "Under 26") (27/36 = 2 "27-36") (37/50 = 3 "37-50") (51/65 = 4 "51-65") (66/max = 5 "65+"), gen(Age)
*label define Age_lbla 1 """
*label values Age Age_lbla
codebook Age

**************************************



* EDUC, 

codebook EDU, d
 tab EDU
 tab EDU, nolabel

*careful, non linear ordering in original 

recode EDU (1=1) (2=1) (3=3) (4=1) (5=2) (6=3) (7=2) (8=3) (9=3) (10=4) (11=4) (12=4), gen(Education)
label define Education_lbla 1 "basic educ" 2 "high school without maturita" 3 "high school with maturita" 4 "university" 
label values Education Education_lbla
tab Education

                              Education |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
Incomplete elementary or without any sc |          2        0.20        0.20
                      Elementary school |         44        4.40        4.60
          Lower high school (gymnasium) |         30        3.00        7.60
                Basic vocational school |        312       31.20       38.80
Comprehensive high school without a mat |         34        3.40       42.20
Comprehensive high school with a matric |        157       15.70       57.90
Vocational high school without a matric |         78        7.80       65.70
Vocational high school with a matricula |         82        8.20       73.90
     Post-secondary or post-high school |         11        1.10       75.00
University degree in engineering, licen |         53        5.30       80.30
University degree of magister (master's |        168       16.80       97.10
University with a doctorate or a higher |         29        2.90      100.00
----------------------------------------+-----------------------------------
                                  Total |      1,000      100.00

. 
tab EDU, nolabel

  Education |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |          2        0.20        0.20
          2 |         44        4.40        4.60
          3 |         30        3.00        7.60
          4 |        312       31.20       38.80
          5 |         34        3.40       42.20
          6 |        157       15.70       57.90
          7 |         78        7.80       65.70
          8 |         82        8.20       73.90
          9 |         11        1.10       75.00
         10 |         53        5.30       80.30
         11 |        168       16.80       97.10
         12 |         29        2.90      100.00
------------+-----------------------------------
      Total |      1,000      100.00


	  

* Capital - v everything else

codebook REG
recode REG (1=0) (2=0) (3=0) (4=0) (5=0) (6=0) (7=1) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) (14=0) (15=0) (16=0), gen(Capital)
codebook Capital
label define Capital_lbla 0 "Not Warsaw region" 1 "Warsaw Capital Central Region" 
label values Capital Capital_lbla
codebook Capital


* Ideology (L-R) 

codebook  Q03, d 
recode Q03 (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(IdeologyLR)
label define IdeologyLR_lbla 1 "def left" 2 "rather left" 3 "middle" 4 "rather right" 5 "def right"
label values IdeologyLR IdeologyLR_lbla
tab IdeologyLR


* Income -
codebook PINC 

recode PINC (1=1) (2=1) (3=2) (4=2) (5=2) (6=3) (7=3) (8=3) (9=3) (10=4) (11=4) (12=4) (13=5) (14=5) (15=3) (16=3), gen(Income)

label define Income_lbla 1 "up to 1000" 2 "up to 4000" 3 "up to 6000" 4 "to to 9k" 5 "9k+"

label values Income Income_lbla

tab Income


****************************************
Political and economic grievances
******************************************


* DemPolGrievence
codebook Q04_01, d



recode Q04_01 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(DemPolGrievance)

label define DemPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values DemPolGrievance DemPolGrievance_lbla

tab DemPolGrievance



*  PolicyPolGrievance 


codebook Q04_02, d

recode Q04_02 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(PolicyPolGrievance)

label define PolicyPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values PolicyPolGrievance PolicyPolGrievance_lbla

codebook PolicyPolGrievance, d




* EUPolGrievance  (satisfied or unsatisfied are you with the membership of [X] in the European Union)

codebook Q04_03, d

recode Q04_03 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(EUPolGrievance)

label define EUPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values EUPolGrievance EUPolGrievance_lbla

tab EUPolGrievance


****Econ grievance***

* EconGrievanceRetro Q5A rename q5a EconGrievanceRetro In general, when you think about your financial situation compared with two years ago. Are you and your family 


codebook Q05A, d

recode Q05A (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceRetro)

label define EconGrievanceRetro_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceRetro EconGrievanceRetro_lbla

codebook EconGrievanceRetro, d


* EconGrievanceProspInd * rename q5b EconGrievanceProspInd Looking ahead, do you think that two years from now, you will be financially 

codebook Q05B, d

recode Q05B (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceProspInd)

label define EconGrievanceProspInd_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspInd EconGrievanceProspInd_lbla

codebook EconGrievanceProspInd, d


* EconGrievanceProspAgg  Looking ahead, do you expect that the economy will be 

codebook Q05C, d

recode Q05C (1=1) (2=2) (3=3) (4=4) (5=5) (9=3)(.=3), gen(EconGrievanceProspAgg)

label define EconGrievanceProspAgg_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspAgg EconGrievanceProspAgg_lbla

codebook EconGrievanceProspAgg, d


* EconGrievanceProspMostFams* rename q5d XXXXX Now, let's talk about the country as a whole. Would you say that the financial situation of most families in [the Czech Republic] compared with two years ago is 

codebook Q05D, d

recode Q05D (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceProspMostFams)

label define EconGrievanceProspMostFams_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspMostFams EconGrievanceProspMostFams_lbla

codebook EconGrievanceProspMostFams, d


****************************************
Nationalism and Nativism
******************************************


*NativeRights = Q9A,

codebook Q09A_01, d 

recode Q09A_01 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeRights)

label define NativeRights_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeRights NativeRights_lbla

codebook NativeRights, d




* NativeJobs A9_13

codebook Q09A_13, d

recode Q09A_13 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeJobs)

label define NativeJobs_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeJobs NativeJobs_lbla

codebook NativeJobs




* DemonstrateNational = Q12b

codebook Q12B , d
                               
recode Q12B (1=1) (2=2) (3=3) (4=4) (9=1) (.=1), gen(DemonstrateNational)

label define DemonstrateNational_lbla 1 "never" 2 "Once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateNational DemonstrateNational_lbla

codebook DemonstrateNational, d


****************************************
Xenophobia and Prejudice: Group Boundaries
******************************************

* gay neighbor


codebook Q08_03, d

recode Q08_03 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(GayNeighbor)

label define GayNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayNeighbor GayNeighbor_lbla

codebook GayNeighbor, d




* Gay Fam Member

codebook Q08_04, d

recode Q08_04 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(GayFamily)

label define GayFamily_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayFamily GayFamily_lbla

codebook GayFamily



*  GayLesRights - Gays and lesbians should have the same rights as other citizens - also doesn't need to be recoded



codebook Q09A_10, d


recode Q09A_10 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(GayLesRights)

label define GayLesRights_lbla 1 "def agree" 2 "rather agree" 3 "not suree" 4 "rather disagree" 5 "def disagree" 

label values GayLesRights GayLesRights_lbla

codebook GayLesRights

* Foreigner Neighbor

codebook Q08_05, d

recode Q08_05 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(ForNeighbor)

label define ForNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForNeighbor ForNeighbor_lbla

codebook ForNeighbor, d


*  Foreigner Partner

codebook Q08_06, d

recode Q08_06 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(ForPartner)

label define ForPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForPartner ForPartner_lbla

codebook ForPartner, d




*  Ukrainian refugee neighbor

codebook Q08_07, d

recode Q08_07 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(Ukraine)

label define Ukraine_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values Ukraine Ukraine_lbla

codebook Ukraine

*************
OTHERS
***********



*Religiosty - On average, how often do you visit a house of worship (such as a church or a building where people pray together). Never, a few times in a year, once a month, a few times a week (1-4).

codebook Q11


recode Q11 (1=1) (2=2) (3=3) (4=4) (5=5) (9=1) (.=1), gen(Religiosity)


label define Religiosity_lbla 1 "never" 2 "few times a year" 3 "once per month" 4 "once per week" 5 "few times a week" 

label values Religiosity Religiosity_lbla

codebook Religiosity


*****Voting Party Preferences******************* 

*Far right parties in PL, Law justice, kukik and freedom/ind

codebook Q01, d

recode Q01 (1=1) (2=0) (3=0) (4=1) (5=0) (6=0) (7=1) (8=0) (91=0) (98=0) (99=0) (.=0), gen(VoteFarRight)

label define VoteFarRight_lbla 0 "Other parties" 1 "Far right parties voter"  

label values VoteFarRight VoteFarRight_lbla

codebook VoteFarRight

****************************************
END (March 7, 2025)
******************************************







****************
OTHERS
****************




* NB: family Income doesn't seem to exist in the polish data

codebook q406

generate FamIncome = q406 

label define FamIncome_lbla 1 "major fin probs" 2 "some fin probs" 3 "barey make ends meet" 4 "make ends meet" 5 "living without worries"

label values FamIncome FamIncome_lbla

tab FamIncome

recode FamIncome (.=3)

***************


*****Voting Party Preferences******************* 


* far FAR right, narrower def


codebook A2, d

recode A2 (1=0) (2=0) (3=1) (4=0) (5=0) (6=0) (7=0) (11=0) (12=0) (13=0) (88=0) (99=0) (.=0), gen(VoteFarFarRight)

label define VoteFarFarRight_lbla 0 "Other parties" 1 "Far FAR right parties voter"  

label values VoteFarFarRight VoteFarFaright_lbla

codebook VoteFarFarRight, d



    Q01. If the elections to the Polish |
parliament were to be held next Sunday, |
                                   whic |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
Law and Justice [Prawo i Sprawiedliwość |        190       19.00       19.00
 Civic Coalition [Koalicja Obywatelska] |        235       23.50       42.50
                   Lewica [Left Forces] |         63        6.30       48.80
Freedom and Independence Confederation  |         63        6.30       55.10
Polish Peasants' Party [Polskie Stronni |         16        1.60       56.70
         Poland 2050 by Szymon Hołownia |        100       10.00       66.70
                               Kukiz'15 |         15        1.50       68.20
           'Agreement of Jarosław Gowin |          2        0.20       68.40
    Another party | electoral committee |         15        1.50       69.90
                      Refused to answer |        190       19.00       88.90
                        I wouldn't vote |        111       11.10      100.00
----------------------------------------+-----------------------------------
                                  Total |      1,000      100.00

. tab Q01, nolabel

Q01. If the |
  elections |
     to the |
     Polish |
 parliament |
 were to be |
  held next |
    Sunday, |
       whic |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |        190       19.00       19.00
          2 |        235       23.50       42.50
          3 |         63        6.30       48.80
          4 |         63        6.30       55.10
          5 |         16        1.60       56.70
          6 |        100       10.00       66.70
          7 |         15        1.50       68.20
          8 |          2        0.20       68.40
         91 |         15        1.50       69.90
         98 |        190       19.00       88.90
         99 |        111       11.10      100.00
------------+-----------------------------------
      Total |      1,000      100.00

. 



****************************



****************************



****************************



****************************


* the variables below are NOT used in HU for the mitilias
                  
* NationalistPolitics - not available in HU

codebook y3

recode y3 (1=0) (2=0) (3=0) (4=0) (5=1) (9=0), gen(Nationalist)
codebook Nationalist
label define Nationalist_lbla 0 "Other: conserv, lib, socidem, progressive, DK" 1 "Prefers national(ist) politics, which emphasizes priority and defense of one's own national and its interests"  
label values Nationalist Nationalist_lbla
tab Nationalist

*Nat Pride, not included in estimation

* rename q7 Would you say you are very proud, fairly proud, not very proud, not at all proud to be a citizen of [the 

codebook q7

recode q7 (1=4) (4=1) (2=3) (3=2), gen(NatPride)

label define NatPride_lbla 1 "not proud at all" 2 "not very proud" 3 "a bit proud" 4 "very proud"

label values NatPride NatPride_lbla

codebook NatPride


******Attitudes (Prejudice - boundary maintenance)********


*Roma Neighbor

codebook q8a

recode q8a (1=4) (4=1) (2=3) (3=2), gen(RomaNeighbor)

label define RomaNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values RomaNeighbor RomaNeighbor_lbla

codebook RomaNeighbor

*  Roma Partner

codebook q8b

recode q8b (1=4) (4=1) (2=3) (3=2), gen(RomaPartner)

label define RomaPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values RomaPartner RomaPartner_lbla

codebook RomaPartner


* The government should play a bigger role in the economy (1-4)

codebook q9b

recode q9b (1=4) (4=1) (2=3) (3=2), gen(Statism)

label define Statism_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values Statism Statism_lbla

codebook Statism

*not used
* recode/rename  q9c ChristianSchool -  It should be mandatory to learn about the basics of Christian culture in secondary schools.

codebook q9c

recode q9c (1=4) (4=1) (2=3) (3=2), gen(ChristianSchool)

label define ChristianSchool_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values ChristianSchool ChristianSchool_lbla

codebook ChristianSchool

*not used
* recode/rename  q9d MaleChauvinism - previously this was called just Chauvinism - On the whole, men make better political leaders than women do


codebook q9d

recode q9d (1=4) (4=1) (2=3) (3=2), gen(MaleChauvinism)

label define MaleChauvinism_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values MaleChauvinism MaleChauvinism_lbla

codebook MaleChauvinism


*not used
* recode/rename  q9e LawOrder It is better to live in an orderly society where laws are vigorously enforced than to give people too much freedom.

codebook q9e

recode q9e (1=4) (4=1) (2=3) (3=2), gen(LawOrder)

label define LawOrder_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values LawOrder LawOrder_lbla

codebook LawOrder

*not used
* recode/rename  q9f ChurchPolitics The church should play a bigger role in the politics of X country

codebook q9f

recode q9f (1=4) (4=1) (2=3) (3=2), gen(ChurchPolitics)

label define ChurchPolitics_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values ChurchPolitics ChurchPolitics_lbla

codebook ChurchPolitics

*not used
* recode/rename  q9g Abortion

codebook q9g

recode q9g (1=4) (4=1) (2=3) (3=2), gen(Abortion)

label define Abortion_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values Abortion Abortion_lbla

codebook Abortion

*not used
* recode/rename  q9h TradMarriage


codebook q9h

recode q9h (1=4) (4=1) (2=3) (3=2), gen(TradMarriage)

label define TradMarriage_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values TradMarriage TradMarriage_lbla

codebook TradMarriage

*not used
* recode/rename  q9i SexbMarriage - this one does not need to be reverse coded bc disagreeeing with the statement is more conservative (4) - 
* "Individuals should be allowed to have sex before marriage."

codebook q9i

generate SexbMarriage = q9i

label define SexbMarriage_lbla 1 "def agree" 2 "rather agree" 3 "rather disagree" 4 "def disagree" 

label values SexbMarriage SexbMarriage_lbla

codebook SexbMarriage




* recode/rename  q9k ChildHome - back to recoding - A child needs a home with both a father and mother to grow up happily.

codebook q9k

recode q9k (1=4) (4=1) (2=3) (3=2), gen(ChildHome)

label define ChildHome_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values ChildHome ChildHome_lbla

codebook ChildHome


* recode/rename  q9l MaleJobs - When jobs are scarce, men should have more rights to a job than women.

codebook q9l


recode q9l (1=4) (4=1) (2=3) (3=2), gen(MaleJobs)

label define MaleJobs_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values MaleJobs MaleJobs_lbla

codebook MaleJobs





*****

******Behavioral (mobilization, demonstrations and petitions, etc)********

* q12a DemonstrateTrad

codebook q12a

generate DemonstrateTrad = q12a

label define DemonstrateTrad_lbla 1 "no/never" 2 "once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateTrad DemonstrateTrad_lbla

codebook DemonstrateTrad




* q13 PetitionSameSex - need to recode/rename

codebook q13

recode q13 (1=4) (4=1) (2=3) (3=2), gen(PetitionSameSex)

label define PetitionSameSex_lbla 1 "def no" 2 "rather no" 3 "rather yes" 4 "def yes" 

label values PetitionSameSex PetitionSameSex_lbla

codebook PetitionSameSex

* q1 party vote today - code for right wing parties - 3, 4, ,10, 12 - kot, sme, sns, and rep



******Voting Party Preferences********



* q2 party vote PAST - code for right wing parties - 3,4, 10, 11 - sme, kot, sns, and vlast

codebook q2

recode q2 (1=0) (2=0) (3=1) (4=1) (5=0) (6=0) (7=0) (8=0) (9=0) (10=1) (11=1) (12=0) (13=0) (14=0) (15=0) (16=0) (99=0) (100=0), gen(VotePrevFarRight)

label define VotePrevFarRight_lbla 0 "Other parties" 1 "Far right parties voter (PAST)"  

label values VotePrevFarRight VoteFarPrevRight_lbla

codebook VotePrevFarRight

*


******Bonus questions? Other things......********

*mm3_2  internet and social media usage - how often

codebook mm3_2

recode mm3_2 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1), gen(SocialMediaUse)

label define SocialMediaUse_lbla 1 "never" 2 "not often" 3 "2-3 times per month" 4 "once per week" 5 "several times per week" 6 "every day"  

label values SocialMediaUse SocialMediaUse_lbla

codebook SocialMediaUse



****internet usage


codebook mm3_1

recode mm3_1 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1), gen(InternetUse)

label define InternetUse_lbla 1 "never" 2 "not often" 3 "2-3 times per month" 4 "once per week" 5 "several times per week" 6 "every day"  

label values InternetUse InternetUse_lbla

codebook InternetUse

** 
***
*****
*******





      
						   
						   
						   
						   
#####




#####END
