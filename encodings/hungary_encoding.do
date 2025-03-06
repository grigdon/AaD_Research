
****HU Data: Do file for pre-processing endorsement experiment data

***March 5, 2025


*Load Data HU

HUDataMOD1.dta 

*********DV preprocessing q10 militias***********

*questions <- c("id", "A10A_1", "A10A_2", "A10A_3", "A10B_1", "A10B_2", "A10B_3")

*label_map <- c("A10A_1" = "Spending on Roma_control",
               "A10B_1" = "Spending on Roma_experiment",
               
			   "A10A_2" = "Euroscepticism_control",
               "A10B_2" = "Euroscepticism_experiment",
               
			   "A10A_3" = "Laxer Gun Control_control",
               "A10B_3" = "Laxer Gun Control_experiment")

*Roma	Some people think that the government of [the Czech Republic] should spend less money on Roma (1-4)		   


*recode the direction (1=4, etc), so 4 is more support (strongly agree) for the policy and 1 and less support (strongly disagree)

*create duplicate (=_reversed) of each policy question (control and experiment) in the ee

*control Roma

codebook A10A_1 

recode A10A_1 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10A_1_control_reversed)

label define A10A_1_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10A_1_control_reversed A10A_1_control_reversed_lbla

codebook A10A_1_control_reversed




*"Spending on Roma_experiment"

codebook A10B_1

recode A10B_1 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10B_1_experiment_reversed)

label define A10B_1_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10B_1_experiment_reversed A10B_1_experiment_reversed_lbla

codebook A10B_1_experiment_reversed


****************

			   
	
* EU control  

codebook A10A_2

recode A10A_2 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10A_2_control_reversed)

label define A10A_2_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10A_2_control_reversed A10A_2_control_reversed_lbla

codebook A10A_2_control_reversed



**EU experiment

codebook A10B_2

recode A10B_2 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10B_2_experiment_reversed)

label define A10B_2_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10B_2_experiment_reversed A10B_2_experiment_reversed_lbla

codebook A10B_2_experiment_reversed


	
*****************


*Laxer Guns - control 
			   
codebook  A10A_3	

recode A10A_3 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10A_3_control_reversed)

label define A10A_3_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10A_3_control_reversed A10A_3_control_reversed_lbla

codebook A10A_3_control_reversed


	
*Laxer Guns exp

codebook A10B_3

recode A10B_3 (1=4) (4=1) (2=3) (3=2) (9=.), gen(A10B_3_experiment_reversed)

label define A10B_3_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values A10B_3_experiment_reversed A10B_3_experiment_reversed_lbla

codebook A10B_3_experiment_reversed


****************end of recoding of DV 
****************	
	
**************
			   
****************
			   
****************
****************


*t-test for differences, independennt t test with unequal variances


ttest A10A_1 == A10B_1, unpaired


ttest A10A_1_control_reversed == A10B_1_experiment_reversed, unpaired


ttest A10A_2_control_reversed == A10B_2_experiment_reversed, unpaired


ttest A10A_3_control_reversed == A10B_3_experiment_reversed, unpaired


* all three t-tests very sig - backlash effect



*********DV2 preprocessing q6 NGOs***********

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

data_hu_vars <- mutate(data_hu,
                  ideology = as.numeric(A3), 
                  gender = as.numeric(gndr),
                  capital =recode(as.numeric(county1),
                                  `99` = 1L,
                                  .default = 0L),
                  finance_security = as.numeric(q406), 
                  age = (2022 - as.numeric(yrbrn)),
                  education = as.numeric(educ),
                  DemPolGrievence = A4_1,
                  PolicyPolGrievence = A4_2,
                  EUPolGrievence = A4_3,
                  EconGrievenceRetro = A5A,
                  EconGrievenceProspInd = A5B,
                  EconGrievenceProspAgg = A5C,
                  NatPride = A7,
                  NativeRights = A9_1,
                  NativeJobs = A9_13,
                  LawOrder = A9_5,
                  Chauvinism = A9_4, 
                  ChristianSchool = A9_3,
                  GayNeighbor = A8_3,
                  GayPartner =  A8_4,
                  ForNeighbor = A8_5,
                  ForPartner = A8_6,
                  Ukraine = A8_7,
                  DemonstrateTrad = A12A,
                  DemonstrateNational = A12B
                  )
				  
				  
	*Remove Income due to missingness
	
				  > ci$variables <- c("Age", "Male", "Education", "Capital", "Conservative ideology", "Individual income", 
+                   "Political grievance (democracy)", "Political grievance (policy)", 
+                   "Economic grievance (retrospective-ind)", "Economic grievance (prospective-ind)", 
+                   "Economic grievance (prospective-agg)", "Native rights", "Native jobs", 
+                   "Demonstrated for national values", "Anti-Gay neighbor", "Anti-Gay family member", 
+                   "Anti-Foreigner neighbor", "Anti-Foreigner partner", "Anti-Ukrainian refugee", 
+                   "Far Right Voter", "Family income", "Religiosity")
*****

*Here we go Hungary

****************************************
SES and demographics
******************************************


codebook gndr

recode gndr (1=2) (2=1), gen(Male)

label define Male_lbla 1 "woman" 2 "male" 

label values Male Male_lbla

codebook Male



* AGE, yrbrn


codebook yrbrn
generate ageraw = 2022-yrbrn
codebook ageraw


recode ageraw (16/26 = 1 "Under 26") (27/36 = 2 "27-36") (37/50 = 3 "37-50") (51/65 = 4 "51-65") (66/max = 5 "65+"), gen(Age)
*label define Age_lbla 1 """
*label values Age Age_lbla
codebook Age


* EDUC, 

 codebook educ
 tab educ
 tab educ, nolabel


recode educ (1=1) (2=1) (3=1) (4=2) (5=2) (6=2) (7=2) (8=3) (9=3) (10=3), gen(Education)
label define Education_lbla 1 "basic educ" 2 "high school with maturita" 3 "higher educ" 
label values Education Education_lbla
tab Education



* Capital - v everything else

codebook reg1
recode reg1 (1=1) (2=0) (3=0) (4=0) (5=0) (6=0) (7=0), gen(Capital)
codebook Capital
label define Capital_lbla 0 "Not Budapest" 1 "Budapest Capital Central Region" 
label values Capital Capital_lbla
codebook Capital


* Ideology (L-R) 

codebook A3
recode A3 (1=5) (2=4) (3=3) (4=2) (5=1) (88=3) (99=3) (.=3), gen(IdeologyLR)
label define IdeologyLR_lbla 1 "def left" 2 "rather left" 3 "middle" 4 "rather right" 5 "def right"
label values IdeologyLR IdeologyLR_lbla
tab IdeologyLR




* Income - lots of missing...can't use

codebook q403a

recode q403a (1=1) (2=2) (3=3) (4=4) (5=5) (6=5) (7=5) (8=5) (9=5), gen(Income)

label define Income_lbla 1 "less than 100,000 HUF" 2 "100-150" 3 "150-200" 4 "200-250" 5 "300+"

label values Income income_lbla

tab Income


* family Income q406 HUN

codebook q406

generate FamIncome = q406 

label define FamIncome_lbla 1 "major fin probs" 2 "some fin probs" 3 "barey make ends meet" 4 "make ends meet" 5 "living without worries"

label values FamIncome FamIncome_lbla

tab FamIncome

recode FamIncome (.=3)



****************************************
Political and economic grievances
******************************************


* DemPolGrievence
codebook A4_1, d



recode A4_1 (1=1) (2=2) (3=4) (4=5) (.=3), gen(DemPolGrievance)

label define DemPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values DemPolGrievance DemPolGrievance_lbla

tab DemPolGrievance


*  PolicyPolGrievance 


codebook A4_2, d

recode A4_2 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(PolicyPolGrievance)

label define PolicyPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values PolicyPolGrievance PolicyPolGrievance_lbla

codebook PolicyPolGrievance, d



* EUPolGrievance  (satisfied or unsatisfied are you with the membership of [X] in the European Union)

codebook A4_3, d

recode A4_3 (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(EUPolGrievance)

label define EUPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values EUPolGrievance EUPolGrievance_lbla

tab EUPolGrievance



* EconGrievanceRetro Q5A rename q5a EconGrievanceRetro In general, when you think about your financial situation compared with two years ago. Are you and your family 


codebook A5A, d

recode A5A (1=1) (2=2) (3=3) (4=4) (5=5) (.=3), gen(EconGrievanceRetro)

label define EconGrievanceRetro_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceRetro EconGrievanceRetro_lbla

codebook EconGrievanceRetro


* EconGrievanceProspInd * rename q5b EconGrievanceProspInd Looking ahead, do you think that two years from now, you will be financially 

codebook A5B, d

recode A5B (1=1) (2=2) (3=3) (4=4) (5=5) (.=3), gen(EconGrievanceProspInd)

label define EconGrievanceProspInd_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspInd EconGrievanceProspInd_lbla

codebook EconGrievanceProspInd, d



* EconGrievanceProspAgg  Looking ahead, do you expect that the economy will be 

codebook A5C, d

recode A5C (1=1) (2=2) (3=3) (4=4) (5=5) (.=3), gen(EconGrievanceProspAgg)

label define EconGrievanceProspAgg_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspAgg EconGrievanceProspAgg_lbla

codebook EconGrievanceProspAgg, d


* EconGrievanceProspMostFams* rename q5d XXXXX Now, let's talk about the country as a whole. Would you say that the financial situation of most families in [the Czech Republic] compared with two years ago is 

codebook A5D, d

recode A5D (1=1) (2=2) (3=3) (4=4) (5=5)  (.=3), gen(EconGrievanceProspMostFams)

label define EconGrievanceProspMostFams_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspMostFams EconGrievanceProspMostFams_lbla

codebook EconGrievanceProspMostFams, d


****************************************
Nationalism and Nativism
******************************************


*NativeRights = Q9A,

codebook A9_1, d 

recode A9_1 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeRights)

label define NativeRights_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeRights NativeRights_lbla

codebook NativeRights, d



* NativeJobs A9_13

codebook A9_13, d

recode A9_13 (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeJobs)

label define NativeJobs_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeJobs NativeJobs_lbla

codebook NativeJobs


* DemonstrateNational = Q12b

codebook A12B, d
                               
recode A12B (1=1) (2=2) (3=3) (4=4) (9=1) (.=1), gen(DemonstrateNational)

label define DemonstrateNational_lbla 1 "never" 2 "Once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateNational DemonstrateNational_lbla

codebook DemonstrateNational, d


****************************************
Xenophobia and Prejudice: Group Boundaries
******************************************

* gay neighbor


codebook A8_3, d

recode A8_3 (1=1) (2=2) (3=4) (4=5) (.=3), gen(GayNeighbor)

label define GayNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayNeighbor GayNeighbor_lbla

codebook GayNeighbor, d


* Gay Fam Member

codebook A8_4, d

recode A8_4 (1=1) (2=2) (3=4) (4=5) (.=3), gen(GayFamily)

label define GayFamily_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayFamily GayFamily_lbla

codebook GayFamily



*  GayLesRights - Gays and lesbians should have the same rights as other citizens - also doesn't need to be recoded



codebook A9_10, d


recode A9_10 (1=1) (2=2) (3=4) (4=5) (.=3), gen(GayLesRights)

label define GayLesRights_lbla 1 "def agree" 2 "rather agree" 3 "not suree" 4 "rather disagree" 5 "def disagree" 

label values GayLesRights GayLesRights_lbla

codebook GayLesRights


* Foreigner Neighbor

codebook A8_5, d

recode A8_5 (1=1) (2=2) (3=4) (4=5) (.=3), gen(ForNeighbor)

label define ForNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForNeighbor ForNeighbor_lbla

codebook ForNeighbor, d


*  Foreigner Partner

codebook A8_6, d

recode A8_6 (1=1) (2=2) (3=4) (4=5) (.=3), gen(ForPartner)

label define ForPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForPartner ForPartner_lbla

codebook ForPartner, d



*  Ukrainian refugee neighbor

codebook A8_7, d

recode A8_7 (1=1) (2=2) (3=4) (4=5) (.=3), gen(Ukraine)

label define Ukraine_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values Ukraine Ukraine_lbla

codebook Ukraine


****************
OTHERS
****************




*Religiosty - On average, how often do you visit a house of worship (such as a church or a building where people pray together). Never, a few times in a year, once a month, a few times a week (1-4).

codebook A11


recode A11 (1=1) (2=2) (3=3) (4=4) (5=5) (.=1), gen(Religiosity)


label define Religiosity_lbla 1 "never" 2 "few times a year" 3 "once per month" 4 "once per week" 5 "few times a week" 

label values Religiosity Religiosity_lbla

codebook Religiosity



*****Voting Party Preferences******************* 

*Far right parties in HU: Jobbik and Fidesz

codebook A2, d

recode A2 (1=1) (2=0) (3=1) (4=0) (5=0) (6=0) (7=0) (11=0) (12=0) (13=0) (88=0) (99=0) (.=0), gen(VoteFarRight)

label define VoteFarRight_lbla 0 "Other parties" 1 "Far right parties voter"  

label values VoteFarRight VoteFarRight_lbla

codebook VoteFarRight

* far FAR right, only jobbik


codebook A2, d

recode A2 (1=0) (2=0) (3=1) (4=0) (5=0) (6=0) (7=0) (11=0) (12=0) (13=0) (88=0) (99=0) (.=0), gen(VoteFarFarRight)

label define VoteFarFarRight_lbla 0 "Other parties" 1 "Far FAR right parties voter"  

label values VoteFarFarRight VoteFarFaright_lbla

codebook VoteFarFarRight, d





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
