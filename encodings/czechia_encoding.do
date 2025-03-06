
****CZ Data: Do file for pre-processing endorsement experiment data

***March 4, 2025


*Load Data CZ

CZDataMOD1.dta 

*********DV preprocessing q10 militias***********



*label_map <- c("Q10AA" = "Spending on Roma_control",
               "Q10BA" = "Spending on Roma_experiment",
               
			   "Q10AB" = "Euroscepticism_control",
               "Q10BB" = "Euroscepticism_experiment",
               
			   "Q10AC" = "Laxer Gun Control_control",
               "Q10BC" = "Laxer Gun Control_experiment")

*Roma	Some people think that the government of [the Czech Republic] should spend less money on Roma (1-4)		   
codebook Q10AA
codebook Q10BA

*recode the direction (1=4, etc), so 4 is more support (strongly agree) for the policy and 1 and less support (strongly disagree)

*DK = 36 (numeric = 9), delete?

*create duplicate (=_reversed) of each policy question (control and experiment) in the ee

*Q10AA control Roma

recode Q10AA (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10AA_control_reversed)

label define Q10AA_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10AA_control_reversed Q10AA_control_reversed_lbla

tab Q10AA_control_reversed

tab Q10AA


*Q10BA = "Spending on Roma_experiment", DKs are 29, dropped

recode Q10BA (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10BA_experiment_reversed)

label define Q10BA_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10BA_experiment_reversed Q10BA_experiment_reversed_lbla

tab Q10BA_experiment_reversed

tab Q10BA

****************
****************

*recode "Q10AB" = "Euroscepticism_control", Some people think that the sovereignty of [the Czech Republic] is threatened by the European Union. (1-4)
*and "Q10BB" = "Euroscepticism_experiment",
			   
codebook Q10AB
codebook Q10BB
			   
****************
****************
			   
	
*Q10AB control EU , DK 44

recode Q10AB (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10AB_control_reversed)

label define Q10AB_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10AB_control_reversed Q10AB_control_reversed_lbla

tab Q10AB_control_reversed

tab Q10AB	

**Q10BB = EU experiment, DKs are 29, dropped

recode Q10BB (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10BB_experiment_reversed)

label define Q10BB_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10BB_experiment_reversed Q10BB_experiment_reversed_lbla

tab Q10BB_experiment_reversed

tab Q10BB
	
*****************
****************
	
*"Q10AC" = "Laxer Gun Control_control", 20 DKs
*"Q10BC" = "Laxer Gun Control_experiment") 25 DKs

			   
recode Q10AC (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10AC_control_reversed)

label define Q10AC_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10AC_control_reversed Q10AC_control_reversed_lbla

tab Q10AC_control_reversed

tab Q10AC	
	
***Q10BC = exp

recode Q10BC (1=4) (4=1) (2=3) (3=2) (9=.), gen(Q10BC_experiment_reversed)

label define Q10BC_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values Q10BC_experiment_reversed Q10BC_experiment_reversed_lbla

tab Q10BC_experiment_reversed

tab Q10BC



****************
****************	
	
**************
			   



*t-test for differences, independennt t test with unequal variances


ttest Q10AA_control_reversed == Q10BA_experiment_reversed, unpaired


ttest Q10AB_control_reversed == Q10BB_experiment_reversed, unpaired


ttest Q10AC_control_reversed == Q10BC_experiment_reversed, unpaired

* t-tests NOT SIG

*gen index of control and experiment separately

generate Q10AA_control_index = Q10AA_control_reversed + Q10AB_control_reversed + Q10AC_control_reversed

generate Q10BB_experiment_index = Q10BA_experiment_reversed + Q10BB_experiment_reversed + Q10BC_experiment_reversed

ttest Q10AA_control_index == Q10BB_experiment_index, unpaired

*Index NOT SIG

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

*********Predictors preprocessing***********

******SES, Demographics and Ideology*********

*rename and recode (if in the right direction) or just rename IVs (if in the right direction already)

*MALE, IDE_2 is raw age and  = gender, 1=male, 2=female; recode so male is 2 and female is 1

codebook IDE_8
tab IDE_8
sum IDE_8 

recode IDE_8 (1=2) (2=1), gen(Male)

label define Male_lbla 1 "woman" 2 "male" 
label values Male Male_lbla
tab Male
tab IDE_8


* AGE, t_VEK_6 (age category)

codebook t_VEK_6
generate Age = t_VEK_6

label define Age_lbla 1 "15-19" 2 "20-29" 3 "30-39" 4 "40-54" 5 "55-64" 6 "65+"
label values Age Age_lbla
codebook Age


* EDUC, there is raw education VZD and then transformed t_VZD into three categories

codebook t_VZD

generate Education = t_VZD

label define Education_lbla 1 "basic educ" 2 "high school with maturita" 3 "higher educ" 
label values Education Education_lbla
tab Education
tab t_VZD


* Capital - KRAJ r13/capital region - Praha hl m v everything else

codebook KRAJ

recode KRAJ (1=1) (2=0) (3=0) (4=0) (5=0) (6=0) (7=0) (8=0) (9=0) (10=0) (11=0) (12=0) (13=0) (14=0), gen(Capital)
codebook Capital
label define Capital_lbla 0 "Not Prague" 1 "Capital Prague" 
label values Capital Capital_lbla
tab Capital


* Ideology (L-R) t_lepra1 (5 cats) and t_lepra2 (3 ctas)

codebook t_lepra1
recode t_lepra1 (1=1) (2=2) (3=3) (4=4) (5=5) (99=3) (.=3), gen(IdeologyLR)
label define IdeologyLR_lbla 1 "def left" 2 "rather left" 3 "middle" 4 "rather right" 5 "def right"
label values IdeologyLR IdeologyLR_lbla
tab IdeologyLR


* Income t_IDE_10a recode/rename income

codebook t_IDE_10a

recode t_IDE_10a (1=1) (2=1) (3=2) (4=2) (5=2) (6=3) (7=3) (8=3) (9=4) (10=4) (11=4) (12=4) (13=5) (14=5) (77=1) (88=4) , gen(Income)

recode Income (.=3)

label define Income_lbla 1 "up tp 4k" 2 "3,4,5" 3 "6,7,8" 4 "9,10,11,12" 5 "13,14"

label values Income income_lbla

tab Income


******Attitudes (Political and economic grievances, etc)********

* rename (satisfied or unsatisfied with how democracy functions in X)
*recode Dk and missing as new middle category


* DemPolGrievence = Q4A
codebook Q4A

recode Q4A (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(DemPolGrievance)

label define DemPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values DemPolGrievance DemPolGrievance_lbla

tab DemPolGrievance


*  Q4B PolicyPolGrievance =

codebook Q4B

recode Q4B (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(PolicyPolGrievance)

label define PolicyPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values PolicyPolGrievance PolicyPolGrievance_lbla

tab PolicyPolGrievance

* EUPolGrievance Q4C * rename q4c (satisfied or unsatisfied are you with the membership of [X] in the European Union)

codebook Q4C

recode Q4C (1=1) (2=2) (3=4) (4=5) (9=3) (.=3), gen(EUPolGrievance)

label define EUPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "not sure" 4 "rather unsat" 5 "very unsat" 

label values EUPolGrievance EUPolGrievance_lbla

tab EUPolGrievance

*

* EconGrievanceRetro Q5A rename q5a EconGrievanceRetro In general, when you think about your financial situation compared with two years ago. Are you and your family 


codebook Q5A

recode Q5A (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceRetro)

label define EconGrievanceRetro_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceRetro EconGrievanceRetro_lbla

codebook EconGrievanceRetro

* EconGrievanceProspInd * rename q5b EconGrievanceProspInd Looking ahead, do you think that two years from now, you will be financially 

codebook Q5B

recode Q5B (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceProspInd)

label define EconGrievanceProspInd_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspInd EconGrievanceProspInd_lbla

codebook EconGrievanceProspInd

* EconGrievanceProspAgg * rename q5c  Looking ahead, do you expect that the economy will be 

codebook Q5C

recode Q5C (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceProspAgg)

label define EconGrievanceProspAgg_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspAgg EconGrievanceProspAgg_lbla

codebook EconGrievanceProspAgg 


* EconGrievanceProspMostFams* rename q5d XXXXX Now, let's talk about the country as a whole. Would you say that the financial situation of most families in [the Czech Republic] compared with two years ago is 

codebook Q5D

recode Q5D (1=1) (2=2) (3=3) (4=4) (5=5) (9=3) (.=3), gen(EconGrievanceProspMostFams)

label define EconGrievanceProspMostFams_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspMostFams EconGrievanceProspMostFams_lbla

codebook EconGrievanceProspMostFams


**NATIVISM/NATIONALISM****

*NativeRights = Q9A,

codebook Q9A 

recode Q9A (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeRights)

label define NativeRights_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeRights NativeRights_lbla

codebook NativeRights
	
* NativeJobs = Q9M

codebook Q9M

recode Q9M (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(NativeJobs)

label define NativeJobs_lbla 1 "def disagree" 2 "rather disagree" 3 "not sure" 4 "rather agree" 5 "def agree"

label values NativeJobs NativeJobs_lbla

codebook NativeJobs


* DemonstrateNational = Q12b

codebook Q12b
                               
recode Q12b (1=1) (2=2) (3=3) (4=4) (9=1) (.=1), gen(DemonstrateNational)

label define DemonstrateNational_lbla 1 "never" 2 "Once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateNational DemonstrateNational_lbla

codebook DemonstrateNational


****************************************
Xenophobia and Prejudice: Group Boundaries
******************************************


* recode/rename q8c

codebook Q8C

recode Q8C (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(GayNeighbor)

label define GayNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayNeighbor GayNeighbor_lbla

codebook GayNeighbor

* recode/rename q8d Gay Fam Member

codebook Q8D

recode Q8D (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(GayFamily)

label define GayFamily_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values GayFamily GayFamily_lbla

codebook GayFamily


* recode/rename q8e Foreigner Neighbor

codebook Q8E

recode Q8E (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(ForNeighbor)

label define ForNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForNeighbor ForNeighbor_lbla

codebook ForNeighbor



* recode/rename q8f Foreigner Partner

codebook Q8F

recode Q8F (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(ForPartner)

label define ForPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values ForPartner ForPartner_lbla

codebook ForPartner

* ecode/rename q8g Ukrainian refugee neighbor

codebook Q8G

recode Q8G (1=5) (2=4) (3=2) (4=1) (9=3) (.=3), gen(Ukraine)

label define Ukraine_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "not sure" 4 "would probably mind" 5 "would certainly mind" 

label values Ukraine Ukraine_lbla

codebook Ukraine

********************

         
*Religiosty - On average, how often do you visit a house of worship (such as a church or a building where people pray together). Never, a few times in a year, once a month, a few times a week (1-4).

codebook Q11


recode Q11 (1=1) (2=2) (3=3) (4=4) (5=5) (9=1) (.=1), gen(Religiosity)


label define Religiosity_lbla 1 "never" 2 "few times a year" 3 "once per month" 4 "once per week" 5 "few times a week" 

label values Religiosity Religiosity_lbla

codebook Religiosity

***

codebook IDE_10

recode IDE_10 (1=1) (2=2) (3=3) (4=4) (5=5) (99=3), gen(FAMincome)

recode FamIncome (.=3)

label define FAMincome_lbla 1 "up to 800 euro" 2 "800-1200 euro" 3 "1201-1600 euro" 4 "1601-2000 euro" 5 "2001 + euro"

label values FAMincome FAMincome_lbla

tab FAMincome


***
* family Income t_IDE_10a recode/rename income

codebook t_IDE_10

recode t_IDE_10 (1=1) (2=1) (3=2) (4=2) (5=2) (6=3) (7=3) (8=3) (9=4) (10=4) (11=4) (12=4) (13=5) (14=5) (88=4) (99=4), gen(FamIncome)

label define FamIncome_lbla 1 "1,2" 2 "3,4,5" 3 "6,7,8" 4 "9,10,11,12" 5 "13,14"

label values FamIncome FamIncome_lbla

tab FamIncome


*living standard of household

codebook IDE_1


recode IDE_1 (1=5) (2=4) (3=3) (4=2) (5=1) (9=3) (.=3), gen(FamIncome2)

label define FamIncome2_lbla 1 "very bad" 2 "rather bad" 3 "so so" 4 "rather good" 5 "very good"

label values FamIncome2 FamIncome2_lbla

tab FamIncome2

*another option is: EU_6



*****Voting Party Preferences******************* Far right parties in CZ are: Spd (8), Prisaha (10) and trikolora (15)

codebook PV_4

recode PV_4 (1=0) (2=0) (3=0) (4=0) (5=0) (6=0) (7=0) (8=1) (9=0) (10=1) (11=0) (12=0) (13=0) (14=0) (15=1) (16=0) (95=0) (96=0) (99=0) (.=0), gen(VoteFarRight)

label define VoteFarRight_lbla 0 "Other parties" 1 "Far right parties voter"  

label values VoteFarRight VoteFarRight_lbla

codebook VoteFarRight
			  
*********

--------------------END---------------------------------

ci$variables <- c("Age", "Male", "Education", "Capital", "Conservative ideology", "Individual income", "Political grievance (democracy)", 
                  "Political grievance (policy)", "Economic grievance (retrospective-ind)", "Economic grievance (prospective-ind)", 
                  "Economic grievance (prospective-agg)", "Native rights", "Native jobs", "Demonstrated for national values", 
                  "Anti-Gay neighbor", "Anti-Gay family member", "Anti-Foreigner neighbor", "Anti-Foreigner partner", 
                  "Anti-Ukrainian refugee", "Far Right Voter", "Prefers Nationalist Politics", "Family income", "Religiosity")


***************************
****************************
codebook r11a

recode r11a (1=1) (2=2) (3=3) (4=4) (5=5) (99=3), gen(FAMincome)

label define FAMincome_lbla 1 "up to 800 euro" 2 "800-1200 euro" 3 "1201-1600 euro" 4 "1601-2000 euro" 5 "2001 + euro"

label values FAMincome FAMincome_lbla

tab FAMincome

codebook y3

recode y3 (1=0) (2=0) (3=0) (4=0) (5=1) (9=0), gen(Nationalist)
codebook Nationalist
label define Nationalist_lbla 0 "Other: conserv, lib, socidem, progressive, DK" 1 "Prefers national(ist) politics, which emphasizes priority and defense of one's own national and its interests"  
label values Nationalist Nationalist_lbla
tab Nationalist

* y4 are you vacinnated against covid?

* e14 what's your view of the EU?





* rename q7 Would you say you are very proud, fairly proud, not very proud, not at all proud to be a citizen of [the 

codebook q7

recode q7 (1=4) (4=1) (2=3) (3=2), gen(NatPride)

label define NatPride_lbla 1 "not proud at all" 2 "not very proud" 3 "a bit proud" 4 "very proud"

label values NatPride NatPride_lbla

codebook NatPride



******Attitudes (Prejudice - boundary maintenance)********

* rename q8 a-g

* rename q8a Roma Neighbor

codebook q8a

recode q8a (1=4) (4=1) (2=3) (3=2), gen(RomaNeighbor)

label define RomaNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values RomaNeighbor RomaNeighbor_lbla

codebook RomaNeighbor

* recode/rename q8b Roma Partner

codebook q8b

recode q8b (1=4) (4=1) (2=3) (3=2), gen(RomaPartner)

label define RomaPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values RomaPartner RomaPartner_lbla

codebook RomaPartner

* recode/rename q8c

codebook q8c

recode q8c (1=4) (4=1) (2=3) (3=2), gen(GayNeighbor)

label define GayNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values GayNeighbor GayNeighbor_lbla

codebook GayNeighbor

* recode/rename q8d Gay Fam Member

codebook q8d

recode q8d (1=4) (4=1) (2=3) (3=2), gen(GayFamily)

label define GayFamily_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values GayFamily GayFamily_lbla

codebook GayFamily

* recode/rename q8e Foreigner Neighbor

codebook q8e

recode q8e (1=4) (4=1) (2=3) (3=2), gen(ForNeighbor)

label define ForNeighbor_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values ForNeighbor ForNeighbor_lbla

codebook ForNeighbor

* recode/rename q8f Foreigner Partner

codebook q8f

recode q8f (1=4) (4=1) (2=3) (3=2), gen(ForPartner)

label define ForPartner_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values ForPartner ForPartner_lbla

codebook ForPartner

* recode/rename q8g Ukrainian refugee neighbor

codebook q8g

recode q8g (1=4) (4=1) (2=3) (3=2), gen(Ukraine)

label define Ukraine_lbla 1 "would certainly NOT bother me" 2 "would probably NOT mind" 3 "would probably mind" 4 "would certainly mind" 

label values Ukraine Ukraine_lbla

codebook Ukraine

*** Questions 9

* recode/rename q9a NativeRights The government of [the Czech Republic] should always defend the rights of [Czechs], even if it means limiting violating the rights of other groups. (1-4)


******Attitudes (Nativism and traditionalism, etc)********



* recode/rename q9b The government should play a bigger role in the economy (1-4)

codebook q9b

recode q9b (1=4) (4=1) (2=3) (3=2), gen(Statism)

label define Statism_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values Statism Statism_lbla

codebook Statism

* recode/rename  q9c ChristianSchool -  It should be mandatory to learn about the basics of Christian culture in secondary schools.

codebook q9c

recode q9c (1=4) (4=1) (2=3) (3=2), gen(ChristianSchool)

label define ChristianSchool_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values ChristianSchool ChristianSchool_lbla

codebook ChristianSchool


* recode/rename  q9d MaleChauvinism - previously this was called just Chauvinism - On the whole, men make better political leaders than women do


codebook q9d

recode q9d (1=4) (4=1) (2=3) (3=2), gen(MaleChauvinism)

label define MaleChauvinism_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values MaleChauvinism MaleChauvinism_lbla

codebook MaleChauvinism



* recode/rename  q9e LawOrder It is better to live in an orderly society where laws are vigorously enforced than to give people too much freedom.

codebook q9e

recode q9e (1=4) (4=1) (2=3) (3=2), gen(LawOrder)

label define LawOrder_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values LawOrder LawOrder_lbla

codebook LawOrder

* recode/rename  q9f ChurchPolitics The church should play a bigger role in the politics of X country

codebook q9f

recode q9f (1=4) (4=1) (2=3) (3=2), gen(ChurchPolitics)

label define ChurchPolitics_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values ChurchPolitics ChurchPolitics_lbla

codebook ChurchPolitics

* recode/rename  q9g Abortion

codebook q9g

recode q9g (1=4) (4=1) (2=3) (3=2), gen(Abortion)

label define Abortion_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values Abortion Abortion_lbla

codebook Abortion

* recode/rename  q9h TradMarriage


codebook q9h

recode q9h (1=4) (4=1) (2=3) (3=2), gen(TradMarriage)

label define TradMarriage_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values TradMarriage TradMarriage_lbla

codebook TradMarriage


* recode/rename  q9i SexbMarriage - this one does not need to be reverse coded bc disagreeeing with the statement is more conservative (4) - 
* "Individuals should be allowed to have sex before marriage."

codebook q9i

generate SexbMarriage = q9i

label define SexbMarriage_lbla 1 "def agree" 2 "rather agree" 3 "rather disagree" 4 "def disagree" 

label values SexbMarriage SexbMarriage_lbla

codebook SexbMarriage


* recode/rename  q9j GayLesRights - new name - old GayRights - Gays and lesbians should have the same rights as other citizens - also doesn't need to be recoded


codebook q9j

generate GayLesRights = q9j

label define GayLesRights_lbla 1 "def agree" 2 "rather agree" 3 "rather disagree" 4 "def disagree" 

label values GayLesRights GayLesRights_lbla

codebook GayLesRights


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

* recode/rename  q9m NativeJobs - When jobs are scarce, citizens of [the Czech Republic] should have more rights to a job than immigrants.


codebook q9m

recode q9m (1=4) (4=1) (2=3) (3=2), gen(NativeJobs)

label define NativeJobs_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values NativeJobs NativeJobs_lbla

codebook NativeJobs


*******
        
*Religiosty - On average, how often do you visit a house of worship (such as a church or a building where people pray together). Never, a few times in a year, once a month, a few times a week (1-4).

codebook q11

generate Religiosity = q11

label define Religiosity_lbla 1 "never" 2 "few times a year" 3 "once per month" 4 "once per week" 5 "few times a week" 

label values Religiosity Religiosity_lbla

codebook Religiosity

*****

******Behavioral (mobilization, demonstrations and petitions, etc)********

* q12a DemonstrateTrad

codebook q12a

generate DemonstrateTrad = q12a

label define DemonstrateTrad_lbla 1 "no/never" 2 "once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateTrad DemonstrateTrad_lbla

codebook DemonstrateTrad


* q12b DemonstrateNational

codebook q12b

generate DemonstrateNational = q12b

label define DemonstrateNational_lbla 1 "no/never" 2 "once" 3 "2-3 times" 4 "3+ times" 

label values DemonstrateNational DemonstrateNational_lbla

codebook DemonstrateNational

* q13 PetitionSameSex - need to recode/rename

codebook q13

recode q13 (1=4) (4=1) (2=3) (3=2), gen(PetitionSameSex)

label define PetitionSameSex_lbla 1 "def no" 2 "rather no" 3 "rather yes" 4 "def yes" 

label values PetitionSameSex PetitionSameSex_lbla

codebook PetitionSameSex

* q1 party vote today - code for right wing parties - 3, 4, ,10, 12 - kot, sme, sns, and rep



******Voting Party Preferences********

codebook q1

label list labels1

label drop VoteFarRight_lbla

drop VoteFarRight

*

recode q1 (1=0) (2=0) (3=1) (4=1) (5=0) (6=0) (7=0) (8=0) (9=0) (10=1) (11=0) (12=1) (13=0) (14=0) (15=0) (16=0) (99=0) (100=0), gen(VoteFarRight)

label define VoteFarRight_lbla 0 "Other parties" 1 "Far right parties voter"  

label values VoteFarRight VoteFarRight_lbla

codebook VoteFarRight

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
