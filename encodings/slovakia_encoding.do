

Do file for pre-processing endorsement experiment data


*********DV preprocessing q10 militias***********

#recode the direction (1=4, etc), so 4 is more support (strongly agree) for the policy and 1 and less support (strongly disagree)

#create duplicate (=_reversed) of each policy question (control and experiment) in the ee


*q10c control

recode q10c_control (1=4) (4=1) (2=3) (3=2), gen(q10c_control_reversed)

label define q10c_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10c_control_reversed q10c_control_reversed_lbla

tab q10c_control_reversed

tab q10c_control

*q10c experiment

recode q10c_experiment (1=4) (4=1) (2=3) (3=2), gen(q10c_experiment_reversed)

label define q10c_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10c_experiment_reversed q10c_experiment_reversed_lbla

tab q10c_experiment_reversed

tab q10c_experiment

*q10b control

recode q10b_control (1=4) (4=1) (2=3) (3=2), gen(q10b_control_reversed)

label define q10b_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10b_control_reversed q10b_control_reversed_lbla

tab q10b_control_reversed

tab q10b_control

*q10b experiment

recode q10b_experiment (1=4) (4=1) (2=3) (3=2), gen(q10b_experiment_reversed)

label define q10b_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10b_experiment_reversed q10b_experiment_reversed_lbla

tab q10b_experiment_reversed

tab q10b_experiment


*q10a control

recode q10a_control (1=4) (4=1) (2=3) (3=2), gen(q10a_control_reversed)

label define q10a_control_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10a_control_reversed q10a_control_reversed_lbla

tab q10a_control_reversed

tab q10a_control


*q10a experiment


recode q10a_experiment (1=4) (4=1) (2=3) (3=2), gen(q10a_experiment_reversed)

label define q10a_experiment_reversed_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree"

label values q10a_experiment_reversed q10a_experiment_reversed_lbla

tab q10a_experiment_reversed

tab q10a_experiment



*t-test for differences, independentn t test with unequal variances


ttest q10a_control_reversed == q10a_experiment_reversed, unpaired


ttest q10b_control_reversed == q10b_experiment_reversed, unpaired


ttest q10c_control_reversed == q10c_experiment_reversed, unpaired



*gen index of control and experiment separately


generate q10_control_index = q10a_control_reversed + q10b_control_reversed + q10c_control_reversed

generate q10_experiment_index = q10a_experiment_reversed + q10b_experiment_reversed + q10c_experiment_reversed

ttest q10_c_index == q10_e_index, unpaired


*********DV2 preprocessing q6 NGOs***********


***policy C


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



*********Predictors preprocessing***********

******SES, Demographics and Ideology*********

*rename and recode (if in the right direction) or just rename IVs (if in the right direction already)

*r1 = gender/male

codebook r1
tab r1
sum r1 

recode r1 (1=2) (2=1), gen(male)
label define male_lbla 1 "woman" 2 "male" 
label values male male_lbla
tab male
tab r1

*r2 = age

codebook r2
tab r2
sum r2
generate age = r2
label define age_lbla 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "65+"
label values age age_lbla
tab age
tab r2

*r4 = educ

codebook r4
tab r4
sum r4
generate educ = r4
label define educ_lbla 1 "basic educ" 2 "high school with maturita" 3 "higher educ" 
label values educ educ_lbla
tab educ
tab r4

*r5 = SlovakNationality

codebook r5
recode r5 (1=1) (2=0) (3=0), gen(SlovakNationality)
codebook SlovakNationality
label define SlovakNationality_lbla 0 "Hungarian and Other" 1 "Slovak"  
label values SlovakNationality SlovakNationality_lbla
tab SlovakNationality
tab r5

* r6 Zaraďte sa, prosím, do jednej z nasledujúcich kategórií podľa vášho  (occupation)


* r7a  Celkový počet členov domácnosti (total number of people in household)

* r9 - Hlásite sa k nejakému vierovyznaniu? Ak áno k akému? (which religion)

* r10 A aký je váš rodinný stav?

* r11a Čistý mesačný príjem DOMÁCNOSTI

codebook r11a

recode r11a (1=1) (2=2) (3=3) (4=4) (5=5) (99=3), gen(FAMincome)

label define FAMincome_lbla 1 "up to 800 euro" 2 "800-1200 euro" 3 "1201-1600 euro" 4 "1601-2000 euro" 5 "2001 + euro"

label values FAMincome FAMincome_lbla

tab FAMincome

* r11b recode/rename r11b/income

codebook r11b

recode r11b (0=0) (1=1) (2=2) (3=3) (4=4) (5=5) (99=3), gen(income)

label define income_lbla 0 "none" 1 "up to 300 euro" 2 "300-500 euro" 3 "501-700 euro" 4 "701-900 euro" 5 "900 + euro"

label values income income_lbla

tab income

* r12 size of town

*r13/capital region - Bratislavský v everything else

codebook r13
tab r13
sum r13
recode r13 (1=2) (2=1) (3=1) (4=1) (5=1) (6=1) (7=1) (8=1), gen(capital)
codebook capital
label define capital_lbla 1 "rural" 2 "capital" 
label values capital capital_lbla
tab capital
tab r13

* ideology (L-R)

codebook y1
recode y1 (9=3), gen(ideology)
label define ideology_lbla 1 "def left" 2 "rather left" 3 "middle" 4 "rather right" 5 "def right"
label values ideology ideology_lbla
tab ideology


* ideology (L-C)

codebook q3

recode q3 (9=3), gen(ideologyLC)
label define ideologyLC_lbla 1 "def lib" 2 "rather lib" 3 "middle" 4 "rather cons" 5 "def conserv"
label values ideologyLC ideologyLC_lbla
tab ideologyLC


* ideology (types) y3 - what types of politics do you prefer: social dem, conservative, liberal, progressive, nationalist (5), or don't know (9)
*create nationalist type

codebook y3

recode y3 (1=0) (2=0) (3=0) (4=0) (5=1) (9=0), gen(Nationalist)
codebook Nationalist
label define Nationalist_lbla 0 "Other: conserv, lib, socidem, progressive, DK" 1 "Prefers national(ist) politics, which emphasizes priority and defense of one's own national and its interests"  
label values Nationalist Nationalist_lbla
tab Nationalist

* y4 are you vacinnated against covid?

* e14 what's your view of the EU?


******Attitudes (Political and economic grievances, etc)********


* rename q4a (satisfied or unsatisfied with how democracy functions in X)

codebook q4a

generate DemPolGrievance = q4a

label define DemPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "rather unsat" 4 "very unsat" 

label values DemPolGrievance DemPolGrievance_lbla

tab DemPolGrievance


* rename q4b (satisfied or unsatisfied are you with the way the current government is handling domestic policy issues)

codebook q4b

generate PolicyPolGrievance = q4b

label define PolicyPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "rather unsat" 4 "very unsat" 

label values PolicyPolGrievance PolicyPolGrievance_lbla

codebook PolicyPolGrievance

* rename q4c (satisfied or unsatisfied are you with the membership of [X] in the European Union)

codebook q4c

generate EUPolGrievance = q4c

label define EUPolGrievance_lbla 1 "very sat" 2 "rather sat" 3 "rather unsat" 4 "very unsat" 

label values EUPolGrievance EUPolGrievance_lbla

codebook EUPolGrievance

* rename q5a EconGrievanceRetro In general, when you think about your financial situation compared with two years ago. Are you and your family 


codebook q5a

generate EconGrievanceRetro = q5a

label define EconGrievanceRetro_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceRetro EconGrievanceRetro_lbla

codebook EconGrievanceRetro


* rename q5b EconGrievanceProspInd Looking ahead, do you think that two years from now, you will be financially 

codebook q5b

generate EconGrievanceProspInd = q5b

label define EconGrievanceProspInd_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspInd EconGrievanceProspInd_lbla

codebook EconGrievanceProspInd

* rename q5c EconGrievanceProspAgg Looking ahead, do you expect that the economy will be 

codebook q5c

generate EconGrievanceProspAgg = q5c

label define EconGrievanceProspAgg_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspAgg EconGrievanceProspAgg_lbla

codebook EconGrievanceProspAgg 

* rename q5d XXXXX Now, let's talk about the country as a whole. Would you say that the financial situation of most families in [the Czech Republic] compared with two years ago is 

codebook q5d

generate EconGrievanceProspMostFams = q5d

label define EconGrievanceProspMostFams_lbla 1 "much better" 2 "better" 3 "same" 4 "worse" 5 "much worse"

label values EconGrievanceProspMostFams EconGrievanceProspMostFams_lbla

codebook EconGrievanceProspMostFams


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

codebook q9a 

recode q9a (1=4) (4=1) (2=3) (3=2), gen(NativeRights)

label define NativeRights_lbla 1 "def disagree" 2 "rather disagree" 3 "rather agree" 4 "def agree" 

label values NativeRights NativeRights_lbla

codebook NativeRights

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
