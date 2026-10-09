*** Replication for "The Micro-Task Market for Lemons"
*** Doug Ahler, Carrie Roush, & Gaurav Sood
*** June 2020 survey analysis

** Set WD
cd "~/Dropbox/August2018_TurkExperiments/replication_public/data"

** Load data
insheet using "turk_07_12_2020/merged_survey_ip_07_12_2020_final_public.csv", clear names

** Generating dummies for various indicators of low quality responding from IPs
*these tabs produce the numbers found in row 3 of Table 3

gen miss=1 if missing_ip=="TRUE"
replace miss=0 if missing_ip=="FALSE"
tab miss
*0%

gen dup=1 if duplicated=="TRUE"
replace dup=0 if duplicated=="FALSE"
tab dup
*3.18%

gen foreign=1 if foreign_ip=="TRUE"
replace foreign=0 if foreign_ip=="FALSE"
tab foreign
*0.73%

*all suspicious IPs
gen funny=1 if funny_ip=="TRUE"
replace funny=0 if funny_ip=="FALSE"
tab funny
*9.78%

**Generating dummies for low-incidence screener questions
*these tabs produce the numbers found in Figure 1 

gen troll_prosthetic=1 if prosthetic=="TRUE"
replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
tab troll_prosthetic
*20.05%

gen troll_blind=1 if blind=="TRUE"
replace troll_blind=0 if blind=="FALSE"|blind=="NA"
tab troll_blind
*17.36%

gen troll_deaf=1 if deaf=="TRUE"
replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
tab troll_deaf
*16.14%

gen troll_gang=1 if gang_resp=="TRUE"
replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
tab troll_gang
*20.29%
	
gen troll_famgang=1 if gang_fam=="TRUE"
replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
tab troll_famgang
*20.29%

gen troll_sleep=1 if sleep=="1"
replace troll_sleep=0 if sleep=="0"|sleep=="NA"
tab troll_sleep
*1.47%

**Two or more rare behaviors/traits

egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
tab troll_index
gen likely_troll=1 if troll_index>1
replace likely_troll=0 if troll_index<2
tab likely_troll
*21.03%

** Generate self-reported sincerity measure

tab sincerity
replace sincerity ="0" if sincerity == "NA"
destring sincerity, replace
recode sincerity (1/3 = 0)(4/5 = 1)
tab sincerity
tab sincerity likely_troll, col chi
	
******************
***TIMING STUFF***
******************

rename durationinseconds time
destring time, replace

sum time, d
*median response time = 322 seconds, or about 5 minutes and 37 seconds

*generating outlier variables based on "time outside whiskers" in the box plot
*anything outside 167% of the IQR gets classified as "fast" or "slow"
*25th percentile = 216 ; 75th percentile =   501    
	
display (322 - 216) * (5/3) /* 176.66667 */
gen fast = 0
replace fast = 1 if time <= 176.66667
tab fast
*14.25% are fast
	
display (501 - 322) * (5/3) + 501 /* 799.33333*/
gen slow = 0
replace slow = 1 if time > 799.33333
tab slow
*8.60% are slow 

**********
***DATE***
**********

*how many people reasonably got the date correct?
gen date_ok = 0
replace date_ok = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20"
tab date_ok
*20.54% wrote the date incorrectly

*what about people possibly taking it from another country? (that is, date/month/|year format)
gen date_poss_foreign = 0
replace date_poss_foreign = 1 if date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
tab date_poss_foreign
*16.38% of sample could possibly be foreigners

*okay, what's the estimate of inattentive or trolling respondents? (that is, anyone who didn't do one of the above)
gen correct_date = 0
replace correct_date = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20" | date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
tab correct_date
*okay, so 95.84% of people wrote SOME reasonably correct date

*okay, how many people wrote a nonsensical response to the date question?
gen inattentive = 1
replace inattentive = 0 if date_ok==1|date_poss_foreign==1
tab inattentive
*4.16% of respondents potentially inattentive

************************************************************
***CREATING UPPER AND LOWER BOUND ESTIMATES OF BAD ACTORS***
************************************************************

*creating a variable that combines trolling and weird IPs ("lower bound")
gen combined_troll_1=.
replace combined_troll_1 = 1 if funny_ip == "TRUE" | likely_troll == 1
replace combined_troll_1 = 0 if funny_ip == "FALSE" & likely_troll == 0
tab combined_troll_1
*okay, so 27.63% of data is suspicious using this measure - row 1, column 3 of Table 4

*creating a variable that combines trolling, weird IP, and weirdly written date 
gen combined_troll_2=.
replace combined_troll_2=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1
replace combined_troll_2=0 if funny_ip=="FALSE" & likely_troll==0 & date_poss_foreign==0
tab combined_troll_2
*35.21% of respondents are suspicious using this measure - row 2, column 3 of Table 4

*creating a variable that combines trolling, weird IP, weirdly written date, or inattentive (nonsensical date write-in)
gen combined_troll_3=.
replace combined_troll_3=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1|inattentive==1
replace combined_troll_3=0 if funny_ip=="FALSE"  & likely_troll==0 & date_poss_foreign==0 & inattentive==0
tab combined_troll_3
*35.21% - row 3, column 3 of Table 4

*******************
***HIT THRESHOLD***
*******************

gen hits_recode=.
replace hits_recode=1 if hits=="1"
replace hits_recode=2 if hits=="2"
replace hits_recode=3 if hits=="3"
replace hits_recode=4 if hits=="4"
tab hits_recode 
*this tab provides the numbers for row 1 in Table 5 (July 2020 study)

label define hits_l 1 "Fewer than 100 HITs" 2 "Between 100 and 500 HITs" 3 "Between 500 and 1000 HITs" 4 "More than 1k HITs", replace
label values hits_recode hits_l
tab hits_recode

tab combined_troll_1 hits_recode, col
*this tab provides the numbers for row 2 in Table 5 (July 2020 study)
tab combined_troll_2 hits_recode, col
*this tab provides the numbers for row 3 in Table 5 (July 2020 study) 
tab combined_troll_3 hits_recode, col
*this tab provides the numbers for row 4 in Table 5 (July 2020 study)
tab sincerity hits_recode, col
*this tab provides the numbers for row 5 in Table 5 (July 2020 study)
sum time if hits_recode==1
sum time if hits_recode==2
sum time if hits_recode==3
sum time if hits_recode==4
*these sums provide the numbers for row 6 in Table 5 (July 2020 study)
