*** Replication for "The Micro-Task Market for Lemons"
*** Doug Ahler, Carrie Roush, & Gaurav Sood
*** June 2020 survey analysis

** Set WD
cd "~/Dropbox/August2018_TurkExperiments/replication_public/data"

** Load data
insheet using "turk_06_29_2020/merged_survey_ip_06_29_2020_final_public.csv", clear names

** Generating dummies for various indicators of low quality responding from IPs
**These tabs also make up the numbers in row 2 of Table 3

gen miss=1 if missing_ip=="TRUE"
replace miss=0 if missing_ip=="FALSE"
tab miss
*0%

gen dup=1 if duplicated=="TRUE"
replace dup=0 if duplicated=="FALSE"
tab dup 
*5.3%

gen foreign=1 if foreign_ip=="TRUE"
replace foreign=0 if foreign_ip=="FALSE"
tab foreign
*1.13%

gen funny=1 if funny_ip=="TRUE"
replace funny=0 if funny_ip=="FALSE"
tab funny
*12.09% - % of bad actors based on bad IPs - noted in manuscript

**Generating dummies for low-incidence screener questions
**The numbers from these tabs also show up in Figure 1
gen troll_prosthetic=1 if prosthetic=="TRUE"
replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
tab troll_prosthetic
*26.98%

gen troll_blind=1 if blind=="TRUE"
replace troll_blind=0 if blind=="FALSE"|blind=="NA"
tab troll_blind
*20.47%

gen troll_deaf=1 if deaf=="TRUE"
replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
tab troll_deaf
*20.33%

gen troll_gang=1 if gang_resp=="TRUE"
replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
tab troll_gang
*25.85%

gen troll_famgang=1 if gang_fam=="TRUE"
replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
tab troll_famgang
*27.5%

gen troll_sleep=1 if sleep=="1"
replace troll_sleep=0 if sleep=="0"|sleep=="NA"
tab troll_sleep
*2.13%

**Two or more rare behaviors/traits

egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
tab troll_index
gen likely_troll=1 if troll_index>1
replace likely_troll=0 if troll_index<2
tab likely_troll
*30.03%


** Generate self-reported sincerity measure
replace sincerity = "" if sincerity == "NA"
destring sincerity, replace
tab sincerity
recode sincerity (1/3 = 0) (4/5 = 1), gen(insincere)
tab insincere
*14% admit to responding non-seriously - noted in manuscript

tab insincere likely_troll, col chi
*among those flagged for trolling, 109/452 admit to almost or almost always answering sincerely /
*= 24.1% (chi2=49.9%, p<0.001)- as reported in manuscript

*among those not flagged for trolling, 107/1051 admit to this - 10.2%, (chi2=49.9%, p<0.001)- as reported in manuscript

**********
***DATE***
**********
		
gen date_ok=0
replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
tab date_ok
*so 24.92% of respondents entered the date incorrectly

*okay, what about those who wrote DD/MM/YYYY
gen date_poss_foreign = 0
replace date_poss_foreign = 1 if date == "20/06/2020" | date == "28.06.2020" | date == "28/06/2020" | date == "28/6/2020" ///
	|date == " 29 06 2020" | date ==  "29-06-2020" | date == "29-Jun-20" | date == "29.06 2020" | date == "29.06.2020" ///
	|date == "29/06/2020"| date == "29/6/2020" | date == " 29/6/2020." | date == "29\06\2020" | date == "29|06|2020" ///
	|date == "30/06/2020"
tab date_poss_foreign 
*20% of the sample is possibly comprised of foreigners - noted in manuscript

*okay, how many people wrote a nonsensical response to the date question?
gen inattentive = 1
replace inattentive = 0 if date_ok==1|date_poss_foreign==1
tab inattentive
*4.92%


************************************************************
***CREATING UPPER AND LOWER BOUND ESTIMATES OF BAD ACTORS***
************************************************************


*creating a variable that combines trolling and weird IPs ("lower bound")
gen combined_troll_1=.
replace combined_troll_1 = 1 if funny_ip == "TRUE" | likely_troll == 1
replace combined_troll_1 = 0 if funny_ip == "FALSE" & likely_troll == 0
tab combined_troll_1
*37.94% low quality - column 2, row 1 of Table 4

*creating a variable that combines trolling, weird IP, and weirdly written date 
gen combined_troll_2=.
replace combined_troll_2=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1
replace combined_troll_2=0 if funny_ip=="FALSE" & likely_troll==0 & date_poss_foreign==0
tab combined_troll_2
*44.58% of respondents are suspicious using this measure - column 2, row 2 of Table 4

*creating a variable that combines trolling, weird IP, weirdly written date, or inattentive (nonsensical date write-in)
gen combined_troll_3=.
replace combined_troll_3=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1|inattentive==1
replace combined_troll_3=0 if funny_ip=="FALSE"  & likely_troll==0 & date_poss_foreign==0 & inattentive==0
tab combined_troll_3
*46.25% marked as suspicious here - column 2, row 3 of Table 4

************
***Timing***
************

rename durationinseconds time
sum time, d

************************************************************
***Table 5: Low_Quality Responses by HIT Completion Rates***
************************************************************
gen hits_recode=.
replace hits_recode=1 if hits=="1"
replace hits_recode=2 if hits=="2"
replace hits_recode=3 if hits=="3"
replace hits_recode=4 if hits=="4"
tab hits_recode 

label define hits_lb 1 "Fewer than 100 HITs" 2 "Between 100 and 500 HITs" 3 "Between 500 and 1000 HITs" 4 "More than 1k HITs", replace
label values hits_recode hits_lb
tab hits_recode
*this tab provides the data for row 1 in Table 5 (June 2020 study)

tab combined_troll_1 hits_recode, col
*this tab provides the data for row 2 in Table 5 (June 2020 study)
tab combined_troll_2 hits_recode, col
*this tab provides the data for row 3 in Table 5 (June 2020 study)
tab combined_troll_3 hits_recode, col
*this tab provides data for row 4 in Table 5 (June 2020 study)

tab insincere hits_recode, col
*this tab provides the data for row 5 in Table 5 (June 2020 study)

*this provides the data for row 6 in Table 5 (June 2020 study)
sum time if hits_recode==1
sum time if hits_recode==2
sum time if hits_recode==3
sum time if hits_recode==4
*average time for high HIT workers = 495.93; average time for second highest category = 586.16
display (586.16 - 495.93)/586.16

*********************************
**EXPERIMENTAL RESULTS -- FIG 2**
*********************************

** Coding PID

*7-point party ID
gen pid7 = .
replace pid7 = 1 if pid_dem == "1"
replace pid7 = 2 if pid_dem == "2"
replace pid7 = 3 if pid_ind == "2"
replace pid7 = 4 if pid_ind == "3"
replace pid7 = 5 if pid_ind == "1"
replace pid7 = 6 if pid_rep == "2"
replace pid7 = 7 if pid_rep == "1"	
label define pid7_lbl 1 "Strong Democrat" 2 "Weak Democrat" 3 "Leaning Democrat" 4 "Independent" 5 "Leaning Republican" 6 "Weak Republican" 7 "Strong Republican", replace
label values pid7 pid7_lbl

*3-point party ID
recode pid7 (1/3=1)(4=2)(5/7=3), gen(pid3)
label define pid3_lbl 1 "Democratic" 2 "Independent" 3 "Republican", replace
label values pid3 pid3_lbl

*Democratic dummy for comparing Dems and Reps
gen dem_rep = .
replace dem_rep = 1 if pid3 == 1
replace dem_rep = 0 if pid3 == 3
label define dem_rep_lbl 0 "Republican" 1 "Democrat", replace
label values dem_rep dem_rep_lbl

*recoding DVs

gen gop_unemploy=1 if gop_unemployment=="1"
replace gop_unemploy=.5 if gop_unemployment=="2"
replace gop_unemploy=0 if gop_unemployment=="3"
tab gop_unemploy

gen gop_inflate=1 if gop_inflation=="1"
replace gop_inflate=.5 if gop_inflation=="2"
replace gop_inflate=0 if gop_inflation=="3"
tab gop_inflate

gen obama_unemploy=1 if obama_unemployment=="1"
replace obama_unemploy=.5 if obama_unemployment=="2"
replace obama_unemploy=0 if obama_unemployment=="3"
tab obama_unemploy

gen obama_inflate=1 if obama_inflation=="1"
replace obama_inflate=.5 if obama_inflation=="2"
replace obama_inflate=0 if obama_inflation=="3"
tab obama_inflate


*creating a collapsed unemployment DV
gen unemploy = gop_unemploy
replace unemploy = obama_unemploy if unemploy==.
tab unemploy

*creating a collapsed inflation DV
gen inflation = gop_inflate
replace inflation = obama_inflate if inflation==.
tab inflation

*generating treatment variable
gen dem_treat=1 if randomization_1 == "obama"
replace dem_treat=0 if randomization_1 == "congress"
tab dem_treat

*creating a party x treatment variable 
*1 = if you got the out-party treatment, 0 = in-party treatment
gen out_party_treat=.
replace out_party_treat = 1 if dem_rep == 1 & dem_treat == 0
replace out_party_treat = 1 if dem_rep == 0 & dem_treat == 1
replace out_party_treat = 0 if dem_rep == 0 & dem_treat == 0
replace out_party_treat = 0 if dem_rep == 1 & dem_treat == 1
tab out_party_treat

**the following regressions were also ran in R to produce Figure 2 

*okay, what are the effects among the full sample? (n=1425)
reg unemploy out_party_treat
reg inflation out_party_treat
*betas and se's calculated here used in spreadsheet below (attenuation effects)

*effects among non-flagged respondents (n=861)
reg unemploy out_party_treat if combined_troll_1 == 0
reg inflation out_party_treat if combined_troll_1 == 0

*effects among all flagged respondents (n=564)
reg unemploy out_party_treat if combined_troll_1 == 1
reg inflation out_party_treat if combined_troll_1 == 1
*betas and se's calculated here used in spreadsheet below (attenuation effects)

*effects among flagged IPs only
reg unemploy out_party_treat if funny_ip =="TRUE"
reg inflation out_party_treat if funny_ip =="TRUE"

*effects among trolls only
reg unemploy out_party_treat if likely_troll==1
reg inflation out_party_treat if likely_troll==1

*effects among those with 1k+ HITs
reg unemploy out_party_treat if hits=="4"
reg inflation out_party_treat if hits=="4"

*interactive effects with trolling indicator
reg unemploy i.out_party_treat##i.combined_troll_1
reg inflation i.out_party_treat##i.combined_troll_1

*************************
***ATTENUATION EFFECTS***
*************************

*betas and SEs catalogued in this spreadsheet were entered manually entered based on the regression results above
*numbers needed = betas and se's for the full sample and beta's and se's for all flagged respondents 

insheet using "turk_06_29_2020/june_2020_attenuation_fx.csv", clear names

gen diff = nonsusp_beta - susp_beta
gen se_diff = sqrt(((nonsusp_se ^ 2) / 861) + (susp_se ^ 2) / 564)
gen weight = 1 / se_diff
reg diff [aw = weight]
	
*average treatment effect in the non-troll group
gen weight_nonsusp = 1 / nonsusp_se
reg nonsusp_beta [aw = weight_nonsusp]
	* -.108 
		
*getting an attenuation effect, weighted by the inverse of the estimated SE of the differences
gen attn = nonsusp_beta - full_beta
gen se_attn = sqrt(((nonsusp_se ^ 2) / 861) + (full_se ^ 2) / 1425)
gen attn_wt = 1 / se_attn
reg attn [aw = attn_wt] 
	*-.0298358

*putting it in percentage point terms. we observe treatment effects that are...
gen attn_pct = full_beta / nonsusp_beta
reg attn_pct [aw = attn_wt]
	* .7165279 what they would be without suspicious responses
*in other words, our treatment effects are attenuated by...
	display 1 -   .7165279
	* = .2834721 or 28.3% 
