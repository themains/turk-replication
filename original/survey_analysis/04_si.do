*** Replication for "The Micro-Task Market for Lemons"
*** Doug Ahler, Carrie Roush, & Gaurav Sood
*** Supporting Information

*** Set working directory
cd "~/Dropbox/August2018_TurkExperiments/replication_public/data"

************
*** SI 1 ***
************

*** Tables SI 1.1 -- SI 1.3

	** See "06_table_1_2_table_si_figure_1_si.R"

*** Analysis from SI 1.2

	* Load data and clean (with code from 02_study2.do)
	insheet using "turk_06_29_2020/merged_survey_ip_06_29_2020_final_public.csv", clear names
	gen susp=1 if untrustworthy=="TRUE"
	replace susp=0 if untrustworthy=="FALSE"
	gen miss=1 if missing_ip=="TRUE"
	replace miss=0 if missing_ip=="FALSE"
	gen dup=1 if duplicated=="TRUE"
	replace dup=0 if duplicated=="FALSE"
	gen foreign=1 if foreign_ip=="TRUE"
	replace foreign=0 if foreign_ip=="FALSE"
	gen funny=1 if funny_ip=="TRUE"
	replace funny=0 if funny_ip=="FALSE"
	gen troll_prosthetic=1 if prosthetic=="TRUE"
	replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
	gen troll_blind=1 if blind=="TRUE"
	replace troll_blind=0 if blind=="FALSE"|blind=="NA"
	gen troll_deaf=1 if deaf=="TRUE"
	replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
	gen troll_gang=1 if gang_resp=="TRUE"
	replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
	gen troll_famgang=1 if gang_fam=="TRUE"
	replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
	gen troll_sleep=1 if sleep=="1"
	replace troll_sleep=0 if sleep=="0"|sleep=="NA"
	egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
	gen likely_troll=1 if troll_index>1
	replace likely_troll=0 if troll_index<2
	replace sincerity = "" if sincerity == "NA"
	destring sincerity, replace
	recode sincerity (1/2 = 0)(3/5 = 1)
	gen date_ok = 0
	replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
	gen date_poss_foreign = 0
	replace date_poss_foreign = 1 if date == "20/06/2020" | date == "28.06.2020" | date == "28/06/2020" | date == "28/6/2020" ///
	|date == " 29 06 2020" | date ==  "29-06-2020" | date == "29-Jun-20" | date == "29.06 2020" | date == "29.06.2020" ///
	|date == "29/06/2020"| date == "29/6/2020" | date == " 29/6/2020." | date == "29\06\2020" | date == "29|06|2020" ///
	|date == "30/06/2020"
	gen inattentive = 1
	replace inattentive = 0 if date_ok==1|date_poss_foreign==1
	
	* Coding and analysis specific to SI 1.2
	order duplicate ip_index startdate enddate
		
		*looking at start/stop times of duplicate IPs to see 
		gen starthour = substr(startdate, -5, 2)
		gen startminute = substr(startdate, -2, .)
		destring starthour startminute, replace
		gen start_md = startminute / 60
		gen starttime = starthour + start_md
		gen stophour = substr(enddate, -5, 2)
		gen stopminute = substr(enddate, -2, .)
		destring stophour stopminute, replace
		gen stop_md = stopminute / 60
		gen stoptime = stophour + stop_md

		order ip_index ip starttime stoptime
		sort ip_index starttime
		edit if duplicate == "TRUE"
			* Looking at the data and coding the 37 duplicate IP addresses accordingly
			gen dup_nature = .
			replace dup_nature = 1 if inlist(ip_index,17,110,117,210,414,671,759,762,805,820,932,936,937,1088,1104,1140,1157,1196,1227,1279,1349,1394)
			replace dup_nature = 2 if inlist(ip_index,251,511,801,933,938,983,1089,1114,1127,1179,1199,1221)
			replace dup_nature = 3 if dup_nature == . & duplicate == "TRUE"
			label define dup_label 1 "likely cluster" 2 "likely same respondent" 3 "uncertain", replace
			label values dup_nature dup_label
			* 22 of 37 classified as a "likely cluster" = 59%, if overlap between start/stop times
			* 12 of 37 classfiied as "likely same respondent" = 32%, if no overlapping start/stop times but start/stop times are within 15 minutes of each other


*** Figure SI 1.1 and analyses from SI 1.3	

	** Fig. 1.1, Panel A
	twoway kdensity duration if date_ok == 1 || kdensity duration if date_ok == 0, ///
	ytitle("Density") xt("Survey duration, in seconds") /// 
	xline(900, lc(black) lp(dot)) ///
	text(0.002 1600 "Target time = 15 minutes") ///
	legend(label(1 "Date formatted MM/DD/YYYY") label(2 "Date formatted otherwise"))
	
	** Load and clean Study 3 for the next part
	insheet using "turk_07_12_2020/merged_survey_ip_07_12_2020_final_public.csv", clear names
	gen susp=1 if untrustworthy=="TRUE"
	replace susp=0 if untrustworthy=="FALSE"
	gen miss=1 if missing_ip=="TRUE"
	replace miss=0 if missing_ip=="FALSE"
	gen dup=1 if duplicated=="TRUE"
	replace dup=0 if duplicated=="FALSE"
	gen foreign=1 if foreign_ip=="TRUE"
	replace foreign=0 if foreign_ip=="FALSE"
	gen funny=1 if funny_ip=="TRUE"
	replace funny=0 if funny_ip=="FALSE"
	gen troll_prosthetic=1 if prosthetic=="TRUE"
	replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
	gen troll_blind=1 if blind=="TRUE"
	replace troll_blind=0 if blind=="FALSE"|blind=="NA"
	gen troll_deaf=1 if deaf=="TRUE"
	replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
	gen troll_gang=1 if gang_resp=="TRUE"
	replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
	gen troll_famgang=1 if gang_fam=="TRUE"
	replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
	gen troll_sleep=1 if sleep=="1"
	replace troll_sleep=0 if sleep=="0"|sleep=="NA"
	egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
	gen likely_troll=1 if troll_index>1
	replace likely_troll=0 if troll_index<2
	replace sincerity = "" if sincerity == "NA"
	destring sincerity, replace
	recode sincerity (1/2 = 0)(3/5 = 1)
	gen date_ok = 0
	replace date_ok = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20"
	gen date_poss_foreign = 0
	replace date_poss_foreign = 1 if date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
	gen correct_date = 0
	replace correct_date = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20" | date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
		gen inattentive = 1
	replace inattentive = 0 if date_ok==1|date_poss_foreign==1
	
	** Fig. 1.1, Panel B
	twoway kdensity duration if date_ok == 1 || kdensity duration if date_ok == 0, ///
	ytitle("Density") xt("Survey duration, in seconds") /// 
	xline(300, lc(black) lp(dot)) ///
	text(0.0025 630 "Target time = 5 minutes") ///
	legend(label(1 "Date formatted MM/DD/YYYY") label(2 "Date formatted otherwise"))	
	
*** Figure SI 1.2

	** See "06_table_1_2_table_si_figure_1_si.R"

*** Study 2 analysis in SI 1.5
	
	* Load data and clean (with code from 02_study2.do)
	insheet using "turk_06_29_2020/merged_survey_ip_06_29_2020_final_public.csv", clear names
	gen susp=1 if untrustworthy=="TRUE"
	replace susp=0 if untrustworthy=="FALSE"
	gen miss=1 if missing_ip=="TRUE"
	replace miss=0 if missing_ip=="FALSE"
	gen dup=1 if duplicated=="TRUE"
	replace dup=0 if duplicated=="FALSE"
	gen foreign=1 if foreign_ip=="TRUE"
	replace foreign=0 if foreign_ip=="FALSE"
	gen funny=1 if funny_ip=="TRUE"
	replace funny=0 if funny_ip=="FALSE"
	gen troll_prosthetic=1 if prosthetic=="TRUE"
	replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
	gen troll_blind=1 if blind=="TRUE"
	replace troll_blind=0 if blind=="FALSE"|blind=="NA"
	gen troll_deaf=1 if deaf=="TRUE"
	replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
	gen troll_gang=1 if gang_resp=="TRUE"
	replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
	gen troll_famgang=1 if gang_fam=="TRUE"
	replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
	gen troll_sleep=1 if sleep=="1"
	replace troll_sleep=0 if sleep=="0"|sleep=="NA"
	egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
	gen likely_troll=1 if troll_index>1
	replace likely_troll=0 if troll_index<2
	replace sincerity = "" if sincerity == "NA"
	destring sincerity, replace
	recode sincerity (1/2 = 0)(3/5 = 1)
	gen date_ok = 0
	replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
	gen date_poss_foreign = 0
	replace date_poss_foreign = 1 if date == "20/06/2020" | date == "28.06.2020" | date == "28/06/2020" | date == "28/6/2020" ///
	|date == " 29 06 2020" | date ==  "29-06-2020" | date == "29-Jun-20" | date == "29.06 2020" | date == "29.06.2020" ///
	|date == "29/06/2020"| date == "29/6/2020" | date == " 29/6/2020." | date == "29\06\2020" | date == "29|06|2020" ///
	|date == "30/06/2020"
	gen inattentive = 1
	replace inattentive = 0 if date_ok==1|date_poss_foreign==1
	rename durationinseconds time
	destring time, replace
	gen combined_troll_1=.
	replace combined_troll_1 = 1 if funny_ip == "TRUE" | likely_troll == 1
	replace combined_troll_1 = 0 if funny_ip == "FALSE" & likely_troll == 0
	gen combined_troll_2=.
	replace combined_troll_2=1 if funny_ip=="TRUE" | likely_troll==1|date_poss_foreign==1
	replace combined_troll_2=0 if funny_ip=="FALSE" & likely_troll==0 & date_poss_foreign==0
	gen combined_troll_3=.
	replace combined_troll_3=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1|inattentive==1
	replace combined_troll_3=0 if funny_ip=="FALSE"  & likely_troll==0 & date_poss_foreign==0 & inattentive==0
	*recoding DVs
	gen gop_unemploy=1 if gop_unemployment=="1"
	replace gop_unemploy=.5 if gop_unemployment=="2"
	replace gop_unemploy=0 if gop_unemployment=="3"
	gen gop_inflate=1 if gop_inflation=="1"
	replace gop_inflate=.5 if gop_inflation=="2"
	replace gop_inflate=0 if gop_inflation=="3"
	gen obama_unemploy=1 if obama_unemployment=="1"
	replace obama_unemploy=.5 if obama_unemployment=="2"
	replace obama_unemploy=0 if obama_unemployment=="3"
	gen obama_inflate=1 if obama_inflation=="1"
	replace obama_inflate=.5 if obama_inflation=="2"
	replace obama_inflate=0 if obama_inflation=="3"
	*generating treatment variable
	gen dem_treat=1 if randomization_1 == "obama"
	replace dem_treat=0 if randomization_1 == "congress"
	tab dem_treat
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
	*1 = if you got the out-party treatment, 0 = in-party treatment
	gen out_party_treat=.
	replace out_party_treat = 1 if dem_rep == 1 & dem_treat == 0
	replace out_party_treat = 1 if dem_rep == 0 & dem_treat == 1
	replace out_party_treat = 0 if dem_rep == 0 & dem_treat == 0
	replace out_party_treat = 0 if dem_rep == 1 & dem_treat == 1
	*creating a collapsed unemployment DV
	gen unemploy = gop_unemploy
	replace unemploy = obama_unemploy if unemploy==.
	tab unemploy
	*creating a collapsed inflation DV
	gen inflation = gop_inflate
	replace inflation = obama_inflate if inflation==.
	
	sum time, d
	*median response time = 488 seconds, or about 8 minutes and 13 seconds

	*generating outlier variables based on "time outside whiskers" in the box plot
	*anything outside 167% of the IQR gets classified as "fast" or "slow"
	*25th percentile = 349; 75th percentile =  722   
	
	display (488 - 349) * (5/3) /* 231.66667 */
	gen fast = 0
	replace fast = 1 if time <= 231.66667
	tab fast
	*6.25% are fast
	
	display (722 - 488) * (5/3) + 722 /* 1112 */
	gen slow = 0
	replace slow = 1 if time > 1112
	*7.92% are slow 

	*average completion time for trolls/ppl with bad IPs
	sum time if combined_troll_1==1 /*619.5792 seconds */
	reg fast combined_troll_1 /*4.7% are fast; 2.4 pp less so than non-flagged people, p = 0.057 */
	reg slow combined_troll_1 /*7.6% are slow; .04 pp less so than non-flagged people, p = 0.773 */

	*average completion time for trolls/ppl with bad IPs/date written incorrectly
	sum time if combined_troll_2==1  /*639.5974 seconds */
	reg fast combined_troll_2 /*4.4% are fast, 3.4 pp less so than non-flagged people, p = 2.73 */
	reg slow combined_troll_2 /*9.2% are slow, 2.4 pp moreso than non-flagged people, p = 0.087 */

	*average completion time for trolls/ppl with bad IPs/date written incorrectly or nonsensically
	sum time if combined_troll_3==1 /*634.4213 seconds */
	reg fast combined_troll_3 /*4.8% are fast, 2.7 pp less so than non-flagged people, p =  0.031 */ 
	reg slow combined_troll_3 /*8.9% are slow, 1.9 pp more so than non-flagged people, p =  0.169 */

	/* okay, so bad actors are mostly less likely to be fast outliers than non-flagged people, and more likely to be slow outliers
than non-flagged people, but not by any real measurable difference */

	*are people who format the date DD/MM/YYYY take longer than ppl who didn't? 
	sum time if date_poss_foreign==1 /*nah, average = 651.8538, so within the IQR */
	reg slow date_poss_foreign /* 9.6% are slow, 2.1 pp moreso than ppl who didn't, p =  0.218 */
	reg fast date_poss_foreign /* 10.2% are fast, 3.3 pp less so than people who didn't, p =  0.037 */
	*not really slower than people who didn't write the date DD/MM/YYYY

	** Table SI 1.4
	reg unemploy i.out_party_treat##i.fast 
	reg inflation i.out_party_treat##i.fast


*** Study 3 analysis in SI 1.5

	** Load and clean Study 3
	insheet using "turk_07_12_2020/merged_survey_ip_07_12_2020_final_public.csv", clear names
	gen susp=1 if untrustworthy=="TRUE"
	replace susp=0 if untrustworthy=="FALSE"
	gen miss=1 if missing_ip=="TRUE"
	replace miss=0 if missing_ip=="FALSE"
	gen dup=1 if duplicated=="TRUE"
	replace dup=0 if duplicated=="FALSE"
	gen foreign=1 if foreign_ip=="TRUE"
	replace foreign=0 if foreign_ip=="FALSE"
	gen funny=1 if funny_ip=="TRUE"
	replace funny=0 if funny_ip=="FALSE"
	gen troll_prosthetic=1 if prosthetic=="TRUE"
	replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
	gen troll_blind=1 if blind=="TRUE"
	replace troll_blind=0 if blind=="FALSE"|blind=="NA"
	gen troll_deaf=1 if deaf=="TRUE"
	replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
	gen troll_gang=1 if gang_resp=="TRUE"
	replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
	gen troll_famgang=1 if gang_fam=="TRUE"
	replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
	gen troll_sleep=1 if sleep=="1"
	replace troll_sleep=0 if sleep=="0"|sleep=="NA"
	egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
	gen likely_troll=1 if troll_index>1
	replace likely_troll=0 if troll_index<2
	replace sincerity = "" if sincerity == "NA"
	destring sincerity, replace
	recode sincerity (1/2 = 0)(3/5 = 1)
	gen date_ok = 0
	replace date_ok = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20"
	gen date_poss_foreign = 0
	replace date_poss_foreign = 1 if date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
	gen correct_date = 0
	replace correct_date = 1 if date == "07 11 2020" | date == "07/11.2020" | date == "6/11/20" | date== "7/10/20" ///
	| date == "7/11/20" | date == "7/11/19" | date == "7/11/20" | date == "7/12/00" | date == "7/12/20" ///
	| date == "7/13/20" | date == "11/7/20" | date == "12/7/20" | date == "12/7/2020" | date == "12/7/2020/" ///
	| date == "12\07\2020" | date == "13/07/2020"
		gen inattentive = 1
	replace inattentive = 0 if date_ok==1|date_poss_foreign==1
	gen combined_troll_1=.
	replace combined_troll_1 = 1 if funny_ip == "TRUE" | likely_troll == 1
	replace combined_troll_1 = 0 if funny_ip == "FALSE" & likely_troll == 0
	gen combined_troll_2=.
	replace combined_troll_2=1 if funny_ip=="TRUE" | likely_troll==1|date_poss_foreign==1
	replace combined_troll_2=0 if funny_ip=="FALSE" & likely_troll==0 & date_poss_foreign==0
	gen combined_troll_3=.
	replace combined_troll_3=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1|inattentive==1
	replace combined_troll_3=0 if funny_ip=="FALSE"  & likely_troll==0 & date_poss_foreign==0 & inattentive==0
	
	* Creating the timing variables
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

	*average completion time for trolls/ppl with bad IPs
	sum time if combined_troll_1==1 /*514.86 seconds */
	reg fast combined_troll_1 /*3.5% are fast; 15 pp less so than non-flagged people, p = 0.000  */
	reg slow combined_troll_1 /*7.6% are slow; .04 pp less so than non-flagged people, p = 0.773 */

	*average completion time for trolls/ppl with bad IPs/date written incorrectly
	sum time if combined_troll_2==1  /*639.5974 seconds */
	reg fast combined_troll_2 /*4.4% are fast, 3.4 pp less so than non-flagged people, p = 2.73 */
	reg slow combined_troll_2 /*9.2% are slow, 2.4 pp moreso than non-flagged people, p = 0.087 */

	*average completion time for trolls/ppl with bad IPs/date written incorrectly or nonsensically
	sum time if combined_troll_3==1 /*634.4213 seconds */
	reg fast combined_troll_3 /*4.8% are fast, 2.7 pp less so than non-flagged people, p =  0.031 */ 
	reg slow combined_troll_3 /*8.9% are slow, 1.9 pp more so than non-flagged people, p =  0.169 */

	/* okay, so bad actors are mostly less likely to be fast outliers than non-flagged people, and more likely to be slow outliers
than non-flagged people, but not by any real measurable difference */


	*are people who format the date DD/MM/YYYY take longer than ppl who didn't? 
	sum time if date_poss_foreign==1 /*nah, average = 651.8538, so within the IQR */
	reg slow date_poss_foreign /* 9.6% are slow, 2.1 pp moreso than ppl who didn't, p =  0.218 */
	reg fast date_poss_foreign /* 10.2% are fast, 3.3 pp less so than people who didn't, p =  0.037 */
	*not really slower than people who didn't write the date DD/MM/YYYY

	*what about comparing to people who wrote the date correctly? 
	gen foreign_dummy = .
	replace foreign_dummy = 1 if date_poss_foreign == 1
	replace foreign_dummy = 0 if date_ok == 1
	tab foreign_dummy

	reg slow foreign_dummy /* foreign = 9.6%, correct date = 7.4%, p = 0.209, so no stat sig difference between the two */
	reg fast foreign_dummy /*foreign = 3.7%, correct date = 6.8%, p =  0.043, so substantially less fast than ppl who wrote the date correctly */

************
*** SI 3 ***
************

	* Load data and clean (with code from 02_study2.do)
	insheet using "turk_06_29_2020/merged_survey_ip_06_29_2020_final_public.csv", clear names
	gen susp=1 if untrustworthy=="TRUE"
	replace susp=0 if untrustworthy=="FALSE"
	gen miss=1 if missing_ip=="TRUE"
	replace miss=0 if missing_ip=="FALSE"
	gen dup=1 if duplicated=="TRUE"
	replace dup=0 if duplicated=="FALSE"
	gen foreign=1 if foreign_ip=="TRUE"
	replace foreign=0 if foreign_ip=="FALSE"
	gen funny=1 if funny_ip=="TRUE"
	replace funny=0 if funny_ip=="FALSE"
	gen troll_prosthetic=1 if prosthetic=="TRUE"
	replace troll_prosthetic=0 if prosthetic=="FALSE"|prosthetic=="NA"
	gen troll_blind=1 if blind=="TRUE"
	replace troll_blind=0 if blind=="FALSE"|blind=="NA"
	gen troll_deaf=1 if deaf=="TRUE"
	replace troll_deaf=0 if deaf=="FALSE"|deaf=="NA"
	gen troll_gang=1 if gang_resp=="TRUE"
	replace troll_gang=0 if gang_resp=="FALSE"|gang_resp=="NA"
	gen troll_famgang=1 if gang_fam=="TRUE"
	replace troll_famgang=0 if gang_fam=="FALSE"|gang_fam=="NA"
	gen troll_sleep=1 if sleep=="1"
	replace troll_sleep=0 if sleep=="0"|sleep=="NA"
	egen troll_index=rowtotal(troll_prosthetic troll_blind troll_deaf troll_gang troll_famgang troll_sleep)
	gen likely_troll=1 if troll_index>1
	replace likely_troll=0 if troll_index<2
	replace sincerity = "" if sincerity == "NA"
	destring sincerity, replace
	recode sincerity (1/2 = 0)(3/5 = 1)
	gen date_ok = 0
	replace date_ok=1 if date=="06 29 2020"|date=="06.29.2020"|date=="06\29\2020"|date=="06\30\2020" ///
	|date=="6/29/20"|date=="6/29/2020."|date=="6/29/20209"|date=="6/29/2020`"|date=="60/29/2020" ///
	|date=="ju/26/2020"|date=="june/29/20"|date=="o6/29/2020" | date=="16/29/2020"
	gen date_poss_foreign = 0
	replace date_poss_foreign = 1 if date == "20/06/2020" | date == "28.06.2020" | date == "28/06/2020" | date == "28/6/2020" ///
	|date == " 29 06 2020" | date ==  "29-06-2020" | date == "29-Jun-20" | date == "29.06 2020" | date == "29.06.2020" ///
	|date == "29/06/2020"| date == "29/6/2020" | date == " 29/6/2020." | date == "29\06\2020" | date == "29|06|2020" ///
	|date == "30/06/2020"
	gen inattentive = 1
	replace inattentive = 0 if date_ok==1|date_poss_foreign==1
	rename durationinseconds time
	destring time, replace
	gen combined_troll_1=.
	replace combined_troll_1 = 1 if funny_ip == "TRUE" | likely_troll == 1
	replace combined_troll_1 = 0 if funny_ip == "FALSE" & likely_troll == 0
	gen combined_troll_2=.
	replace combined_troll_2=1 if funny_ip=="TRUE" | likely_troll==1|date_poss_foreign==1
	replace combined_troll_2=0 if funny_ip=="FALSE" & likely_troll==0 & date_poss_foreign==0
	gen combined_troll_3=.
	replace combined_troll_3=1 if funny_ip=="TRUE" |likely_troll==1|date_poss_foreign==1|inattentive==1
	replace combined_troll_3=0 if funny_ip=="FALSE"  & likely_troll==0 & date_poss_foreign==0 & inattentive==0
	*recoding DVs
	gen gop_unemploy=1 if gop_unemployment=="1"
	replace gop_unemploy=.5 if gop_unemployment=="2"
	replace gop_unemploy=0 if gop_unemployment=="3"
	gen gop_inflate=1 if gop_inflation=="1"
	replace gop_inflate=.5 if gop_inflation=="2"
	replace gop_inflate=0 if gop_inflation=="3"
	gen obama_unemploy=1 if obama_unemployment=="1"
	replace obama_unemploy=.5 if obama_unemployment=="2"
	replace obama_unemploy=0 if obama_unemployment=="3"
	gen obama_inflate=1 if obama_inflation=="1"
	replace obama_inflate=.5 if obama_inflation=="2"
	replace obama_inflate=0 if obama_inflation=="3"
	*generating treatment variable
	gen dem_treat=1 if randomization_1 == "obama"
	replace dem_treat=0 if randomization_1 == "congress"
	tab dem_treat
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
	*1 = if you got the out-party treatment, 0 = in-party treatment
	gen out_party_treat=.
	replace out_party_treat = 1 if dem_rep == 1 & dem_treat == 0
	replace out_party_treat = 1 if dem_rep == 0 & dem_treat == 1
	replace out_party_treat = 0 if dem_rep == 0 & dem_treat == 0
	replace out_party_treat = 0 if dem_rep == 1 & dem_treat == 1
	*creating a collapsed unemployment DV
	gen unemploy = gop_unemploy
	replace unemploy = obama_unemploy if unemploy==.
	tab unemploy
	*creating a collapsed inflation DV
	gen inflation = gop_inflate
	replace inflation = obama_inflate if inflation==.
	
	*** Table SI 3.5
	*effects among non-flagged respondents
	reg unemploy out_party_treat if combined_troll_1 == 0
	reg inflation out_party_treat if combined_troll_1 == 0

	*interactive effects with trolling indicator
	reg unemploy i.out_party_treat##i.combined_troll_1
	reg inflation i.out_party_treat##i.combined_troll_1

	*effects among all flagged respondents
	reg unemploy out_party_treat if combined_troll_1 == 1
	reg inflation out_party_treat if combined_troll_1 == 1

	*effects among flagged IPs only
	reg unemploy out_party_treat if funny_ip =="TRUE"
	reg inflation out_party_treat if funny_ip =="TRUE"

	*effects among trolls only
	reg unemploy out_party_treat if likely_troll==1
	reg inflation out_party_treat if likely_troll==1

	*effects among those with 1k+ HITs
	reg unemploy out_party_treat if hits=="4"
	reg inflation out_party_treat if hits=="4"

	
************
*** SI 4 ***
************

*** The main experimental effects and attenuation from apparent bad actors, shown in Table SI 4.6

	* Load and clean data
	insheet using "turk_08_17_2018/turk_recoded_public.csv", clear names
	
		**Generating dummies for various indicators of low quality responding from IPs (consistent with 01_study1.do)
		gen black=1 if blacklisted=="TRUE"
		replace black=0 if blacklisted=="FALSE"
		gen miss=1 if missing_ip=="TRUE"
		replace miss=0 if missing_ip=="FALSE"
		gen dup=1 if duplicated=="TRUE"
		replace dup=0 if duplicated=="FALSE"
		gen foreign=1 if foreign_ip=="TRUE"
		replace foreign=0 if foreign_ip=="FALSE"
		*any of the above
		gen funny=1 if funny_ip=="TRUE"
		replace funny=0 if funny_ip=="FALSE"

		**Generating dummies for low-incidence screener questions
		gen prosthetic_troll=0 if prosthetic=="0"|prosthetic=="NA"
		replace prosthetic_troll=1 if prosthetic=="1"
		gen blind_troll=0 if blind=="0"|blind=="NA"
		replace blind_troll=1 if blind=="1"
		gen deaf_troll=0 if deaf=="0"|blind=="NA"
		replace deaf_troll=1 if deaf=="1"
		gen gang_resp_troll=0 if gang_resp=="0"|gang_resp=="NA"
		replace gang_resp_troll=1 if gang_resp=="1"
		gen gang_fam_troll=0 if gang_fam=="0"|gang_fam=="NA"
		replace gang_fam_troll=1 if gang_fam=="1"
		gen troll_sleep=0 if sleep=="0"|sleep=="NA"
		replace troll_sleep=1 if sleep=="1"
		*Two or more rare behaviors/traits
		egen troll_index=rowtotal(prosthetic_troll blind_troll deaf_troll gang_resp_troll gang_fam_troll troll_sleep)
		gen likely_troll=1 if troll_index>1
		replace likely_troll = 0 if likely_troll == .
		
		* Bad actor indicator
		gen badactor = 0
		replace badactor = 1 if funny == 1 | likely_troll == 1
	
		* Generating treatment variables
		gen james_black = 0
		replace james_black = 1 if cf_race == "black"
		
		gen james_gay = 0
		replace james_gay = 1 if cf_spouse == "Keith"
		
		gen james_lib = 0
		replace james_lib = 1 if cf_policy == "living-wage demonstrations"
		gen james_con = 0
		replace james_con = 1 if cf_policy == "anti-tax demonstrations"
		
		gen james_evang = 0
		replace james_evang = 1 if cf_relig == "leads his son's Cub Scouts group, organized through the Baptist Church the family attends"
		gen james_aa = 0
		replace james_aa = 1 if cf_relig == "leads his son's Junior Explorers group, organized through the Secular Families Foundation"

		* Generating outcome variables
		replace james_cf = "" if james_cf == "NA"
		destring james_cf, replace
		
		gen dem_cf = .
		replace dem_cf = 1 if james_cf == 2
		replace dem_cf = 0 if inlist(james_cf, 1, 3)
		gen rep_cf = .
		replace rep_cf = 1 if james_cf == 3
		replace rep_cf = 0 if inlist(james_cf, 1, 2)
				
		* Simple OLS model just to take a look...
		reg dem_cf james_black james_gay james_evang james_aa james_lib james_con
		reg rep_cf james_black james_gay james_evang james_aa james_lib james_con
		
		* The main outcome variable for the ordered logit model
		recode james_cf (1=0)(2=-1)(3=1), gen(cf_ordered)
		
		* For time analyses especially, a "made CF" DV
		gen made_cf = .
		replace made_cf = 1 if dem_cf == 1 | rep_cf == 1
		replace made_cf = 0 if dem_cf != 1 & rep_cf != 1
	
	* The analysis in SI 3.6
	
		* COL. 1 & 2: NON-FLAGGED RESPONDENTS
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con if untrustworthy == "FALSE"

			* MARGINAL EFFECTS FOR NON-FLAGGED RESPONDENTS
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))
			
		* COL. 3 & 4: ALL LOW QUALITY RESPONDENTS
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con if untrustworthy == "TRUE"

			* MARGINAL EFFECTS FOR ALL LOW QUALITY RESPONDENTS
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))
			
		* COL. 5 & 6 FLAGGED IP ADDRESSES
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con if funny == 1

			* MARGINAL EFFECTS FOR FLAGGED IP ADDRESSES
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))

		* COL. 7 & 8: LIKELY TROLLS
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con if likely_troll == 1

			* MARGINAL EFFECTS FOR LIKELY TROLLS
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))
			
*** SI 3.2: The interactive model, for completeness
	
	* All bad actors (col. 1)
	ologit cf_ordered i.james_black##i.badactor i.james_gay##i.badactor i.james_evang##i.badactor i.james_aa##i.badactor i.james_lib##i.badactor i.james_con##i.badactor 
	
	* Flagged IP addresses (col. 2)
	ologit cf_ordered i.james_black##i.funny i.james_gay##i.funny i.james_evang##i.funny i.james_aa##i.funny i.james_lib##i.funny i.james_con##i.funny if likely_troll==0
	
	* Likely trolls (col. 3)
	ologit cf_ordered i.james_black##i.likely_troll i.james_gay##i.likely_troll i.james_evang##i.likely_troll i.james_aa##i.likely_troll i.james_lib##i.likely_troll i.james_con##i.likely_troll if funny == 0
		
*** Table SI 3.8 --- going slightly out of order for efficiency of code

	* Creating timing variables
	replace durationinseconds = "" if durationinseconds == "NA"
	destring durationinseconds, gen(time)
	*median response time = 573 seconds, or about 9 minutes and 33 seconds
	*generating outlier variables based on "time outside whiskers" in the box plot
	*anything outside 167% of the IQR gets classified as "fast" or "slow"
	*25th percentile = 426; 75th percentile = 785   
	gen fast = 0
	replace fast = 1 if time <= 245
	gen slow = 0
	replace slow = 1 if time > 1138
	
	* Running the model
	ologit cf_ordered i.james_black##i.fast i.james_gay##i.fast i.james_evang##i.fast i.james_aa##i.fast i.james_lib##i.fast i.james_con##i.fast if badactor == 0

*** Finally, we can estimate the attenuation effects, consistent with SI 3.3

	* Running the full model and the "all suspicious respondents" model for comparisons
		* Full sample
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))
		* Nonsuspicious respondents
		ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con if badactor == 0
			margins james_black james_gay james_evang james_aa james_lib james_con
			margins, dydx(james_black) predict(outcome(1))
			margins, dydx(james_black) predict(outcome(-1))
			margins, dydx(james_gay) predict(outcome(1))
			margins, dydx(james_gay) predict(outcome(-1))
			margins, dydx(james_evang) predict(outcome(1))		
			margins, dydx(james_evang) predict(outcome(-1))
			margins, dydx(james_aa) predict(outcome(1))
			margins, dydx(james_aa) predict(outcome(-1))
			margins, dydx(james_lib) predict(outcome(1))
			margins, dydx(james_lib) predict(outcome(-1))
			margins, dydx(james_con) predict(outcome(1))
			margins, dydx(james_con) predict(outcome(-1))
		* Recorded and stored, along with estimates from Table SI 4.6, in attenuation_fx.csv
		* Note that we are interested in whether effects "go in the expected direction," so they are stored as absolute magnitudes in that file (and theoretically "x -1" when they "go against the expect direction," though none do)

	* Load that summary data
	insheet using "turk_08_17_2018/attenuation_fx.csv", clear names
	
	gen diff = nonsusp_beta - susp_beta
	gen se_diff = sqrt(((nonsusp_se ^ 2) / 1507) + (susp_se ^ 2) / 484)
	gen weight = 1 / se_diff
	reg diff [aw = weight]
	
	*average treatment effect in the non-troll group
	gen weight_nonsusp = 1 / nonsusp_se
	reg nonsusp_beta [aw = weight_nonsusp]
		
		*getting an attenuation effect, weighted by the inverse of the estimated SE of the differences
	gen attn = nonsusp_beta - full_beta
	gen se_attn = sqrt(((nonsusp_se ^ 2) / 484) + (full_se ^ 2) / 1991)
	gen attn_wt = 1 / se_attn
	reg attn [aw = attn_wt] 

	*putting it in percentage point terms. we observe treatment effects that are...
	gen attn_pct = full_beta / nonsusp_beta
	reg attn_pct [aw = attn_wt]
		* .8991007  what they would be without suspicious responses
*in other words, our treatment effects are attenuated by...
		display 1 - .8991007 

		
