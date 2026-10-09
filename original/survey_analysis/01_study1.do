*** Replication for "The Micro-Task Market for Lemons"
*** Doug Ahler, Carrie Roush, & Gaurav Sood
*** August 2018 survey analysis

** Set WD
cd "~/Dropbox/August2018_TurkExperiments/replication_public/data/"

** Load data
insheet using "turk_08_17_2018/turk_recoded_public.csv", clear names

**Generating dummies for various indicators of low quality responding from IPs
**The "tabs" lead to numbers produced in Table 1
**These numbers are also repeated in row 1 of Table 3

gen black=1 if blacklisted=="TRUE"
replace black=0 if blacklisted=="FALSE"
tab black

gen miss=1 if missing_ip=="TRUE"
replace miss=0 if missing_ip=="FALSE"
tab miss

gen dup=1 if duplicated=="TRUE"
replace dup=0 if duplicated=="FALSE"
tab dup

gen foreign=1 if foreign_ip=="TRUE"
replace foreign=0 if foreign_ip=="FALSE"
tab foreign

*any of the above
gen funny=1 if funny_ip=="TRUE"
replace funny=0 if funny_ip=="FALSE"
tab funny

**Generating dummies for low-incidence screener questions
**The "tabs" lead to numbers produced in Table 2

gen prosthetic_troll=0 if prosthetic=="0"|prosthetic=="NA"
replace prosthetic_troll=1 if prosthetic=="1"
tab prosthetic_troll 

gen blind_troll=0 if blind=="0"|blind=="NA"
replace blind_troll=1 if blind=="1"
tab blind_troll 

gen deaf_troll=0 if deaf=="0"|blind=="NA"
replace deaf_troll=1 if deaf=="1"
tab deaf_troll

gen gang_resp_troll=0 if gang_resp=="0"|gang_resp=="NA"
replace gang_resp_troll=1 if gang_resp=="1"
tab gang_resp_troll

gen gang_fam_troll=0 if gang_fam=="0"|gang_fam=="NA"
replace gang_fam_troll=1 if gang_fam=="1"
tab gang_fam_troll 

gen troll_sleep=0 if sleep=="0"|sleep=="NA"
replace troll_sleep=1 if sleep=="1"
tab troll_sleep

***************************************************************
*see figure_1.do in replication files for estimates of trolling
***************************************************************

**Two or more rare behaviors/traits

egen troll_index=rowtotal(prosthetic_troll blind_troll deaf_troll gang_resp_troll gang_fam_troll troll_sleep)
tab troll_index
gen likely_troll=1 if troll_index>1
replace likely_troll=0 if troll_index<2
tab likely_troll

**Proportion of bad actors (classified by bad IPs or trolls)
gen troll=0 if likely_troll==0 & funny==0
replace troll=1 if likely_troll==1|funny==1
tab troll
	*24.65%, noted in Table 4, row 1

** Generate self-reported sincerity measure
gen insincere_dummy = .
replace insincere_dummy = 1 if trolling == "3" |trolling == "4"| trolling == "5" 
replace insincere_dummy = 0 if trolling == "1"| trolling == "2"
tab insincere_dummy
*8.79% admit to responding insincerely "always" or "almost always" - reported in manuscript

** Association between various measures of LQ responding
tab insincere_dummy likely_troll, col chi
	*93% of people not tagged for trolling "never" or "rarely" answer humorously/insincerely - reported in manuscript
tab insincere_dummy likely_troll, col chi
	*58% of 125 tolls say they answer sincerely
	tab funny likely_troll
tab funny likely_troll, col chi
*how many people from bad IP addresses reported being insincere? 38/406 = 9.4% - reported in manuscript
*how many people from non-suspicious IP addresses admitted to being insincere? 87/1594 = 5.45% - reported in manuscript
	
** Timing
replace durationinseconds = "" if durationinseconds == "NA"
destring durationinseconds, gen(time)
sum time, d
	*median response time = 573 seconds, or about 9 minutes and 33 seconds
	*generating outlier variables based on "time outside whiskers" in the box plot
	*anything outside 167% of the IQR gets classified as "fast" or "slow"
	*25th percentile = 426; 75th percentile = 785   
	
display (573 - 426) * (5/3) /* 245 */
gen fast = 0
replace fast = 1 if time <= 245
tab fast
*2.05% are fast
	
display (785 - 573) * (5/3) + 785 /* 1138 */
gen slow = 0
replace slow = 1 if time > 1138
tab slow
*11.65% are slow 

* Are suspicious respondents faster or slower? 

reg time troll /* trolls 166 seconds slower */
reg slow troll /*beta = 0.14, p<.001 - reported in manuscript */
reg fast troll /*beta=-.003, p=.686 - reported in manuscript */

* Overall measure of bad actors:
gen badactor = 0
replace badactor = 1 if funny == 1 | likely_troll == 1
tab badactor
