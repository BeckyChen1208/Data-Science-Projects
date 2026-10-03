clear all

import delimited "clean_dataset_final.csv", clear

* 查看資料
describe
tab body_battery_status

* 建立 HIGH vs LOW 群組
gen bb_group=.
replace bb_group=1 if body_battery_status=="HIGH"
replace bb_group=0 if body_battery_status=="LOW"

* t-test
ttest sleep_hours, by(bb_group)
ttest deep_sleep_ratio, by(bb_group)
ttest steps, by(bb_group)
ttest average_heart_rate, by(bb_group)

* ANOVA
encode body_battery_status, gen(bb_status)

oneway sleep_hours bb_status
oneway deep_sleep_ratio bb_status
oneway steps bb_status
oneway average_heart_rate bb_status

* post-hoc 事後檢定
pwmean sleep_hours, over(bb_status) mcompare(tukey)
pwmean average_heart_rate, over(bb_status) mcompare(tukey)
oneway steps bb_status, bonferroni

* Body Battery分數化
gen bb_score=.
replace bb_score=4 if body_battery_status=="HIGH"
replace bb_score=3 if body_battery_status=="GOOD"
replace bb_score=2 if body_battery_status=="LOW"
replace bb_score=1 if body_battery_status=="VERY_LOW"

* Multiple Regression
reg bb_score sleep_hours deep_sleep_ratio rem_ratio
outreg2 using regression_final.doc, replace word ctitle(Model 1 Sleep)

reg bb_score steps average_heart_rate
outreg2 using regression_final.doc, append word ctitle(Model 2 Activity)

reg bb_score sleep_hours deep_sleep_ratio rem_ratio steps average_heart_rate
outreg2 using regression_final.doc, append word ctitle(Model 3 Full)

reg bb_score sleep_hours steps average_heart_rate, beta
outreg2 using regression_final.doc, append word ctitle(base Model Beta)