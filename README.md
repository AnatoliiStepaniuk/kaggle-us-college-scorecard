# kaggle-us-college-shutdowns
Predicting US Colleges shutdowns using - https://www.kaggle.com/kaggle/college-scorecard

The data set contains vast amount of data about US colleges (>1700 features for around 7800 colleges) divided by year (1996-2013).

One of the most interesting feature for me was the `CURROPER` flag which indicates whether a school is considered currently operating. As education sector is being changed very dynamically recently I was interested in answering the question what are the reasons for schools to shutdown nowadays as well as being able to predict whether a school is going to shutdown anytime soon.

It turned out that dividing schools by "Currenly operating" flag leads to imbalanced data (less than 5% of schools are stated to be non-operating). It meant that I should be careful with choosing a way for estimating classificator's efficiency (because even predicting every school to be operating will have 0.95 error rate). I've decided to draw ROC plots in order to gather more information than a single variable(like error rate or AUC).

### Feature selection

Looking through data description, I intuitively selected several groups of features that could possibly correlate with response:

1. **Program percentages**. If an institution teaches something that's not in demand, it could correlate with shutting down. Features: `PCIP01`, `PCIP03`, ...
2. **Admission rate**. Probably the school with low admission rate wouldn't close tomorrow. Feature: `ADM_RATE_ALL`
3. **Costs**. I would expect the school with high education cost to be more stable. Features: `TUITIONFEE_*`, `COSTT4_*`, `NPT4*`
4. **Number of students**. Same as the cost. I would expect a school with higher number of students to be robust to shutdowns. Features: `NUM4*`, ...
5. **Revenue/expenditure per student**. Obvious. The schools that consistenly spends more than earns per student won't last forever. Features: `TUITFTE` and `INEXPFTE`
6. **Retention/Completion**. The higher are these, the more money school will earn per student. Features: `RET_*`, `C150_*`, `D150_*`

I had to reject first 2 groups at the very beginning because "Program percentages" demonstrated very low correlation with response and "Admission Rate" was only available for 40% of colleges.
Except by these 2 groups, I've added all these variables to initial feature set to perform subsequent feature elimination.

For estimating classificators final efficiency, I've chosen partial AUC for low FPRs. This was imposed by hypothetic business situation when there's cost for predicting school to be both operating and non-operating and profit for correctly predicted non-operating one. I've chosen area for FPR <= 5% according to omitted estimations.
Applying feature elimination demonstrated that we can noticeably increase AUC in the region of interest by discarding `COSTT4` ("Average cost of attendance") and `RET` ("Retention rate") features. The elimination was performed manually and was not included in the final code for this project.

The features that were left in the final model (ordered by decreasing importance)
`REVENUE_EXPENDITURE` - Net tuition revenue minus instructional expenditures per full-time equivalent student.
`TUITIONFEE` - Tuition and fees
`PAR_ED_PCT_HS` Percent of students whose parents' highest educational level was is some form of postsecondary education
`NPT4` - Average net price for Title IV institutions
`NUM4` - Number of Title IV students
`D150`, `C150` - Completion rates.

There's no surprise except for `PAR_ED_PCT_HS`. One of the possible explanations can be that parents with higher education would recommend their children to apply to more stable schools.

### Tuning Classificator

Because the data set appeared to have imbalanced data I was concerned about classificator having access to sufficient number of non-operating schools during training process. In order to achieve this I've provided balanced data (operating/non-operating) for training while testing classificator's performance on natural, imbalanced data. However after experimenting with different balancing ratios it turned out that Random Forest algorithm was perfoming better on the data as is, without additional balancing.
