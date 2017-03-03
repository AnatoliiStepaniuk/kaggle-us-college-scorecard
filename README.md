# kaggle-us-college-shutdowns
Predicting US Colleges shutdowns using - https://www.kaggle.com/kaggle/college-scorecard

The data set contains vast amount of data about US colleges (>1700 features for around 7800 colleges) divided by year (1996-2013).

One of the most interesting feature for me was the `CURROPER` flag which indicates whether a school is considered currently operating. As education sector is being changed very dynamically recently I was interested in answering the question what are the reasons for schools to shutdown nowadays as well as being able to predict whether a school is going to shutdown anytime soon.

It turned out that dividing schools by "Currenly operating" flag leads to somewhat imbalanced data (less than 5% of schools are stated to be non-operating). It meant that I should be careful with choosing a way for estimating classificator's efficiency (because even predicting every school to be operating will have 0.95 error rate). I've decided to draw ROC plots in order to gather more information than a single variable (like error rate or AUC).

### Feature selection

Looking through data description, I intuitively selected several groups of features that could possibly correlate with response:

1. **Program percentages**. If an institution teaches something that's not in demand, it could correlate with shutting down. Features: `PCIP01`, `PCIP03`, ...
2. **Admission rate**. Probably the school with low admission rate wouldn't close anytime soon. Feature: `ADM_RATE_ALL`
3. **Costs**. I would expect the school with high education cost to be more stable. Features: `TUITIONFEE_*`, `COSTT4_*`, `NPT4*`
4. **Number of students**. Same as the cost. I would expect a school with higher number of students to be more robust to shutdowns. Features: `NUM4*`, ...
5. **Revenue/expenditure per student**. Obvious. The schools that consistenly spends more than earns per student won't last forever. Features: `TUITFTE` and `INEXPFTE`
6. **Retention/Completion**. The higher are these, the more money school will earn per student. Features: `RET_*`, `C150_*`, `D150_*`

I had to reject first 2 groups at the very beginning because "Program percentages" demonstrated very low correlation with response and "Admission Rate" was only available for 40% of examples. For other groups I performed feature engineering (mainly merging logically connected columns together) which resulted in initial feature set for subsequent feature elimination.

For estimating classificators' efficiency, I've chosen partial AUC for low FPRs. This was imposed by hypothetic business situation when there's a cost for predicting school to be non-operating and profit for correctly predicted non-operating ones. I've chosen area for FPR <= 5% according to calculations that were not included into this project.
Applying feature elimination demonstrated that we can noticeably increase AUC in the region of interest by discarding `COSTT4` ("Average cost of attendance") and `RET` ("Retention rate") features. Elimination was performed manually, also not included in the final code.

![Comparing ROC curves for different algorithms](https://raw.githubusercontent.com/AnatoliiStepaniuk/kaggle-us-college-shutdowns/master/roc-curves.png)


The features left for the final model (ordered by decreasing importance):

1. `TUITIONFEE` - Tuition and fees.
2. `D150` - Completion rates.
3. `PAR_ED_PCT_HS` - Percent of students whose parents' highest educational level was is some form of postsecondary education.
4. `NPT4` - Average net price for Title IV institutions.
5. `REV_EXP` - Net tuition revenue minus instructional expenditures per full-time equivalent student.
6. `NUM4` - Number of Title IV students.
7. `C150` - Completion rates.

There's no surprise except for `PAR_ED_PCT_HS`. One of the possible explanations can be that parents with a higher education would recommend their children to apply to more stable schools.

![Feature importance for Random Forest](https://raw.githubusercontent.com/AnatoliiStepaniuk/kaggle-us-college-shutdowns/master/feature-importance.png)

### Choosing Classificator

During feature selection, I've plotted ROC curves for several classification algorithms: 

- **Naive Bayes**
- **Linear Discriminant Analysis**
- **Logistic Regression**
- **Decision Trees**
- **Random Forest**

Although most of them produced quite similar ROC curves, Random Forest substantially outperformed others in terms of partial AUC for low FPRs. In addition to the mentioned algorithms I've also tried to apply anomaly detection framework (from `kmodR` package) in order to predict non-operating schools. However, error rate was unexceptable in comparison with Random Forest.

Because the data set appeared to have somewhat imbalanced data I was concerned about classificator having access to sufficient number of non-operating schools during training process. In order to achieve this I've provided balanced data (operating/non-operating) for training, while testing classificators' performance on natural, imbalanced data. However after experimenting with different balancing ratios it turned out that Random Forest algorithm was perfoming better on the data as is, without additional balancing.

Tuning Random Forest parameters, showed that defaults (`nodesize=1`, `ntree=500`) give the best partial auc.

### Hunting more data

After applying classificator for 2013 data, I've decided to gather additional information from files for other years. The algorithm was straightforward - for every school that's missing data for selected features, try to get data for that column from other files. It allowed me to decrease number of incomplete rows from 2011 to 1445.

### Estimating final performance

I've chosen classification probability threshold to achieve FPR around 5% according to aforementioned calculations which turned out to be around 0.132.
It resulted in following estimations:

- **Precision** : `0.27` - More than one quarter of non-operating predictions were correct.
- **TPR(Recall)** : `0.42` - I've been able to correctly predict about a half of all the non-operating schools.
- **FPR** : `0.048` - expected FPR value of around 5%.
