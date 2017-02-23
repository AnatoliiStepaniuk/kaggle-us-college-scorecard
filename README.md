# kaggle-us-college-shutdowns
Predicting US Colleges shutdowns using - https://www.kaggle.com/kaggle/college-scorecard

The data set contains vast amount of data about US colleges (>1700 features for around 7800 colleges) divided by year (1996-2013).

One of the most interesting feature for me was the `CURROPER` flag which indicates whether a school is considered currently operating. As education sector is being changed very dynamically recently I was interested in answering the question what are the reasons for schools to shutdown nowadays.

Looking through data description, I intuitively selected several groups of features that could possibly give the answer:

1. **Program percentages**. If an institution teaches something that's not in demand, it could correlate with shutting down. Features: `PCIP01`, `PCIP03`, ...
2. **Admission rate**. Probably the school with low admission rate wouldn't close tomorrow. Feature: `ADM_RATE_ALL`
3. **Costs**. I would expect the school with high education cost to be more stable. Features: `TUITIONFEE_*`, `COSTT4_*`, `NPT4*`
4. **Number of students**. Same as the cost. I would expect a school with higher number of students to be robust to shutdowns. Features: `NUM4*`, ...
5. **Revenue/expenditure per student**. Obvious. The schools that consistenly spends more than earns per student won't last forever. Features: `TUITFTE` and `INEXPFTE`
6. **Retention/Completion**. The higher are these, the more money school will earn per student. Features: `RET_*`, `C150_*`, `D150_*`
