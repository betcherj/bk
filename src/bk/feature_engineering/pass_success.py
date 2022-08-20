import pandas as pd
import os
import numpy as np
from sklearn.calibration import calibration_curve
import matplotlib.pyplot as plt
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from sklearn.metrics import roc_auc_score

pbp = pd.read_csv( os.path.dirname(os.path.abspath(os.curdir)) + '/data/pbp21.csv')
pass_data = pbp[pbp['play_type'] == "pass"]
pass_data['pass_count'] = 1
pass_data = pass_data[pass_data['success'].notna()]
pass_data = pass_data[pass_data['down'].notna()]

# Check for missing down or distance or NAs
# Should we subset between certain win probability?
# The final "above expected features" should correlate better to future pass success/ above expected success
# Can we determine "into the wind" based on the direction of the field they are throwing into?

# Features to use:

x_feature = pass_data[['yardline_100','ydstogo','down']]
y_feature = pass_data['success']

# Down Subsets
pass_data1 = pass_data[pass_data['down'] == 1]
pass_data2 = pass_data[pass_data['down'] == 2]
pass_data3 = pass_data[(pass_data['down'] == 3) | (pass_data['down'] == 4)]

# Model Creation
from sklearn.linear_model import LogisticRegression
lr0 = LogisticRegression(C=0.001, random_state=1)
lr0.fit(x_feature, y_feature)
y_pred0 = lr0.predict(x_feature)

print (roc_auc_score(y_feature, y_pred0))

y_pred0 = lr0.predict_proba(x_feature)

# Calibration plot of expected vs actual + AUC score

x = pass_data['success']
y = y_pred0[:,1]
x, y = calibration_curve(x, y , n_bins = 20, normalize = True)
plt.plot([0, 1], [0, 1], linestyle='--', label='Ideally Calibrated')
plt.plot(y, x, marker='.', label='Model Predictions')
leg = plt.legend(loc='upper left')
plt.xlabel('True Rate')
plt.ylabel('Predicted Rate')
plt.show()



# Success tables

pass_data['expected_success'] = y_pred0[:,1]
pass_success = pass_data.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
 {'success': 'sum', 'pass_count': 'sum', 'expected_success': 'sum'})

pass_success.rename(columns={"success": "pass_success"}, inplace=True)
# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

pass_success.to_csv(os.path.dirname(os.path.abspath(os.curdir)) + "/processed_data/pass_success.csv")