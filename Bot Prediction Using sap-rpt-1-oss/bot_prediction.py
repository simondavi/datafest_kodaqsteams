import pandas as pd
from sklearn.metrics import accuracy_score, classification_report
from sklearn.model_selection import train_test_split

from sap_rpt_oss import SAP_RPT_OSS_Classifier

# Load data prepared by prepare_bot_data.R
df = pd.read_csv("bot_features.csv")

# Subsample for speed
df = df.sample(n=min(2000, len(df)), random_state=42)

# Separate features and target
X = df.drop(columns=["is_bot"])
y = df["is_bot"]

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.5, random_state=42)

import traceback, sys

try:
    # Initialize classifier
    print("Initializing classifier...", flush=True)
    clf = SAP_RPT_OSS_Classifier(max_context_size=2048, bagging=1)
    print("Fitting model...", flush=True)
    clf.fit(X_train, y_train)
    print("Fit done. Predicting...", flush=True)

    prediction_probabilities = clf.predict_proba(X_test)
    predictions = clf.predict(X_test)

    print("Accuracy:", accuracy_score(y_test, predictions))
    print("\nClassification Report:")
    print(classification_report(y_test, predictions, target_names=["Human", "Bot"]))

except Exception as e:
    print("\n--- ERROR ---", flush=True)
    traceback.print_exc()
    sys.exit(1)
