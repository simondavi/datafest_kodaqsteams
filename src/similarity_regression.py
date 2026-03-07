import pandas as pd
from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split

from sap_rpt_oss import SAP_RPT_OSS_Regressor

# Load data prepared by prepare_similarity_data.R
df = pd.read_csv("similarity_features.csv")

# Subsample for speed
df = df.sample(n=min(1000, len(df)), random_state=42)

X = df.drop(columns=["cosine_sim"])
y = df["cosine_sim"].astype(float)

# Train-test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.5, random_state=42)

import traceback, sys

try:
    # Initialize the regressor
    # 8k context and 8-fold bagging gives best performance; reduce if running out of memory
    print("Initializing regressor...", flush=True)
    regressor = SAP_RPT_OSS_Regressor(max_context_size=2048, bagging=1)

    print("Fitting model...", flush=True)
    regressor.fit(X_train, y_train)
    print("Fit done. Predicting...", flush=True)

    # Predict on the test set
    predictions = regressor.predict(X_test)

    r2  = r2_score(y_test, predictions)
    mae = mean_absolute_error(y_test, predictions)
    print("R² Score:", round(r2, 4))
    print("MAE:     ", round(mae, 4))

except Exception as e:
    print("\n--- ERROR ---", flush=True)
    traceback.print_exc()
    sys.exit(1)
