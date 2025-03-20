import pandas as pd
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt

# Încarcă datele
data = pd.read_csv("date_clusterizate.csv")

# Definirea variabilei țintă și a clusterelor
target = "Ladder score"
clusters = data['Cluster'].unique()

# Eliminăm coloanele care nu sunt relevante pentru Random Forest
features = data.drop(columns=["Country name", "Regional indicator","upperwhisker","lowerwhisker","Dystopia + residual", "Cluster"])

# Dicționar pentru a stoca importanța caracteristicilor
feature_importances = {}

# Aplicăm Random Forest pentru fiecare cluster
for cluster in clusters:
    # Selectăm datele din clusterul curent
    cluster_data = data[data['Cluster'] == cluster]

    # Separăm variabilele explicative și variabila țintă
    X = cluster_data[features.columns.drop(target)]
    y = cluster_data[target]

    # Inițializăm și antrenăm modelul Random Forest
    rf = RandomForestRegressor(random_state=42, n_estimators=100)
    rf.fit(X, y)

    # Calculăm importanța caracteristicilor
    feature_importances[cluster] = pd.Series(rf.feature_importances_, index=X.columns)

# Afișăm rezultatele pentru fiecare cluster
for cluster, importance in feature_importances.items():
    print(f"\nCluster {cluster} - Importanța caracteristicilor:")
    print(importance.sort_values(ascending=False))

for cluster, importance in feature_importances.items():
    importance.sort_values(ascending=False).plot(kind="bar", figsize=(10, 6))
    plt.title(f"Feature Importance for Cluster {cluster}")
    plt.xlabel("Features")
    plt.ylabel("Importance")
    plt.show()