import pandas as pd
from grafice import corelograma
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_score


# Citește fișierul CSV
data = pd.read_csv("Wh.csv")
# Selectează coloanele numerice
numerical_columns = [
    'Ladder score', 'Log GDP per capita', 'Social support',
    'Healthy life expectancy', 'Freedom to make life choices',
    'Generosity', 'Perceptions of corruption'
]
data_numerical = data[numerical_columns]

# Scalează datele
scaler = StandardScaler()
data_scaled = scaler.fit_transform(data_numerical)
# Calcularea inerției pentru diferite valori ale lui k
inertia = []
cluster_range = range(2, 11)

for k in cluster_range:
    kmeans = KMeans(n_clusters=k, random_state=42)
    kmeans.fit(data_scaled)
    inertia.append(kmeans.inertia_)

# Vizualizarea metodei "elbow"
plt.figure(figsize=(10, 5))
plt.plot(cluster_range, inertia, marker='o', label="Inerția")
plt.title("Metoda Elbow")
plt.xlabel("Numărul de Clustere (k)")
plt.ylabel("Inerția")
plt.legend()
plt.grid()
plt.show()

silhouette_scores = []

for k in cluster_range:
    kmeans = KMeans(n_clusters=k, random_state=42)
    kmeans.fit(data_scaled)
    silhouette_scores.append(silhouette_score(data_scaled, kmeans.labels_))

# Vizualizarea scorului Silhouette
plt.figure(figsize=(10, 5))
plt.plot(cluster_range, silhouette_scores, marker='o', color='green', label="Scor Silhouette")
plt.title("Scor Silhouette pentru diferite valori de k")
plt.xlabel("Numărul de Clustere (k)")
plt.ylabel("Scor Silhouette")
plt.legend()
plt.grid()
plt.show()

# Alegerea valorii optime pentru k
optimal_k = 3
kmeans = KMeans(n_clusters=optimal_k, random_state=42)
kmeans.fit(data_scaled)

# Adaugă etichetele clusterelor în cadrul de date original
data['Cluster'] = kmeans.labels_

# Vizualizare
print(data[['Country name', 'Cluster']])

# Salvarea datelor într-un fișier CSV
data.to_csv("date_clusterizate.csv", index=False)

print("Fișierul a fost salvat ca 'date_clusterizate.csv'.")
correlation_matrix = data_numerical.corr()

# Afișarea corelogramei folosind funcția corelograma
corelograma(correlation_matrix)







