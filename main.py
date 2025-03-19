import sys

import matplotlib.pyplot as plt

from functii import nan_replace_t, calcul_partitie_, elbow
from garfice import plot_ierarhie, plot_indecsi_silhouette, plot_partitie, plot_histograme, show
import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import linkage
from sklearn.metrics import silhouette_samples,silhouette_score
from sklearn.decomposition import PCA
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis

np.set_printoptions(5,threshold=sys.maxsize,suppress=True)

set_date = pd.read_csv("data_in/CC GENERAL.csv", index_col=0)
# set_date = set_date.iloc[:100]

nan_replace_t(set_date)
pd.options.display.max_columns = None
# print(set_date.head(3))
variabile_observate = list(set_date)
# print(variabile_observate)

x = set_date[variabile_observate].values
metoda = "ward"
h = linkage(x, metoda)
# print(h)

# Determinare partitie optimala dupa metoda Elbow
k,threshold_opt = elbow(h)
p_opt = calcul_partitie_(x,k)
# print(k)

plot_ierarhie(h, set_date.index, "Partitia optimala - Metoda " + metoda, threshold=threshold_opt)

tabel_partitii = pd.DataFrame(index=set_date.index)
tabel_partitii["Partitia optimala"]=p_opt
tabel_partitii["Scoruri Silhouette - POpt"]=silhouette_samples(x,p_opt)

plot_indecsi_silhouette(x,p_opt,"Partitie optimala")

# Plot partitie in axe principale
model_disc = PCA(n_components=2)
print(x.shape)

model_disc.fit(x,p_opt)
z = model_disc.transform(x)
print(z.shape)
print(z[:5])

plot_partitie(z,p_opt,"Plot partitie optimala. Metoda "+metoda,
              np.unique(p_opt),set_date.index)
# plt.savefig("abc.svg", format = "svg")

for i in range(len(variabile_observate)):
    plot_histograme(x,p_opt,i,variabile_observate[i],
                    "Histograme pentru variabila "+variabile_observate[i])

show()

# Calcul si analiza partitie din 5 clusteri
k = 8
k,threshold_k = elbow(h,k)
p_k = calcul_partitie_(x,k)

plot_ierarhie(h,
              set_date.index,
              "Partitia din "+str(k)+" clusteri - Metoda "+metoda,
              threshold=threshold_k
              )
tabel_partitii["P_"+str(k)]=p_k
tabel_partitii["Scoruri Silhouette - P_"+str(k)]=silhouette_samples(x,p_k)

plot_indecsi_silhouette(x,p_k,"Partitia din "+str(k)+" clusteri")

model_disc.fit(x,p_k)
z = model_disc.transform(x)
print(z.shape)
print(z[:5])

plot_partitie(z,p_k,"Partitia din "+str(k)+" clusteri - Metoda "+metoda,
              np.unique(p_k),set_date.index)

for i in range(len(variabile_observate)):
    plot_histograme(x,p_k,i,variabile_observate[i],
                    "Histograme pentru variabila "+variabile_observate[i])

show()



