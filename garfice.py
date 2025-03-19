import matplotlib.pyplot as plt
import numpy as np
from scipy.cluster.hierarchy import dendrogram
from seaborn import scatterplot
from scikitplot.metrics import plot_silhouette
import matplotlib.pyplot as plt
from seaborn import scatterplot
from sklearn.model_selection import train_test_split

def show():
    plt.show()


def plot_ierarhie(h: np.ndarray, etichete, titlu, threshold=0):
    fig = plt.figure(figsize=(10, 6))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_title(titlu, fontdict={"fontsize": 16, "color": "b"})
    dendrogram(h,ax=ax,labels=etichete,color_threshold=threshold, orientation="right")

    plt.savefig("data_out/Dendograma.png")


def plot_indecsi_silhouette(x:np.ndarray,p,titlu):
    fig = plt.figure("SH_"+titlu,figsize=(8, 7))
    ax = fig.add_subplot(1, 1, 1)
    plot_silhouette(x,p,titlu,ax=ax)
    plt.savefig("data_out/SH_"+titlu+".png")

def plot_partitie(z: np.ndarray, p, titlu, clase, etichete=None):
    fig = plt.figure(figsize=(8, 7))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_title(titlu, fontdict={"fontsize": 16, "color": "b"})

    if z.shape[1] > 1:  # Dacă există cel puțin două dimensiuni
        ax.set_xlabel("Z1")
        ax.set_ylabel("Z2")
        scatterplot(x=z[:, 0], y=z[:, 1], hue=p, hue_order=clase, ax=ax, legend=True)
        if etichete is not None:
            # for i in range(len(etichete)):
            for index in range(len(etichete)):
                ax.text(x=z[index, 0], y=z[index, 1], s=etichete[index])
    else:  # Dacă există doar o dimensiune
        ax.set_xlabel("Z1")
        ax.set_ylabel("")  # Doar o axă pe Z1
        scatterplot(x=z[:, 0], y=[0] * len(z), hue=p, hue_order=clase, ax=ax, legend=True)
        if etichete is not None:
            for i in range(len(etichete)):
                ax.text(x=z[i, 0], y=0, s=etichete[i])


def plot_histograme(x:np.ndarray,p,k,nume_variabila,titlu):
    clusteri = np.unique(p)
    q = len(clusteri)
    fig = plt.figure("H_"+nume_variabila, figsize=(12, 7))
    fig.suptitle(titlu)
    axe = fig.subplots(1,q,sharey=True)
    for i in range(q):
        ax = axe[i]
        y = x[p==clusteri[i],k]
        ax.hist(y,10,range=(min(x[:,k]),max(x[:,k])), edgecolor = 'black')
        ax.set_xlabel(clusteri[i])
    plt.savefig("data_out/H_"+nume_variabila+".png")