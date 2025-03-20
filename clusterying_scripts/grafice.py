import matplotlib.pyplot as plt
import pandas as pd
from seaborn import heatmap
def corelograma(t:pd.DataFrame,vmin=-1,vmax=1,cmap="RdYlBu",title="Corelatii variabile-componente",valori=True):
    fig = plt.figure(figsize=(8, 5))
    ax = fig.add_subplot(1, 1, 1)
    ax.set_title(title, fontdict={"fontsize": 16, "color": "b"})
    heatmap(t,vmin=vmin,vmax=vmax,cmap=cmap,ax=ax,annot=valori)
    plt.show()