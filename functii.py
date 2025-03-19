import numpy as np
import pandas as pd
from pandas.api.types import is_numeric_dtype
from sklearn.cluster import AgglomerativeClustering
from yellowbrick.cluster import KElbowVisualizer


def nan_replace_t(t: pd.DataFrame):
    for coloana in t.columns:
        if t[coloana].isna().any():
            if is_numeric_dtype(t[coloana]):
                t.fillna({coloana: t[coloana].mean()}, inplace=True)
            else:
                t.fillna({coloana: t[coloana].mode()[0]}, inplace=True)

def elbow(h: np.ndarray, k=None):
    n = h.shape[0] + 1
    if k is None:
        d = h[1:, 2] - h[:n - 2, 2]
        nr_jonctiuni = np.argmax(d) + 1
        k = n - nr_jonctiuni
    else:
        nr_jonctiuni = n - k
    threshold = (h[nr_jonctiuni, 2] + h[nr_jonctiuni - 1, 2]) / 2

    return k,threshold

def calcul_partitie_(x,k):
    model_hclust = AgglomerativeClustering(k)
    c = model_hclust.fit_predict(x)
    partitie = np.array(["C" + str(v + 1) for v in pd.Categorical(c).codes])
    return partitie