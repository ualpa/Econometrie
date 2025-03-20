import pandas as pd

# Încarcă fișierul CSV
file_path = 'Cluster3.csv'  # Înlocuiește cu calea fișierului tău
data = pd.read_csv(file_path)

# Crearea variabilelor dummy pentru coloana "Regional indicator"
dummy_variables = pd.get_dummies(data['Regional indicator'], prefix='Region')

# Adăugarea variabilelor dummy la dataset-ul original
data_with_dummies = pd.concat([data, dummy_variables], axis=1)

# Convertim toate coloanele de tip boolean în întregi (1 și 0)
for col in data_with_dummies.select_dtypes(include='bool').columns:
    data_with_dummies[col] = data_with_dummies[col].astype(int)

# Salvarea dataset-ului actualizat într-un fișier CSV
updated_file_path = 'Cluster3_updated.csv'  # Numele fișierului actualizat
data_with_dummies.to_csv(updated_file_path, index=False)

print(f"Fișierul actualizat a fost salvat ca {updated_file_path}")
