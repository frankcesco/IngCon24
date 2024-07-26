import os
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from boruta import BorutaPy
import matplotlib.pyplot as plt

# Carica il dataset ridotto
df_reduced = pd.read_csv('../data/merged_data_reduced.csv')

# Convertire le feature categoriche in numeriche utilizzando get_dummies
df_reduced = pd.get_dummies(df_reduced)

# Separare le feature (X) e il target (y)
X = df_reduced.drop(columns=['Lesioni'])
y = df_reduced['Lesioni']

# Gestire eventuali valori mancanti
X.fillna(-999, inplace=True)

# Imposta il modello RandomForest per Boruta
rf = RandomForestClassifier(n_jobs=-1, class_weight='balanced', max_depth=5, random_state=42)

# Modifica l'intervallo dei percentili a intervalli di 5
percentiles = range(5, 101, 5)
selected_features_counts = []

for perc in percentiles:
    print(f"Percentile: {perc}")
    boruta_selector = BorutaPy(rf, perc=perc, n_estimators='auto', verbose=1, random_state=42)
    boruta_selector.fit(X.values, y.values)
    selected_features = boruta_selector.support_.sum()
    selected_features_counts.append(selected_features)
    print(f"Percentile: {perc}, Selected Features: {selected_features}")

# Plot del numero di feature selezionate vs percentile
plt.figure(figsize=(10, 6))
plt.plot(percentiles, selected_features_counts, marker='o')
plt.xlabel('Percentile')
plt.ylabel('Numero di Feature Selezionate')
plt.title('Numero di Feature Selezionate per Percentile')
plt.grid(True)

# Crea la directory se non esiste
plots_directory = os.path.abspath('../plots')
if not os.path.exists(plots_directory):
    os.makedirs(plots_directory)

# Definisci il percorso completo del file per salvare il grafico
output_path_plot = os.path.join(plots_directory, 'num_features_percentile.png')

# Salva il grafico
plt.savefig(output_path_plot)
print(f"Grafico salvato in '{output_path_plot}'")

# Salva i risultati in un file CSV
output_path_percentiles = os.path.abspath('../data/boruta_percentile_results.csv')
pd.DataFrame({'Percentile': percentiles, 'Selected Features': selected_features_counts}).to_csv(output_path_percentiles, index=False)
print(f"Risultati salvati in '{output_path_percentiles}'")
