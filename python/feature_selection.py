import os
from pyswip import Prolog
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from boruta import BorutaPy

def extract_prolog_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre i dati delle strade con 7 argomenti
    strade = list(prolog.query('strada(FullId, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))
    # Estrarre i dati degli incidenti con 18 argomenti
    incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Ora, Minuto, Strada, NearestX, NearestY, Quartiere, FasciaOraria)'))
    # Estrarre i dati dei quartieri con 5 argomenti
    quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011, Densita)'))

    return incidenti, strade, quartieri

incidenti, strade, quartieri = extract_prolog_data()

# Converti i risultati delle query Prolog in DataFrame pandas
df_incidenti = pd.DataFrame(incidenti)
df_strade = pd.DataFrame(strade)
df_quartieri = pd.DataFrame(quartieri)

# Rinomina le colonne per evitare conflitti durante la fusione
df_strade.rename(columns={'FullId': 'Strada_FullId', 'Highway': 'Strada_Highway', 'Name': 'Strada_Name',
                          'Oneway': 'Strada_Oneway', 'Maxspeed': 'Strada_Maxspeed', 'Lanes': 'Strada_Lanes',
                          'Length': 'Strada_Length'}, inplace=True)
df_quartieri.rename(columns={'Id': 'Quartiere_Id', 'Nome': 'Quartiere_Nome', 'Area': 'Quartiere_Area',
                             'Pop2011': 'Quartiere_Pop2011', 'Densita': 'Quartiere_Densita'}, inplace=True)

# Unisci i DataFrame incidenti con strade
df_merged = pd.merge(df_incidenti, df_strade, how='left', left_on='Strada', right_on='Strada_FullId')
# Unisci i DataFrame risultanti con quartieri
df_merged = pd.merge(df_merged, df_quartieri, how='left', left_on='Quartiere', right_on='Quartiere_Id')

# Definisci il percorso assoluto del file CSV
output_path = os.path.abspath('../data/merged_data.csv')

# Salva il DataFrame unificato in un file CSV
df_merged.to_csv(output_path, index=False)

print(f"Dati unificati salvati in '{output_path}'")

# Script aggiuntivo per rimuovere colonne specificate
columns_to_remove = ['Id', 'Data', 'Pavimentaz', 'Ora', 'Minuto', 'Strada', 'NearestX', 'NearestY',
                     'Strada_FullId', 'Strada_Name', 'Strada_Length', 'Quartiere_Id', 'Quartiere_Nome']

df_reduced = df_merged.drop(columns=columns_to_remove)

# Salva il DataFrame ridotto in un file CSV
reduced_output_path = os.path.abspath('../data/merged_data_reduced.csv')
df_reduced.to_csv(reduced_output_path, index=False)

print(f"Dati ridotti salvati in '{reduced_output_path}'")

# Carica il dataset ridotto
df_reduced = pd.read_csv(reduced_output_path)

# Convertire le feature categoriche in numeriche utilizzando get_dummies
df_reduced = pd.get_dummies(df_reduced)

# Separare le feature (X) e il target (y)
X = df_reduced.drop(columns=['Lesioni'])
y = df_reduced['Lesioni']

# Gestire eventuali valori mancanti
X.fillna(-999, inplace=True)

# Imposta il modello RandomForest per Boruta
rf = RandomForestClassifier(n_jobs=-1, class_weight='balanced', max_depth=5, random_state=42)

# Imposta il percentile individuato dal plateau nel grafico
selected_percentile = 85

# Feature selection con BorutaPy usando il percentile selezionato
print(f"Percentile: {selected_percentile}")
boruta_selector = BorutaPy(rf, perc=selected_percentile, n_estimators='auto', verbose=2, random_state=42)
boruta_selector.fit(X.values, y.values)

# Salva i risultati della selezione delle feature
selected_features = boruta_selector.support_.sum()
print(f"Percentile: {selected_percentile}, Selected Features: {selected_features}")

# Salvare i risultati di feature selection (opzionale)
output_path_features = os.path.abspath('../data/selected_features.csv')
pd.DataFrame({'Feature': X.columns[boruta_selector.support_]}).to_csv(output_path_features, index=False)
print(f"Feature selezionate salvate in '{output_path_features}'")
