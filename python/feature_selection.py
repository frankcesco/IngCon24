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

# FEATURE SELECTION

# Carica il dataset ridotto
df_reduced = pd.read_csv(reduced_output_path)

# Verifica che il dataset sia stato caricato correttamente
print("Dati ridotti caricati:")
print(df_reduced.head())

# Convertire le feature categoriche in numeriche utilizzando get_dummies
df_reduced = pd.get_dummies(df_reduced)

# Separare le feature (X) e il target (y)
X = df_reduced.drop(columns=['Lesioni'])
y = df_reduced['Lesioni']

# Gestire eventuali valori mancanti
X.fillna(-999, inplace=True)

# Imposta il modello RandomForest per Boruta
rf = RandomForestClassifier(n_jobs=-1, class_weight='balanced', max_depth=5, random_state=42)

# Esegui Boruta
boruta_selector = BorutaPy(rf, n_estimators='auto', verbose=2, random_state=42)
boruta_selector.fit(X.values, y.values)

# Ottenere i risultati di Boruta
feature_ranks = boruta_selector.ranking_
feature_names = X.columns

# Creare un DataFrame con i risultati
features = pd.DataFrame({
    'Feature': feature_names,
    'Rank': feature_ranks
})

# Ordinare le feature per importanza
features.sort_values(by='Rank', inplace=True)

# Visualizzare le feature più importanti
print("Feature più importanti secondo Boruta:")
print(features.head(10))

# Salva i risultati in un file CSV
output_path_boruta = os.path.abspath('../data/boruta_feature_importance.csv')
features.to_csv(output_path_boruta, index=False)

print(f"Risultati di Boruta salvati in '{output_path_boruta}'")