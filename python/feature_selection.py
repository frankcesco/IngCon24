from pyswip import Prolog
import pandas as pd
import os

def extract_prolog_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre i dati delle strade con 7 argomenti
    strade = list(prolog.query('strada(FullId, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))
    # Estrarre i dati degli incidenti con 18 argomenti
    incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Ora, Minuto, Strada, NearestX, NearestY, Quartiere, FasciaOraria)'))
    # Estrarre i dati dei quartieri con 5 argomenti
    quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011, TEST, Densita)'))

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