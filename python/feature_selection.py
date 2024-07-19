from pyswip import Prolog
import pandas as pd

def extract_prolog_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre i dati delle strade
    strade = list(prolog.query('strada(Id, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))
    # Estrarre i dati degli incidenti
    incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Ora, Minuto, Strada, NearestX, NearestY, Quartiere, FasciaOraria)'))
    # Estrarre i dati dei quartieri
    quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011, Densita)'))

    return incidenti, strade, quartieri

incidenti, strade, quartieri = extract_prolog_data()

# Converti i risultati delle query Prolog in DataFrame pandas
df_incidenti = pd.DataFrame(incidenti)
df_strade = pd.DataFrame(strade)
df_quartieri = pd.DataFrame(quartieri)

# Salva i DataFrame in file CSV
df_incidenti.to_csv('incidenti.csv', index=False)
df_strade.to_csv('strade.csv', index=False)
df_quartieri.to_csv('quartieri.csv', index=False)
