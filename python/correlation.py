from pyswip import Prolog
import pandas as pd
import numpy as np
from scipy.stats import chi2_contingency


def extract_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre dati per danni_cose e lesioni
    danni_cose_data = []
    lesioni_data = []

    query = (
        "incidente(_, _, _, _, _, _, _, _, _, DanniCose, Lesioni, _, _, _, _, _, _, _)"
    )

    for result in prolog.query(query):
        danni_cose_data.append(result["DanniCose"])
        lesioni_data.append(result["Lesioni"])

    # Creare dataframe
    df = pd.DataFrame({
        "DanniCose": danni_cose_data,
        "Lesioni": lesioni_data
    })

    return df


def calculate_tetrachoric_correlation(df):
    # Creare una tabella di contingenza
    contingency_table = pd.crosstab(df['DanniCose'], df['Lesioni'])

    # Stampare la tabella di contingenza per debugging
    print("Tabella di Contingenza:")
    print(contingency_table)

    # Assicurati che la tabella sia 2x2
    if contingency_table.shape != (2, 2):
        raise ValueError("La tabella di contingenza non è 2x2. Verifica i dati.")

    # Calcolare il coefficiente tetracorico
    chi2, p, dof, expected = chi2_contingency(contingency_table, correction=False)

    # Assicurati che il calcolo non dia errori
    print(f"Chi-quadrato: {chi2}, N: {contingency_table.sum().sum()}")

    # Correlazione tetracorica (approssimazione)
    try:
        tetracorr = np.sqrt((chi2 - contingency_table.sum().sum()) / (contingency_table.sum().sum() + 1))
    except ValueError as e:
        print(f"Errore nel calcolo della correlazione tetracorica: {e}")
        tetracorr = np.nan

    return tetracorr


if __name__ == '__main__':
    # Estrarre dati da Prolog
    df = extract_data()

    # Calcolare la correlazione tetracorica
    tetracorr = calculate_tetrachoric_correlation(df)

    # Visualizzare i risultati
    print("Correlazione tetracorica tra 'DanniCose' e 'Lesioni':")
    print(tetracorr)
