from pyswip import Prolog
import pandas as pd
import numpy as np
from scipy.stats import chi2_contingency, pearsonr


def extract_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre dati per danni_cose e lesioni, insieme all'ID dell'incidente
    incident_ids = []
    danni_cose_data = []
    lesioni_data = []

    query = (
        "incidente(ID, _, _, _, _, _, _, _, _, DanniCose, Lesioni, _, _, _, _, _, _, _)"
    )

    for result in prolog.query(query):
        incident_ids.append(result["ID"])
        danni_cose_data.append(result["DanniCose"])
        lesioni_data.append(result["Lesioni"])

    # Creare dataframe
    df = pd.DataFrame({
        "ID": incident_ids,
        "DanniCose": danni_cose_data,
        "Lesioni": lesioni_data
    })

    return df


def calculate_tetrachoric_correlation(contingency_table):
    # Estrarre valori dalla tabella di contingenza
    a = contingency_table[0, 0]
    b = contingency_table[0, 1]
    c = contingency_table[1, 0]
    d = contingency_table[1, 1]

    if (a + b) * (c + d) * (a + c) * (b + d) == 0:
        tetracorr = np.nan
    else:
        # Calcolare la proporzione di successi e fallimenti
        prop = (a + c) * (b + d) / ((a + b) * (c + d))
        # Applicare la formula generale della correlazione tetracorica
        tetracorr = np.cos(np.pi / (1 + np.sqrt(prop)))

    return tetracorr


def calculate_chi_squared(contingency_table):
    # Calcolare il valore chi-quadrato e il p-value
    chi2, p, _, _ = chi2_contingency(contingency_table, correction=False)
    return chi2, p


def calculate_pearson_correlation(df):
    # Assicurati che i dati siano numerici
    df['DanniCose'] = pd.to_numeric(df['DanniCose'])
    df['Lesioni'] = pd.to_numeric(df['Lesioni'])

    # Calcola il coefficiente di correlazione di Pearson
    pearson_corr, _ = pearsonr(df['DanniCose'], df['Lesioni'])

    return pearson_corr


if __name__ == '__main__':
    # Estrarre dati da Prolog
    df = extract_data()

    # Stampare i dati caricati per verifica
    print("Dati estratti:")
    print(df)

    # Calcolare la tabella di contingenza
    contingency_table = np.zeros((2, 2))

    for _, row in df.iterrows():
        if row['DanniCose'] == 0 and row['Lesioni'] == 0:
            contingency_table[0, 0] += 1
        elif row['DanniCose'] == 0 and row['Lesioni'] == 1:
            contingency_table[0, 1] += 1
        elif row['DanniCose'] == 1 and row['Lesioni'] == 0:
            contingency_table[1, 0] += 1
        elif row['DanniCose'] == 1 and row['Lesioni'] == 1:
            contingency_table[1, 1] += 1

    # Calcolare la correlazione tetracorica
    tetracorr = calculate_tetrachoric_correlation(contingency_table)

    # Calcolare il chi-quadrato e il p-value
    chi2, p = calculate_chi_squared(contingency_table)

    # Calcolare la correlazione di Pearson
    pearson_corr = calculate_pearson_correlation(df)

    # Visualizzare i risultati
    print("Tabella di Contingenza:")
    print(contingency_table)

    print("Correlazione tetracorica tra 'DanniCose' e 'Lesioni':")
    print(tetracorr)

    print("Chi-quadrato e p-value:")
    print(f"Chi-quadrato: {chi2}")
    print(f"p-value: {p}")

    print("Coefficiente di correlazione di Pearson tra 'DanniCose' e 'Lesioni':")
    print(pearson_corr)
