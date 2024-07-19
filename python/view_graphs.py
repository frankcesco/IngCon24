from pyswip import Prolog
import pandas as pd
import matplotlib.pyplot as plt
import os

def extract_data():
    prolog = Prolog()
    prolog.consult('../prolog/final_facts.pl')

    # Estrarre dati per pavimentazione e danni_cose
    pavimentazione_data = []
    danni_cose_data = []

    for result in prolog.query(
            "incidente(_, _, _, _, _, StatoFondo, Pavimentazione, _, _, DanniCose, _, _, _, _, _, _, _, _)"):
        pavimentazione_data.append(result["Pavimentazione"])
        danni_cose_data.append(result["DanniCose"])

    # Creare dataframe
    df_pavimentazione = pd.DataFrame(pavimentazione_data, columns=["Pavimentazione"])
    df_danni_cose = pd.DataFrame(danni_cose_data, columns=["DanniCose"])

    return df_pavimentazione, df_danni_cose

def visualize_data(df_pavimentazione, df_danni_cose):
    # Creare la cartella plots se non esiste
    output_dir = '../plots'
    os.makedirs(output_dir, exist_ok=True)

    # Grafico a barre per pavimentazione
    pavimentazione_counts = df_pavimentazione['Pavimentazione'].value_counts()
    plt.figure(figsize=(12, 8))
    pavimentazione_counts.plot(kind='bar')
    plt.title('Distribuzione dei valori di Pavimentazione')
    plt.xlabel('Pavimentazione', fontsize=12)
    plt.ylabel('Frequenza (logaritmica)', fontsize=12)
    plt.yscale('log')  # Imposta l'asse Y come logaritmico

    # Imposta i tick personalizzati per l'asse Y
    yticks = [1, 10, 100, 1000, 10000]  # Personalizza i valori dei tick
    plt.yticks(yticks, [str(y) for y in yticks])  # Imposta le etichette dei tick

    plt.xticks(rotation=45, ha='right', fontsize=10)
    plt.tight_layout()  # Per evitare il taglio delle etichette
    plt.savefig(os.path.join(output_dir, 'pavimentazione_chart.png'))
    plt.show()

    # Grafico a barre per danni_cose
    danni_cose_counts = df_danni_cose['DanniCose'].value_counts()
    plt.figure(figsize=(10, 6))
    danni_cose_counts.plot(kind='bar')
    plt.title('Distribuzione dei valori di Danni Cose')
    plt.xlabel('Danni Cose')
    plt.ylabel('Frequenza')
    plt.xticks(rotation=0)
    plt.savefig(os.path.join(output_dir, 'danni_cose_chart.png'))
    plt.show()

if __name__ == '__main__':
    # Estrarre dati da Prolog
    df_pavimentazione, df_danni_cose = extract_data()

    # Visualizzare e salvare i grafici
    visualize_data(df_pavimentazione, df_danni_cose)

    print("Dati estratti, grafici creati e salvati.")
