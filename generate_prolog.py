import csv
import os

def generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file):
    with open(output_file, 'w', encoding='utf-8') as prolog_file:
        # Dichiarazione di predicati dinamici
        prolog_file.write("% Predicati dinamici\n")
        prolog_file.write(":- dynamic incidente/17.\n")
        prolog_file.write(":- dynamic strada/7.\n")
        prolog_file.write(":- dynamic quartiere/4.\n\n")

        # Funzione per rimuovere apostrofo
        def remove_apostrophe(value):
            return value.replace("'", " ")

        # Processo gli Incidenti
        with open(incidenti_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"incidente('{remove_apostrophe(row['id'])}', '{remove_apostrophe(row['data'])}', "
                                  f"'{remove_apostrophe(row['abitato'])}', '{remove_apostrophe(row['tipo'])}', "
                                  f"'{remove_apostrophe(row['caratteris'])}', '{remove_apostrophe(row['asciutto'])}', "
                                  f"'{remove_apostrophe(row['pavimentaz'])}', '{remove_apostrophe(row['meteo'])}', "
                                  f"'{remove_apostrophe(row['traffico'])}', '{remove_apostrophe(row['danni_cose'])}', "
                                  f"'{remove_apostrophe(row['lesioni'])}', '{remove_apostrophe(row['chiamata'])}', "
                                  f"'{remove_apostrophe(row['arrivo'])}', '{remove_apostrophe(row['strada'])}', "
                                  f"{row['nearest_x']}, {row['nearest_y']}, '{remove_apostrophe(row['quartiere'])}').\n")

        # Processo le Strade
        with open(strade_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"strada('{remove_apostrophe(row['full_id'])}', '{remove_apostrophe(row['highway'])}', "
                                  f"'{remove_apostrophe(row['name'])}', '{remove_apostrophe(row['oneway'])}', "
                                  f"{row['maxspeed'] if row['maxspeed'] else 'null'}, {row['lanes']}, {row['length']}).\n")

        # Processo i Quartieri
        with open(quartieri_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"quartiere('{remove_apostrophe(row['id'])}', '{remove_apostrophe(row['nome'])}', "
                                  f"{row['area']}, {row['pop2011']}).\n")

if __name__ == "__main__":
    # Definisco i percorsi
    base_path = os.path.abspath(os.path.dirname(__file__))
    data_path = os.path.join(base_path, 'data')
    incidenti_file = os.path.join(data_path, 'incidenti.csv')
    strade_file = os.path.join(data_path, 'strade.csv')
    quartieri_file = os.path.join(data_path, 'quartieri.csv')
    output_file = os.path.join(base_path, 'facts.pl')

    # Genero la base di conoscenza Prolog
    generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file)

    print(f"Base di conoscenza generata e salvata in {output_file}")
