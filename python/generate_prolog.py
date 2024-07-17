import csv
import os

def generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file):
    with open(output_file, 'w', encoding='utf-8') as prolog_file:
        # Dichiarazione di predicati dinamici
        prolog_file.write("% Predicati dinamici\n")
        prolog_file.write(":- dynamic incidente/17.\n")
        prolog_file.write(":- dynamic strada/7.\n")
        prolog_file.write(":- dynamic quartiere/4.\n\n")

        # Funzione per rimuovere apostrofi
        def remove_apostrophe(value):
            return value.replace("'", " ")

        # Funzione per convertire valori booleani
        def convert_boolean(value):
            if value.lower() in ['yes', 'si']:
                return '1'
            elif value.lower() == 'no':
                return '0'
            return value

        # Funzione per rimpiazzare valori vuoti con 'null'
        def replace_empty_with_null(value):
            return 'null' if value == '' else value

        # Funzione per trasformare il campo 'chiamata' in 'ora' e 'minuto'
        def transform_chiamata(value):
            parts = value.split(':')
            if len(parts) >= 2:
                return parts[0], parts[1]
            return 'null', 'null'

        # Funzione per formattare i valori per Prolog
        def format_value(value, is_boolean=False, is_chiamata=False):
            if is_chiamata:
                ora, minuto = transform_chiamata(value)
                return f"{ora}, {minuto}"
            value = replace_empty_with_null(value)
            if is_boolean:
                value = convert_boolean(value)
            return value if value == 'null' or is_boolean else f"'{remove_apostrophe(value)}'"

        # Processo gli Incidenti
        with open(incidenti_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"incidente({format_value(row['id'])}, {format_value(row['data'])}, "
                                  f"{format_value(row['abitato'], is_boolean=True)}, {format_value(row['tipo'])}, "
                                  f"{format_value(row['caratteris'])}, {format_value(row['asciutto'], is_boolean=True)}, "
                                  f"{format_value(row['pavimentaz'])}, {format_value(row['meteo'])}, "
                                  f"{format_value(row['traffico'])}, {format_value(row['danni_cose'], is_boolean=True)}, "
                                  f"{format_value(row['lesioni'], is_boolean=True)}, {format_value(row['chiamata'], is_chiamata=True)}, "
                                  f"{format_value(row['strada'])}, {replace_empty_with_null(row['nearest_x'])}, "
                                  f"{replace_empty_with_null(row['nearest_y'])}, {format_value(row['quartiere'])}).\n")

        # Processo le Strade
        with open(strade_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"strada({format_value(row['full_id'])}, {format_value(row['highway'])}, "
                                  f"{format_value(row['name'])}, {format_value(row['oneway'], is_boolean=True)}, "
                                  f"{replace_empty_with_null(row['maxspeed'])}, {replace_empty_with_null(row['lanes'])}, "
                                  f"{replace_empty_with_null(row['length'])}).\n")

        # Processo i Quartieri
        with open(quartieri_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"quartiere({format_value(row['id'])}, {format_value(row['nome'])}, "
                                  f"{replace_empty_with_null(row['area'])}, {replace_empty_with_null(row['pop2011'])}).\n")

if __name__ == "__main__":
    # Definisco i percorsi
    base_path = os.path.abspath(os.path.dirname(__file__))
    data_path = os.path.join(base_path, '..\data')
    prolog_path = os.path.join(base_path, '..\prolog')
    incidenti_file = os.path.join(data_path, 'incidenti.csv')
    strade_file = os.path.join(data_path, 'strade.csv')
    quartieri_file = os.path.join(data_path, 'quartieri.csv')
    output_file = os.path.join(prolog_path, 'facts.pl')

    # Genero la base di conoscenza Prolog
    generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file)

    print(f"Base di conoscenza generata e salvata in {output_file}")
