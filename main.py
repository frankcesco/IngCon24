import csv

def generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file):
    with open(output_file, 'w', encoding='utf-8') as prolog_file:
        # Process Incidenti
        with open(incidenti_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"incidente('{row['data']}', '{row['abitato']}', '{row['tipo']}', '{row['carattistiche']}', "
                                  f"'{row['stato_fondo']}', '{row['pavimentazione']}', '{row['meteo']}', '{row['traffico']}', "
                                  f"'{row['danni_cose']}', '{row['lesioni']}', '{row['chiamata']}', '{row['arrivo']}', "
                                  f"'{row['strada']}', {row['nearest_x']}, {row['nearest_y']}, '{row['quartiere']}').\n")

        # Process Strade
        with open(strade_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"strada('{row['full_id']}', '{row['highway']}', '{row['name']}', '{row['oneway']}', "
                                  f"{row['maxspeed'] if row['maxspeed'] else 'null'}, {row['lanes']}, {row['length']}).\n")

        # Process Quartieri
        with open(quartieri_file, newline='', encoding='utf-8') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                prolog_file.write(f"quartiere('{row['id']}', '{row['nome']}', {row['area']}, {row['pop2011']}).\n")

# File paths
incidenti_file = 'incidenti223.csv'
strade_file = 'strade.csv'
quartieri_file = 'quartieri.csv'
output_file = 'knowledge_base.pl'

# Generate the Prolog knowledge base
generate_prolog_kb(incidenti_file, strade_file, quartieri_file, output_file)

print(f"Knowledge base generated and saved to {output_file}")