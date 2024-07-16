from pyswip import Prolog
import csv

def run_prolog_rules():
    prolog = Prolog()

    # Carica i fatti e le regole
    prolog.consult('facts.pl')
    prolog.consult('rules.pl')

    # Esegui la regola per elaborare le velocità massime con un'ipotesi di mondo aperto
    list(prolog.query('assign_speeds'))

    # Recupera tutti i fatti aggiornati
    updated_strade = list(prolog.query('strada(Id, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))
    updated_incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere)'))
    updated_quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011)'))

    # Funzione per verificare se ci sono valori nulli
    def has_null_values(fact):
        return any(value == 'null' or value is None for value in fact.values())

    # Filtra i fatti aggiornati per rimuovere quelli con valori nulli
    filtered_strade = [fact for fact in updated_strade if not has_null_values(fact)]
    filtered_incidenti = [fact for fact in updated_incidenti if not has_null_values(fact)]
    filtered_quartieri = [fact for fact in updated_quartieri if not has_null_values(fact)]

    # Leggi i fatti originali da facts.pl
    with open('facts.pl', 'r', encoding='utf-8') as original_file:
        lines = original_file.readlines()

    # Scrivi solo i fatti aggiornati e non nulli in updated_facts.pl
    with open('updated_facts.pl', 'w', encoding='utf-8') as updated_file:
        for line in lines:
            if line.startswith('strada('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in filtered_strade:
                    if fact['Id'] == original_id:
                        updated_file.write(f"strada('{fact['Id']}', '{fact['Highway']}', '{fact['Name']}', {fact['Oneway']}, "
                                           f"{fact['Maxspeed']}, {fact['Lanes']}, {fact['Length']}).\n")
                        break
            elif line.startswith('incidente('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in filtered_incidenti:
                    if fact['Id'] == original_id:
                        updated_file.write(f"incidente('{fact['Id']}', '{fact['Data']}', {fact['Abitato']}, '{fact['Tipo']}', '{fact['Caratteris']}', "
                                           f"{fact['Asciutto']}, '{fact['Pavimentaz']}', '{fact['Meteo']}', '{fact['Traffico']}', {fact['DanniCose']}, "
                                           f"{fact['Lesioni']}, '{fact['Chiamata']}', '{fact['Arrivo']}', '{fact['Strada']}', {fact['NearestX']}, "
                                           f"{fact['NearestY']}, '{fact['Quartiere']}').\n")
                        break
            # I quartieri non vengono gestiti in quanto non contengono valori nulli, e una eventuale gestione rimuove cifre significative dall'area.
            else:
                updated_file.write(line)

    print("Fatti filtrati e aggiornati salvati in updated_facts.pl")

    # Esporta i risultati in un file CSV
    with open('updated_speeds.csv', 'w', newline='', encoding='utf-8') as csvfile:
        fieldnames = ['Id', 'Maxspeed']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()
        for fact in filtered_strade:
            writer.writerow({'Id': fact['Id'], 'Maxspeed': fact['Maxspeed']})

    print("Velocità aggiornate esportate in updated_speeds.csv")

if __name__ == '__main__':
    run_prolog_rules()
