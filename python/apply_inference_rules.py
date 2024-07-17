from pyswip import Prolog

def apply_inference_rules():
    prolog = Prolog()

    # Dichiarazione dei predicati dinamici con la nuova arità
    prolog.assertz(':- dynamic incidente/18')
    prolog.assertz(':- dynamic quartiere/5')

    # Carica i fatti aggiornati e le regole
    prolog.consult('updated_facts.pl')
    prolog.consult('rules.pl')

    # Debug: stampa messaggio di inizio
    print("Inizio esecuzione delle regole di inferenza")

    # Esegui le regole per aggiungere le nuove feature
    print("Esecuzione della regola: assign_fascia_oraria")
    list(prolog.query('assign_fascia_oraria'))
    print("Esecuzione della regola: assign_incidentalita")
    list(prolog.query('assign_incidentalita'))

    # Debug: stampa messaggio di fine
    print("Fine esecuzione delle regole di inferenza")

    # Recupera tutti i fatti aggiornati con le nuove feature
    updated_incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere, FasciaOraria)'))
    updated_quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011, Incidentalita)'))

    # Debug: stampa il numero di fatti aggiornati trovati
    print(f"Fatti aggiornati degli incidenti trovati: {len(updated_incidenti)}")
    print(f"Fatti aggiornati dei quartieri trovati: {len(updated_quartieri)}")

    # Leggi i fatti originali da updated_facts.pl
    with open('../prolog/updated_facts.pl', 'r', encoding='utf-8') as original_file:
        lines = original_file.readlines()

    # Scrivi solo i fatti aggiornati e non nulli in final_facts.pl
    with open('final_facts.pl', 'w', encoding='utf-8') as final_file:
        for line in lines:
            if line.startswith('incidente('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in updated_incidenti:
                    if fact['Id'] == original_id:
                        final_file.write(f"incidente('{fact['Id']}', '{fact['Data']}', '{fact['Abitato']}', '{fact['Tipo']}', '{fact['Caratteris']}', {fact['Asciutto']}, '{fact['Pavimentaz']}', '{fact['Meteo']}', '{fact['Traffico']}', {fact['DanniCose']}, {fact['Lesioni']}, {fact['Chiamata']}, {fact['Arrivo']}, '{fact['Strada']}', {fact['NearestX']}, {fact['NearestY']}, '{fact['Quartiere']}', '{fact['FasciaOraria']}').\n")
                        break
            elif line.startswith('quartiere('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in updated_quartieri:
                    if fact['Id'] == original_id:
                        final_file.write(f"quartiere('{fact['Id']}', '{fact['Nome']}', {fact['Area']}, {fact['Pop2011']}, {fact['Incidentalita']}).\n")
                        break
            else:
                final_file.write(line)

    print("Fatti con nuove feature salvati in final_facts.pl")

if __name__ == '__main__':
    apply_inference_rules()
