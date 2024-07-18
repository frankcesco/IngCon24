def apply_inference_rules():
    from pyswip import Prolog

    prolog = Prolog()

    # Dichiarazione dei predicati dinamici iniziali
    prolog.assertz(':- dynamic incidente/18')
    prolog.assertz(':- dynamic quartiere/6')

    # Carica i fatti aggiornati e le regole dalla cartella prolog
    prolog.consult('../prolog/updated_facts.pl')
    prolog.consult('../prolog/rules.pl')

    # Debug: stampa messaggio di inizio
    print("Inizio esecuzione delle regole di inferenza")

    # Verifica i dati caricati
    incidenti = list(prolog.query('incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere)'))
    quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011)'))

    print(f"Incidenti caricati: {len(incidenti)}")
    print(f"Quartieri caricati: {len(quartieri)}")

    # Stampa i fatti caricati per verifica
    for inc in incidenti:
        print(f"Incidente: {inc}")
    for qrt in quartieri:
        print(f"Quartiere: {qrt}")

    # Esegui le regole per aggiungere le nuove feature
    print("Esecuzione della regola: assign_fascia_oraria")
    list(prolog.query('assign_fascia_oraria'))
    print("Esecuzione della regola: assign_features")
    list(prolog.query('assign_features'))

    # Debug: stampa messaggio di fine
    print("Fine esecuzione delle regole di inferenza")

    # Recupera tutti i fatti aggiornati con le nuove feature
    updated_incidenti = list(prolog.query(
        'incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere, FasciaOraria)'))
    updated_quartieri = list(prolog.query('quartiere(Id, Nome, Area, Pop2011, NumeroIncidenti, Densita)'))

    # Debug: stampa il numero di fatti aggiornati trovati
    print(f"Fatti aggiornati degli incidenti trovati: {len(updated_incidenti)}")
    print(f"Fatti aggiornati dei quartieri trovati: {len(updated_quartieri)}")

    # Determina i nuovi predicati dinamici
    incidente_arity = 18 if not updated_incidenti else len(updated_incidenti[0])
    quartiere_arity = 6 if not updated_quartieri else len(updated_quartieri[0])

    # Leggi i fatti originali da updated_facts.pl
    with open('../prolog/updated_facts.pl', 'r', encoding='utf-8') as original_file:
        lines = original_file.readlines()

    # Scrivi solo i fatti aggiornati e non nulli in final_facts.pl
    with open('../prolog/final_facts.pl', 'w', encoding='utf-8') as final_file:
        # Scrivi i nuovi predicati dinamici
        final_file.write('% Predicati dinamici\n')
        final_file.write(f':- dynamic incidente/{incidente_arity}.\n')
        final_file.write(':- dynamic strada/7.\n')
        final_file.write(f':- dynamic quartiere/{quartiere_arity}.\n')

        for line in lines:
            # Salta le vecchie dichiarazioni dinamiche e il commento duplicato
            if line.startswith('% Predicati dinamici') or line.startswith(':- dynamic '):
                continue
            if line.startswith('incidente('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in updated_incidenti:
                    if fact['Id'] == original_id:
                        final_file.write(
                            f"incidente('{fact['Id']}', '{fact['Data']}', '{fact['Abitato']}', '{fact['Tipo']}', '{fact['Caratteris']}', {fact['Asciutto']}, '{fact['Pavimentaz']}', '{fact['Meteo']}', '{fact['Traffico']}', {fact['DanniCose']}, {fact['Lesioni']}, {fact['Chiamata']}, {fact['Arrivo']}, '{fact['Strada']}', {fact['NearestX']}, {fact['NearestY']}, '{fact['Quartiere']}', '{fact['FasciaOraria']}').\n")
                        break
            elif line.startswith('quartiere('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in updated_quartieri:
                    if fact['Id'] == original_id:
                        final_file.write(
                            f"quartiere('{fact['Id']}', '{fact['Nome']}', {fact['Area']}, {fact['Pop2011']}, {fact['NumeroIncidenti']}, {fact['Densita']}).\n")
                        break
            else:
                final_file.write(line)

    print("Fatti con nuove feature salvati in ../prolog/final_facts.pl")


if __name__ == '__main__':
    apply_inference_rules()
