from pyswip import Prolog

def run_prolog_rules():
    prolog = Prolog()

    # Carica i fatti e le regole
    prolog.consult('facts.pl')
    prolog.consult('rules.pl')

    # Esegui la regola per elaborare le velocità massime con una open-world assumption
    list(prolog.query('assign_speeds'))

    # Verifica i risultati
    results = list(prolog.query('strada(Id, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))
    for result in results:
        print(result)

if __name__ == '__main__':
    run_prolog_rules()
