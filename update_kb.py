from pyswip import Prolog
import csv

def run_prolog_rules():
    prolog = Prolog()

    # Load facts and rules
    prolog.consult('facts.pl')
    prolog.consult('rules.pl')

    # Execute the rule to process max speeds with an open-world assumption
    list(prolog.query('assign_speeds'))

    # Retrieve all updated facts
    updated_facts = list(prolog.query('strada(Id, Highway, Name, Oneway, Maxspeed, Lanes, Length)'))

    # Read the original facts from facts.pl
    with open('facts.pl', 'r', encoding='utf-8') as original_file:
        lines = original_file.readlines()

    # Replace the updated facts in the facts.pl file
    with open('facts.pl', 'w', encoding='utf-8') as updated_file:
        for line in lines:
            if line.startswith('strada('):
                original_id = line.split(',')[0].split('(')[1].strip("'")
                for fact in updated_facts:
                    if fact['Id'] == original_id:
                        updated_file.write(f"strada('{fact['Id']}', '{fact['Highway']}', '{fact['Name']}', '{fact['Oneway']}', "
                                           f"{fact['Maxspeed']}, {fact['Lanes']}, {fact['Length']}).\n")
                        break
            else:
                updated_file.write(line)

    print("Updated speeds saved to facts.pl")

    # Export the results to a CSV file
    with open('updated_speeds.csv', 'w', newline='', encoding='utf-8') as csvfile:
        fieldnames = ['Id', 'Maxspeed']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()
        for fact in updated_facts:
            writer.writerow({'Id': fact['Id'], 'Maxspeed': fact['Maxspeed']})

    print("Updated speeds exported to updated_speeds.csv")

if __name__ == '__main__':
    run_prolog_rules()
