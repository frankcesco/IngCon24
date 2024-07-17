% Regola per assegnare 30 km/h alle strade di tipo track con maxspeed nullo.
assign_track_speed :-
    retract(strada(Id, 'track', Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, 'track', Name, Oneway, 30, Lanes, Length)),
    fail.
assign_track_speed.

% Predicati per memorizzare la distribuzione di probabilità pesata
:- dynamic speed_distribution/3.

% Calcolo della distribuzione pesata per ciascuna categoria di strada
calculate_speed_distribution :-
    findall(Highway, strada(_, Highway, _, _, _, _, _), Highways),
    list_to_set(Highways, UniqueHighways),
    forall(member(Hwy, UniqueHighways), calculate_distribution_for(Hwy)).

calculate_distribution_for(Highway) :-
    findall([MaxSpeed, Length], strada(_, Highway, _, _, MaxSpeed, _, Length), SpeedsAndLengths),
    exclude(is_null_speed, SpeedsAndLengths, ValidSpeedsAndLengths),
    calculate_weighted_distribution(ValidSpeedsAndLengths, WeightedDistribution, TotalWeight),
    retractall(speed_distribution(Highway, _, _)),
    assertz(speed_distribution(Highway, WeightedDistribution, TotalWeight)).

is_null_speed([null, _]).

calculate_weighted_distribution(SpeedsAndLengths, WeightedDistribution, TotalWeight) :-
    findall([Speed, Weight], (member([Speed, Length], SpeedsAndLengths), ceiling(Length, Weight)), WeightedSpeedsAndWeights),
    msort(WeightedSpeedsAndWeights, SortedWeightedSpeedsAndWeights),
    calculate_totals(SortedWeightedSpeedsAndWeights, WeightedDistribution, 0, TotalWeight).

calculate_totals([], [], TotalWeight, TotalWeight).
calculate_totals([[Speed, Weight]|Rest], [[Speed, CumulativeWeight]|WeightedDistribution], Accum, TotalWeight) :-
    CumulativeWeight is Accum + Weight,
    calculate_totals(Rest, WeightedDistribution, CumulativeWeight, TotalWeight).

% Regola per assegnare i valori mancanti basandosi sulla distribuzione di probabilità pesata
assign_missing_speeds :-
    strada(Id, Highway, Name, Oneway, null, Lanes, Length),
    speed_distribution(Highway, Distribution, TotalWeight),
    random_between(1, TotalWeight, Rand),
    select_speed(Distribution, Rand, SelectedSpeed),
    retract(strada(Id, Highway, Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, Highway, Name, Oneway, SelectedSpeed, Lanes, Length)),
    fail.
assign_missing_speeds.

select_speed([[Speed, CumulativeWeight]|_], Rand, Speed) :-
    Rand =< CumulativeWeight.
select_speed([[_, CumulativeWeight]|Rest], Rand, Speed) :-
    Rand > CumulativeWeight,
    NewRand is Rand - CumulativeWeight,
    select_speed(Rest, NewRand, Speed).

% Regola che chiama le regole di assegnazione della velocità con open-world assumption
assign_speeds :-
    assign_track_speed,
    calculate_speed_distribution,
    assign_missing_speeds.

% Calcolo della fascia oraria di un incidente basata sull'orario di chiamata
fascia_oraria(Chiamata, 'mattina') :- Chiamata >= 6, Chiamata < 12.
fascia_oraria(Chiamata, 'pomeriggio') :- Chiamata >= 12, Chiamata < 18.
fascia_oraria(Chiamata, 'sera') :- Chiamata >= 18, Chiamata < 24.
fascia_oraria(Chiamata, 'notte') :- Chiamata >= 0, Chiamata < 6.

% Calcolo della densità di popolazione di un quartiere
densita_quartiere(Quartiere, Densita) :-
    quartiere(Quartiere, _, Area, Popolazione),
    Area > 0,
    Densita is Popolazione / Area.

% Conta il numero di incidenti per quartiere
incidenti_per_quartiere(Quartiere, NumeroIncidenti) :-
    findall(_, incidente(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Quartiere), Incidenti),
    length(Incidenti, NumeroIncidenti),
    writeln(incidenti_per_quartiere(Quartiere, NumeroIncidenti)).

% Regola per aggiungere fascia oraria agli incidenti
assign_fascia_oraria :-
    incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere),
    fascia_oraria(Chiamata, FasciaOraria),
    retract(incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere)),
    assertz(incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere, FasciaOraria)),
    writeln(assign_fascia_oraria(Id, FasciaOraria)),
    fail.
assign_fascia_oraria.

% Calcolo dell'incidentalità di un quartiere
incidentalita_quartiere(Quartiere, Incidentalita) :-
    incidenti_per_quartiere(Quartiere, NumeroIncidentiQuartiere),
    findall(_, incidente(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), IncidentiTotali),
    length(IncidentiTotali, NumeroIncidentiTotali),
    writeln(total_incidents(NumeroIncidentiTotali)),  % Debug: stampa il numero totale di incidenti
    densita_quartiere(Quartiere, Densita),
    writeln(density(Quartiere, Densita)),  % Debug: stampa la densità del quartiere
    Densita > 0,
    Incidentalita is (NumeroIncidentiQuartiere / NumeroIncidentiTotali) / Densita,
    writeln(incidentalita_quartiere(Quartiere, Incidentalita)).

% Regola per aggiungere incidentalità ai quartieri
assign_incidentalita :-
    quartiere(Id, Nome, Area, Pop2011),
    incidentalita_quartiere(Id, Incidentalita),
    retract(quartiere(Id, Nome, Area, Pop2011)),
    assertz(quartiere(Id, Nome, Area, Pop2011, Incidentalita)),
    writeln(assign_incidentalita(Id, Incidentalita)),
    fail.
assign_incidentalita.
