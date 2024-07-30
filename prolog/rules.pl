% Regola per assegnare 30 km/h alle strade di tipo track con maxspeed nullo
assign_track_speed :-
    retract(strada(Id, 'track', Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, 'track', Name, Oneway, 30, Lanes, Length)),
    fail.
assign_track_speed.

% Predicati per memorizzare la media pesata della velocità
:- dynamic speed_mean/2.

% Calcolo della media pesata per ciascuna categoria di strada
calculate_speed_mean :-
    findall(Highway, strada(_, Highway, _, _, _, _, _), Highways),
    list_to_set(Highways, UniqueHighways),
    forall(member(Hwy, UniqueHighways), calculate_mean_for(Hwy)).

calculate_mean_for(Highway) :-
    findall([MaxSpeed, Length], strada(_, Highway, _, _, MaxSpeed, _, Length), SpeedsAndLengths),
    exclude(is_null_speed, SpeedsAndLengths, ValidSpeedsAndLengths),
    calculate_weighted_mean(ValidSpeedsAndLengths, MeanSpeed),
    retractall(speed_mean(Highway, _)),
    assertz(speed_mean(Highway, MeanSpeed)).

is_null_speed([null, _]).

calculate_weighted_mean(SpeedsAndLengths, MeanSpeed) :-
    findall(Speed * Length, (member([Speed, Length], SpeedsAndLengths), Speed \= null), WeightedSpeeds),
    sum_list(WeightedSpeeds, TotalWeightedSpeed),
    findall(Length, member([_, Length], SpeedsAndLengths), Lengths),
    sum_list(Lengths, TotalLength),
    ( TotalLength > 0 -> MeanSpeed is TotalWeightedSpeed / TotalLength ; MeanSpeed = 0 ).

% Regola per assegnare i valori mancanti basandosi sulla media pesata
assign_missing_speeds :-
    strada(Id, Highway, Name, Oneway, null, Lanes, Length),
    speed_mean(Highway, MeanSpeed),
    retract(strada(Id, Highway, Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, Highway, Name, Oneway, MeanSpeed, Lanes, Length)),
    fail.
assign_missing_speeds.

% Regola che chiama le regole di assegnazione della velocità con open-world assumption
assign_speeds :-
    assign_track_speed,
    calculate_speed_mean,
    assign_missing_speeds.

% Calcolo della fascia oraria di un incidente basata sull'orario di chiamata
fascia_oraria(Chiamata, 'mattina') :- Chiamata >= 6, Chiamata < 12.
fascia_oraria(Chiamata, 'pomeriggio') :- Chiamata >= 12, Chiamata < 18.
fascia_oraria(Chiamata, 'sera') :- Chiamata >= 18, Chiamata < 24.
fascia_oraria(Chiamata, 'notte') :- Chiamata >= 0, Chiamata < 6.

% Regola per aggiungere fascia oraria agli incidenti
assign_fascia_oraria :-
    incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere),
    fascia_oraria(Chiamata, FasciaOraria),
    retract(incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere)),
    assertz(incidente(Id, Data, Abitato, Tipo, Caratteris, Asciutto, Pavimentaz, Meteo, Traffico, DanniCose, Lesioni, Chiamata, Arrivo, Strada, NearestX, NearestY, Quartiere, FasciaOraria)),
    fail.
assign_fascia_oraria.

% Calcolo della densità di popolazione di un quartiere in abitanti per km quadrato
densita_quartiere(Quartiere, Densita) :-
    quartiere(Quartiere, _, Area, Popolazione),
    AreaKm2 is Area / 1000000,  % Converti l'area da m^2 a km^2
    AreaKm2 > 0,
    Densita is Popolazione / AreaKm2.

% Regola per aggiungere la densità ai quartieri
assign_features :-
    quartiere(Id, Nome, Area, Pop2011),
    densita_quartiere(Id, Densita),
    retract(quartiere(Id, Nome, Area, Pop2011)),
    assertz(quartiere(Id, Nome, Area, Pop2011, Densita)),
    fail.
assign_features.
