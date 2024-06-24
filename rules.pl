% Regola per assegnare 30 km/h alle strade di tipo track con maxspeed nullo.
assign_track_speed :-
    retract(strada(Id, 'track', Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, 'track', Name, Oneway, 30, Lanes, Length)),
    fail.
assign_track_speed.

% Predicati per memorizzare la distribuzione di probabilità
:- dynamic speed_distribution/3.

% Calcolo della distribuzione per ciascuna categoria di strada
calculate_speed_distribution :-
    findall(Highway, strada(_, Highway, _, _, _, _, _), Highways),
    list_to_set(Highways, UniqueHighways),
    forall(member(Hwy, UniqueHighways), calculate_distribution_for(Hwy)).

calculate_distribution_for(Highway) :-
    findall(MaxSpeed, strada(_, Highway, _, _, MaxSpeed, _, _), Speeds),
    exclude(=(null), Speeds, ValidSpeeds),
    msort(ValidSpeeds, SortedSpeeds),
    length(SortedSpeeds, Total),
    findall([Speed, Count], (member(Speed, SortedSpeeds), count(SortedSpeeds, Speed, Count)), Counts),
    retractall(speed_distribution(Highway, _, _)),
    assertz(speed_distribution(Highway, Counts, Total)).

count(List, Element, Count) :-
    include(=(Element), List, Filtered),
    length(Filtered, Count).

% Regola per assegnare i valori mancanti basandosi sulla distribuzione di probabilità
assign_missing_speeds :-
    strada(Id, Highway, Name, Oneway, null, Lanes, Length),
    speed_distribution(Highway, Distribution, Total),
    random_between(1, Total, Rand),
    select_speed(Distribution, Rand, SelectedSpeed),
    retract(strada(Id, Highway, Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, Highway, Name, Oneway, SelectedSpeed, Lanes, Length)),
    fail.
assign_missing_speeds.

select_speed([[Speed, Count]|_], Rand, Speed) :-
    Rand =< Count.
select_speed([[_, Count]|Rest], Rand, Speed) :-
    NewRand is Rand - Count,
    select_speed(Rest, NewRand, Speed).

% Regola che chiama le regole di assegnazione della velocità con open-world assumption
assign_speeds :-
    assign_track_speed,
    calculate_speed_distribution,
    assign_missing_speeds.
