% pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense).
pokemon_stats(bulbasaur, [grass, poison], 45, 49, 49).
pokemon_stats(ivysaur, [grass, poison], 60, 62, 63).
pokemon_stats(venusaur, [grass, poison], 80, 100, 123).
pokemon_stats(charmander, [fire], 39, 52, 43).
pokemon_stats(charmeleon, [fire], 58, 64, 58).
pokemon_stats(charizard, [fire, flying], 78, 104, 78).
pokemon_stats(squirtle, [water], 44, 48, 65).
pokemon_stats(wartortle, [water], 59, 63, 80).
pokemon_stats(blastoise, [water], 79, 103, 120).
pokemon_stats(caterpie, [bug], 45, 30, 35).
pokemon_stats(metapod, [bug], 50, 20, 55).
pokemon_stats(butterfree, [bug, flying], 60, 45, 50).
pokemon_stats(pidgey, [normal, flying], 40, 45, 40).
pokemon_stats(pidgeotto, [normal, flying], 63, 60, 55).
pokemon_stats(pidgeot, [normal, flying], 83, 80, 80).
pokemon_stats(ekans, [poison], 35, 60, 44).
pokemon_stats(arbok, [poison], 60, 95, 69).
pokemon_stats(pikachu, [electric], 35, 55, 40).
pokemon_stats(raichu, [electric], 60, 85, 50).
pokemon_stats(vulpix, [fire, ice], 38, 41, 40).
pokemon_stats(ninetales, [fire, ice], 73, 67, 75).
pokemon_stats(jigglypuff, [normal, fairy], 115, 45, 20).
pokemon_stats(wigglytuff, [normal, fairy], 140, 70, 45).
pokemon_stats(zubat, [poison, flying], 40, 45, 35).
pokemon_stats(golbat, [poison, flying], 75, 80, 70).
pokemon_stats(meowth, [normal, dark], 40, 35, 35).
pokemon_stats(persian, [normal, dark], 65, 60, 60).
pokemon_stats(psyduck, [water], 50, 52, 48).
pokemon_stats(golduck, [water], 80, 82, 78).
pokemon_stats(abra, [psychic], 25, 20, 15).
pokemon_stats(kadabra, [psychic], 40, 35, 30).
pokemon_stats(alakazam, [psychic], 55, 50, 65).
pokemon_stats(machop, [fighting], 70, 80, 50).
pokemon_stats(machoke, [fighting], 80, 100, 70).
pokemon_stats(machamp, [fighting], 90, 130, 80).
pokemon_stats(geodude, [rock, ground], 40, 80, 100).
pokemon_stats(graveler, [rock, ground], 55, 95, 115).
pokemon_stats(golem, [rock, ground], 80, 120, 130).
pokemon_stats(grimer, [poison], 80, 80, 50).
pokemon_stats(muk, [poison], 105, 105, 75).
pokemon_stats(shellder, [water], 30, 65, 100).
pokemon_stats(cloyster, [water, ice], 50, 95, 180).
pokemon_stats(gastly, [ghost, poison], 30, 35, 30).
pokemon_stats(haunter, [ghost, poison], 45, 50, 45).
pokemon_stats(gengar, [ghost, poison], 60, 65, 80).
pokemon_stats(onix, [rock, ground], 35, 45, 160).
pokemon_stats(drowzee, [psychic], 60, 48, 45).
pokemon_stats(hypno, [psychic], 85, 73, 70).
pokemon_stats(exeggcute, [grass, psychic], 60, 40, 80).
pokemon_stats(exeggutor, [grass, psychic], 95, 105, 85).
pokemon_stats(lickitung, [normal], 90, 55, 75).
pokemon_stats(koffing, [poison], 40, 65, 95).
pokemon_stats(weezing, [poison], 65, 90, 120).
pokemon_stats(horsea, [water], 30, 40, 70).
pokemon_stats(seadra, [water], 55, 65, 95).
pokemon_stats(staryu, [water], 30, 45, 55).
pokemon_stats(starmie, [water, psychic], 60, 75, 85).
pokemon_stats(magmar, [fire], 65, 95, 57).
pokemon_stats(magikarp, [water], 20, 10, 55).
pokemon_stats(gyarados, [water, flying], 95, 155, 109).
pokemon_stats(lapras, [water, ice], 130, 85, 80).
pokemon_stats(eevee, [normal], 55, 55, 50).
pokemon_stats(articuno, [ice, flying], 90, 85, 100).
pokemon_stats(zapdos, [electric, flying], 90, 90, 85).
pokemon_stats(moltres, [fire, flying], 90, 100, 90).
pokemon_stats(dratini, [dragon], 41, 64, 45).
pokemon_stats(dragonair, [dragon], 61, 84, 65).
pokemon_stats(dragonite, [dragon, flying], 91, 134, 95).
pokemon_stats(mewtwo, [psychic], 106, 150, 70).
pokemon_stats(mew, [psychic], 100, 100, 100).


% pokemon_evolution(Pokemon, EvolvedPokemon, MinRequiredLevel).
pokemon_evolution(bulbasaur, ivysaur, 16).
pokemon_evolution(ivysaur, venusaur, 32).
pokemon_evolution(charmander, charmeleon, 16).
pokemon_evolution(charmeleon, charizard, 36).
pokemon_evolution(squirtle, wartortle, 16).
pokemon_evolution(wartortle, blastoise, 36).
pokemon_evolution(caterpie, metapod, 7).
pokemon_evolution(metapod, butterfree, 10).
pokemon_evolution(pidgey, pidgeotto, 18).
pokemon_evolution(pidgeotto, pidgeot, 36).
pokemon_evolution(ekans, arbok, 22).
pokemon_evolution(zubat, golbat, 22).
pokemon_evolution(meowth, persian, 28).
pokemon_evolution(psyduck, golduck, 33).
pokemon_evolution(abra, kadabra, 16).
pokemon_evolution(machop, machoke, 28).
pokemon_evolution(geodude, graveler, 25).
pokemon_evolution(grimer, muk, 38).
pokemon_evolution(gastly, haunter, 25).
pokemon_evolution(drowzee, hypno, 26).
pokemon_evolution(koffing, weezing, 35).
pokemon_evolution(horsea, seadra, 32).
pokemon_evolution(magikarp, gyarados, 20).
pokemon_evolution(dratini, dragonair, 30).
pokemon_evolution(dragonair, dragonite, 55).



% pokemon_types(PokemonTypes).
pokemon_types([normal, fire, water, electric, grass, ice, fighting, poison, ground, flying, psychic, bug, rock, ghost, dragon, dark, steel, fairy]).

% type_chart_attack(AttackingType, TypeMultipliers).
type_chart_attack(normal, [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.0, 1.0, 1.0, 0.5, 1.0]).
type_chart_attack(fire, [1.0, 0.5, 0.5, 1.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 0.5, 1.0, 2.0, 1.0]).
type_chart_attack(water, [1.0, 2.0, 0.5, 1.0, 0.5, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 1.0, 1.0]).
type_chart_attack(electric, [1.0, 1.0, 2.0, 0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0]).
type_chart_attack(grass, [1.0, 0.5, 2.0, 1.0, 0.5, 1.0, 1.0, 0.5, 2.0, 0.5, 1.0, 0.5, 2.0, 1.0, 0.5, 1.0, 0.5, 1.0]).
type_chart_attack(ice, [1.0, 0.5, 0.5, 1.0, 2.0, 0.5, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0]).
type_chart_attack(fighting, [2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 0.5, 0.5, 0.5, 2.0, 0.0, 1.0, 2.0, 2.0, 0.5]).
type_chart_attack(poison, [1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 0.0, 2.0]).
type_chart_attack(ground, [1.0, 2.0, 1.0, 2.0, 0.5, 1.0, 1.0, 2.0, 1.0, 0.0, 1.0, 0.5, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0]).
type_chart_attack(flying, [1.0, 1.0, 1.0, 0.5, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.0]).
type_chart_attack(psychic, [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0, 0.5, 1.0]).
type_chart_attack(bug, [1.0, 0.5, 1.0, 1.0, 2.0, 1.0, 0.5, 0.5, 1.0, 0.5, 2.0, 1.0, 1.0, 0.5, 1.0, 2.0, 0.5, 0.5]).
type_chart_attack(rock, [1.0, 2.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 0.5, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0]).
type_chart_attack(ghost, [0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 1.0]).
type_chart_attack(dragon, [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 0.0]).
type_chart_attack(dark, [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 0.5]).
type_chart_attack(steel, [1.0, 0.5, 0.5, 0.5, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 0.5, 2.0]).
type_chart_attack(fairy, [1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 0.5, 1.0]).


% pokemon_trainer(PokemonTrainer, PokemonTeam, PokemonLevels).
pokemon_trainer(ash, [pikachu, ivysaur, charmeleon, squirtle], [45, 15, 28, 50]).
pokemon_trainer(misty, [psyduck, staryu, starmie, seadra], [10, 15, 48, 45]).
pokemon_trainer(brock, [onix, geodude, golbat, machop], [18, 33, 42, 33]).
pokemon_trainer(team_rocket, [meowth, ekans, gyarados, weezing], [15, 30, 29, 35]).

%%%%%%%%%%%%%%%%%%%%%%Case 1%%%%%%%%%%%%%%%%%%%%%%%%
evolution_path(Level, Pokemon, EvolvedPokemon):-
    pokemon_evolution(Pokemon,EvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level.
evolution_path(Level, Pokemon, EvolvedPokemon):-
    pokemon_evolution(Pokemon,DiffEvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level,
    evolution_path(Level, DiffEvolvedPokemon, EvolvedPokemon).

find_pokemon_evolution(Level, Pokemon, EvolvedPokemon):-
    findall(EvolvedPokemon,evolution_path(Level, Pokemon, EvolvedPokemon),Path),
    append([Pokemon],Path,List),
    last(List,EvolvedPokemon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 2%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack, PokemonDefense):-
    pokemon_stats(Pokemon, _, HealthPoint, Attack, Defense),
    PokemonHp is HealthPoint+PokemonLevel*2,
    PokemonAttack is Attack+PokemonLevel,
    PokemonDefense is Defense+PokemonLevel.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 3%%%%%%%%%%%%%%%%%%%%%%%%%
element_at(X,[X|_],0).%element_at is to search an element in a list.
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

single_type_multiplier(AttackerType, DefenderType, Multiplier):-
    type_chart_attack(AttackerType, List),
    pokemon_types(TypeList),
    nth0(Index, TypeList, DefenderType),                                    %Search DefenderType in pokemon_types
    element_at(Multiplier,List,Index).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 4%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier(_,[], 1).
type_multiplier(AttackerType,[H|T], Sum) :-
   type_multiplier(AttackerType,T, Rest),
   single_type_multiplier(AttackerType,H,Multiplier),
   Sum is Multiplier * Rest.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 5%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier_complex([],_, 0).                               %This function takes AttackerPokemon list and for each of them calls type_multiplier function.It compares their multipliers choosing max multiplier.
type_multiplier_complex([H|T],DefenderTypes, Sum) :-
   type_multiplier_complex(T,DefenderTypes,Rest),
   type_multiplier(H,DefenderTypes,Multiplier),
   Sum is max(Multiplier , Rest).

pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,Multiplier):-
    pokemon_stats(AttackerPokemon, AttackerTypes, _, _, _),                         %Find the type/types of attacker pokemon
    pokemon_stats(DefenderPokemon, DefenderTypes, _, _, _),                         %Find the type/types of defender pokemon
    type_multiplier_complex(AttackerTypes,DefenderTypes,Multiplier).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 6%%%%%%%%%%%%%%%%%%%%%%%%%                                %%%%%%%%%% DefenderPokemonLevel is not used !!!!!!!
pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage):-
    pokemon_level_stats(AttackerPokemonLevel,AttackerPokemon,_,AttackerPokemonAttack,_),
    pokemon_level_stats(DefenderPokemonLevel,DefenderPokemon,_,_,DefenderPokemonDefense),
    pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,TypeMultiplier),
    Damage is (0.5 * AttackerPokemonLevel * (AttackerPokemonAttack / DefenderPokemonDefense) * TypeMultiplier + 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 7%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, Rounds):-
    pokemon_level_stats(Pokemon1Level,Pokemon1,Pokemon1HpInit,_,_),
    pokemon_level_stats(Pokemon2Level,Pokemon2,Pokemon2HpInit,_,_),
    pokemon_attack(Pokemon1,Pokemon1Level,Pokemon2,Pokemon2Level,Damage1),
    pokemon_attack(Pokemon2,Pokemon2Level,Pokemon1,Pokemon1Level,Damage2),
    finish_fight(Pokemon1HpInit,Pokemon2HpInit,Damage1,Damage2,Pokemon1Hp,Pokemon2Hp,Rounds).

finish_fight(Pokemon1Hp,Pokemon2Hp,Damage1,Damage2,FinalHp1,FinalHp2,MinRound):-
    Round1 is (Pokemon1Hp / Damage2),
    Round2 is (Pokemon2Hp / Damage1),
    MinRound is floor(min(Round1,Round2))+1,
    FinalHp1 is (Pokemon1Hp - MinRound * Damage2),
    FinalHp2 is (Pokemon2Hp - MinRound * Damage1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 8%%%%%%%%%%%%%%%%%%%%%%%%%%
brute_force([Pokemon], [Level], [EvolvedPokemon]):-
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).
brute_force([Pokemon|Pokemons], [Level|Levels], [EvolvedPokemon|T]):-
    brute_force(Pokemons, Levels, T),
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).

match([Team1],[Level1],PokemonTrainer1,[Team2],[Level2],PokemonTrainer2,[Result]):-
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2).
match([Team1|Team1s],[Level1|Level1s],PokemonTrainer1,[Team2|Team2s],[Level2|Level2s],PokemonTrainer2,[Result|Results]):-
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2),
    match(Team1s,Level1s,PokemonTrainer1,Team2s,Level2s,PokemonTrainer2,Results).

pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-
    pokemon_trainer(PokemonTrainer1, PokemonTeam1, PokemonLevels1),
    pokemon_trainer(PokemonTrainer2, PokemonTeam2, PokemonLevels2),
    brute_force(PokemonTeam1,PokemonLevels1,EvolvedPokemonTeam1),
    brute_force(PokemonTeam2,PokemonLevels2,EvolvedPokemonTeam2),
    match(EvolvedPokemonTeam1,PokemonLevels1,PokemonTrainer1,EvolvedPokemonTeam2,PokemonLevels2,PokemonTrainer2,WinnerTrainerList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 9%%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon):-
    findall(RemainingHP,pokemon_fight(EnemyPokemon,LevelCap,_,LevelCap,_,RemainingHP,_),HPList),
    list_max(HPList,RemainingHP),
    pokemon_fight(EnemyPokemon,LevelCap,BestPokemon,LevelCap,_,RemainingHP,_).

list_max([], R, R). %end
list_max([X|Xs], WK, R):- X >  WK, list_max(Xs, X, R). %WK is Carry about
list_max([X|Xs], WK, R):- X =< WK, list_max(Xs, WK, R).
list_max([X|Xs], R):- list_max(Xs, X, R). %start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 10%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon_team(OpponentTrainer, PokemonTeam):-
    pokemon_trainer(OpponentTrainer, EnemyPokemonTeam, PokemonLevels),
    best_pokemon_recursion(EnemyPokemonTeam,PokemonTeam,PokemonLevels).

best_pokemon_recursion([E],[B],[PL]):-
    best_pokemon(E,PL,_,B).
best_pokemon_recursion([E|EnemyPokemonTeam],[B|BestPokemons],[PL|PokemonLevels]):-
    best_pokemon(E,PL,_,B),
    best_pokemon_recursion(EnemyPokemonTeam,BestPokemons,PokemonLevels).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 11%%%%%%%%%%%%%%%%%%%%%%%
pokemon_types(TypeList,InitialPokemonList,PokemonList):-
    pokemon_type(TypeList,InitialPokemonList,PokeList),
    destroy_inner_list(PokeList,PokemonList).

destroy_inner_list([],[]).
destroy_inner_list([H|Tail],List):-
    append(H,Ys0,List),
    destroy_inner_list(Tail,Ys0).

pokemon_type([T], InitialPokemonList, [PokemonList]):-
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(T,PokemonTypes))),member(Pokemon,InitialPokemonList)),PokemonList).
pokemon_type([T|TypeList], InitialPokemonList, [P|PokemonList]):-
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(T,PokemonTypes))),member(Pokemon,InitialPokemonList)),P),
    pokemon_type(TypeList,InitialPokemonList,PokemonList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 12%%%%%%%%%%%%%%%%%%%%%%%
generate_pokemon_team(LikedTypes,DislikedTypes,Criterion,Count,PokemonTeam):-
    like(LikedTypes,Likedd),
    destroy_inner_list(Likedd,Liked),
    dislike(DislikedTypes,Dislikedd),
    destroy_inner_list(Dislikedd,Disliked),
    remove_list(Liked,Disliked,Removed),
    switch(Criterion, [
        h : predsort(health_criteria,Removed, Sorted),
        a : predsort(attack_criteria,Removed, Sorted),
        d : predsort(defense_criteria,Removed, Sorted)
    ]),
    split(Sorted,Count,PokemonTeam,_).

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

like([L],[P]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P).
like([L|LikedTypes],[P|PokemonTeam]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P),
    like(LikedTypes,PokemonTeam).

dislike([D],[P]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(D,Types)),P).
dislike([D|DislikedTypes],[P|PokemonTeam]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(D,Types)),P),
    dislike(DislikedTypes,PokemonTeam).

health_criteria(R,[_,N1,_,_],[_,N2,_,_]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
health_criteria(R,E1,E2) :-
    compare(R,E1,E2).

attack_criteria(R,[_,_,N1,_],[_,_,N2,_]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
attack_criteria(R,E1,E2) :-
    compare(R,E1,E2).

defense_criteria(R,[_,_,_,N1],[_,_,_,N2]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
defense_criteria(R,E1,E2) :-
    compare(R,E1,E2).

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
