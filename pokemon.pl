% sabri bayrakdar
% 2016400378
% compiling:  yes
% complete:  yes

:- [pokemon_data].                                                                                  %Include pokemon_data.pl file.
%%%%%%%%%%%%%%%%%%%%%%Case 1%%%%%%%%%%%%%%%%%%%%%%%%
evolution_path(Level, Pokemon, EvolvedPokemon):-                                                    %This function finds all available evolution pokemons for given pokemon with recursive.
    pokemon_evolution(Pokemon,EvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level.            
evolution_path(Level, Pokemon, EvolvedPokemon):-                                                    %This function finds all available evolution pokemons for given pokemon with recursive.
    pokemon_evolution(Pokemon,DiffEvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level,
    evolution_path(Level, DiffEvolvedPokemon, EvolvedPokemon).

find_pokemon_evolution(Level, Pokemon, EvolvedPokemon):-                                            %This function calls evolution_path function with three parameters and lists its return values.
    findall(EvolvedPokemon,evolution_path(Level, Pokemon, EvolvedPokemon),Path),
    append([Pokemon],Path,List),                                                                    %After listing return value of evolution_path function it appends pokemon itself, because in case of it has no adequate level for evolution, to show it remains as same pokemon. 
    last(List,EvolvedPokemon).                                                                      %Finally get the last element of the list to show that the pokemon can evolve as long as its level let itself to evolve.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 2%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack, PokemonDefense):-              %This function gets the base values of pokemon (health, attack and defense) and calculate its actual values according to pokemon level.
    pokemon_stats(Pokemon, _, HealthPoint, Attack, Defense),                                        %Get the values of pokemon from pokemon_data.pl file
    PokemonHp is HealthPoint+PokemonLevel*2,                                                        %Calculate health
    PokemonAttack is Attack+PokemonLevel,                                                           %Calculate attack
    PokemonDefense is Defense+PokemonLevel.                                                         %Calculate defense
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 3%%%%%%%%%%%%%%%%%%%%%%%%%
element_at(X,[X|_],0).                                                                              %I defined element_at function as helper function and used it in single_type_multiplier function.
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.                                           %Element_at function is to search an element in a list.It search an element and if it finds the given element then returns index of the element.

single_type_multiplier(AttackerType, DefenderType, Multiplier):-                                    %This function finds the multiplier of AttackerType for DefenderType.
    pokemon_types(TypeNameList),                                                                    %Get the type names as list from the data
    type_chart_attack(AttackerType, AttackerMultiplierList),                                        %Get the multiplier list of AttackerType.
    nth0(Index, TypeNameList, DefenderType),                                                        %Find the index of DefenderType in TypeNameList (name list of types).
    element_at(Multiplier,AttackerMultiplierList,Index).                                            %Find the element with index that i found in previous instruction (Index).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 4%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier(_,[], 1).                                                                           
type_multiplier(AttackerType,[H|T], EffectOfMultipliers) :-                                         %This function takes defender type list and goes to recursive after calling single_type_multiplier for each element of the list.
   single_type_multiplier(AttackerType,H,Multiplier),                                               %Call single_type_multiplier
   type_multiplier(AttackerType,T, Rest),                                                           %Go to recursive with rest of the DefenderType list (without its first element).In this step Rest represents the multiplier I will find multiplier and it represents always next multiplier.That is why I call it Rest.
   EffectOfMultipliers is Multiplier * Rest.                                                        %EffectOfMultipliers represents effect of all multipliers.It finally calculated all multipliers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 5%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier_complex([],_, 0).                                                                   %In case of empthy list return 0 value which is appropriate because I choose max element in next instruction.
type_multiplier_complex([H|T],DefenderTypes, MultiplierWithMaxEffect) :-                            %This function takes AttackerType list and for each of them calls type_multiplier function.It compares their multipliers choosing max multiplier.
   type_multiplier(H,DefenderTypes,Multiplier),                                                     %Call type_multiplier function for header of the AttackerType list.
   type_multiplier_complex(T,DefenderTypes,Rest),                                                   %Go to recursive with rest of the AttackerType list (without its first element).
   MultiplierWithMaxEffect is max(Multiplier , Rest).

pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,Multiplier):-                               %This function gets the information about the given pokemon and gives these information the type_multiplier_complex function as parameters.
    pokemon_stats(AttackerPokemon, AttackerTypes, _, _, _),                                         %Find the type/types of attacker pokemon.
    pokemon_stats(DefenderPokemon, DefenderTypes, _, _, _),                                         %Find the type/types of defender pokemon.
    type_multiplier_complex(AttackerTypes,DefenderTypes,Multiplier).                                %Give the information that I just collects from pokemon_data.pl as parameters.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 6%%%%%%%%%%%%%%%%%%%%%%%%%                                                
pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage):-                  %This function find the actual information about pokemons using pokemon_level_stats function.Then it finds out the multiplier giving these data to pokemon_type_multiplier function. 
    pokemon_level_stats(AttackerPokemonLevel,AttackerPokemon,_,AttackerPokemonAttack,_),                                %Get the AttackerPokemonAttack value from pokemon_data.pl 
    pokemon_level_stats(DefenderPokemonLevel,DefenderPokemon,_,_,DefenderPokemonDefense),                               %Get the DefenderPokemonDefense value from pokemon_data.pl 
    pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,TypeMultiplier),                                            %Get TypeMultiplier calling pokemon_type_multiplier function giving AttackerPokemonAttack and DefenderPokemonDefense parameters. 
    Damage is (0.5 * AttackerPokemonLevel * (AttackerPokemonAttack / DefenderPokemonDefense) * TypeMultiplier + 1).     %After finding out TypeMultiplier, calculate the damage.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 7%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, Rounds):-                       %This function gets the health of pokemons in given level using pokemon_level_stats function and their damages using pokemon_attack function.Then gives these information simulate_fight function as parameter.
    pokemon_level_stats(Pokemon1Level,Pokemon1,Pokemon1HpInit,_,_),                                                     %Get the health value of first pokemon according to its level.
    pokemon_level_stats(Pokemon2Level,Pokemon2,Pokemon2HpInit,_,_),                                                     %Get the health value of second pokemon according to its level.
    pokemon_attack(Pokemon1,Pokemon1Level,Pokemon2,Pokemon2Level,Damage1),                                              %Get the damage of first pokemon according to level of pokemons (first and second pokemon).
    pokemon_attack(Pokemon2,Pokemon2Level,Pokemon1,Pokemon1Level,Damage2),                                              %Get the damage of second pokemon according to level of pokemons (first and second pokemon).
    simulate_fight(Pokemon1HpInit,Pokemon2HpInit,Damage1,Damage2,Pokemon1Hp,Pokemon2Hp,Rounds).                         %Pass these information (healt and damage values) to simulate_fight function and return its return values.

simulate_fight(Pokemon1Hp,Pokemon2Hp,Damage1,Damage2,FinalHp1,FinalHp2,Round):-                                         %This function simulate the fight according to pokemons health and damages.Then return its final health values and round value.
    Round1 is (Pokemon1Hp / Damage2),                                                                                   %Round1 is float and it represents that first pokemon dies if second pokemon attacks to first pokemon for Round1 times.For example Pokemon1Hp is 10 and Damage2 is 4 then first pokemon dies if second pokemon attacks to first pokemon for 2.5 (10/4) times. 
    Round2 is (Pokemon2Hp / Damage1),                                                                                   %Round2 is float and it represents that second pokemon dies if first pokemon attacks to second pokemon for Round2 times.For example Pokemon2Hp is 10 and Damage1 is 4 then second pokemon dies if first pokemon attacks to second pokemon for 2.5 (10/4) times. 
    Round is ceiling(min(Round1,Round2)),                                                                               %This instruction is the most important instruction of this case.It decides who dies first.It ceils the decided value after getting minimum of Round1 and Round2 values.Then it assings this value to Round variable.My purpose of taking minimum is to find the pokemon who dies first.
    FinalHp1 is (Pokemon1Hp - Round * Damage2),                                                                         %After calculating Round, I find the health of first pokemon in the end of the fight.
    FinalHp2 is (Pokemon2Hp - Round * Damage1).                                                                         %After calculating Round, I find the health of second pokemon in the end of the fight.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 8%%%%%%%%%%%%%%%%%%%%%%%%%%
brute_force_evolution([Pokemon], [Level], [EvolvedPokemon]):-                                                                               %If lists have single element, then finish the recursion.
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).
brute_force_evolution([Pokemon|Pokemons], [Level|Levels], [EvolvedPokemon|T]):-                                                             %This function force pokemons to evolution before the tournament.Actually duty of this function and find_pokemon_evolution function is the same,but this function take pokemon names and its levels as two separate lists.That is why, this function calls find_pokemon_evolution function with recursion.It pass the header of the lists to find_pokemon_evolution function in each recursion.
    brute_force_evolution(Pokemons, Levels, T),                                                                                             %Go to recursion passing the remaining list elements as a list.I call recursion in this function but it did not affect execution of the program.
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).                                                                                 %Pass the header of the lists (Level, Pokemon and EvolvedPokemon) to find_pokemon_evolution function.So return value is assinged to the header of the return list.

match([Team1],[Level1],PokemonTrainer1,[Team2],[Level2],PokemonTrainer2,[Result]):-                                                         %If lists have single function find the result and get rid of the recursion.
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2).
match([Team1|Team1s],[Level1|Level1s],PokemonTrainer1,[Team2|Team2s],[Level2|Level2s],PokemonTrainer2,[Result|Results]):-                   %This function matches the teams with pokemon_fight function and recursive.It matches pokemons according to its team order (first to first, second to second,...) in each recursive call.
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),                                                                                     %Match pokemons with same order and find the final health values.
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2),                                                                           %Decide winner pokemon examining final health values.Assing result the winner trainer in each match.
    match(Team1s,Level1s,PokemonTrainer1,Team2s,Level2s,PokemonTrainer2,Results).                                                           %Repeat these instructions until lists have single elements.

pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-                                                                   %This function takes trainer names and gives winner list prepared according to each match.
    pokemon_trainer(PokemonTrainer1, PokemonTeam1, PokemonLevels1),                                                                         %Get the pokemon names and levels of first trainer.
    pokemon_trainer(PokemonTrainer2, PokemonTeam2, PokemonLevels2),                                                                         %Get the pokemon names and levels of second trainer.
    brute_force_evolution(PokemonTeam1,PokemonLevels1,EvolvedPokemonTeam1),                                                                 %Force all pokemons to evolution in first team, if their levels are adequate for evolution calling brute_force_evolution.
    brute_force_evolution(PokemonTeam2,PokemonLevels2,EvolvedPokemonTeam2),                                                                 %Force all pokemons to evolution in second team, if their levels are adequate for evolution calling brute_force_evolution.
    match(EvolvedPokemonTeam1,PokemonLevels1,PokemonTrainer1,EvolvedPokemonTeam2,PokemonLevels2,PokemonTrainer2,WinnerTrainerList).         %After forcing pokemons to evolution, match them using match function.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 9%%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon):-                                            %This function find the best opponent for given enemy pokemon.
    findall(RemainingHp,pokemon_fight(EnemyPokemon,LevelCap,_,LevelCap,_,RemainingHp,_),HPList),            %Fight all pokemons with enemy pokemon and list RemainingHp values of all pokemons as HPList.
    list_max(HPList,RemainingHP),                                                                           %Find the maximum value in the HPList and assing it RemainingHP variable.
    pokemon_fight(EnemyPokemon,LevelCap,BestPokemon,LevelCap,_,RemainingHP,_).                              %Find the name of the pokemon with maximum RemainingHP value in pokemon_fight with enemy pokemon.

list_max(Max, [], Max).                                                                                     %If list has no element stop the recursion.
list_max(X1 ,[X2|Xs], Max):- X1 >  X2, list_max(X1, Xs, Max).                                               %What I am doing here is that comparing first and second element of the given list and decide which is greater than the other and call recursive with remaining list (Xs) and the value which I find the greater.
list_max(X1 ,[X2|Xs], Max):- X1 =< X2, list_max(X2, Xs, Max).                                               %What I am doing here is that comparing first and second element of the given list and decide which is greater than the other and call recursive with remaining list (Xs) and the value which I find the greater.
list_max([X1|Xs], Max):- list_max(X1, Xs, Max).                                                             %This function finds the maximum element of the given list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 10%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon_team(OpponentTrainer, PokemonTeam):-                                                           %This function creates a team against pokemons of the OpponentTrainer.
    pokemon_trainer(OpponentTrainer, EnemyPokemonTeam, PokemonLevels),                                      %Get the pokemons and levels of the OpponentTrainer.
    best_pokemon_recursion(EnemyPokemonTeam,PokemonLevels,PokemonTeam).                                     %Call the best_pokemon_recursion function with +EnemyPokemonTeam,+PokemonLevels and -PokemonTeam parameters.

best_pokemon_recursion([E],[PL],[B]):-                                                                      %If lists have a single elements get rid of the recursion.
    best_pokemon(E,PL,_,B).
best_pokemon_recursion([E|EnemyPokemonTeam],[PL|PokemonLevels],[B|BestPokemons]):-                          %This function finds the best pokemon against the header of the EnemyPokemonTeam list in each step.
    best_pokemon(E,PL,_,B),                                                                                 %Find the best pokemon of the header element of the enemy pokemon list.
    best_pokemon_recursion(EnemyPokemonTeam,PokemonLevels,BestPokemons).                                    %Go to recursive with remaining list (without header elements).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 11%%%%%%%%%%%%%%%%%%%%%%%                                                        %In this case I find all pokemons that have at least one type which is a member of the TypeList
pokemon_types(TypeList,GivenPokemonList,PokemonList):-                                                      %This function is used to just combine following two functions.
    pokemon_type(TypeList,GivenPokemonList,PokeList),                                                       %You can think pokemon_type function as a main function in this case because it is the function which take over the actual duties.I will explain it later.
    destroy_inner_list(PokeList,PokemonList).                                                               %I used this function to convert list of list to list.

destroy_inner_list([],[]).                                                                                  %If emptyh list then get rid of recursion and quit.Let me give an example: destroy_inner_list([[1,2],[3,4]],List).=>List is append([1,2],Rest)=>destroy_inner_list([[3,4]],Rest)=>Rest is append([3,4],AnotherRest)=>destroy_inner_list([],AnotherRest)=>AnotherRest is append([],[]).=>List is append([1,2],[3,4]).=>List=[1,2,3,4].
destroy_inner_list([H|Tail],List):-                                                                         %I defined and used this function convert listf of list to list.For example destroy_inner_list([[1,2,3],[4,5],X]) gives me X = [1,2,3,4,5].
    append(H,Rest,List),                                                                                    %In this instruction I append header element and an unknown variable (Rest) obtaining List.But following instruction I call recursion with that unknown variable and find it and so on.It goes until it finds empthy list.
    destroy_inner_list(Tail,Rest).                                                                          %Call recursion with our unknown variable (Rest) and Tail.And when it finds the last unknown variable, then assing these unknown variables to values which we found later.

pokemon_type([SingleType], GivenPokemonList, [SinglePokemonElement]):-                                                                                                    %If lists have single element then get rid of recursion and do the same instruction.
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(SingleType,PokemonTypes))),member(Pokemon,GivenPokemonList)),SinglePokemonElement).
pokemon_type([T|TypeList], GivenPokemonList, [P|PokemonList]):-                                                                                                           %This function using findall with some queries.I place some constraints in findall,So it accepts every pokemon who is suitable these constraints.Example output:"pokemon_type([grass, flying, ground], [bulbasaur, charmander, charizard, gyarados, pikachu] , PokemonList)." gives me "PokemonList = [[bulbasaur], [charizard, gyarados], []]".
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(T,PokemonTypes))),member(Pokemon,GivenPokemonList)),P),                                       %Firstly, I get pokemon names (Pokemon) and their type list (PokemonTypes).I get the pokemons if their types include header type of the given TypeList and this pokemon is in the given pokemon list (GivenPokemonList).After getting pokemons which is suitable these constraints, list them with findall and assing this list to result list (PokemonList).
    pokemon_type(TypeList,GivenPokemonList,PokemonList).                                                                                                                  %Go to recursion with tail of the TypeList until TypeList has single element.This function give list of list so I did not make this function main function (pokemon_types).Because its output require destroy_inner_list function to convert its output (list of list) to list. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 12%%%%%%%%%%%%%%%%%%%%%%%
generate_pokemon_team(LikedTypes,DislikedTypes,Criterion,Count,PokemonTeam):-                       %This function is most complicated function in this project so I used a few auxilary functions to get rid of complexity.I will explain those functions later.Logic of this function is that get liked pokemons and disliked pokemons, remove disliked pokemons from liked pokemons (Liked/Disliked),sort remain liked pokemons according to Criterion and finally I get the suffix according to Count (first count element of the list).
    choose(LikedTypes,LikedInnerList),                                                              %I get liked pokemons using choose(+LikedTypes,-LikedInnerList).This gives me output like [[dratini,drogonair,dragonite]] according to liked types for example this list is prepared for dragon type.
    destroy_inner_list(LikedInnerList,Liked),                                                       %Convert the inner list to list [[[bulbasaur,100,100,100],[ivysaur,110,110,110]],[[charmander,120,120,120],[charmelon,130,130,130]]]->[[bulbasaur,100,100,100],[ivysaur,110,110,110],[charmander,120,120,120],[charmelon,130,130,130]].This function is defined in case 11.
    choose(DislikedTypes,DisLikedInnerList),                                                        %I get disliked pokemons using choose(+DislikedTypes,-DislikedInnerList).This gives me output like [[...,dragonite,...]] according to liked types for example this list is prepared for flying type.
    destroy_inner_list(DisLikedInnerList,Disliked),                                                 %Convert the inner list to list [[[bulbasaur,100,100,100],[ivysaur,110,110,110]],[[charmander,120,120,120],[charmelon,130,130,130]]]->[[bulbasaur,100,100,100],[ivysaur,110,110,110],[charmander,120,120,120],[charmelon,130,130,130]]..This function is defined in case 11.
    remove_list(Liked,Disliked,Removed),                                                            %Remove Disliked pokemons from Liked pokemons and get the Removed list which is we want.                                                  
    switch(Criterion, [                                                                             %Sort Removed list according to Criterion using switch-case.
        h : predsort(health_criteria,Removed, Sorted),                                              %I used in here predsort and if Criterion is h then its criterion is health_criteria so predsort sorts pokemons according to their health values.
        a : predsort(attack_criteria,Removed, Sorted),                                              %I used in here predsort and if Criterion is a then its criterion is attack_criteria so predsort sorts pokemons according to their attack values.
        d : predsort(defense_criteria,Removed, Sorted)                                              %I used in here predsort and if Criterion is d then its criterion is defense_criteria so predsort sorts pokemons according to their defense values.
    ]),                                                                                             %health_criteria,attack_criteria and defense_criteria are defined next instructions so I will explain these later.
    suffix(Sorted,Count,PokemonTeam).                                                               %Finally, I get the suffix of the Sorted list according to Count and return the PokemonTeam.

choose([L],[P]):-                                                                                                                           
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P).
choose([L|WantedTypes],[P|PokemonTeam]):-                                                                                                        %This function select the pokemons who has the type which is member of the given WantedTypes.
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P),              %This is a query instruction.It lists each pokemon that has the type which is a member of the WantedTypes.
    choose(WantedTypes,PokemonTeam).                                                                                                             %After check header type of the WantedTypes list call recursion with remaining list.Recursion.

remove_list([], _, []).                                                                             %Stop in case of empthy list.This function find the difference between two list based on first list.I mean that let assume L1=[1,2,3],L2=[2,3,4] then when we call remove(L1,L2,X), it gives us X = [1].
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result).                %If header of the first list is member of the second list, then remove it
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).                              %In this instruction if first elements of first list and result list then do nothing and continue with next element.

switch(X, [Val:Goal|Cases]) :-                                                  %I defined switch-case structure to decrease complexity using it.In this structure there is a Cases list and it consists of values (Val) and goals (Goal).If key value is matches with any Val then call its Goal.
    (X=Val ->call(Goal);switch(X, Cases)).                                      %If key value (X) equals to Val then call its Goal.

health_criteria(R,[_,N1,_,_],[_,N2,_,_]) :-                                     %Choose second elements as N1 and N2 and compare them.If N2 is greater than N1 then reverse whole list not just N1 and N2.
    N1=\=N2, !,
    compare(R,N2,N1).
health_criteria(R,E1,E2) :-
    compare(R,E1,E2).

attack_criteria(R,[_,_,N1,_],[_,_,N2,_]) :-                                     %Choose third elements as N1 and N2 and compare them.If N2 is greater than N1 then reverse whole list not just N1 and N2.
    N1=\=N2, !,
    compare(R,N2,N1).
attack_criteria(R,E1,E2) :-
    compare(R,E1,E2).

defense_criteria(R,[_,_,_,N1],[_,_,_,N2]) :-                                    %Choose fourth elements as N1 and N2 and compare them.If N2 is greater than N1 then reverse whole list not just N1 and N2.
    N1=\=N2, !,
    compare(R,N2,N1).
defense_criteria(R,E1,E2) :-
    compare(R,E1,E2).

suffix(_,0,[]).                                                                 %If N is 0 then exit.
suffix([X|Xs],N,[X|Suffix]) :-                                                  %Take first N elements of the given list with recursive.Each function call decrease N variable, if N is 0 get rid of the recursive and return the suffix list.
    N > 0, N1 is N - 1,
    suffix(Xs,N1,Suffix).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
