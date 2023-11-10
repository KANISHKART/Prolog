flight(london,dublin,aerlingus,500,45,150).
flight(rome,london,ba,1500,150,400).
flight(rome,paris,airfrance,1200,120,500). 
flight(paris,dublin,airfrance,600,60,200).
flight(berlin,moscow,lufthansa,3000,300,900).
flight(paris,amsterdam,airfrance,400,30,100).
flight(berlin,dublin,lufthansa,1200,120,900).
flight(london,newyork,ba,5000,700,1100).
flight(dublin,newyork,aerlingus,4500,360,800).
flight(dublin,cork,ryanair,300,50,50).
flight(dublin,rome,ryanair,2000,150,70).
flight(dublin,chicago,aerlingus,5500,480,890).
flight(amsterdam,hongkong,klm,7000,660,750).
flight(london,hongkong,ba,7500,700,1000).
flight(dublin,amsterdam,ryanair,1000,90,60).
flight(moscow,newyork,aerflot,9000,720,1000).
flight(moscow,hongkong,aerflot,5500,420,500).
flight(newyork,chicago,aa,3000,240,430).
flight(dublin,london,aerlingus,500,45,150).
flight(london,rome,ba,1500,150,400).
flight(paris,rome,airfrance,1200,120,500). 
flight(dublin,paris,airfrance,600,60,200).
flight(moscow,berlin,lufthansa,3000,300,900).
flight(amsterdam,paris,airfrance,400,30,100).
flight(dublin,berlin,lufthansa,1200,120,900).
flight(newyork,london,ba,5000,700,1100).
flight(newyork,dublin,aerlingus,4500,360,800).
flight(cork,dublin,ryanair,300,50,50).
flight(rome,dublin,ryanair,2000,150,70).
flight(chicago,dublin,aerlingus,5500,480,890).
flight(hongkong,amsterdam,klm,7000,660,750).
flight(hongkong,london,ba,7500,700,1000).
flight(amsterdam,dublin,ryanair,1000,90,60).
flight(newyork,moscow,aerflot,9000,720,1000).
flight(hongkong,moscow,aerflot,5500,420,500).
flight(chicago,newyork,aa,3000,240,430).

country(dublin,ireland).
country(cork,ireland).
country(london,uk).
country(rome,italy).
country(moscow,russia).
country(hongkong,china).
country(amsterdam,holland).
country(berlin,germany).
country(paris,france).
country(newyork,usa).
country(chennai,india).


/*part 1 ----------------------------------------------*/


/*Answer 1*/

itr_airports(Iterator, Country,List):- country(Iterator, Country),flight(Iterator,_,_,_,_,_).
itr_airports(Iterator, Country,List):- country(Iterator, Country),flight(_,Iterator,_,_,_,_).
find_airports(Country, List):- setof(Iterator, itr_airports(Iterator, Country,List), List).

/* Answer 2
Explanation: Step1 & 2. city 1 and unknown + unknown and city 2 should have a connection 
            Step 3 append if there is a connection using append. */

find_trip(X,Y,T):- flight(X,Y,_,_,_,_), append([X],[Y],T).
find_trip(X,Y,T):- flight(X,Z,A,B,C,D), flight(Z,Y,Q,W,E,R), append([X,Z],[Y],T).

/* Answer 3
 Finding all trips
*/

find_all_trips(X,Y,T):- findall(N,find_trip(X,Y,N),T).


/* Answer 4
 Finding all trips distance
*/

trip_dist(X,Y,List):- flight(X,Y,_,D1,_,_),append([[X,Y]],[D1],List).
trip_dist(X,Y,List):- flight(X,Z,_,D1,_,_), flight(Z,Y,_,D2,_,_), Distance is D1+D2 ,append([[X,Z,Y]],[Distance],List) .


/* Answer 5
 Finding all trips distance_Cost
*/
trip_cost(X,Y,List):- flight(X,Y,_,_,_,C1),append([[X,Y]],[C1],List).
trip_cost(X,Y,List):- flight(X,Z,_,_,_,C1), flight(Z,Y,_,_,_,C2), Cost is C1+C2 ,append([[X,Z,Y]],[Cost],List) .

/* Answer 6
 Finding all trips trip_change
*/


length_of([],0).
length_of([Head|Tail],Count):-length_of(Tail,Z), Count is Z+1.
find_trip_d(X,Y,[List|Count]):- flight(X,Y,_,_,_,_) ,append([X],[Y],List),length_of(List,Count1), Count is Count1-2 ; flight(X,Z,_,_,_,_), flight(Z,Y,_,_,_,_) ,append([X,Z],[Y],List) , length_of(List,Count1), Count is Count1-2.


/* Answer 7
Need to consider all routes
*/

find_trip_search(X,Y,List,[X|Response],Airlines):-flight(X,Z,Airways,_,_,_), \+member(Airways,[Airlines]) ,\+member(Z, List),find_trip_search(Z,Y,[X|List],Response, Airlines).
find_trip_search(X,Y,List,[X,Y],Airlines):- flight(X,Y,Airways,_,_,_), \+member(Y, List), \+member(Airways,[Airlines]).
find_trip_filter(X,Y,Response, Airlines):- find_trip_search(X,Y,[],Response,Airlines).


chkMember(A1,A2):- member(A1,[A2]).

/* Answer 8

Firstly I had written a predicate to calculate the trip timing.

Next I was just testing the one case where the list will be having only one route.

And if multiple cases are there just compare the first two of them and change the list and reduce.

*/

trip_time(X,Y,List):- flight(X,Y,_,_,T1,_),append([[X,Y]],[T1],List).
trip_time(X,Y,List):- flight(X,Z,_,_,T1,_), flight(Z,Y,_,_,T2,_), Time is T1+T2 ,append([[X,Z,Y]],[Time],List).

min_time_list([[Trip, Result]], Trip, Result).
min_time_list([[Trip1, Result1],[Trip2|Result2]|TailList], Trip, Result):-Result1 =< Result2, min_time_list([[Trip1,Result1]|TailList], Trip, Result).
min_time_list([[Trip1, Result1],[Trip2|Result2]|TailList], Trip, Result):-Result1 > Result2, min_time_list([[Trip2,Result2]|TailList], Trip, Result).

fastest(X,Y,Trip,Result):- findall(Variable, trip_time(X,Y,Variable), List), min_time_list(List,Trip,Result).
shortest(X,Y,Trip,Result):- findall(Variable, trip_dist(X,Y,Variable), List), min_time_list(List,Trip,Result).
cheapest(X,Y,Trip,Result):- findall(Variable, trip_cost(X,Y,Variable), List), min_time_list(List,Trip,Result).


/* Answer 9*/
trip_to_nation(X, Y, T) :- country(C, Y), flight(X, Z, _, _, _, _), flight(Z, C, _, _, _, _), append([X, Z],[C], T).


/* Answer 10 
total_trips(X,Y,Response):- look_flights(X,Y,[],Response).
look_flights(X,Y,List,[X,Y]):- flight(X,Y,_,_,_,_), \+member(Y,List).
look_flights(X,Y,List,[X|Response]):-flight(X,Z,_,_,_,_), \+member(Z,List), look_flights(Z,Y,[X|List],Response).*/


all_trip_to_nation(X,Y,Response):- country(Itr, Y), iterate_all(X,Itr,[],Response).

iterate_all(X,Itr_City,List,[X,Itr_City]):- flight(X,Itr_City,_,_,_,_), \+member(Itr_City, List).

iterate_all(X,Itr_City,List,[X|Response]):- flight(X,Z,_,_,_,_), \+ member(Z, List), iterate_all(Z,Itr_City,[X|List],Response).








