%Definim relatia de succesiune; consideram ca toate arcele au costul 1

s(stare(B,NMB,NCB,NMis,NCan,M,N), stare(B1,NMB1,NCB1,NMis1,NCan1,M,N)):-
	opusul(B,B1),
	minus(NRMIS,M),				% Nr misionari barca
	minus(NRCAN,M),				% Nr canibali  barca
	NRMIS + NRCAN =< M,			% Verificam daca se respecta regula
	NRMIS =< NMB,				% Verificam daca se pot lua in barca
	NRCAN =< NCB,				% Verificam daca se pot lua in barca

	NCan1 is NCB - NRCAN,		% Numar canibali ramasi pe insula curenta
	NMis1 is NMB - NRMIS,		% Numar misionari ramasi pe insula curenta

	NCan1 =< NMis1,				% Verificam daca nu se mananca intre ei

	NCB1 is NCan + NRCAN,		% Numarul de canibali  de pe insula opusa dupa plecarea barcii
	NMB1 is NMis + NRMIS,		% Numarul de misionari de pe insula opusa dupa plecarea barcii

	NCB1 =< NMB1.				% Verificam daca nu se mananca intre ei

% Numarul maxim de persoane de fiecar tip din barca
minus(N,N).
minus(N,N1):- N1>0, N2 is N1-1, minus(N,N2).

h(LR,H):- .

scop(stare(vest,N,N,0,0,_,N)).
initial(stare(est,N,N,0,0,M,N),M,N).
solve(Sol):-initial(Poz),bestfirst(Poz,Sol).

%Strategia best-first
%Predicatul bestfirst(Nod_initial,Solutie) este adevarat daca Solutie este un drum (obtinut
%folosind strategia best-first) de la nodul Nod_initial la o stare scop

bestfirst(Nod_initial,Solutie):- expandeaza([],l(Nod_initial,0/0),9999999,_,da,Solutie).
expandeaza(Drum,l(N,_),_,_, da,[N|Drum]):-scop(N).

%Caz 1: daca N este nod scop, atunci construim o cale solutie
expandeaza(Drum,l(N,F/G),Limita,Arb1,Rez,Sol):- F=<Limita,
(bagof(M/C,(s(N,M,C), \+ (member(M,Drum))),Succ),!,
listasucc(G,Succ,As),cea_mai_buna_f(As,F1),
expandeaza(Drum,t(N,F1/G,As),Limita,Arb1, Rez,Sol);
Rez=imposibil).

% Caz 2: Daca N este nod frunza a carui 𝑓 -valoare este mai mica decat Limita,atunci ii
%generez succesorii si ii expandez in limita Limita
expandeaza(Drum,t(N,F/G,[A|As]),Limita,Arb1,Rez,Sol):-F=<Limita,
cea_mai_buna_f(As,BF),min(Limita,BF,Limita1),
expandeaza([N|Drum],A,Limita1,A1,Rez1,Sol),
continua(Drum,t(N,F/G,[A1|As]),Limita,Arb1,Rez1,Rez,Sol).

% Caz 3 Daca arborele de radacina N are subarbori nevizi si 𝑓 -valoarea este mai mica decat
% Limita, atunci expandam cel mai "promitator" subarbore al sau; in functie de rezultatul
%obtinut Rez vom decide cum anume vom continua cautarea prin intermediul procedurii
%(predicatului) continua
expandeaza(_,t(_,_,[]),_,_,imposibil,_):-!.

%Caz 4: pe aceasta varianta nu o sa obtinem niciodata o solutie
expandeaza(_,Arb,Limita,Arb,nu,_):-f(Arb,F),F>Limita.

%Caz 5: In cazul unor 𝑓 -valori mai mari decat Bound, arborele numai poate fi extins
continua(_,_,_,_,da,da,Sol).

continua(P,t(N,F/G,[A1|As]),Limita,Arb1,nu,Rez, Sol):-
insereaza(A1,As,NAs),cea_mai_buna_f(NAs,F1),
expandeaza(P,t(N,F1/G,NAs),Limita,Arb1,Rez,Sol).
continua(P,t(N,F/G,[_|As]),Limita,Arb1,imposibil,Rez,Sol):- cea_mai_buna_f(As,F1),
expandeaza(P,t(N,F1/G,As),Limita,Arb1,Rez,Sol).
listasucc(_,[],[]).
listasucc(G0,[N/C|NCs],Ts):-G is G0+C,h(N,H),F is G+H,listasucc(G0,NCs,Ts1),
insereaza(l(N,F/G),Ts1,Ts).
%Predicatul insereaza(A,As,As1) este utilizat pentru inserarea unui arbore A intr-o lista de
%arbori As, mentinand ordineaimpusa de 𝑓 -valorile lor
insereaza(A,As,[A|As]):-f(A,F),cea_mai_buna_f(As,F1),F=<F1,!.
insereaza(A,[A1|As],[A1|As1]):-insereaza(A,As,As1).
min(X,Y,X):-X=<Y,!.
min(_,Y,Y).
f(l(_,F/_),F). % f-val unei frunze
f(t(_,F/_,_),F). % f-val unui arbore
% Predicatul cea_mai_buna_f(As,F) este utilizat pentru a determina cea mai buna 𝑓 -valoare
%a unui arbore din lista de arbori As, daca aceasta lista este nevida; lista As este ordonata
%dupa fˆ -valorile subarborilor constituenti
cea_mai_buna_f([A|_],F):-f(A,F).
cea_mai_buna_f([],999999).
% In cazul unei liste de arbori vide, 𝑓 -valoarea determinata este foarte mare
