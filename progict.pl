go :- hypothesize(Personality),
      write('I think your personality : '),
      write(Personality),
      nl,
      undo.

/* hypotheses to be tested */
hypothesize(nervous)   :- nervous, !.
hypothesize(social)     :- social, !.
hypothesize(leadership)   :- leadership, !.
hypothesize(pacified)    :- pacified, !.
hypothesize(selfish)   :- selfish, !.
hypothesize(sensitive)   :- sensitive, !.
hypothesize(unknown).             /* no diagnosis */

/* Personality identification rules */
nervous :- negative,
           verify(irritable),
           verify(declamatory),
            verify(lively).
social :- positive,
         verify(love_to_socialize),
         verify(lollaborator),
         verify(lovable).

leadership :- positive,
           verify(confident),
           verify(creative_and_innovative),
          verify(multi_interest).

pacified :- positive,
         verify(tolerant),
         verify(lovable),
         verify(quiet).

selfish :- negative,
           verify(voice_is_low),
           verify(oversensitive),
           verify(crying_fast).

sensitive :- negative,

           verify(dont_care_about_others),
           verify(you_always_deserve_everything),
           verify(tamper).


/* classification rules */

positive :-
            verify(optimistic),
            verify(forgiving),
            verify(ambitious),
            verify(honest),
            verify(patient),
            verify(collaborator),
            verify(sympathetic).


negative :-
            verify(pessimistic),
            verify(worried),
            verify(oversensitive),
            verify(tamper),
            verify(impose_style_on_others),
            verify(very_sensitive_to_criticism),
            verify(melancholic_and_always_anxious).

/* how to ask questions */
ask(Question) :-
    write('Does you have the following attribute: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.
