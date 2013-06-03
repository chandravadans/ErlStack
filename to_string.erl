-module(to_string).
-compile(export_all).

generate_option_string(List)->
    Fixed="vzctl create 101", 
    list_to_string(List,Fixed).

list_to_string([],Acc)->
    Acc;
list_to_string(List,Acc) ->
    [H|T]=List,
    Prefix=" --",
    Option=erlang:element(1,H),
    Value=erlang:element(2,H),
    Prefixed=string:concat(Prefix,Option),
    Formattedval=string:concat(" ",Value),
    Final=string:concat(Prefixed,Formattedval),
    list_to_string(T,string:concat(Acc,Final)).
    

    
