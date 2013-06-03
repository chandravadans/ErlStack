-module(management_process).
-compile(export_all).
-vsn(1.0).
-description("").
-include("records.hrl").

start() ->
    
    %Start with an empty host_list
    Manager1=#manager{host_agent_info=[]},

    %Create the listener process
    Pid1=spawn(fun() -> cloud_manager(Manager1) end),
    register(cloud_manager,Pid1),
    Pid1.

cloud_manager(Manager1) ->
    receive 
        {stop_manager} -> ok;
	
	%register a host machine
	{register_host,Host_info}->
		Host_info2=lists:append(Manager1#manager.host_agent_info,Host_info),
		Manager2=Manager1#manager{host_agent_info=Host_info2},
	        cloud_manager(Manager2);

        {log, {Severity, Message}} ->
            log(Severity, Message),
            cloud_manager(Manager1);


        Any1 -> 
            Message1=lists:flatten(io_lib:format("Unknown message ~p received at manager.", 
                     [Any1])),
            log(info, Message1),
            cloud_manager(Manager1)
    end.

log(Type,Message) ->
    io:format("~p ~p~n",[Type,Message]).
