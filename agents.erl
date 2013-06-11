-module(agents).
-compile(export_all).
-vsn(1.0).
-description("This module is used to implement a host agent which will "
	     "respond to host specific queries and commands sent by overall "
	     "cloud manager.").

%%% This file includes definition of agent and manager records.
-include("records.hrl").


%%% This is the main start function.  This should be called
%%% on base host to start the agent process.  Registration
%%% message for registering agent process with cloud manager
%%% is sent automatically.
start()->
    %% For now we are using node name instead of hostname.
    %% This would be changed in later versions.
    Host=node(),

    %%Get pid of management process
    Agent1=#agent{management_process_pid=get_manager(),hostname=Host},

    %%Start new base agent
    Q=spawn(fun() -> base_host_agent(Agent1) end),

    %%Register started agent with management process
    Agent1#agent.management_process_pid ! {register_base_agent,Host,Q},
    Q.


%%% This function waits till it can resolve cloud_manager
%%% globally.  Once the resolution is successful it returns
%%% the Pid of the cloud_manager process.
get_manager() ->
    case global:whereis_name(cloud_manager) of
        undefined ->
	    net_adm:ping('manager1@cloudmanager.virtual-labs.ac.in'),
	    io:format("~p~n",["Pinging Manager"]),
	    timer:sleep(2000),
            get_manager();
        Pid -> Pid
    end.


%%% Takes a list of option-value tuples and merges them into a single string
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



%%% This is the main process loop for host agent which
%%% receives for various types of messages from cloud
%%% manager and responds / acts accordingly.
base_host_agent(Agent1) ->
    receive
	{stop_agent} ->
            %% Stop agent after sending log to management process at notice level 
            Pid1 = Agent1#agent.management_process_pid,
	    Hostname1 = Agent1#agent.hostname,
            Message1 = lists:flatten(io_lib:format("Agent on host ~p with Pid ~p is stopping.",[Hostname1, self()])),
	    Pid1 ! {log, {notice, Message1}},
            ok;

	{update_management_process_pid, New_pid} ->
            %% Change Pid of management process to New_pid
            Agent2 = Agent1#agent{management_process_pid=New_pid},
            base_host_agent(Agent2);

	{update_hostname, New_hostname} ->
            Agent2 = Agent1#agent{hostname=New_hostname},
            base_host_agent(Agent2);


	{get_vm_list, Sender, Request_id} ->
	    %% Get list of VMs on base machine using "virsh list --all"  %% stub
	    VM_list=[4,5,6],
	    Sender ! {Request_id, VM_list},
	    base_host_agent(Agent1);



	{stop_vm, VMID } ->
	    %% Stop a running VM using "virsh destroy  {VMID|name}"
	    %% stub
	    base_host_agent(Agent1);



	{get_container_list, Sender, Request_id} ->
	    %% Get list of containers on base machine using "vzlist -a" 
	    OutStr=os:cmd("vzlist -a"),
	    %%Check if root permissions are required
	    case string:equal(OutStr,"This program can only be run under root.\n") of
		true ->
		    io:format("In false ~p~n",[Sender]),
		    Sender!{Request_id,err_root_required},
		    base_host_agent(Agent1);
		false->
		    io:format("In false ~p~n",[Sender]),
		    Lines=string:tokens(OutStr,"\n"),
		    [_|Container_list]=Lines,
		    Sender ! {Request_id, Container_list},
		    base_host_agent(Agent1)
	    end;



	{create_container, Sender ,CTID, Options} ->
            %% create container with "vzctl create CTID  --option value" command
            %% Example message {create_container, Sender, Request_id, 101, 
	    %%             [{"hostname", "test.virtual-labs.ac.in"},
	    %%              {"ostemplate", "centos-6.3-x86_64"},
	    %%              {"ipaddress", "10.4.15.201"}]}
            %% should result into command
            %%    vzctl create 101 --hostname test.virtual-labs.ac.in 
            %%          --ostemplate centos-6.3-x86_64 --ipaddress 10.4.15.201
	    %% stub
	    Command="vzctl create ",
	    Containerid=integer_to_list(CTID),
	    Fixed=[Command]++Containerid,
	    Result=list_to_string(Options,Fixed),
	    Returnval=os:cmd(list_to_string(Options,Fixed)),
						%io:format("Result is ~s~n",[Returnval]),
	    Sender!{Returnval},
	    %%To see command printed out as a string
						%io:format("~s~n",[lists:flatten(Command)]),
	    base_host_agent(Agent1);


	{destroy_container,Sender, CTID} ->
            %%Destroy container with given CTID
	    Command="vzctl destroy ",
	    Containerid=integer_to_list(CTID),
	    Execute=[Command]++Containerid,
	    ReturnVal=os:cmd(Execute),
	    Sender!{ReturnVal},
            base_host_agent(Agent1);	

	{start_container,Sender, CTID} ->
            %%Start container with given CTID
	    Command="vzctl start ",
	    Containerid=integer_to_list(CTID),
	    Execute=[Command]++Containerid,
	    ReturnVal=os:cmd(Execute),
	    Sender!{ReturnVal},
            base_host_agent(Agent1);	

	{stop_container,Sender, CTID} ->
            %%Stop container with given CTID
	    Command="vzctl stop ",
	    Containerid=integer_to_list(CTID),
	    Execute=[Command]++Containerid,
	    ReturnVal=os:cmd(Execute),
	    Sender!{ReturnVal},
            base_host_agent(Agent1);	


	{get_num_containers, Sender, Request_id} ->
            %% Get number of containers on host. Reply is in the form of {Req_id,Total_Containers,Running_Containers,Stopped_Containers} 
	    All_containers=os:cmd("vzlist -a|wc -l"),

	    %%Check if root permissions are required
	    case string:equal(All_containers,"This program can only be run under root.\n") of 
		true -> 
		    Sender!{Request_id,err_root_needed},
		    base_host_agent(Agent1);
		false->
		    %%Hack to strip out the \n and return a pure integer
		    Num=re:replace(All_containers,"\n",""),	   
		    [H|_]=Num,
		    Total_number_of_containers=list_to_integer(binary_to_list(H))-1,

		    %%Running Containers
		    Running=os:cmd("vzlist|wc -l"),
		    Num_running=re:replace(Running,"\n",""),
		    [H1|_]=Num_running,
		    Running_containers=list_to_integer(binary_to_list(H1))-1,

		    %%Stopped containers
		    Stopped=os:cmd("vzlist --stopped |wc -l"),
		    Num_stopped=re:replace(Stopped,"\n",""),
		    [H2|_]=Num_stopped,
		    Stopped_containers=list_to_integer(binary_to_list(H2))-1,

		    Sender!{Request_id,Total_number_of_containers,Running_containers,Stopped_containers},
		    base_host_agent(Agent1)
	    end; 


	{modify_container, Sender,CTID, Options} ->
            %% modify container with "vzctl set CTID  --option value" command
            %% Example message {modify_container, Sender, Request_id, 101, 
            %%             [{"onboot", "yes"},
            %%              {"nameserver", "8.8.4.4"}]}
            %% Results into command
            %%    vzctl set 101 --onboot yes 
            %%          --nameserver 8.8.4.4
	    Command="vzctl set ",
            Containerid=integer_to_list(CTID),
            Fixed=[Command]++Containerid,
	    Returnval=os:cmd(list_to_string(Options,Fixed)),
	    %io:format("Result is ~s~n",[Returnval]),
	    Sender!{Returnval},
	    %%To see command printed out as a string
	    %%io:format("~s~n",[lists:flatten(Command)]),
            base_host_agent(Agent1);




	%% Similarly more functions for VMs, networks, etc. that are easily
	%% achievable.

	Any1 -> 
	    Pid1 = Agent1#agent.management_process_pid,
	    Hostname1=Agent1#agent.hostname,
	    Message1=lists:flatten(io_lib:format("Unknown message ~p received at base ~p.", [Any1, Hostname1])),
	    Pid1 ! {log, {info, Message1}},
	    base_host_agent(Agent1)
    end.

