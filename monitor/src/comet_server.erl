-module(comet_server).
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([add_job/3, push/1, pop/1, config/2, flush_jobs/0]).

-record(job, {id, init, cleanup, state}).
-record(frame, {name, jobs = []}).
-record(state, {pid, jobs = [], savedframes = []}).

-record(client, {task, server}).

start_link() ->
    {ok, Pid} = gen_server:start(?MODULE, [self()], []),
    link(Pid),
    
    {ok, Pid}.

config(TaskPid, ServerPid) ->
    wf:state(comet_state, #client{task = TaskPid, server = ServerPid}).  

rpc(Pid, Request) ->
    case gen_server:call(Pid, Request) of
	ok -> ok;
	{error, {exception, throw, Error}} ->
	    throw(Error);
	{error, {exception, error, Error}} ->
	    erlang:error(Error);
	{error, {exception, exit, Error}} ->
	    exit(Error)
    end.
    

add_job(Init, Cleanup, State) ->
    Client = wf:state(comet_state),
    add_job(Client#client.server, Init, Cleanup, State).

add_job(Pid, Init, Cleanup, State) ->
    rpc(Pid, {add_job, Init, Cleanup, State}).

push(Name) ->
    Client = wf:state(comet_state),
    push(Client#client.server).

push(Pid, Name) ->
    rpc(Pid, {push, Name}).

pop(Name) ->
    Client = wf:state(comet_state),
    push(Client#client.server).

pop(Pid, Name) ->
    rpc(Pid, {pop, Name}).

flush_jobs() ->
    Client = wf:state(comet_state),
    flush_jobs(Client#client.server).

flush_jobs(Pid) ->
    rpc(Pid, flush_jobs).

init([Pid]) ->
    process_flag(trap_exit, true),
    {ok, #state{pid = Pid}}.

handle_call({add_job, Init, Cleanup, JobState}, _From, State) ->
    try Init(State#state.pid, JobState) of
	{ok, NewState} ->
	    Jobs = [ #job{init = Init, cleanup = Cleanup, state = NewState}
		     | State#state.jobs ],

	    {reply, ok, State#state{jobs = Jobs}};
	Ret ->
	    {reply, {error, Ret}, State}
		
    catch
	Type:Error ->
	    {reply, {error, {exception, Type, Error}}, State}
    end;
    
handle_call({push, Name}, _From, State) ->
    NewFrame = #frame{name=Name, jobs=State#state.jobs},
    SavedFrames = State#state.savedframes,

    {reply, ok, State#state{jobs=[], savedframes=[NewFrame|SavedFrames]}};

handle_call({pop, Name}, _From, State) ->
    finish_jobs(State#state.jobs),
    Frames = finish_frame(State#state.savedframes, Name),
    {reply, ok, State#state{jobs = [], savedframes=Frames}};

handle_call(flush_jobs, _From, State) ->
    {reply, ok, finish_all(State)};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(Message, State) ->
    {noreply, State}.

finish_all(State) ->
    finish_jobs(State#state.jobs),
    [] = finish_frame(State#state.savedframes, undefined), 
    State#state{jobs = [], savedframes=[]}.

finish(Job) ->
    Cleanup = Job#job.cleanup,
    try Cleanup(Job#job.state)
    catch
	Type:Error ->
	    error_logger:warning_msg("~p:~p crash while finishing ~p~n",
				     [Type, Error, Job])
    end.

finish_frame([Frame | T], Stop) when Frame#frame.name =:= Stop ->
    [Frame | T];
finish_frame([Frame | T], Stop) ->
    finish_jobs(Frame#frame.jobs),
    finish_frame(T, Stop);
finish_frame([], _) ->
    [].

finish_jobs(Jobs) ->
    [ finish(Job) || Job <- Jobs].   

handle_info({'EXIT', From, Type}, State) ->
    {stop, normal, finish_all(State)}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%-------------------------------
% unit tests
%-------------------------------

-record(teststate, {init, instance}).

myinit(KnownPid, PresentedPid, State) ->
    if
	KnownPid =/= PresentedPid ->
	    throw({bad_pid, KnownPid, PresentedPid});
	true -> ok
    end,
    {ok, State#teststate{init=ok}}.

mycleanup(Pid, State=#teststate{init=ok}) ->
    Pid ! State#teststate.instance,
    ok.

job_test() ->
    S = self(),
    {ok, Pid} = start_link(),

    Init = fun(CallerPid, State) ->
		   myinit(S, CallerPid, State)
	   end,

    Cleanup = fun(State) -> mycleanup(S, State) end,

    ok = add_job(Pid, Init, Cleanup, #teststate{instance=alpha}),
    ok = push(Pid, first),
    ok = add_job(Pid, Init, Cleanup, #teststate{instance=bravo}),
    ok = push(Pid, second),
    ok = add_job(Pid, Init, Cleanup, #teststate{instance=charlie}),
    ok = pop(Pid, first),

    receive
	charlie -> ok;
	Msg -> throw({unexpected_msg, Msg})
    after 1000 ->
	    throw(timeout)
    end,

    receive
	bravo -> ok;
	Msg1 -> throw({unexpected_msg, Msg1})
    after 1000 ->
	    throw(timeout)
    end,

    % ensure the queue is actually empty
    receive
	Msg2 -> throw({unexpected_msg, Msg2})
    after 0 ->
	    ok
    end,

    ok = flush_jobs(Pid),

    receive
	alpha -> ok;
	Msg3 -> throw({unexpected_msg, Msg3})
    after 1000 ->
	    throw(timeout)
    end.


	      

