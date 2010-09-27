-module(comet_server).
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-export([add_job/3, config/2, flush_jobs/0]).

-record(job, {id, init, cleanup, state}).
-record(state, {pid, jobs = []}).

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
    
handle_call(flush_jobs, _From, State) ->
    {reply, ok, finish_all(State)};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(Message, State) ->
    {noreply, State}.

finish(Job) ->
    Cleanup = Job#job.cleanup,
    try Cleanup(Job#job.state)
    catch
	Type:Error ->
	    error_logger:warning_msg("~p:~p crash while finishing ~p~n",
				     [Type, Error, Job])
    end.

finish_all(State) ->
    [ finish(Job) || Job <- State#state.jobs],
    State#state{jobs = []}.

handle_info({'EXIT', From, Type}, State) ->
    {stop, normal, finish_all(State)}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%-------------------------------
% unit tests
%-------------------------------

myinit(KnownPid, PresentedPid, State) ->
    if
	KnownPid =/= PresentedPid ->
	    throw({bad_pid, KnownPid, PresentedPid});
	true -> ok
    end,
    {ok, init_ok}.

mycleanup(Pid, init_ok) ->
    Pid ! complete,
    ok.

job_test() ->
    S = self(),
    {ok, Pid} = start_link(),

    Init = fun(CallerPid, State) ->
		   myinit(S, CallerPid, State)
	   end,

    Cleanup = fun(State) -> mycleanup(S, State) end,

    ok = add_job(Pid, Init, Cleanup, none),
    ok = flush_jobs(Pid),
    
    receive
	complete -> ok;
	Msg -> throw({unexpected_msg, Msg})
    after 1000 ->
	    throw(timeout)
    end.
	      

