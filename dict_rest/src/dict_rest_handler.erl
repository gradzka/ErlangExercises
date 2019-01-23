-module(dict_rest_handler).

%% Webmachine API
-export([
         init/3,
		 rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         resource_exists/2
        ]).

-export([
         db_to_json/2,
         db_to_text/2,
         text_to_db/2
        ]).

-record(state, {op}).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
	{ok, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, db_to_json},
      {<<"text/plain">>, db_to_text}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"text/plain">>, text_to_db},
      {<<"application/x-www-form-urlencoded">>, text_to_db}
     ], Req, State}.

db_to_json(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        list ->
            get_record_list(Req, State);
        get ->
            get_one_record(Req, State);
        help ->
            get_help(Req, State)
    end,
	{Body, Req1, State1}.

db_to_text(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        list ->
            get_record_list_text(Req, State);
        get ->
            get_one_record_text(Req, State);
        help ->
            get_help_text(Req, State)
    end,
	{Body, Req1, State1}.

text_to_db(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        create ->
            create_record_to_json(Req, State);
        update ->
            update_record_to_json(Req, State)
    end,
	{Body, Req1, State1}.


delete_resource(Req, State) ->
    {RecordId, _} = cowboy_req:binding(word, Req),	
    RecordId1 = binary_to_list(RecordId),
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
    {ok, _} = dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    Result = dets:delete(records_db, RecordId1),
    ok = dets:close(records_db),
    Response = case Result of
        ok ->
            true;
        {error, _Reason} ->
            false
    end,
    {Response, Req, State}.

resource_exists(Req, State) ->
	{Method, Req0} = cowboy_req:method(Req),
    case Method of
        <<"DELETE">> ->
			{RecordId, _} = cowboy_req:binding(word, Req0),			
            RecordId1 = binary_to_list(RecordId),
            {ok, Recordfilename} = application:get_env(
                 dict_rest, pl_en),
            {ok, _} = dets:open_file(
                records_db, [{file, Recordfilename}, {type, set}]),
            Records = dets:lookup(records_db, RecordId1),
            ok = dets:close(records_db),
            Response = case Records of
                [_] ->
                    {true, Req, State};
                _ ->
                    {false, Req, State}
            end,
            Response;
        _ ->
            {true, Req, State}
    end.

	
get_record_list(Req, State) ->
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
	dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    F = fun (Item, Acc) -> Acc1 = [Item | Acc], Acc1 end,
    Items = dets:foldl(F, [], records_db),
    dets:close(records_db),
    Items1 = lists:sort(Items),
    Body = "
{
    \"list\": ~p,
}",
    Body1 = io_lib:format(Body, [Items1]),
    {Body1, Req, State}.

get_record_list_text(Req, State) ->
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
    dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    F1 = fun (Item, Acc) -> Acc1 = [Item | Acc], Acc1 end,
    Items = dets:foldl(F1, [], records_db),
    dets:close(records_db),
    F2 = fun ({Id, Rec}, Acc) ->
                 Val = io_lib:format("- ~s: ~s~n", [Id, Rec]),
                 [Val | Acc]
         end,
    Items1 = lists:foldl(F2, [], Items),
    Items2 = lists:sort(Items1),
    Items3 = lists:flatten(lists:concat(Items2)),
    Body = "list:~n~s",
    Body1 = io_lib:format(Body, [Items3]),
    {Body1, Req, State}.

get_one_record(Req, State) ->
    {RecordId, _} = cowboy_req:binding(word, Req),
    RecordId1 = binary_to_list(RecordId),
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
    {ok, _} = dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    Records = dets:lookup(records_db, RecordId1),
    ok = dets:close(records_db),
    Body = case Records of
        [{RecordId2, Data}] ->
            io_lib:format("{\"id\": \"~s\", \"record\": \"~s\"}",
                          [RecordId2, binary_to_list(Data)]);
        [] ->
            io_lib:format("{\"not_found\": \"word: ~s not found\"}",
                          [RecordId1]);
        _ ->
            io_lib:format("{\"extra_records\": \"extra records for ~s\"}",
                          [RecordId1])
    end,
    {list_to_binary(Body), Req, State}.

get_one_record_text(Req, State) ->
    {RecordId, _} = cowboy_req:binding(word, Req),	
    RecordId1 = binary_to_list(RecordId),
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
    {ok, _} = dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    Records = dets:lookup(records_db, RecordId1),
    ok = dets:close(records_db),
    Body = case Records of
        [{RecordId2, Data}] ->
            io_lib:format("id: \"~s\", record: \"~s\"",
                          [RecordId2, binary_to_list(Data)]);
        [] ->
            io_lib:format("not_found: word: ~p not found",
                          [RecordId1]);
        _ ->
            io_lib:format("extra_records: extra records for ~p",
                          [RecordId1])
    end,
    {list_to_binary(Body), Req, State}.

create_record_to_json(Req, State) ->
    {RecordId1, _} = cowboy_req:binding(word, Req),	
    RecordId = binary_to_list(RecordId1),
	{ok, [{<<"content">>, Content}], Req1} = cowboy_req:body_qs(Req,[]),
    {ok, Recordfilename} = application:get_env(dict_rest, pl_en),
    {ok, _} = dets:open_file(records_db, [{file, Recordfilename}, {type, set}]),
    ok = dets:insert(records_db, {RecordId, Content}),
    ok = dets:sync(records_db),
    ok = dets:close(records_db),
	{Method, Req2} = cowboy_req:method(Req1),
    case Method of
        <<"POST">> ->
            Response = io_lib:format("/get/~s", [RecordId]),
            {{true, list_to_binary(Response)}, Req2, State};
        _ ->
            {true, Req2, State}
    end.

update_record_to_json(Req, State) ->
	{Method, Req0} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
			{RecId, _} = cowboy_req:binding(word, Req0),			
            RecId1 = binary_to_list(RecId),
            {ok, [{<<"content">>, NewContent}], Req1} =
				cowboy_req:body_qs(Req,[]),
            {ok, Recordfilename} = application:get_env(
                dict_rest, pl_en),
            {ok, _} = dets:open_file(
                records_db, [{file, Recordfilename}, {type, set}]),
            DBResponse = dets:lookup(records_db, RecId1),
            Result = case DBResponse of
                [_] ->
                    ok = dets:insert(records_db, {RecId1, NewContent}),
                    ok = dets:sync(records_db),
                    Response = io_lib:format("/get/~s", [RecId1]),
                    Response1 = list_to_binary(Response),
                    {{true, Response1}, Req1, State};
                [] ->
                    {true, Req1, State}
            end,
            ok = dets:close(records_db),
            Result;
        _ ->
            {true, Req, State}
    end.

get_help(Req, State) ->
    Body = "{
    \"/list\": \"return a list of words\",
    \"/get/word\": \"retrieve translations of word\",
    \"/create/word\": \"add a new word with translations \",
    \"/update/word\": \"update an existing word with translations\",
    \"/delete/word\": \"delete a word with translations\",
    \"/help\": \"show help\"
}",
    Body1 = io_lib:format(Body, []),
    {Body1, Req, State}.

get_help_text(Req, State) ->
    Body = "
- /list: return a list of words~n
- /get/word: retrieve translations of word~n
- /create/word: add a new word with translations~n
- /update/word: update an existing word with translations~n
- /delete/word: delete a word~n
- /help: show help~n
",
    Body1 = io_lib:format(Body, []),
    {Body1, Req, State}.
