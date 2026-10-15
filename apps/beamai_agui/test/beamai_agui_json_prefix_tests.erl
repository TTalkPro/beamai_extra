%%%-------------------------------------------------------------------
%%% @doc `beamai_agui_json_prefix` 的用例
%%%
%%% 要证的核心不是「能切」，而是**切出来的每一份都是合法 JSON、内容只增不减** ——
%%% 前端拿它整块换掉自己那份，错一帧就是闪一下错界面。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_json_prefix_tests).

-include_lib("eunit/include/eunit.hrl").

json_prefix_test_() ->
    [%% 第一个值都没写完 → 没有中间态，不是错误
     {"空输入", fun() -> ?assertEqual(incomplete, scan(<<>>)) end},
     {"只有半个键", fun() -> ?assertEqual(incomplete, scan(<<"{\"sur">>)) end},
     {"值还没开始", fun() -> ?assertEqual(incomplete, scan(<<"{\"a\":">>)) end},
     {"数字写了一半不能切（12 切成 1 就是错的）",
      fun() -> ?assertEqual(incomplete, scan(<<"{\"n\":12">>)) end},
     {"引号没闭合", fun() -> ?assertEqual(incomplete, scan(<<"{\"a\":\"x">>)) end},

     {"值刚写完就能切", fun() ->
         ?assertEqual(#{<<"a">> => 1}, parse(<<"{\"a\":1,">>)) end},
     {"字符串里的括号与转义引号不算结构",
      fun() ->
          Json = <<"{\"a\":\"x{y\\\"z}\",\"b\":2}">>,
          ?assertEqual(#{<<"a">> => <<"x{y\"z}">>}, parse(cut(Json, 14))),
          ?assertEqual(#{<<"a">> => <<"x{y\"z}">>, <<"b">> => 2}, parse(Json))
      end},
     {"数组：完整元素保留，半截元素丢掉",
      fun() ->
          Json = <<"{\"components\":[{\"id\":\"root\"},{\"id\":\"x",
                   "\",\"text\":\"hi\"}]}">>,
          ?assertEqual([#{<<"id">> => <<"root">>}],
                       maps:get(<<"components">>, parse(cut(Json, 29)))),
          ?assertEqual(2, length(maps:get(<<"components">>, parse(Json))))
      end},
     {"空对象是一个完整的值",
      fun() -> ?assertEqual(#{<<"a">> => #{}}, parse(cut(<<"{\"a\":{},\"b\":1}">>, 8))) end},
     {"null / true / false 同样要等分隔符",
      fun() ->
          ?assertEqual(incomplete, scan(<<"{\"a\":tru">>)),
          ?assertEqual(#{<<"a">> => true}, parse(<<"{\"a\":true,">>))
      end},
     {"模型吐了非法字符：停在最近一个完整值，不往前凑",
      fun() -> ?assertEqual(#{<<"a">> => 1}, parse(<<"{\"a\":1,@#$">>)) end},

     {"逐字节前缀都是合法 JSON，内容单调不减",
      fun() -> monotonic(a2ui_args()) end}
    ].

%%====================================================================
%% 夹具
%%====================================================================

scan(Json) -> beamai_agui_json_prefix:parse_prefix(Json).
parse(Json) ->
    case scan(Json) of
        {ok, Term} -> Term;
        Other -> Other
    end.

cut(Json, N) -> binary:part(Json, 0, min(N, byte_size(Json))).

%% 一份真实形状的 render_a2ui 参数（比 partial_every 的缺省节流长）
a2ui_args() ->
    <<"{\"surfaceId\":\"dashboard\",\"components\":["
      "{\"id\":\"root\",\"component\":\"Column\",\"children\":[\"title\",\"row\"]},"
      "{\"id\":\"title\",\"component\":\"Text\",\"text\":\"Overview\",\"variant\":\"h2\"},"
      "{\"id\":\"row\",\"component\":\"Row\",\"children\":[\"c1\"]},"
      "{\"id\":\"c1\",\"component\":\"Text\",\"text\":\"1,200\"}],"
      "\"data\":{\"kpi\":\"revenue\"}}">>.

monotonic(Json) ->
    {Counts, _} = lists:mapfoldl(
        fun(N, Prev) ->
            case beamai_agui_json_prefix:parse_prefix(binary:part(Json, 0, N)) of
                {ok, Term} when is_map(Term) ->
                    NComponents = length(maps:get(<<"components">>, Term, [])),
                    ?assert(NComponents >= Prev),
                    {NComponents, NComponents};
                _Other ->
                    {Prev, Prev}
            end
        end, 0, lists:seq(1, byte_size(Json))),
    %% 最后必须是完整的一份（4 个组件 + data），否则这条链没意义
    ?assertEqual(4, lists:last(Counts)).
