-module(quad_tree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type bounds() :: {LeftTop :: point(), RightBottom :: point()}.

-record(qtree_leaf_node, { bounds :: bounds(), point :: point(), nodes = [] :: [qtree_node()], val :: any()}).
-record(qtree_node, { bounds :: bounds(), point :: point(), nodes = [] :: [qtree_node()], val :: any()}).
-type qtree_node() :: #qtree_node{}.
-type qtree() :: qtree_node().

%%-type qtree_node() :: {}
%% API
-export([]).

%% node
%%  point (x,y)
%%  bounds (x,y) - (x,y)
%%  nodes [nw,ne,sw,se]
%%  val any()

new(Bounds) ->
        case validate_bounds(Bounds) of
          true ->
            {ok, #qtree_node { point = get_center(Bounds),
                               bounds = Bounds,
                               nodes = [],
                               val = undefined }};
          false ->
            {error, not_power_of_2}
        end.

validate_bounds({{X1, Y1}, {X2, Y2}}) ->
        W = erlang:abs(X2 - X1),
        H = erlang:abs(Y2 - Y1),
        W =:= H andalso
        (W band (W - 1)) =:= 0 andalso
        (H band (H - 1)) =:= 0.

get_center({{X1, Y1}, {X2, Y2}}) ->
        {erlang:trunc((X2 - X1) / 2), erlang:trunc((Y2 - Y1) / 2)}.
