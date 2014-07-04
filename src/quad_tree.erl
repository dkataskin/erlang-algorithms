-module(quad_tree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type bounding_rect() :: {LeftTop :: point(), RightBottom :: point()}.

%-record(qtree_leaf_node, { bounds :: bounding_rect(), point :: point(), val :: any()}).
-record(qtree_node, { bounds :: bounding_rect(),
                      point :: point(),
                      nodes = [] :: [qtree_node()],
                      val :: any()}).
-type qtree_node() :: #qtree_node{}.
-type qtree() :: qtree_node().

-type error() :: {error, Reason :: atom()}.

-export([new/1, add_point/2, remove_point/2]).

%% node
%%  point (x,y)
%%  bounds (x,y) - (x,y)
%%  nodes [nw,ne,sw,se]
%%  val any()

-spec new(BoudingRect :: bounding_rect()) -> {ok, qtree()} | error().
new(BoundingRect) ->
        case validate_bounding(BoundingRect) of
          {ok, valid} ->
            {ok, #qtree_node { point = get_center(BoundingRect),
                               bounds = BoundingRect,
                               nodes = [],
                               val = undefined }};
          {error, Reason} ->
            {error, Reason}
        end.

add_point(QTree=#qtree_node{ bounds = BoundingRect }, Point) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, QTree};
          false ->
            {error, point_no_in_bounding_rect}
        end.

remove_point(QTree=#qtree_node{ bounds = BoundingRect }, Point) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, QTree};
          false ->
            {error, point_no_in_bounding_rect}
        end.

validate_bounding({{X1, Y1}, {X2, Y2}}) ->
        W = erlang:abs(X2 - X1),
        H = erlang:abs(Y2 - Y1),
        case W =:= H of
          true ->
            case (W band (W - 1)) of
              0 ->
                case (H band (H - 1)) of
                  0 ->
                    {ok, valid};
                  _ ->
                    {error, height_not_power_of_2}
                end;
              _ ->
                {error, width_not_power_of_2}
            end;
          false ->
            {error, sides_not_equal}
        end.

get_center({{X1, Y1}, {X2, Y2}}) ->
        {erlang:trunc((X2 - X1) / 2), erlang:trunc((Y2 - Y1) / 2)}.

is_in_rect(BoundingRect, Point) ->
        {{X1, Y1}, {X2, Y2}} = BoundingRect,
        {X, Y} = Point,
        X >= X1 andalso X =< X2 andalso
        Y >= Y1 andalso Y =< Y2.
