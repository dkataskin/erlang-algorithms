-module(quad_tree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type bounding_rect() :: {LeftTop :: point(), RightBottom :: point()}.

-record(qtree_node, { bounds :: bounding_rect(),
                      point :: point(),
                      nodes = [] :: [qtree_node()],
                      val :: any()}).
-type qtree_node() :: #qtree_node{}.
-type qtree() :: qtree_node().

-type error() :: {error, Reason :: atom()}.

-export([new/1, add_point/2, add_point/3, remove_point/2]).

%% node
%%  point (x,y)
%%  bounds (x,y) - (x,y)
%%  nodes [nw,ne,sw,se]
%%  val any()

-spec new(BoudingRect :: bounding_rect()) -> {ok, qtree()} | error().
new(BoundingRect) ->
        case validate_bounds(BoundingRect) of
          {ok, valid} ->
            {ok, #qtree_node { point = get_center(BoundingRect),
                               bounds = BoundingRect,
                               nodes = [],
                               val = undefined }};
          {error, Reason} ->
            {error, Reason}
        end.

add_point(QTree=#qtree_node{}, Point) ->
        add_point(QTree, Point, undefined).

add_point(QTree=#qtree_node{ bounds = BoundingRect }, Point, Value) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, QTree};
          false ->
            {error, point_no_in_bounding_rect}
        end.

remove_point(QTree=#qtree_node{ bounds = BoundingRect }, Point) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, remove_valid_point(QTree, Point)};
          false ->
            {error, point_no_in_bounding_rect}
        end.

add_valid_point(QTree=#qtree_node{ bounds = BoundingRect, nodes = Nodes }, Point, Value) ->
        case is_leaf_node(QTree) of
          true ->
            QTree#qtree_node { val = Value };
          false ->
            FilterFun = fun(#qtree_node{ bounds = BoundingRect }) -> is_in_rect(BoundingRect, Point) end,
            case lists:filter(FilterFun, Nodes) of
              [] ->
                NewNode = create_child_node(BoundingRect, Point),
                NewNode1 = add_valid_point(NewNode, Point, Value),
                QTree#qtree_node { nodes = Nodes ++ [NewNode1] };
              [Node] ->
                Node1 = add_valid_point(Node, Point, Value),
                QTree#qtree_node { nodes = (Nodes -- [Node])++ [Node1] }
            end
        end.

-spec create_child_node(BoundingRect :: bounding_rect(), Point :: point()) -> qtree_node().
create_child_node({{X1, Y1}, {X2, Y2}}, Point) ->
        CX = X1 + erlang:trunc((X2 - X1) / 2),
        CY = Y1 + erlang:trunc((Y2 - Y1) / 2),

        NW = #qtree_node { bounds = {{X1, CY}, {CX, Y2}} },
        NE = #qtree_node { bounds = {{CX + 1, CY}, {X2, Y2}} },
        SW = #qtree_node { bounds = {{X1, Y1}, {CX, CY - 1}} },
        SE = #qtree_node { bounds = {{CX + 1, Y1}, {X2, CY - 1}} },

        FilterFun = fun(#qtree_node{ bounds = BoundingRect }) -> is_in_rect(BoundingRect, Point) end,
        [ChildNode] = lists:filter(FilterFun, [NW, NE, SW, SE]),
        ChildNode.

is_leaf_node(#qtree_node{ bounds = {{X1, _}, {X2, _}} }) ->
        X2 - X1 =:= 0.

validate_bounds({{X1, Y1}, {X2, Y2}}) ->
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