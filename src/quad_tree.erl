-module(quad_tree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type bounding_rect() :: {LeftTop :: point(), RightBottom :: point()}.

-record(qtree_node, { bounds :: bounding_rect(),
                      nodes = [] :: [qtree_node()],
                      is_leaf = false :: boolean(),
                      val :: any()}).
-type qtree_node() :: #qtree_node{}.
-type qtree() :: qtree_node().

-type error() :: {error, Reason :: atom()}.

-export([new/1, add_point/2, add_point/3, remove_point/2, flatten/1]).

-spec new(BoudingRect :: bounding_rect()) -> {ok, qtree()} | error().
new(BoundingRect) ->
        case validate_bounds(BoundingRect) of
          {ok, valid} ->
            {ok, #qtree_node { bounds = BoundingRect,
                               nodes = [],
                               val = undefined }};
          {error, Reason} ->
            {error, Reason}
        end.

-spec add_point(QTree :: qtree(), Point :: point()) -> {ok, qtree()} |  error().
add_point(QTree=#qtree_node{}, Point) ->
        add_point(QTree, Point, undefined).

-spec add_point(QTree :: qtree(), Point :: point(), Value :: any()) -> {ok, qtree()} | error().
add_point(QTree=#qtree_node { bounds = BoundingRect }, Point, Value) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, add_valid_point(QTree, Point, Value)};
          false ->
            {error, point_no_in_bounding_rect}
        end.

-spec remove_point(QTree :: qtree(), Point :: point()) -> {ok, qtree()} | error().
remove_point(QTree=#qtree_node{ bounds = BoundingRect }, Point) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, remove_valid_point(QTree, Point)};
          false ->
            {error, point_no_in_bounding_rect}
        end.

-spec flatten(QTree :: qtree()) -> [qtree_node()].
flatten(QTree) ->
        flatten(QTree, []).

flatten(QTree=#qtree_node { is_leaf = true }, Acc) ->
        [QTree | Acc];

flatten(#qtree_node { nodes = Nodes }, Acc) ->
        lists:foldl(fun(QTreeNode, Acc) -> flatten(QTreeNode, Acc) end, Acc, Nodes).

-spec add_valid_point(QTree :: qtree(), Point :: point(), Value :: any()) -> qtree().
add_valid_point(QTree=#qtree_node{ bounds = BoundingRect, nodes = Nodes }, Point, Value) ->
        case QTree#qtree_node.is_leaf of
          true ->
            QTree#qtree_node { val = Value };

          false ->
            case find_child_node(QTree, Point) of
              undefined ->
                NewNode = create_child_node(BoundingRect, Point),
                NewNode1 = add_valid_point(NewNode, Point, Value),
                QTree#qtree_node { nodes = Nodes ++ [NewNode1] };

              Node ->
                Node1 = add_valid_point(Node, Point, Value),
                QTree#qtree_node { nodes = (Nodes -- [Node]) ++ [Node1] }
            end
        end.

-spec remove_valid_point(QTree :: qtree(), Point :: point()) -> qtree().
remove_valid_point(QTree=#qtree_node{ nodes = Nodes }, Point) ->
        case find_child_node(QTree, Point) of
          undefined ->
            QTree;

          Node ->
            case Node#qtree_node.is_leaf of
              true ->
                QTree#qtree_node { nodes = Nodes -- [Node] };

              false ->
                NewNode = remove_valid_point(Node, Point),
                case NewNode#qtree_node.nodes of
                  [] ->
                    QTree#qtree_node { nodes = Nodes -- [Node] };

                  _ ->
                    QTree#qtree_node { nodes = (Nodes - [Node]) ++ [NewNode] }
                end
            end
        end.

-spec find_child_node(Node :: qtree_node(), Point :: point()) -> undefined | qtree_node().
find_child_node(#qtree_node{ nodes = Nodes }, Point) ->
        FilterFun = fun(#qtree_node{ bounds = BoundingRect }) -> is_in_rect(BoundingRect, Point) end,
        case lists:filter(FilterFun, Nodes) of
          [] ->
            undefined;
          [Node] ->
            Node
        end.

-spec create_child_node(BoundingRect :: bounding_rect(), Point :: point()) -> qtree_node().
create_child_node({{X1, Y1}, {X2, Y2}}, Point) ->
        CX = X1 + erlang:trunc((X2 - X1) / 2),
        CY = Y1 + erlang:trunc((Y2 - Y1) / 2),

        BoundsNW = {{X1, CY}, {CX, Y2}},
        BoundsNE = {{CX + 1, CY}, {X2, Y2}},
        BoundsSW = {{X1, Y1}, {CX, CY - 1}},
        BoundsSE = {{CX + 1, Y1}, {X2, CY - 1}},

        NW = #qtree_node { bounds = BoundsNW, is_leaf = would_be_leaf(BoundsNW) },
        NE = #qtree_node { bounds = BoundsNE, is_leaf = would_be_leaf(BoundsNE) },
        SW = #qtree_node { bounds = BoundsSW, is_leaf = would_be_leaf(BoundsSW) },
        SE = #qtree_node { bounds = BoundsSE, is_leaf = would_be_leaf(BoundsSE) },

        FilterFun = fun(#qtree_node{ bounds = BoundingRect }) -> is_in_rect(BoundingRect, Point) end,
        [ChildNode] = lists:filter(FilterFun, [NW, NE, SW, SE]),
        ChildNode.

-spec would_be_leaf(BoundingRect :: bounding_rect()) -> boolean().
would_be_leaf({{X1, _}, {X2, _}}) ->
        X2 - X1 =:= 0.

-spec validate_bounds(BoundingRect :: bounding_rect()) -> {ok, valid} | error().
validate_bounds({{X1, Y1}, {X2, Y2}}) ->
        W = erlang:abs(X2 - X1) + 1,
        H = erlang:abs(Y2 - Y1) + 1,
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

-spec is_in_rect(BoundingRect :: bounding_rect(), Point :: point()) -> boolean().
is_in_rect(BoundingRect, Point) ->
        {{X1, Y1}, {X2, Y2}} = BoundingRect,
        {X, Y} = Point,
        X >= X1 andalso X =< X2 andalso
        Y >= Y1 andalso Y =< Y2.