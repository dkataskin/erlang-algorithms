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

-export([new/1, add_point/2, add_point/3, remove_point/2, remove_point/3]).

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

remove_point(QTree=#qtree_node{}, Point) ->
        remove_point(QTree, Point).

remove_point(QTree=#qtree_node{ bounds = BoundingRect }, Point, Value) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            {ok, add_valid_point(QTree, Point, Value)};
          false ->
            {error, point_no_in_bounding_rect}
        end.

add_valid_point(QTree=#qtree_node{ bounds = BoundingRect, nodes = Nodes }, Point, Value) ->
        case is_in_rect(BoundingRect, Point) of
          true ->
            case Nodes of
              [] ->
                case is_leaf_node(QTree) of
                  true ->
                    QTree#qtree_node { val = Value };
                  false ->
                    NewNodes = create_child_nodes(BoundingRect),
                    NewNodes1 = lists:map(fun(QTreeNode) -> add_valid_point(QTreeNode, Point, Value) end, NewNodes),
                    QTree#qtree_node { nodes = NewNodes1 }
                end;
              List ->
                NewNodes = lists:map(fun(QTreeNode) -> add_valid_point(QTreeNode, Point, Value) end, List),
                QTree#qtree_node { nodes = NewNodes }
            end;
          false ->
            QTree
        end.

create_child_nodes({{X1, Y1}, {X2, Y2}}) ->
        CenterX = X1 + erlang:trunc((X2 - X1) / 2),
        CenterY = Y1 + erlang:trunc((Y2 - Y1) / 2),

        TopCenter = {CenterX, Y2},
        TopRight = {X2, Y2},

        CenterLeft = {X1, CenterY},
        CenterCenter = {CenterX, CenterY},

        BottomLeft = {X1, Y1},
        BottomCenter = {CenterX, Y1},
        BottomRight = {X2, Y1},

        NW = #qtree_node { bounds = {CenterLeft, TopCenter} },
        NE = #qtree_node { bounds = {CenterCenter, TopRight} },
        SW = #qtree_node { bounds = {BottomLeft, CenterCenter} },
        SE = #qtree_node { bounds = {BottomCenter, BottomRight} },
        [NW, NE, SE, SW].

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