% Copyright (c) 2014, Dmitry Kataskin
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% * Redistributions of source code must retain the above copyright notice, this
% list of conditions and the following disclaimer.
%
% * Redistributions in binary form must reproduce the above copyright notice,
% this list of conditions and the following disclaimer in the documentation
% and/or other materials provided with the distribution.
%
% * Neither the name of the erlchat nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(quad_tree).
-author("Dmitry Kataskin").

-type point() :: {integer(), integer()}.
-type rect() :: {BottomLeft :: point(), TopRight :: point()}.

-record(qtree_node, { bounds :: rect(),
                      nodes = [] :: [qtree_node()],
                      is_leaf = false :: boolean(),
                      val :: any()}).
-type qtree_node() :: #qtree_node{}.
-type qtree() :: qtree_node().

-type error() :: {error, Reason :: atom()}.

-export([new/1, add_point/2, add_point/3, remove_point/2, query/2, flatten/1]).

-spec new(BoudingRect :: rect()) -> {ok, qtree()} | error().
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

-spec query(QTree :: qtree(), QueryRect :: rect()) -> [qtree_node()].
query(QTree, QueryRect) ->
        query(QTree, QueryRect, []).

query(QTreeNode=#qtree_node { bounds = {Point, _}, is_leaf = true }, QueryRect, Acc) ->
        case is_in_rect(QueryRect, Point) of
          true ->
            [QTreeNode | Acc];
          false ->
            Acc
        end;

query(#qtree_node { bounds = BoundingRect, nodes = Nodes }, QueryRect, Acc) ->
        case intersects(BoundingRect, QueryRect) of
          true ->
            lists:foldl(fun(QTreeNode, Acc) -> query(QTreeNode, QueryRect, Acc) end, Acc, Nodes);
          false ->
            Acc
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
                    QTree#qtree_node { nodes = (Nodes -- [Node]) ++ [NewNode] }
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

-spec create_child_node(BoundingRect :: rect(), Point :: point()) -> qtree_node().
create_child_node({{X1, Y1}, {X2, Y2}}, Point = {X, Y}) ->
        CX = X1 + erlang:trunc((X2 - X1) / 2),
        CY = Y1 + erlang:trunc((Y2 - Y1) / 2),

        case CX =:= X1 of
          true ->
            #qtree_node { bounds = {{X, Y}, {X, Y}}, is_leaf = true };

          false ->
            NW = #qtree_node { bounds = {{X1, CY}, {CX, Y2}} },
            NE = #qtree_node { bounds = {{CX + 1, CY}, {X2, Y2}} },
            SW = #qtree_node { bounds = {{X1, Y1}, {CX, CY - 1}} },
            SE = #qtree_node { bounds = {{CX + 1, Y1}, {X2, CY - 1}} },

            FilterFun = fun(#qtree_node{ bounds = BoundingRect }) -> is_in_rect(BoundingRect, Point) end,
            [ChildNode] = lists:filter(FilterFun, [NW, NE, SW, SE]),
            ChildNode
        end.

-spec validate_bounds(BoundingRect :: rect()) -> {ok, valid} | error().
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

-spec is_in_rect(BoundingRect :: rect(), Point :: point()) -> boolean().
is_in_rect(BoundingRect, Point) ->
        {{X1, Y1}, {X2, Y2}} = BoundingRect,
        {X, Y} = Point,
        X >= X1 andalso X =< X2 andalso
        Y >= Y1 andalso Y =< Y2.

-spec intersects(Rect1 :: rect(), Rect2 :: rect()) -> boolean().
intersects(Rect1, Rect2) ->
        {{AX1, AY1}, {AX2, AY2}} = Rect1,
        {{BX1, BY1}, {BX2, BY2}} = Rect2,
        AX1 =< BX2 andalso AX2 >= BX1 andalso
        AY1 =< BY2 andalso AY2 >= BY1.