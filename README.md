snippy
======

A number of useful() | useless() data structure and algorithm implementations in Erlang.

###quad_tree.erl

As the name states, this is yet another quad tree implementation in Erlang. More on quad_trees here http://en.wikipedia.org/wiki/Quadtree.

Operations on quad tree you have:
* new
* add point
* remove point
* flatten
* query

####new
You pass bounding rect you get a new tree. Width and Height must be equal and must be power of 2.

`{ok, Tree} = quad_tree:new({{0,0}, {31,31}})`

###add point
Add random point to the tree. If there is already this point in the tree it will be replaced. You can associate arbitrary value with a node.

`{ok, Tree1} = quad_tree:add_point(Tree, {11, 12})`

Shall you pass arbitrary value to assign it to a node:

`{ok, Tree1} = quad_tree:add_point(Tree, {11, 12}, this_is_it)`

###remove point
Want to remove point? The call is similar to add_point:

`{ok, Tree1} = quad_tree:remove_point(Tree, {11, 12})`

This implementation collapses empty non-leaf nodes.

###flatten
Want to list all leaf nodes? Call flatten:

`List = quad_tree:flatten(Tree)`

###query
Interested in points inside a particular rectangle? That's possible:

`List = quad_tree:query(Tree, {{0,0},{31,31})`

