%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			Map.erl				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

%Returns an empty map
new() -> 
	[].

% Updates Map to reflect that Node has directional
% links to all nodes in the list Links
update(Node, Links, Map) ->
	AuxMap = lists:keydelete(Node, 1, Map),
	[{Node, Links} | AuxMap].

%Returns the list of nodes directly reachable
%from Node
reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		false -> 
			[];
		{_, Links} ->
			Links
		end.

%Returns a list of all nodes in the map, also the ones
%without outgoing links
all_nodes(Map) ->
	AuxMap = lists:flatmap(fun({Node,Links}) -> [Node|Links] end, Map),
	lists:usort(NewMap).
