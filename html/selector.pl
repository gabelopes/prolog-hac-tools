:- module(selector, [
  select/3
]).

:- use_module(matcher, [matches/4]).

select([], _, []).
select(_, [], []).
select(Dom, Selectors, Results) :-
  is_list(Dom), !,
  select(Dom, Dom, Selectors, Results).
select(Element, Selectors, Results) :-
  select([Element], Selectors, Results).

select(_, [], _, []).
select(_, _, [], []).
select(Context, [Element|Siblings], Selectors, Results) :-
  Element = element(_, _, _),
  matches(Context, Element, Selectors, MatchedSelectors),
  combine(Context, [Element|Siblings], Selectors, MatchedSelectors, CombineResults),
  select(Context, Siblings, Selectors, SiblingsResults), !,
  append(CombineResults, SiblingsResults, Results).
select(Context, [_|Siblings], Selectors, Results) :-
  select(Context, Siblings, Selectors, Results).

combine(_, [], _, _, []).
combine(_, [element(_, _, Children)|_], Selectors, [], Results) :-
  filter_selectors(Selectors, [descendant], DescendantSelectors),
  select(Children, Children, DescendantSelectors, Results).
combine(Context, [element(Tag, Attributes, Children), Sibling|Siblings], Selectors, MatchedSelectors, Results) :-
  collect_element(element(Tag, Attributes, Children), MatchedSelectors, CollectedElement),
  select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults),
  select_combinators([adjacent, sibling], MatchedSelectors, Context, [Sibling], AdjacentResults),
  select_combinators([sibling], MatchedSelectors, Context, Siblings, SiblingsResults),
  append([CollectedElement, ChildrenResults, AdjacentResults, SiblingsResults], Results).
combine(_, [element(Tag, Attributes, Children)], Selectors, MatchedSelectors, Results) :-
  collect_element(element(Tag, Attributes, Children), MatchedSelectors, CollectedElement),
  select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults),
  append(CollectedElement, ChildrenResults, Results).

collect_element(Element, MatchedSelectors, [Element]) :-
  has_final_combinator(MatchedSelectors).
collect_element(_, _, []).

has_final_combinator(Selectors) :-
  get_combinators(Selectors, Combinators),
  member(none, Combinators).

select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults) :-
  filter_selectors(Selectors, [descendant], DescendantSelectors),
  get_combinators(MatchedSelectors, Combinators),
  filter_selectors(Combinators, [descendant, child], ChildrenCombinators),
  append(DescendantSelectors, ChildrenCombinators, AllSelectors),
  select(Children, Children, AllSelectors, ChildrenResults).

select_combinators(Types, MatchedSelectors, Context, Elements, Results) :-
  get_combinators(MatchedSelectors, Combinators),
  filter_selectors(Combinators, Types, FilteredCombinators),
  select(Context, Elements, FilteredCombinators, Results).

get_combinators([], []).
get_combinators([selector(_, _, Combinator)|Selectors], [Combinator|Combinators]) :-
  get_combinators(Selectors, Combinators).

filter_selectors([], _, []).
filter_selectors([Selector|Selectors], Types, [Selector|FilteredSelectors]) :-
  Selector = selector(Type, _, _),
  member(Type, Types),
  filter_selectors(Selectors, Types, FilteredSelectors).
filter_selectors([_|Selectors], Types, FilteredSelectors) :-
  filter_selectors(Selectors, Types, FilteredSelectors).
