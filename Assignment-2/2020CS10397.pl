%Defining the integer binary tree, as given in the assignment.
ibt(empty).
ibt(node(N, L, R)) :- integer(N), ibt(L), ibt(R).

% Defines the size of binary tree recursively. Size of empty binary
% tree is 0, and size of binary tree is size of left subtree + size of
% right subtree + 1.
size(node(_, L, R), N):- size(L, NL), size(R, NR), N is NL + NR + 1.
size(empty, 0).

% Defines the height of binary tree recursively. Height of empty
% binary tree is 0, and height of binary tree is max(height(left
% subtree), height(right subtree)) + 1.
height(node(_, L, R), N) :- height(L, NL), height(R, NR), N is max(NL, NR)+1.
height(empty,0).

% Defines the preorder traversal of a binary tree recursively. The
% preorder traversal of a binary tree is a list whose head is the
% value of the root, and tail is formed by appending preorder of left
% binary tree with right binary tree.
preorder(node(X, L, R), [X|Tail]) :- preorder(L, LL), preorder(R, RL), append(LL, RL, Tail).
preorder(empty, []).


% Defines the inorder traversal of a binary tree recursively. The
% inorder traversal of a binary tree is a list formed by appending
% inorder of left binary tree with value of root node, and then
% with inorder of right binary tree.
inorder(node(X, L, R), List) :- inorder(L, LL), inorder(R, RL), append(LL, [X|RL], List).
inorder(empty, []).

% Defines the postorder traversal of a binary tree recursively. The
% postorder traversal of a binary tree is a list formed by appending
% postorder of left binary tree with that of right binary tree, and then
% finally appending the root value to the end of the list.
postorder(node(X, L, R), List) :- postorder(L, LL), postorder(R, RL), append(LL, RL, L1), append(L1, [X], List).
postorder(empty, []).

% To check if a binary tree is Balanced, we check 2 things, firstly,
% both left and right subtrees should be balanced, secondly, the height
% difference between left and right subtree should be -1, 0 or 1.
isBalanced(node(_, L, R)) :- isBalanced(L), isBalanced(R), height(L, HL), height(R, HR), Diff is HL - HR, abs(Diff, D), D < 2 .
isBalanced(empty).

% Defines the string representation of a binary tree as defined in the
% assignment.
toString(node(X, L, R), S) :- toString(L, LS), toString(R, RS), number_string(X, SX), string_concat("(", SX, S1), string_concat(", ", LS, S2), string_concat(", ", RS, S3), string_concat(S3, ")", S4), string_concat(S1, S2, S5), string_concat(S5, S4, S) .
toString(empty, "()").

% Checks if a certain element exists in the binary search Tree. This is
% done by traversing through the tree in the correct direction.
lookup(N, node(X, L, R)):- N < X , lookup(N, L); N > X, lookup(N, R); X is N.
lookup(_, empty) :- false.


attach_recL(node(X, L, R),To_be_attached, node(X, L, New_R)) :- attach_recL(R,To_be_attached, New_R).
attach_recL(node(X, L, empty), To_be_attached, node(X, L, To_be_attached)).

attach_recR(node(X, L, R), To_be_attached, node(X, New_L, R)) :- attach_recR(L,To_be_attached, New_L).
attach_recR(node(X, empty, R), To_be_attached, node(X, To_be_attached, R)).

attachL(node(X, L, R), node(X, New_L, empty)) :- attach_recL(L, R, New_L).
attachL(node(X, L, empty), node(X, L, empty)).
attachL(node(X, empty, R), node(X, R, empty)).
attachL(node(X, empty, empty), node(X, empty, empty)).

attachR(node(X, L, R), node(X, empty, New_R)) :- attach_recR(R, L, New_R).
attachR(node(X, L, empty), node(X, empty, L)).
attachR(node(X, empty, R), node(X, empty, R)).
attachR(node(X, empty, empty), node(X, empty, empty)).

attachIn(node(X, node(Y, L1, R1), R), node(Y, L1, New_R)) :- attach_recL(node(Y, L1, R1), node(X, empty, R), node(Y, L1, New_R)).
attachIn(node(X, empty, R), node(X, empty, R)).

% My approach: I transform the tree in such a way that its preorder
% remains the same. So in this case, I move the right branch at every
% node to the rightmost node of left subtree. Now the recursion can be
% called on only one side of the tree, and thus tail recursive.
trPreorder(node(X, L, R), [X|Tail]) :- attachL(node(X, L, R), node(X, New_L, empty)), trPreorder(New_L, Tail).
trPreorder(empty, []).

trPostorder(node(X, L, R), List) :- attachR(node(X, L, R), node(X, empty, New_R)), append(Y, [X], List), trPostorder(New_R, Y).
trPostorder(empty, []).

trInorder(node(X, empty, R), [X|Tail]) :- trInorder(R, Tail).
trInorder(node(X, L, R), List) :- attachIn(node(X, L, R), NL), trInorder(NL, List).
trInorder(empty, []).

eulerTour(node(X, L, R), [X|Tail]) :- eulerTour(L, LL), eulerTour(R, RL), append(LL, [X|RL], Temp), append(Temp, [X], Tail).
eulerTour(empty, []).

insert(N, empty, node(N, empty, empty)).
insert(X, node(X, _, _), node(X, _, _)).
insert(N, node(X, L, empty), node(X, L, node(N, empty, empty))) :- N>X.
insert(N, node(X, empty, R), node(X, node(N, empty, empty), R)) :- N<X.
insert(N, node(X, L1, R), node(X, L2, R)) :- N<X, insert(N, L1, L2).
insert(N, node(X, L, R1), node(X, L, R2)) :- N>X, insert(N, R1, R2).

count(X, [X|List], N) :- count(X, List, N1), N is N1+1.
count(X, [Y|List], N) :- X \= Y, count(X, List, N).
count(_, [], 0).

extractPre([], []).
extractPre([_| []], _) :- fail.
extractPre([_, _ | []], _) :- fail.
extractPre([X, X, X, X, X, Y, Y, Y, X] , [X, X, Y]).
extractPre([N, T1, T2| TailET], [N|List]):- count(N, [T1, T2|TailET], X), X > 1, append(X1, [N], X2), append(X2, X3, X4), append(X4, [N], [T1, T2 |TailET]),extractPre(X1, LL), extractPre(X3, RL), append(LL, RL, List).


preET(node(X, L, R), List) :- eulerTour(node(X, L, R), ET), extractPre(ET, List) .
preET(empty, []).

extractPost([], []).
extractPost([_| []], _) :- fail.
extractPost([_, _ | []], _) :- fail.
extractPost([X, X, X, X, X, Y, Y, Y, X] , [X, Y, X]).
extractPost([N, T1, T2| TailET], List):- count(N, [T1, T2|TailET], X), X > 1, append(X1, [N], X2), append(X2, X3, X4), append(X4, [N], [T1, T2 |TailET]),extractPost(X1, LL), extractPost(X3, RL), append(LL, RL, X5), append(X5,[N], List).



postET(node(X, L, R), List) :- eulerTour(node(X, L, R), ET), extractPost(ET, List) .
postET(empty, []).

extractIn([], []).
extractIn([_| []], _) :- fail.
extractIn([_, _ | []], _) :- fail.
extractIn([X, X, X, X, X, Y, Y, Y, X] , [X, X, Y]).
extractIn([N, T1, T2| TailET], List):- count(N, [T1, T2|TailET], X), X > 1, append(X1, [N], X2), append(X2, X3, X4), append(X4, [N], [T1, T2 |TailET]),extractIn(X1, LL), extractIn(X3, RL), append(LL, [N], X5), append(X5,RL, List).


inET(node(X, L, R), List) :- eulerTour(node(X, L, R), ET), extractIn(ET, List) .
inET(empty, []).

%returns the maximum value of node in tree.
maxVal(node(_, _, node(Y,L2,R2)), X) :- maxVal(node(Y, L2, R2), X).
maxVal(node(X, _, empty), X).

%returns the minimum value of node in tree.
minVal(node(_, node(Y, L2, R2), _), X) :- minVal(node(Y, L2, R2), X).
minVal(node(X, empty, _), X).

%deletes a given node based on given conditions.
delete(N, node(X, L, R), node(X, L2, R)) :- N<X, lookup(N,L), delete1(N, L, L2).
delete(N, node(X, L, R), node(X, L, R2)) :- (N>X, lookup(N, R), delete1(N, R, R2)).
delete(N, node(N, empty, R), R).
delete(N, node(N, L, empty), L).
delete(N, node(N, node(XL, LL, LR), node(X1, X2, X3)), node(X, L, node(X1, X2, X3))) :- maxVal(node(XL, LL, LR), X) ,rem(node(XL ,LL, LR), L).
delete(_, empty, empty).

%removes the rightmost node of the right subtree of given node.
rem(node(_, _, empty), empty).
rem(node(XL, LL, node(_, _, empty)), node(XL, LL, empty)).
rem(node(XL, LL, node(X1, X2, node(X3, X4, X5))), node(XL, LL, RL)) :- rem(node(X1, X2, node(X3, X4, X5)), RL).

makeBST(List, BT) :- sort(List, SortedList),bstConstruct(SortedList, BT).

split(L, LL, RL, X) :- length(L, N), N2 is N//2, length(LL, N2), append(LL, [X], Temp), append(Temp, RL, L).

% Given a sorted list, constructs the BST. Splits the list in 2 parts,
% calls the function recursively.
bstConstruct([N|List], node(X, L, R)) :- split([N|List], LL, RL, X), bstConstruct(LL, L), bstConstruct(RL, R).
bstConstruct([], empty).

% Checks if the value in 2nd argument is greater than the max value of
% the tree.
checkL(node(X, L, R), Y) :- maxVal(node(X, L, R), M), Y>M.
checkL(empty, _).

% Checks if the value in 2nd argument is lesser than the min value of
% the tree.
checkR(node(X, L, R), Y) :- minVal(node(X, L, R), M), Y<M.
checkR(empty, _).

% Checks if the input tree is BST. A tree is BST if the left and right
% subtrees are also BSTs and if the value at node is greater than the
% maximum value of left subtree and is lesser than the min value of
% right subtree.
isBST(empty).
isBST(node(X, L, R)) :- isBST(L), isBST(R), checkL(L, X), checkR(R, X).
