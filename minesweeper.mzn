int : width;
int : height;

int : w = width-1;
int : h = height-1;

array [0..w, 0..h] of var bool : mined;
%array [0..w, 0..h] of var int : danger;

int : nclues;
array [1..nclues, 1..3] of int : clues;

int : nguesses;
array [1..nguesses, 1..3] of int : guesses;

constraint
  forall (c in 1..nclues)
    ( let { int : x = clues[c,1],
            int : y = clues[c,2],
            int : n = clues[c,3] }
      in if n == -1
         then mined[x,y] = true
         else mined[x,y] = false /\
              sum( [ bool2int(mined[x+i,y+j]) | i,j in [-1,0,1] ] ) == n
         endif );

constraint
  forall (g in 1..nguesses)
    ( let { int : x = guesses[g,1],
            int : y = guesses[g,2] }
      in mined[x,y] = (guesses[g,3] == 1) );

solve satisfy;

% width = 10;
% height = 10;

% nclues = 1;
% clues = [| 0,0,0 |];

output [ if fix(mined[x,y]) then "*" else "." endif ++ if x == w then "\n" else "" endif | y in 0..h, x in 0..w ] ++ ["\n"];
