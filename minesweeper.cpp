#include <gecode/minimodel.hh>
#include <gecode/search.hh>
#include <iostream>

using namespace Gecode;
using namespace std;

class Minesweeper : public Space {
public:
  BoolVarArray bs;
  Minesweeper(int width, int height, IntArgs clues) {
    bs = BoolVarArray(*this, width*height, 0, 1);
    Matrix<BoolVarArray> m(bs, width, height);

    for (int i = 0 ; i < clues.size() ; i += 3) {
      int x = clues[i];
      int y = clues[i+1];
      int n = clues[i+2];
      if (n == -1) {
        rel(*this, m(x,y), IRT_EQ, 1);
      } else {
        rel(*this, m(x,y), IRT_EQ, 0);
        BoolVarArgs neighbours;
        for (int xx = x-1 ; xx <= x+1 ; xx++) {
          for (int yy = y-1 ; yy <= y+1 ; yy++) {
            if (xx >= 0 && yy >= 0 &&
                xx < width && yy < height &&
                (xx != x || yy != y))
              neighbours << m(xx,yy);
          }
        }
        linear(*this, neighbours, IRT_EQ, n);
      }
    }
    branch(*this, bs, INT_VAR_AFC_MAX, INT_VAL_MIN);
  }

  Minesweeper(bool share, Minesweeper& s) : Space(share, s) {
    bs.update(*this, share, s.bs);
  }

  virtual Space* copy(bool share) {
    return new Minesweeper(share, *this);
  }
};

int
main(void) {
  int width, height;
  cin >> width;
  cin >> height;

  int guessx, guessy, guesst;
  cin >> guessx;
  cin >> guessy;
  cin >> guesst;
  
  IntArgs clues;
  int x, y, n;
  while (cin >> x) {
    cin >> y;
    cin >> n;
    clues << x << y << n;
  }
  Minesweeper* s = new Minesweeper(width, height, clues);
  Matrix<BoolVarArray> m(s->bs, width, height);
  rel(*s, m(guessx, guessy), IRT_EQ, guesst);
  // switch (s->status()) {
  // case SS_FAILED: cout << "failed\n"; break;
  // case SS_SOLVED: cout << "solved\n"; break;
  // case SS_BRANCH: cout << "consistent\n"; break;
  // }

  DFS<Minesweeper> e(s);
  delete s;

  if (e.next())
    return 0;
  else
    return 1;

  // for (int x = 0 ; x < width ; x++) {
  //   for (int y = 0 ; y < height ; y++) {
  //     cout << x << " " << y << " ";
  //     if (m(x,y).assigned())
  //       cout << m(x,y);
  //     else
  //       cout << "?";
  //     cout << "\n";
  //   }
  // }

  //  return 0;
}
