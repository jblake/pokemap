divert(-1)

This file should be preprocessed with m4.

The various terrains.

This is a helper for nonparitied terrains.
define(`noparity',
`define(`$1p1', $2)'
`define(`$1p2', $2)'
`define(`$1p3', $2)'
`define(`$1p4', $2)')

Rock does not have parity, but it has all the sides.
noparity(`rocknw',  5c)
noparity(`rockn',   2c)
noparity(`rockne',  5d)
noparity(`rockw',   3b)
noparity(`rock',    3c)
noparity(`rocke',   3d)
noparity(`rocksw',  4b)
noparity(`rocks',   4c)
noparity(`rockse',  4d)
noparity(`rocknwi', 2b)
noparity(`rocknei', 2d)
noparity(`rockswi', 4b)
noparity(`rocksei', 4d)

This is a helper for the completely flat terrains.
define(`flat',
`noparity(`$1nw',  $2)'
`noparity(`$1n',   $2)'
`noparity(`$1ne',  $2)'
`noparity(`$1w',   $2)'
`noparity(`$1',    $2)'
`noparity(`$1e',   $2)'
`noparity(`$1sw',  $2)'
`noparity(`$1s',   $2)'
`noparity(`$1se',  $2)'
`noparity(`$1nwi', $2)'
`noparity(`$1nei', $2)'
`noparity(`$1swi', $2)'
`noparity(`$1sei', $2)')

flat(`dirt',  05)
flat(`grass', 04)
flat(`water', 14)
flat(`sand',  06)

Trees are not flat, but they just have grass for all the border tiles.
We'll first use the flat terrain, then just override the center tiles.

flat(`tree', grassp1)

define(`treep1', 1e)
define(`treep2', 1f)
define(`treep3', 3e)
define(`treep4', 3f)

OK, now we have all our basic terrain tiles.

Remember: The parity rules for a block are *always* this:

  4343
  2121
  4343
  2121

Let's build some solid blocks first.

define(`solid', `
$1p4 $1p3 $1p4 $1p3
$1p2 $1p1 $1p2 $1p1
$1p4 $1p3 $1p4 $1p3
$1p2 $1p1 $1p2 $1p1')

That was easy. Now the interaction blocks.

Starting with vertical and horizontal. Northeastern terrain type is first.

define(`vertical', `
$1p4  $1p3  $1p4  $1p3
$1sp2 $1sp1 $1sp2 $1sp1
$2np4 $2np3 $2np4 $2np3
$2p2  $2p1  $2p2  $2p1')

define(`horizontal', `
$1p4 $1ep3 $2wp4 $2p3
$1p2 $1ep1 $2wp2 $2p1
$1p4 $1ep3 $2wp4 $2p3
$1p2 $1ep1 $2wp2 $2p1')

And now, the corners. Inner terrain type is first.

define(`cornernw', `
$1p4  $1ep3  $2wp4   $2p3
$1sp2 $1sep1 $2wp2   $2p1
$2np4 $2np3  $2nwip4 $2p3
$2p2  $2p1   $2p2    $2p1')

define(`cornerne', `
$2p4 $2ep3   $1wp4  $1p3
$2p2 $2ep1   $1swp2 $1sp1
$2p4 $2neip3 $2np4  $2np3
$2p2 $2p1    $2p2   $2p1')

define(`cornersw', `
$2p4  $2p3   $2p4    $2p3
$2sp2 $2sp1  $2swip2 $2p1
$1np4 $1nep3 $2wp4   $2p3
$1p2  $1ep1  $2wp2   $2p1')

define(`cornerse', `
$2p4 $2p3    $2p4   $2p3
$2p2 $2seip1 $2sp2  $2sp1
$2p4 $2ep3   $1nwp4 $1np3
$2p2 $2ep1   $1wp2  $1p1')

And diagonals. Slash type is first.

define(`diagonal', `
$2p4  $2ep3  $1wp4  $1p3
$2sp2 $2sep1 $1swp2 $1sp1
$1np4 $1nep3 $2nwp4 $2np3
$1p2  $1ep1  $2wp2  $2p1')

A helper for the binary interactions between two terrains.
define(`double', `
vertical($1,$2)
vertical($2,$1)
horizontal($1,$2)
horizontal($2,$1)
cornerne($1,$2)
cornerse($1,$2)
cornernw($1,$2)
cornersw($1,$2)
cornerne($2,$1)
cornerse($2,$1)
cornernw($2,$1)
cornersw($2,$1)
diagonal($1,$2)
diagonal($2,$1)')

There are a few special blocks. First, a cave entrance.
define(`cave', `
rockp4  rockp3  rockp4  rockp3
rocksp2 46      47      rocksp1
dirtnp4 dirtnp3 dirtnp4 dirtnp3
dirtp2  dirtp1  dirtp2  dirtp1')

And the various house bits.
define(`housenw', `
10 11 11 11
0a 0b 0b 0b
25 71 71 71
25 71 71 71')
define(`housene', `
11 11 11 12
0b 0b 0b 0c
71 71 71 22
71 71 71 22')
define(`housesw', `
25      71      71      71
25      71      6b      71
dirtnp4 dirtnp3 dirtnp4 dirtnp3
dirtp2  dirtp1  dirtp2  dirtp1')
define(`housese', `
71      71      71      22
71      71      71      22
dirtnp4 dirtnp3 dirtnp4 dirtnp3
dirtp2  dirtp1  dirtp2  dirtp1')

And, finally, a filler.
define(`nil', `
00 00 00 00
00 00 00 00
00 00 00 00
00 00 00 00')

That's it! Let's define the blockset!

divert(0)dnl
solid(rock) solid(dirt) solid(grass) solid(water) solid(sand) solid(tree) cave nil
nil         nil         nil          nil          nil         nil         nil  nil

double(rock,   dirt) nil nil
double(rock,  grass) nil nil
double(rock,  water) nil nil
double(rock,   sand) nil nil
double(rock,   tree) nil nil

double(dirt,  grass) nil nil
double(dirt,  water) nil nil
double(dirt,   sand) nil nil
double(dirt,   tree) nil nil

double(grass, water) nil nil
double(grass,  sand) nil nil
double(grass,  tree) nil nil

double(water,  sand) nil nil
double(water,  tree) housenw housene

double(sand,   tree) housesw housese
