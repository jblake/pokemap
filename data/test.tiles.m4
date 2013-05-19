divert(-1)

This file should be preprocessed with m4.

This is in relation to stock tileset 1.

Note that this blockset is *really* ugly; this is just so you can get an idea
of what different block shapes are useful for map generation. Any blockset that
you want to really *use* should be setup by hand, because the technique I use
here of automatically creating blocks leads to extremely boring and glitchy
blocksets. You should also almost certainly *not* provide the complete set of
terrain interactions as I do here; maps will look better if they are more
restricted, because to do otherwise leads to a sort of confused jumble.

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
noparity(`rocknwi', 4d)
noparity(`rocknei', 4b)
noparity(`rockswi', 2d)
noparity(`rocksei', 2b)

Water is a little weird because we use rock boundaries.
noparity(`waternw',  rockseip1)
noparity(`watern',   rocksp1)
noparity(`waterne',  rockswip1)
noparity(`waterw',   rockep1)
noparity(`water',    14)
noparity(`watere',   rockwp1)
noparity(`watersw',  rockneip1)
noparity(`waters',   rocknp1)
noparity(`waterse',  rocknwip1)
noparity(`waternwi', rocksep1)
noparity(`waternei', rockswp1)
noparity(`waterswi', rocknep1)
noparity(`watersei', rocknwp1)

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
flat(`sand',  06)
flat(`grass', 04)

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
$2p4 $2ep3 $1wp4 $1p3
$2p2 $2ep1 $1wp2 $1p1
$2p4 $2ep3 $1wp4 $1p3
$2p2 $2ep1 $1wp2 $1p1')

And now, the corners. Dominant terrain type is first.

Inner corners *don't* get a "leaky" edge.

define(`icornerse', `
$2p4 $2p3    $2p4   $2p3
$2p2 $2seip1 $2sp2  $2sp1
$2p4 $2ep3   $1nwp4 $1np3
$2p2 $2ep1   $1wp2  $1p1')

define(`icornersw', `
$2p4  $2p3   $2p4    $2p3
$2sp2 $2sp1  $2swip2 $2p1
$1np4 $1nep3 $2wp4   $2p3
$1p2  $1ep1  $2wp2   $2p1')

define(`icornernw', `
$1p4  $1ep3  $2wp4   $2p3
$1sp2 $1sep1 $2wp2   $2p1
$2np4 $2np3  $2nwip4 $2p3
$2p2  $2p1   $2p2    $2p1')

define(`icornerne', `
$2p4 $2ep3   $1wp4  $1p3
$2p2 $2ep1   $1swp2 $1sp1
$2p4 $2neip3 $2np4  $2np3
$2p2 $2p1    $2p2   $2p1')

But outer corners *do* get a "leaky" edge.

define(`ocornernw', `
$2seip4 $2sep3  $1wp4   $1p3
$2sep2  $1nwp1  $1nwip2 $1p1
$1np4   $1nwip3 $1p4    $1p3
$1p2    $1p1    $1p2    $1p1')

define(`ocornersw', `
$1p4    $1p3    $1p4    $1p3
$1sp2   $1swip1 $1p2    $1p1
$2nep4  $1swp3  $1swip4 $1p3
$2neip2 $2nep1  $1wp2   $1p1')

define(`ocornerse', `
$1p4 $1p3    $1p4    $1p3
$1p2 $1p1    $1seip2 $1sp1
$1p4 $1seip3 $1sep4  $2nwp3
$1p2 $1ep1   $2nwp2  $2nwip1')

define(`ocornerne', `
$1p4 $1ep3   $2swp4  $2swip3
$1p2 $1neip1 $1nep2  $2swp1
$1p4 $1p3    $1neip4 $1np3
$1p2 $1p1    $1p2    $1p1')

And diagonals. Dominant terrain type is first.

define(`slash', `
$2seip4 $2sep3  $1wp4  $1p3
$2sep2  $1nwp1  $1p2   $1sp1
$1np4   $1p3    $1sep4 $2nwp3
$1p2    $1ep1   $2nwp2 $2nwip1')

define(`backslash', `
$1p4    $1ep3  $2swp4 $2swip3
$1sp2   $1p1   $1nep2 $2swp1
$2nep4  $1swp3 $1p4   $1np3
$2neip2 $2nep1 $1wp2  $1p1')

A helper for the binary interactions between two terrains.
Note that the dominant terrain should be first.
define(`double', `
vertical($1,$2)
vertical($2,$1)
horizontal($1,$2)
horizontal($2,$1)
icornerne($1,$2)
icornerse($1,$2)
icornernw($1,$2)
icornersw($1,$2)
ocornerne($1,$2)
ocornerse($1,$2)
ocornernw($1,$2)
ocornersw($1,$2)
slash($1,$2)
backslash($1,$2)')

There are a few special blocks. First, a cave entrance.
define(`cave', `
rockp4  46      47      rockp3
rocksp2 56      57      rocksp1
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

Just a reminder of the dominance order:
  grass > dirt > sand > tree > rock > water

divert(0)dnl
solid(grass) solid(dirt) solid(sand) solid(tree) solid(rock) solid(water) cave nil
nil          nil         nil         nil         nil         nil          nil  nil

double(grass,  dirt) nil nil
double(grass,  sand) nil nil
double(grass,  tree) nil nil
double(grass,  rock) nil nil
double(grass, water) nil nil

double(dirt,   sand) nil nil
double(dirt,   tree) nil nil
double(dirt,   rock) nil nil
double(dirt,  water) nil nil

double(sand,   tree) nil nil
double(sand,   rock) nil nil
double(sand,  water) nil nil

double(tree,   rock) nil nil
double(tree,  water) housenw housene

double(rock,  water) housesw housese
