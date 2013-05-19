divert(-1)

This file should be preprocessed with m4.

Use test.tiles.m4 to generate the graphical component of this blockset.

The various block types.

The solid terrains.
define(`grass', 0x00)
define(`dirt',  0x01)
define(`sand',  0x02)
define(`tree',  0x03)
define(`rock',  0x04)
define(`water', 0x05)

For terrain interactions, one terrain is "lower" and one is "higher".
This refers solely to the terrain numbers, and is only to disambiguate them.

The double-terrain transition offsets.
define(`grassdirt',  0x10)
define(`grasssand',  0x20)
define(`grasstree',  0x30)
define(`grassrock',  0x40)
define(`grasswater', 0x50)
define(`dirtsand',   0x60)
define(`dirttree',   0x70)
define(`dirtrock',   0x80)
define(`dirtwater',  0x90)
define(`sandtree',   0xa0)
define(`sandrock',   0xb0)
define(`sandwater',  0xc0)
define(`treerock',   0xd0)
define(`treewater',  0xe0)
define(`rockwater',  0xf0)

Double-terrain interactions.
define(`vertnorth', 0x00) Two terrains, vertical, lower on north.
define(`vertsouth', 0x01) Two terrains, vertical, lower on south.
define(`horizeast', 0x02) Two terrains, horizontal, lower on east.
define(`horizwest', 0x03) Two terrains, horizontal, lower on west.

define(`innerne',   0x04) Two terrains, inner corner, lower on northeast.
define(`innerse',   0x05) Two terrains, inner corner, lower on southeast.
define(`innernw',   0x06) Two terrains, inner corner, lower on northwest.
define(`innersw',   0x07) Two terrains, inner corner, lower on southwest.

define(`outerne',   0x08) Two terrains, outer corner, higher on northeast.
define(`outerse',   0x09) Two terrains, outer corner, higher on southeast.
define(`outernw',   0x0a) Two terrains, outer corner, higher on northwest.
define(`outersw',   0x0b) Two terrains, outer corner, higher on southwest.

define(`slash',     0x0c) Two terrains, diagonal, lower on northeast and southwest.
define(`backslash', 0x0d) Two terrains, diagonal, lower on southeast and northwest.

For double-terrain interactions, use offset+interaction.

This layout is reasonably organized, and gives us a complete set of two-terrain interaction blocks.
We have 40 blocks left over for special stuff, before we have to start sacrificing interactions.

I've allocated the corner down near 0xee for a 4-block house, and 0x06 as a cave, as examples of special blocks.
The unallocated space remaining on that map totals 35 blocks.

define(`house', 0xee)
define(`cave',  0x06)

Solid blocks.

define( `solid',
block index:`eval($1)' north:``$1'' south:``$1'' east:``$1'' west:``$1'')

Double-terrain interactions.
For these macros, I expect the lower terrain type to be the earlier parameter.

In this blockset, I use the following convention for edge names:
  If an edge is entirely one terrain, it is named for that terrain.
  If an edge is multiple terrains, they are concatenated in order:
    For vertical edges, from north to south.
    For horizontal edges, from west to east.

Horizontal/vertical interactions.

define( `verticals',
block index:`eval($1$2+vertnorth)' north:``$1'' south:``$2'' east:``$1$2'' west:``$1$2''
block index:`eval($1$2+vertsouth)' north:``$2'' south:``$1'' east:``$2$1'' west:``$2$1'')

define( `horizontals',
block index:`eval($1$2+horizeast)' north:``$2$1'' south:``$2$1'' east:``$1'' west:``$2''
block index:`eval($1$2+horizwest)' north:``$1$2'' south:``$1$2'' east:``$2'' west:``$1'')

Corners.

define( `innercorners',
block index:`eval($1$2+innerne)' north:``$2$1'' south:``$2'' east:``$1$2'' west:``$2''
block index:`eval($1$2+innerse)' north:``$2'' south:``$2$1'' east:``$2$1'' west:``$2''
block index:`eval($1$2+innernw)' north:``$1$2'' south:``$2'' east:``$2'' west:``$1$2''
block index:`eval($1$2+innersw)' north:``$2'' south:``$1$2'' east:``$2'' west:``$2$1'')

define( `outercorners',
block index:`eval($1$2+outerne)' north:``$1$2'' south:``$1'' east:``$2$1'' west:``$1''
block index:`eval($1$2+outerse)' north:``$1'' south:``$1$2'' east:``$1$2'' west:``$1''
block index:`eval($1$2+outernw)' north:``$2$1'' south:``$1'' east:``$1'' west:``$2$1''
block index:`eval($1$2+outersw)' north:``$1'' south:``$2$1'' east:``$1'' west:``$1$2'')

Diagonals.

define( `diagonals',
block index:`eval($1$2+slash)' north:``$2$1'' south:``$1$2'' east:``$1$2'' west:``$2$1''
block index:`eval($1$2+backslash)' north:``$1$2'' south:``$2$1'' east:``$2$1'' west:``$1$2'')

All the double transitions.

define( `double',
`horizontals(`$1',`$2')'
`verticals(`$1',`$2')'
`innercorners(`$1',`$2')'
`outercorners(`$1',`$2')'
`diagonals(`$1',`$2')')

This is a helper for 4-block specials. The second parameter defines what terrain the special is embedded in.

define( `special',
block index:`eval($1+0x00)' north:``$2'' south:``$1west'' east:``$1north'' west:``$2''
block index:`eval($1+0x01)' north:``$2'' south:``$1east'' east:``$2'' west:``$1north''
block index:`eval($1+0x10)' north:``$1west'' south:``$2'' east:``$1south'' west:``$2''
block index:`eval($1+0x11)' north:``$1east'' south:``$2'' east:``$2'' west:``$1south'')

I didn't bother with a helper for the 1-block cave special; it was simpler to just directly code it.

Now for the actual block definitions.

divert(0)dnl
solid(`grass')
solid(`dirt')
solid(`sand')
solid(`tree')
solid(`rock')
solid(`water')

double(`grass', `dirt')
double(`grass', `sand')
double(`grass', `tree')
double(`grass', `rock')
double(`grass', `water')

double(`dirt',  `sand')
double(`dirt',  `tree')
double(`dirt',  `rock')
double(`dirt',  `water')

double(`sand',  `tree')
double(`sand',  `rock')
double(`sand',  `water')

double(`tree',  `rock')
double(`tree',  `water')

double(`rock',  `water')

special(`house', `dirt')

block index:eval(cave) north:`rock' south:`dirt' east:`rockdirt' west:`rockdirt'
