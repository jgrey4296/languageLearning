
p1 = "<bd [bd bd] [bd bd]/2> <sd:2 [sd:2 sd:2]/2  hh [hh hh] [hh [hh hh]]>"
d1 $ sound p1

d2 $ gain "1 1 1 [1 [1 1]]" # up "[0, 3, 5]" # sound "arpy"

hush

d1 $ up "0 0*2 0*4 1" # sound "[hh <hh:2 hh:3> bd]"


x a = a * 2
x 2
do { d1 $ sound "hh"; d2 $ sound "bd"}
