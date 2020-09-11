f 1 0 4096 10 1 ; sine table for LFO

f 101 0 7 -2 \
	0 0 \ ; l,r
	-50 -20 -10 -5 0; low,high,dry,rev,clean

f 201 0 8 -2 \ ; squeal!
	1000.1 3000.3 \ ; hz
	10 10 \ ; gain
	80 10  5000 10 ; low, high

f 301 0 8 -2 \
	0 0 0 0 0.3 0.3 \ ; gendy weird
	0.01 0.01 ; dist weird

f 202 0 8 -2 \ ; rumble
	0.09 30.1 \
	0 15 \
	30 20 1000 3

f 203 0 8 -2 \ ; bleat
	0.09 300.1 \
	10 15 \
	90 20 4500 40

f 302 0 8 -2 \
	5 5 0.7 0.8 0.3 0.9 \ ; gendy weird
	0.01 0.01 ; dist weird


i "copytab" 0 0 101 102 \
;	0 -105 1 -105 \
;	4  -10 5 -10 6 -20
i "copytab" 0 0 203 204 \
;	0 200.1 \
;	1 300.3
i "copytab" 0 0 302 303 \
;	6 0.9 \
;	7 0.9

;i "debugtab" 140   0 102   1
;i "debugtab" 190   0 102   1
;i "debugtab" 240   0 102   1
