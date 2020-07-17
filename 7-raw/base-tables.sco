f 1 0 4096 10 1 ; sine table for LFO

f 101 0 7 -2 \
	0 0 \ ; 0,1 l,r
	-50 \ ; 2, low
	-20 \ ; 3, high
	-10 \ ; 4 dry
	-5 \ ; 5 rev
	0 ; 6 clean

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


;; TODO - fix overrides causing blowout below
i "copytab" 0 0 101 102 \
	0 0 1 0 \
	2 -50 \
	3 -20 \
	4 -10 \
	5 -5 \
	6 0

i "debugtab" 0   0 102   1

i "copytab" 0 0 203 204 \
	0 200.1 \
	1 300.3

;; TODO - there's nice nuance lost by
;; the overrides from 302 here
i "copytab" 0 0 302 303 \
	6 0.9 \
	7 0.9

;i "debugtab" 140   0 102   1
;i "debugtab" 190   0 102   1
;i "debugtab" 240   0 102   1
