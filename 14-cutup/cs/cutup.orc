sr = 48000
ksmps  = 1
nchnls = 2
0dbfs  = 1

  instr 1
idur      init p3
idbl      init p4
idbr      init p5
ipos      init p6
ifade_in  init p7
ifade_out init p8
itb       init p9

apos      init (ipos * sr)
asig      tab apos, itb, 0
ipeak     = idur - (ifade_in + ifade_out)
aenv      linseg -65, ifade_in, 0, ipeak, 0, ifade_out, -65
          outs asig*ampdb(aenv+idbl), asig*ampdb(aenv+idbr)

apos      = apos + 1
  endin
