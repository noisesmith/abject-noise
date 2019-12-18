  sr         =  48000
  ksmps      =  512
  nchnls     =  2
  0dbfs      =  1

  gidb       = -6

;;; udos
    opcode lna, a,iiiii
  imn, imx, iin, iiout, idur \
    xin
  aout linen (imx-imn), idur*iin, idur, idur*iiout
       ; between multiplying by (mx-mn), and adding mn back to out,
       ; we effectively the linen output between mn and mx
    xout aout+imn
  endop

  instr 1
  adb     lna -65, 1, 0.1, 0.25, p3
  agl     = ampdbfs(adb+p4+gidb)
  agr     = ampdbfs(adb+p5+gidb)
  imod1   = p6
  icar1   = p7
  iidx    = p8
  icar    = p9
  iidx1   = p10
  igain   = p11
  amod    poscil3 iidx1, imod1
  a1      poscil3 iidx, icar1+amod
  aclean  poscil3 ampdb(igain), icar+a1, 1
  adist   limit aclean, -1, 1
  al      = adist*agl
  ar      = adist*agr
           outs al, ar
  endin

  instr 2
    gidb = p4
  endin
