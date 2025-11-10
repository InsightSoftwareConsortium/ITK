*     fortran version of *dgpfa3* -
*     radix-3 section of self-sorting, in-place
*        generalized PFA
*
*-------------------------------------------------------------------
*
      subroutine dgpfa3f(a,b,trigs,inc,jump,n,mm,lot,isign)
      double precision a(*), b(*), trigs(*)
      integer inc, jump, n, mm, lot, isign
      double precision s, c1, t1, t2, t3, u1, u2, u3, co1, co2
      double precision si1, si2, aja, ajb, ajc, bjb, bjc, bja, ajd, bjd
      double precision aje, ajf, ajh, bje, bjf, bjh, aji, ajg, bji, bjg
      data sin60/0.866025403784437d+0/
      data lvr/128/

*
*     ***************************************************************
*     *                                                             *
*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
*     *                                                             *
*     ***************************************************************
*
      n3 = 3**mm
      inq = n/n3
      jstepx = (n3-n) * inc
      ninc = n * inc
      ink = inc * inq
      mu = mod(inq,3)
      if (isign.eq.-1) mu = 3-mu
      m = mm
      mh = (m+1)/2
      s = dfloat(isign)
      c1 = sin60
      if (mu.eq.2) c1 = -c1
*
      nblox = 1 + (lot-1)/lvr
      left = lot
      s = dfloat(isign)
      istart = 1
*
*  loop on blocks of lvr transforms
*  --------------------------------
      do 500 nb = 1 , nblox
*
      if (left.le.lvr) then
         nvex = left
      else if (left.lt.(2*lvr)) then
         nvex = left/2
         nvex = nvex + mod(nvex,2)
      else
         nvex = lvr
      endif
      left = left - nvex
*
      la = 1
*
*  loop on type I radix-3 passes
*  -----------------------------
      do 160 ipass = 1 , mh
      jstep = (n*inc) / (3*la)
      jstepl = jstep - ninc
*
*  k = 0 loop (no twiddle factors)
*  -------------------------------
      do 120 jjj = 0 , (n-1)*inc , 3*jstep
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 115 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 110 l = 1 , nvex
      ajb = a(jb+j)
      ajc = a(jc+j)
      t1 = ajb + ajc
      aja = a(ja+j)
      t2 = aja - 0.5 * t1
      t3 = c1 * ( ajb - ajc )
      bjb = b(jb+j)
      bjc = b(jc+j)
      u1 = bjb + bjc
      bja = b(ja+j)
      u2 = bja - 0.5 * u1
      u3 = c1 * ( bjb - bjc )
      a(ja+j) = aja + t1
      b(ja+j) = bja + u1
      a(jb+j) = t2 - u3
      b(jb+j) = u2 + t3
      a(jc+j) = t2 + u3
      b(jc+j) = u2 - t3
      j = j + jump
  110 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  115 continue
  120 continue
*
*  finished if n3 = 3
*  ------------------
      if (n3.eq.3) go to 490
      kk = 2 * la
*
*  loop on nonzero k
*  -----------------
      do 150 k = ink , jstep-ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
*
*  loop along transform
*  --------------------
      do 140 jjj = k , (n-1)*inc , 3*jstep
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 135 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep,shortloop
      do 130 l = 1 , nvex
      ajb = a(jb+j)
      ajc = a(jc+j)
      t1 = ajb + ajc
      aja = a(ja+j)
      t2 = aja - 0.5 * t1
      t3 = c1 * ( ajb - ajc )
      bjb = b(jb+j)
      bjc = b(jc+j)
      u1 = bjb + bjc
      bja = b(ja+j)
      u2 = bja - 0.5 * u1
      u3 = c1 * ( bjb - bjc )
      a(ja+j) = aja + t1
      b(ja+j) = bja + u1
      a(jb+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jb+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jc+j) = co2*(t2+u3) - si2*(u2-t3)
      b(jc+j) = si2*(t2+u3) + co2*(u2-t3)
      j = j + jump
  130 continue
*-----( end of loop across transforms )
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  135 continue
  140 continue
*-----( end of loop along transforms )
      kk = kk + 2*la
  150 continue
*-----( end of loop on nonzero k )
      la = 3*la
  160 continue
*-----( end of loop on type I radix-3 passes)
*
*  loop on type II radix-3 passes
*  ------------------------------
  400 continue
*
      do 480 ipass = mh+1 , m
      jstep = (n*inc) / (3*la)
      jstepl = jstep - ninc
      laincl = la*ink - ninc
*
*  k=0 loop (no twiddle factors)
*  -----------------------------
      do 430 ll = 0 , (la-1)*ink , 3*jstep
*
      do 420 jjj = ll , (n-1)*inc , 3*la*ink
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 415 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = ja + laincl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jd + laincl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = jh + jstepl
      if (ji.lt.istart) ji = ji + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 410 l = 1 , nvex
      ajb = a(jb+j)
      ajc = a(jc+j)
      t1 = ajb + ajc
      aja = a(ja+j)
      t2 = aja - 0.5 * t1
      t3 = c1 * ( ajb - ajc )
      ajd = a(jd+j)
      ajb =  ajd
      bjb = b(jb+j)
      bjc = b(jc+j)
      u1 = bjb + bjc
      bja = b(ja+j)
      u2 = bja - 0.5 * u1
      u3 = c1 * ( bjb - bjc )
      bjd = b(jd+j)
      bjb =  bjd
      a(ja+j) = aja + t1
      b(ja+j) = bja + u1
      a(jd+j) = t2 - u3
      b(jd+j) = u2 + t3
      ajc =  t2 + u3
      bjc =  u2 - t3
*----------------------
      aje = a(je+j)
      ajf = a(jf+j)
      t1 = aje + ajf
      t2 = ajb - 0.5 * t1
      t3 = c1 * ( aje - ajf )
      ajh = a(jh+j)
      ajf =  ajh
      bje = b(je+j)
      bjf = b(jf+j)
      u1 = bje + bjf
      u2 = bjb - 0.5 * u1
      u3 = c1 * ( bje - bjf )
      bjh = b(jh+j)
      bjf =  bjh
      a(jb+j) = ajb + t1
      b(jb+j) = bjb + u1
      a(je+j) = t2 - u3
      b(je+j) = u2 + t3
      a(jh+j) = t2 + u3
      b(jh+j) = u2 - t3
*----------------------
      aji = a(ji+j)
      t1 = ajf + aji
      ajg = a(jg+j)
      t2 = ajg - 0.5 * t1
      t3 = c1 * ( ajf - aji )
      t1 = ajg + t1
      a(jg+j) = ajc
      bji = b(ji+j)
      u1 = bjf + bji
      bjg = b(jg+j)
      u2 = bjg - 0.5 * u1
      u3 = c1 * ( bjf - bji )
      u1 = bjg + u1
      b(jg+j) = bjc
      a(jc+j) = t1
      b(jc+j) = u1
      a(jf+j) = t2 - u3
      b(jf+j) = u2 + t3
      a(ji+j) = t2 + u3
      b(ji+j) = u2 - t3
      j = j + jump
  410 continue
*-----( end of loop across transforms )
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  415 continue
  420 continue
  430 continue
*-----( end of double loop for k=0 )
*
*  finished if last pass
*  ---------------------
      if (ipass.eq.m) go to 490
*
      kk = 2*la
*
*     loop on nonzero k
*     -----------------
      do 470 k = ink , jstep-ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
*
*  double loop along first transform in block
*  ------------------------------------------
      do 460 ll = k , (la-1)*ink , 3*jstep
*
      do 450 jjj = ll , (n-1)*inc , 3*la*ink
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 445 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = ja + laincl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jd + laincl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = jh + jstepl
      if (ji.lt.istart) ji = ji + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 440 l = 1 , nvex
      ajb = a(jb+j)
      ajc = a(jc+j)
      t1 = ajb + ajc
      aja = a(ja+j)
      t2 = aja - 0.5 * t1
      t3 = c1 * ( ajb - ajc )
      ajd = a(jd+j)
      ajb =  ajd
      bjb = b(jb+j)
      bjc = b(jc+j)
      u1 = bjb + bjc
      bja = b(ja+j)
      u2 = bja - 0.5 * u1
      u3 = c1 * ( bjb - bjc )
      bjd = b(jd+j)
      bjb =  bjd
      a(ja+j) = aja + t1
      b(ja+j) = bja + u1
      a(jd+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jd+j) = si1*(t2-u3) + co1*(u2+t3)
      ajc =  co2*(t2+u3) - si2*(u2-t3)
      bjc =  si2*(t2+u3) + co2*(u2-t3)
*----------------------
      aje = a(je+j)
      ajf = a(jf+j)
      t1 = aje + ajf
      t2 = ajb - 0.5 * t1
      t3 = c1 * ( aje - ajf )
      ajh = a(jh+j)
      ajf =  ajh
      bje = b(je+j)
      bjf = b(jf+j)
      u1 = bje + bjf
      u2 = bjb - 0.5 * u1
      u3 = c1 * ( bje - bjf )
      bjh = b(jh+j)
      bjf =  bjh
      a(jb+j) = ajb + t1
      b(jb+j) = bjb + u1
      a(je+j) = co1*(t2-u3) - si1*(u2+t3)
      b(je+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jh+j) = co2*(t2+u3) - si2*(u2-t3)
      b(jh+j) = si2*(t2+u3) + co2*(u2-t3)
*----------------------
      aji = a(ji+j)
      t1 = ajf + aji
      ajg = a(jg+j)
      t2 = ajg - 0.5 * t1
      t3 = c1 * ( ajf - aji )
      t1 = ajg + t1
      a(jg+j) = ajc
      bji = b(ji+j)
      u1 = bjf + bji
      bjg = b(jg+j)
      u2 = bjg - 0.5 * u1
      u3 = c1 * ( bjf - bji )
      u1 = bjg + u1
      b(jg+j) = bjc
      a(jc+j) = t1
      b(jc+j) = u1
      a(jf+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jf+j) = si1*(t2-u3) + co1*(u2+t3)
      a(ji+j) = co2*(t2+u3) - si2*(u2-t3)
      b(ji+j) = si2*(t2+u3) + co2*(u2-t3)
      j = j + jump
  440 continue
*-----(end of loop across transforms)
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  445 continue
  450 continue
  460 continue
*-----( end of double loop for this k )
      kk = kk + 2*la
  470 continue
*-----( end of loop over values of k )
      la = 3*la
  480 continue
*-----( end of loop on type II radix-3 passes )
*-----( nvex transforms completed)
  490 continue
      istart = istart + nvex * jump
  500 continue
*-----( end of loop on blocks of transforms )
*
      return
      end
