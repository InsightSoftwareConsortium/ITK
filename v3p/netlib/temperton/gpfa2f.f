*     fortran version of *gpfa2* -
*     radix-2 section of self-sorting, in-place, generalized pfa
*     central radix-2 and radix-8 passes included
*      so that transform length can be any power of 2
*
*-------------------------------------------------------------------
*
      subroutine gpfa2f(a,b,trigs,inc,jump,n,mm,lot,isign)
      real a(*), b(*), trigs(*)
      integer inc, jump, n, mm, lot, isign
      real s, ss, c1, c2, c3, t0, t1, t2, t3, u0, u1, u2, u3
      real co1, co2, co3, co4, co5, co6, co7
      real si1, si2, si3, si4, si5, si6, si7
      real aja, ajb, ajc, ajd, bja, bjc, bjb, bjd
      real aje, ajg, ajf, ajh, bje, bjg, bjf, bjh, aji
      real bjm, ajj, bjj, ajk, ajl, bji, bjk
      real ajo, bjl, bjo, ajm, ajn, ajp, bjn, bjp
      data lvr/1024/

*
*     ***************************************************************
*     *                                                             *
*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
*     *                                                             *
*     ***************************************************************
*
      n2 = 2**mm
      inq = n/n2
      jstepx = (n2-n) * inc
      ninc = n * inc
      ink = inc * inq
*
      m2 = 0
      m8 = 0
      if (mod(mm,2).eq.0) then
         m = mm/2
      else if (mod(mm,4).eq.1) then
         m = (mm-1)/2
         m2 = 1
      else if (mod(mm,4).eq.3) then
         m = (mm-3)/2
         m8 = 1
      endif
      mh = (m+1)/2
*
      nblox = 1 + (lot-1)/lvr
      left = lot
      s = float(isign)
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
*  loop on type I radix-4 passes
*  -----------------------------
      mu = mod(inq,4)
      if (isign.eq.-1) mu = 4 - mu
      ss = 1.0
      if (mu.eq.3) ss = -1.0
*
      if (mh.eq.0) go to 200
*
      do 160 ipass = 1 , mh
      jstep = (n*inc) / (4*la)
      jstepl = jstep - ninc
*
*  k = 0 loop (no twiddle factors)
*  -------------------------------
      do 120 jjj = 0 , (n-1)*inc , 4*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 115 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 110 l = 1 , nvex
      aja = a(ja+j)
      ajc = a(jc+j)
      t0 = aja + ajc
      t2 = aja - ajc
      ajb = a(jb+j)
      ajd = a(jd+j)
      t1 = ajb + ajd
      t3 = ss * ( ajb - ajd )
      bja = b(ja+j)
      bjc = b(jc+j)
      u0 = bja + bjc
      u2 = bja - bjc
      bjb = b(jb+j)
      bjd = b(jd+j)
      u1 = bjb + bjd
      u3 = ss * ( bjb - bjd )
      a(ja+j) = t0 + t1
      a(jc+j) = t0 - t1
      b(ja+j) = u0 + u1
      b(jc+j) = u0 - u1
      a(jb+j) = t2 - u3
      a(jd+j) = t2 + u3
      b(jb+j) = u2 + t3
      b(jd+j) = u2 - t3
      j = j + jump
  110 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  115 continue
  120 continue
*
*  finished if n2 = 4
*  ------------------
      if (n2.eq.4) go to 490
      kk = 2 * la
*
*  loop on nonzero k
*  -----------------
      do 150 k = ink , jstep-ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
      co3 = trigs(3*kk+1)
      si3 = s*trigs(3*kk+2)
*
*  loop along transform
*  --------------------
      do 140 jjj = k , (n-1)*inc , 4*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 135 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep,shortloop
      do 130 l = 1 , nvex
      aja = a(ja+j)
      ajc = a(jc+j)
      t0 = aja + ajc
      t2 = aja - ajc
      ajb = a(jb+j)
      ajd = a(jd+j)
      t1 = ajb + ajd
      t3 = ss * ( ajb - ajd )
      bja = b(ja+j)
      bjc = b(jc+j)
      u0 = bja + bjc
      u2 = bja - bjc
      bjb = b(jb+j)
      bjd = b(jd+j)
      u1 = bjb + bjd
      u3 = ss * ( bjb - bjd )
      a(ja+j) = t0 + t1
      b(ja+j) = u0 + u1
      a(jb+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jb+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jc+j) = co2*(t0-t1) - si2*(u0-u1)
      b(jc+j) = si2*(t0-t1) + co2*(u0-u1)
      a(jd+j) = co3*(t2+u3) - si3*(u2-t3)
      b(jd+j) = si3*(t2+u3) + co3*(u2-t3)
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
      la = 4*la
  160 continue
*-----( end of loop on type I radix-4 passes)
*
*  central radix-2 pass
*  --------------------
  200 continue
      if (m2.eq.0) go to 300
*
      jstep = (n*inc) / (2*la)
      jstepl = jstep - ninc
*
*  k=0 loop (no twiddle factors)
*  -----------------------------
      do 220 jjj = 0 , (n-1)*inc , 2*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 215 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 210 l = 1 , nvex
      aja = a(ja+j)
      ajb = a(jb+j)
      t0 = aja - ajb
      a(ja+j) = aja + ajb
      a(jb+j) = t0
      bja = b(ja+j)
      bjb = b(jb+j)
      u0 = bja - bjb
      b(ja+j) = bja + bjb
      b(jb+j) = u0
      j = j + jump
  210 continue
*-----(end of loop across transforms)
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  215 continue
  220 continue
*
*  finished if n2=2
*  ----------------
      if (n2.eq.2) go to 490
*
      kk = 2 * la
*
*  loop on nonzero k
*  -----------------
      do 260 k = ink , jstep - ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
*
*  loop along transforms
*  ---------------------
      do 250 jjj = k , (n-1)*inc , 2*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 245 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
      if (kk.eq.n2/2) then
cdir$ ivdep, shortloop
      do 230 l = 1 , nvex
      aja = a(ja+j)
      ajb = a(jb+j)
      t0 = ss * ( aja - ajb )
      a(ja+j) = aja + ajb
      bjb = b(jb+j)
      bja = b(ja+j)
      a(jb+j) = ss * ( bjb - bja )
      b(ja+j) = bja + bjb
      b(jb+j) = t0
      j = j + jump
  230 continue
*
      else
*
cdir$ ivdep, shortloop
      do 240 l = 1 , nvex
      aja = a(ja+j)
      ajb = a(jb+j)
      t0 = aja - ajb
      a(ja+j) = aja + ajb
      bja = b(ja+j)
      bjb = b(jb+j)
      u0 = bja - bjb
      b(ja+j) = bja + bjb
      a(jb+j) = co1*t0 - si1*u0
      b(jb+j) = si1*t0 + co1*u0
      j = j + jump
  240 continue
*
      endif
*
*-----(end of loop across transforms)
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  245 continue
  250 continue
*-----(end of loop along transforms)
      kk = kk + 2 * la
  260 continue
*-----(end of loop on nonzero k)
*-----(end of radix-2 pass)
*
      la = 2 * la
      go to 400
*
*  central radix-8 pass
*  --------------------
  300 continue
      if (m8.eq.0) go to 400
      jstep = (n*inc) / (8*la)
      jstepl = jstep - ninc
      mu = mod(inq,8)
      if (isign.eq.-1) mu = 8 - mu
      c1 = 1.0
      if (mu.eq.3.or.mu.eq.7) c1 = -1.0
      c2 = sqrt(0.5)
      if (mu.eq.3.or.mu.eq.5) c2 = -c2
      c3 = c1 * c2
*
*  stage 1
*  -------
      do 320 k = 0 , jstep - ink , ink
      do 315 jjj = k , (n-1)*inc , 8*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 312 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      j = 0
cdir$ ivdep, shortloop
      do 310 l = 1 , nvex
      aja = a(ja+j)
      aje = a(je+j)
      t0 = aja - aje
      a(ja+j) = aja + aje
      ajc = a(jc+j)
      ajg = a(jg+j)
      t1 = c1 * ( ajc - ajg )
      a(je+j) = ajc + ajg
      ajb = a(jb+j)
      ajf = a(jf+j)
      t2 = ajb - ajf
      a(jc+j) = ajb + ajf
      ajd = a(jd+j)
      ajh = a(jh+j)
      t3 = ajd - ajh
      a(jg+j) = ajd + ajh
      a(jb+j) = t0
      a(jf+j) = t1
      a(jd+j) = c2 * ( t2 - t3 )
      a(jh+j) = c3 * ( t2 + t3 )
      bja = b(ja+j)
      bje = b(je+j)
      u0 = bja - bje
      b(ja+j) = bja + bje
      bjc = b(jc+j)
      bjg = b(jg+j)
      u1 = c1 * ( bjc - bjg )
      b(je+j) = bjc + bjg
      bjb = b(jb+j)
      bjf = b(jf+j)
      u2 = bjb - bjf
      b(jc+j) = bjb + bjf
      bjd = b(jd+j)
      bjh = b(jh+j)
      u3 = bjd - bjh
      b(jg+j) = bjd + bjh
      b(jb+j) = u0
      b(jf+j) = u1
      b(jd+j) = c2 * ( u2 - u3 )
      b(jh+j) = c3 * ( u2 + u3 )
      j = j + jump
  310 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  312 continue
  315 continue
  320 continue
*
*  stage 2
*  -------
*
*  k=0 (no twiddle factors)
*  ------------------------
      do 330 jjj = 0 , (n-1)*inc , 8*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 328 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      j = 0
cdir$ ivdep, shortloop
      do 325 l = 1 , nvex
      aja = a(ja+j)
      aje = a(je+j)
      t0 = aja + aje
      t2 = aja - aje
      ajc = a(jc+j)
      ajg = a(jg+j)
      t1 = ajc + ajg
      t3 = c1 * ( ajc - ajg )
      bja = b(ja+j)
      bje = b(je+j)
      u0 = bja + bje
      u2 = bja - bje
      bjc = b(jc+j)
      bjg = b(jg+j)
      u1 = bjc + bjg
      u3 = c1 * ( bjc - bjg )
      a(ja+j) = t0 + t1
      a(je+j) = t0 - t1
      b(ja+j) = u0 + u1
      b(je+j) = u0 - u1
      a(jc+j) = t2 - u3
      a(jg+j) = t2 + u3
      b(jc+j) = u2 + t3
      b(jg+j) = u2 - t3
      ajb = a(jb+j)
      ajd = a(jd+j)
      t0 = ajb + ajd
      t2 = ajb - ajd
      ajf = a(jf+j)
      ajh = a(jh+j)
      t1 = ajf - ajh
      t3 = ajf + ajh
      bjb = b(jb+j)
      bjd = b(jd+j)
      u0 = bjb + bjd
      u2 = bjb - bjd
      bjf = b(jf+j)
      bjh = b(jh+j)
      u1 = bjf - bjh
      u3 = bjf + bjh
      a(jb+j) = t0 - u3
      a(jh+j) = t0 + u3
      b(jb+j) = u0 + t3
      b(jh+j) = u0 - t3
      a(jd+j) = t2 + u1
      a(jf+j) = t2 - u1
      b(jd+j) = u2 - t1
      b(jf+j) = u2 + t1
      j = j + jump
  325 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  328 continue
  330 continue
*
      if (n2.eq.8) go to 490
*
*  loop on nonzero k
*  -----------------
      kk = 2 * la
*
      do 350 k = ink , jstep - ink , ink
*
      co1 = trigs(kk+1)
      si1 = s * trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s * trigs(2*kk+2)
      co3 = trigs(3*kk+1)
      si3 = s * trigs(3*kk+2)
      co4 = trigs(4*kk+1)
      si4 = s * trigs(4*kk+2)
      co5 = trigs(5*kk+1)
      si5 = s * trigs(5*kk+2)
      co6 = trigs(6*kk+1)
      si6 = s * trigs(6*kk+2)
      co7 = trigs(7*kk+1)
      si7 = s * trigs(7*kk+2)
*
      do 345 jjj = k , (n-1)*inc , 8*jstep
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 342 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      j = 0
cdir$ ivdep, shortloop
      do 340 l = 1 , nvex
      aja = a(ja+j)
      aje = a(je+j)
      t0 = aja + aje
      t2 = aja - aje
      ajc = a(jc+j)
      ajg = a(jg+j)
      t1 = ajc + ajg
      t3 = c1 * ( ajc - ajg )
      bja = b(ja+j)
      bje = b(je+j)
      u0 = bja + bje
      u2 = bja - bje
      bjc = b(jc+j)
      bjg = b(jg+j)
      u1 = bjc + bjg
      u3 = c1 * ( bjc - bjg )
      a(ja+j) = t0 + t1
      b(ja+j) = u0 + u1
      a(je+j) = co4*(t0-t1) - si4*(u0-u1)
      b(je+j) = si4*(t0-t1) + co4*(u0-u1)
      a(jc+j) = co2*(t2-u3) - si2*(u2+t3)
      b(jc+j) = si2*(t2-u3) + co2*(u2+t3)
      a(jg+j) = co6*(t2+u3) - si6*(u2-t3)
      b(jg+j) = si6*(t2+u3) + co6*(u2-t3)
      ajb = a(jb+j)
      ajd = a(jd+j)
      t0 = ajb + ajd
      t2 = ajb - ajd
      ajf = a(jf+j)
      ajh = a(jh+j)
      t1 = ajf - ajh
      t3 = ajf + ajh
      bjb = b(jb+j)
      bjd = b(jd+j)
      u0 = bjb + bjd
      u2 = bjb - bjd
      bjf = b(jf+j)
      bjh = b(jh+j)
      u1 = bjf - bjh
      u3 = bjf + bjh
      a(jb+j) = co1*(t0-u3) - si1*(u0+t3)
      b(jb+j) = si1*(t0-u3) + co1*(u0+t3)
      a(jh+j) = co7*(t0+u3) - si7*(u0-t3)
      b(jh+j) = si7*(t0+u3) + co7*(u0-t3)
      a(jd+j) = co3*(t2+u1) - si3*(u2-t1)
      b(jd+j) = si3*(t2+u1) + co3*(u2-t1)
      a(jf+j) = co5*(t2-u1) - si5*(u2+t1)
      b(jf+j) = si5*(t2-u1) + co5*(u2+t1)
      j = j + jump
  340 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  342 continue
  345 continue
      kk = kk + 2 * la
  350 continue
*
      la = 8 * la
*
*  loop on type II radix-4 passes
*  ------------------------------
  400 continue
      mu = mod(inq,4)
      if (isign.eq.-1) mu = 4 - mu
      ss = 1.0
      if (mu.eq.3) ss = -1.0
*
      do 480 ipass = mh+1 , m
      jstep = (n*inc) / (4*la)
      jstepl = jstep - ninc
      laincl = la * ink - ninc
*
*  k=0 loop (no twiddle factors)
*  -----------------------------
      do 430 ll = 0 , (la-1)*ink , 4*jstep
*
      do 420 jjj = ll , (n-1)*inc , 4*la*ink
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 415 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      je = ja + laincl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = je + laincl
      if (ji.lt.istart) ji = ji + ninc
      jj = ji + jstepl
      if (jj.lt.istart) jj = jj + ninc
      jk = jj + jstepl
      if (jk.lt.istart) jk = jk + ninc
      jl = jk + jstepl
      if (jl.lt.istart) jl = jl + ninc
      jm = ji + laincl
      if (jm.lt.istart) jm = jm + ninc
      jn = jm + jstepl
      if (jn.lt.istart) jn = jn + ninc
      jo = jn + jstepl
      if (jo.lt.istart) jo = jo + ninc
      jp = jo + jstepl
      if (jp.lt.istart) jp = jp + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 410 l = 1 , nvex
      aja = a(ja+j)
      ajc = a(jc+j)
      t0 = aja + ajc
      t2 = aja - ajc
      ajb = a(jb+j)
      ajd = a(jd+j)
      t1 = ajb + ajd
      t3 = ss * ( ajb - ajd )
      aji = a(ji+j)
      ajc =  aji
      bja = b(ja+j)
      bjc = b(jc+j)
      u0 = bja + bjc
      u2 = bja - bjc
      bjb = b(jb+j)
      bjd = b(jd+j)
      u1 = bjb + bjd
      u3 = ss * ( bjb - bjd )
      aje = a(je+j)
      ajb =  aje
      a(ja+j) = t0 + t1
      a(ji+j) = t0 - t1
      b(ja+j) = u0 + u1
      bjc =  u0 - u1
      bjm = b(jm+j)
      bjd =  bjm
      a(je+j) = t2 - u3
      ajd =  t2 + u3
      bjb =  u2 + t3
      b(jm+j) = u2 - t3
*----------------------
      ajg = a(jg+j)
      t0 = ajb + ajg
      t2 = ajb - ajg
      ajf = a(jf+j)
      ajh = a(jh+j)
      t1 = ajf + ajh
      t3 = ss * ( ajf - ajh )
      ajj = a(jj+j)
      ajg =  ajj
      bje = b(je+j)
      bjg = b(jg+j)
      u0 = bje + bjg
      u2 = bje - bjg
      bjf = b(jf+j)
      bjh = b(jh+j)
      u1 = bjf + bjh
      u3 = ss * ( bjf - bjh )
      b(je+j) = bjb
      a(jb+j) = t0 + t1
      a(jj+j) = t0 - t1
      bjj = b(jj+j)
      bjg =  bjj
      b(jb+j) = u0 + u1
      b(jj+j) = u0 - u1
      a(jf+j) = t2 - u3
      ajh =  t2 + u3
      b(jf+j) = u2 + t3
      bjh =  u2 - t3
*----------------------
      ajk = a(jk+j)
      t0 = ajc + ajk
      t2 = ajc - ajk
      ajl = a(jl+j)
      t1 = ajg + ajl
      t3 = ss * ( ajg - ajl )
      bji = b(ji+j)
      bjk = b(jk+j)
      u0 = bji + bjk
      u2 = bji - bjk
      ajo = a(jo+j)
      ajl =  ajo
      bjl = b(jl+j)
      u1 = bjg + bjl
      u3 = ss * ( bjg - bjl )
      b(ji+j) = bjc
      a(jc+j) = t0 + t1
      a(jk+j) = t0 - t1
      bjo = b(jo+j)
      bjl =  bjo
      b(jc+j) = u0 + u1
      b(jk+j) = u0 - u1
      a(jg+j) = t2 - u3
      a(jo+j) = t2 + u3
      b(jg+j) = u2 + t3
      b(jo+j) = u2 - t3
*----------------------
      ajm = a(jm+j)
      t0 = ajm + ajl
      t2 = ajm - ajl
      ajn = a(jn+j)
      ajp = a(jp+j)
      t1 = ajn + ajp
      t3 = ss * ( ajn - ajp )
      a(jm+j) = ajd
      u0 = bjd + bjl
      u2 = bjd - bjl
      bjn = b(jn+j)
      bjp = b(jp+j)
      u1 = bjn + bjp
      u3 = ss * ( bjn - bjp )
      a(jn+j) = ajh
      a(jd+j) = t0 + t1
      a(jl+j) = t0 - t1
      b(jd+j) = u0 + u1
      b(jl+j) = u0 - u1
      b(jn+j) = bjh
      a(jh+j) = t2 - u3
      a(jp+j) = t2 + u3
      b(jh+j) = u2 + t3
      b(jp+j) = u2 - t3
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
      co3 = trigs(3*kk+1)
      si3 = s*trigs(3*kk+2)
*
*  double loop along first transform in block
*  ------------------------------------------
      do 460 ll = k , (la-1)*ink , 4*jstep
*
      do 450 jjj = ll , (n-1)*inc , 4*la*ink
      ja = istart + jjj
*
*     "transverse" loop
*     -----------------
      do 445 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = jc + jstepl
      if (jd.lt.istart) jd = jd + ninc
      je = ja + laincl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = je + laincl
      if (ji.lt.istart) ji = ji + ninc
      jj = ji + jstepl
      if (jj.lt.istart) jj = jj + ninc
      jk = jj + jstepl
      if (jk.lt.istart) jk = jk + ninc
      jl = jk + jstepl
      if (jl.lt.istart) jl = jl + ninc
      jm = ji + laincl
      if (jm.lt.istart) jm = jm + ninc
      jn = jm + jstepl
      if (jn.lt.istart) jn = jn + ninc
      jo = jn + jstepl
      if (jo.lt.istart) jo = jo + ninc
      jp = jo + jstepl
      if (jp.lt.istart) jp = jp + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 440 l = 1 , nvex
      aja = a(ja+j)
      ajc = a(jc+j)
      t0 = aja + ajc
      t2 = aja - ajc
      ajb = a(jb+j)
      ajd = a(jd+j)
      t1 = ajb + ajd
      t3 = ss * ( ajb - ajd )
      aji = a(ji+j)
      ajc =  aji
      bja = b(ja+j)
      bjc = b(jc+j)
      u0 = bja + bjc
      u2 = bja - bjc
      bjb = b(jb+j)
      bjd = b(jd+j)
      u1 = bjb + bjd
      u3 = ss * ( bjb - bjd )
      aje = a(je+j)
      ajb =  aje
      a(ja+j) = t0 + t1
      b(ja+j) = u0 + u1
      a(je+j) = co1*(t2-u3) - si1*(u2+t3)
      bjb =  si1*(t2-u3) + co1*(u2+t3)
      bjm = b(jm+j)
      bjd =  bjm
      a(ji+j) = co2*(t0-t1) - si2*(u0-u1)
      bjc =  si2*(t0-t1) + co2*(u0-u1)
      ajd =  co3*(t2+u3) - si3*(u2-t3)
      b(jm+j) = si3*(t2+u3) + co3*(u2-t3)
*----------------------------------------
      ajg = a(jg+j)
      t0 = ajb + ajg
      t2 = ajb - ajg
      ajf = a(jf+j)
      ajh = a(jh+j)
      t1 = ajf + ajh
      t3 = ss * ( ajf - ajh )
      ajj = a(jj+j)
      ajg =  ajj
      bje = b(je+j)
      bjg = b(jg+j)
      u0 = bje + bjg
      u2 = bje - bjg
      bjf = b(jf+j)
      bjh = b(jh+j)
      u1 = bjf + bjh
      u3 = ss * ( bjf - bjh )
      b(je+j) = bjb
      a(jb+j) = t0 + t1
      b(jb+j) = u0 + u1
      bjj = b(jj+j)
      bjg =  bjj
      a(jf+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jf+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jj+j) = co2*(t0-t1) - si2*(u0-u1)
      b(jj+j) = si2*(t0-t1) + co2*(u0-u1)
      ajh =  co3*(t2+u3) - si3*(u2-t3)
      bjh =  si3*(t2+u3) + co3*(u2-t3)
*----------------------------------------
      ajk = a(jk+j)
      t0 = ajc + ajk
      t2 = ajc - ajk
      ajl = a(jl+j)
      t1 = ajg + ajl
      t3 = ss * ( ajg - ajl )
      bji = b(ji+j)
      bjk = b(jk+j)
      u0 = bji + bjk
      u2 = bji - bjk
      ajo = a(jo+j)
      ajl =  ajo
      bjl = b(jl+j)
      u1 = bjg + bjl
      u3 = ss * ( bjg - bjl )
      b(ji+j) = bjc
      a(jc+j) = t0 + t1
      b(jc+j) = u0 + u1
      bjo = b(jo+j)
      bjl =  bjo
      a(jg+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jg+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jk+j) = co2*(t0-t1) - si2*(u0-u1)
      b(jk+j) = si2*(t0-t1) + co2*(u0-u1)
      a(jo+j) = co3*(t2+u3) - si3*(u2-t3)
      b(jo+j) = si3*(t2+u3) + co3*(u2-t3)
*----------------------------------------
      ajm = a(jm+j)
      t0 = ajm + ajl
      t2 = ajm - ajl
      ajn = a(jn+j)
      ajp = a(jp+j)
      t1 = ajn + ajp
      t3 = ss * ( ajn - ajp )
      a(jm+j) = ajd
      u0 = bjd + bjl
      u2 = bjd - bjl
      a(jn+j) = ajh
      bjn = b(jn+j)
      bjp = b(jp+j)
      u1 = bjn + bjp
      u3 = ss * ( bjn - bjp )
      b(jn+j) = bjh
      a(jd+j) = t0 + t1
      b(jd+j) = u0 + u1
      a(jh+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jh+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jl+j) = co2*(t0-t1) - si2*(u0-u1)
      b(jl+j) = si2*(t0-t1) + co2*(u0-u1)
      a(jp+j) = co3*(t2+u3) - si3*(u2-t3)
      b(jp+j) = si3*(t2+u3) + co3*(u2-t3)
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
      la = 4*la
  480 continue
*-----( end of loop on type II radix-4 passes )
*-----( nvex transforms completed)
  490 continue
      istart = istart + nvex * jump
  500 continue
*-----( end of loop on blocks of transforms )
*
      return
      end
