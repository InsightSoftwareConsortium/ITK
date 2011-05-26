*     fortran version of *dgpfa5* -
*     radix-5 section of self-sorting, in-place,
*        generalized pfa
*
*-------------------------------------------------------------------
*
      subroutine dgpfa5f(a,b,trigs,inc,jump,n,mm,lot,isign)
      double precision a(*), b(*), trigs(*)
      integer inc, jump, n, mm, lot, isign
      double precision s, ax, bx, c1, c2, c3
      double precision t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11
      double precision u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11
      double precision co1, co2, co3, co4, si1, si2, si3, si4
      double precision aja, ajb, ajc, ajd, aje, bjb, bje, bjc
      double precision bjd, bja, ajf, ajk, bjf, bjk, ajg, ajj
      double precision ajh, aji, ajl, ajq, bjg, bjj, bjh, bji
      double precision bjl, bjq, ajo, ajm, ajn, ajr, ajw, bjo
      double precision bjm, bjn, bjr, bjw, ajt, ajs, ajx, ajp
      double precision bjt, bjs, bjx, bjp, ajv, ajy, aju, bjv
      double precision bjy, bju
      data sin36/0.587785252292473d+0/,
     *     sin72/0.951056516295154d+0/,
     *      qrt5/0.559016994374947d+0/
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
      n5 = 5 ** mm
      inq = n / n5
      jstepx = (n5-n) * inc
      ninc = n * inc
      ink = inc * inq
      mu = mod(inq,5)
      if (isign.eq.-1) mu = 5 - mu
*
      m = mm
      mh = (m+1)/2
      s = dfloat(isign)
      c1 = qrt5
      c2 = sin72
      c3 = sin36
      if (mu.eq.2.or.mu.eq.3) then
         c1 = -c1
         c2 = sin36
         c3 = sin72
      endif
      if (mu.eq.3.or.mu.eq.4) c2 = -c2
      if (mu.eq.2.or.mu.eq.4) c3 = -c3
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
*  loop on type I radix-5 passes
*  -----------------------------
      do 160 ipass = 1 , mh
      jstep = (n*inc) / (5*la)
      jstepl = jstep - ninc
      kk = 0
*
*  loop on k
*  ---------
      do 150 k = 0 , jstep-ink , ink
*
      if (k.gt.0) then
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
      co3 = trigs(3*kk+1)
      si3 = s*trigs(3*kk+2)
      co4 = trigs(4*kk+1)
      si4 = s*trigs(4*kk+2)
      endif
*
*  loop along transform
*  --------------------
      do 140 jjj = k , (n-1)*inc , 5*jstep
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
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
      if (k.eq.0) then
*
cdir$ ivdep, shortloop
      do 110 l = 1 , nvex
      ajb = a(jb+j)
      aje = a(je+j)
      t1 = ajb + aje
      ajc = a(jc+j)
      ajd = a(jd+j)
      t2 = ajc + ajd
      t3 = ajb - aje
      t4 = ajc - ajd
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aja = a(ja+j)
      t7 = aja - 0.25 * t5
      a(ja+j) = aja + t5
      t8 = t7 + t6
      t9 = t7 - t6
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjb = b(jb+j)
      bje = b(je+j)
      u1 = bjb + bje
      bjc = b(jc+j)
      bjd = b(jd+j)
      u2 = bjc + bjd
      u3 = bjb - bje
      u4 = bjc - bjd
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bja = b(ja+j)
      u7 = bja - 0.25 * u5
      b(ja+j) = bja + u5
      u8 = u7 + u6
      u9 = u7 - u6
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jb+j) = t8 - u11
      b(jb+j) = u8 + t11
      a(je+j) = t8 + u11
      b(je+j) = u8 - t11
      a(jc+j) = t9 - u10
      b(jc+j) = u9 + t10
      a(jd+j) = t9 + u10
      b(jd+j) = u9 - t10
      j = j + jump
  110 continue
*
      else
*
cdir$ ivdep,shortloop
      do 130 l = 1 , nvex
      ajb = a(jb+j)
      aje = a(je+j)
      t1 = ajb + aje
      ajc = a(jc+j)
      ajd = a(jd+j)
      t2 = ajc + ajd
      t3 = ajb - aje
      t4 = ajc - ajd
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aja = a(ja+j)
      t7 = aja - 0.25 * t5
      a(ja+j) = aja + t5
      t8 = t7 + t6
      t9 = t7 - t6
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjb = b(jb+j)
      bje = b(je+j)
      u1 = bjb + bje
      bjc = b(jc+j)
      bjd = b(jd+j)
      u2 = bjc + bjd
      u3 = bjb - bje
      u4 = bjc - bjd
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bja = b(ja+j)
      u7 = bja - 0.25 * u5
      b(ja+j) = bja + u5
      u8 = u7 + u6
      u9 = u7 - u6
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jb+j) = co1*(t8-u11) - si1*(u8+t11)
      b(jb+j) = si1*(t8-u11) + co1*(u8+t11)
      a(je+j) = co4*(t8+u11) - si4*(u8-t11)
      b(je+j) = si4*(t8+u11) + co4*(u8-t11)
      a(jc+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jc+j) = si2*(t9-u10) + co2*(u9+t10)
      a(jd+j) = co3*(t9+u10) - si3*(u9-t10)
      b(jd+j) = si3*(t9+u10) + co3*(u9-t10)
      j = j + jump
  130 continue
*
      endif
*
*-----( end of loop across transforms )
*
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  135 continue
  140 continue
*-----( end of loop along transforms )
      kk = kk + 2*la
  150 continue
*-----( end of loop on nonzero k )
      la = 5*la
  160 continue
*-----( end of loop on type I radix-5 passes)
*
      if (n.eq.5) go to 490
*
*  loop on type II radix-5 passes
*  ------------------------------
  400 continue
*
      do 480 ipass = mh+1 , m
      jstep = (n*inc) / (5*la)
      jstepl = jstep - ninc
      laincl = la * ink - ninc
      kk = 0
*
*     loop on k
*     ---------
      do 470 k = 0 , jstep-ink , ink
*
      if (k.gt.0) then
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
      co3 = trigs(3*kk+1)
      si3 = s*trigs(3*kk+2)
      co4 = trigs(4*kk+1)
      si4 = s*trigs(4*kk+2)
      endif
*
*  double loop along first transform in block
*  ------------------------------------------
      do 460 ll = k , (la-1)*ink , 5*jstep
*
      do 450 jjj = ll , (n-1)*inc , 5*la*ink
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
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = ja + laincl
      if (jf.lt.istart) jf = jf + ninc
      jg = jf + jstepl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = jh + jstepl
      if (ji.lt.istart) ji = ji + ninc
      jj = ji + jstepl
      if (jj.lt.istart) jj = jj + ninc
      jk = jf + laincl
      if (jk.lt.istart) jk = jk + ninc
      jl = jk + jstepl
      if (jl.lt.istart) jl = jl + ninc
      jm = jl + jstepl
      if (jm.lt.istart) jm = jm + ninc
      jn = jm + jstepl
      if (jn.lt.istart) jn = jn + ninc
      jo = jn + jstepl
      if (jo.lt.istart) jo = jo + ninc
      jp = jk + laincl
      if (jp.lt.istart) jp = jp + ninc
      jq = jp + jstepl
      if (jq.lt.istart) jq = jq + ninc
      jr = jq + jstepl
      if (jr.lt.istart) jr = jr + ninc
      js = jr + jstepl
      if (js.lt.istart) js = js + ninc
      jt = js + jstepl
      if (jt.lt.istart) jt = jt + ninc
      ju = jp + laincl
      if (ju.lt.istart) ju = ju + ninc
      jv = ju + jstepl
      if (jv.lt.istart) jv = jv + ninc
      jw = jv + jstepl
      if (jw.lt.istart) jw = jw + ninc
      jx = jw + jstepl
      if (jx.lt.istart) jx = jx + ninc
      jy = jx + jstepl
      if (jy.lt.istart) jy = jy + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
      if (k.eq.0) then
*
cdir$ ivdep, shortloop
      do 410 l = 1 , nvex
      ajb = a(jb+j)
      aje = a(je+j)
      t1 = ajb + aje
      ajc = a(jc+j)
      ajd = a(jd+j)
      t2 = ajc + ajd
      t3 = ajb - aje
      t4 = ajc - ajd
      ajf = a(jf+j)
      ajb =  ajf
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aja = a(ja+j)
      t7 = aja - 0.25 * t5
      a(ja+j) = aja + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajk = a(jk+j)
      ajc =  ajk
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjb = b(jb+j)
      bje = b(je+j)
      u1 = bjb + bje
      bjc = b(jc+j)
      bjd = b(jd+j)
      u2 = bjc + bjd
      u3 = bjb - bje
      u4 = bjc - bjd
      bjf = b(jf+j)
      bjb =  bjf
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bja = b(ja+j)
      u7 = bja - 0.25 * u5
      b(ja+j) = bja + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjk = b(jk+j)
      bjc =  bjk
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jf+j) = t8 - u11
      b(jf+j) = u8 + t11
      aje =  t8 + u11
      bje =  u8 - t11
      a(jk+j) = t9 - u10
      b(jk+j) = u9 + t10
      ajd =  t9 + u10
      bjd =  u9 - t10
*----------------------
      ajg = a(jg+j)
      ajj = a(jj+j)
      t1 = ajg + ajj
      ajh = a(jh+j)
      aji = a(ji+j)
      t2 = ajh + aji
      t3 = ajg - ajj
      t4 = ajh - aji
      ajl = a(jl+j)
      ajh =  ajl
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      t7 = ajb - 0.25 * t5
      a(jb+j) = ajb + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajq = a(jq+j)
      aji =  ajq
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjg = b(jg+j)
      bjj = b(jj+j)
      u1 = bjg + bjj
      bjh = b(jh+j)
      bji = b(ji+j)
      u2 = bjh + bji
      u3 = bjg - bjj
      u4 = bjh - bji
      bjl = b(jl+j)
      bjh =  bjl
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      u7 = bjb - 0.25 * u5
      b(jb+j) = bjb + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjq = b(jq+j)
      bji =  bjq
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jg+j) = t8 - u11
      b(jg+j) = u8 + t11
      ajj =  t8 + u11
      bjj =  u8 - t11
      a(jl+j) = t9 - u10
      b(jl+j) = u9 + t10
      a(jq+j) = t9 + u10
      b(jq+j) = u9 - t10
*----------------------
      ajo = a(jo+j)
      t1 = ajh + ajo
      ajm = a(jm+j)
      ajn = a(jn+j)
      t2 = ajm + ajn
      t3 = ajh - ajo
      t4 = ajm - ajn
      ajr = a(jr+j)
      ajn =  ajr
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      t7 = ajc - 0.25 * t5
      a(jc+j) = ajc + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajw = a(jw+j)
      ajo =  ajw
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjo = b(jo+j)
      u1 = bjh + bjo
      bjm = b(jm+j)
      bjn = b(jn+j)
      u2 = bjm + bjn
      u3 = bjh - bjo
      u4 = bjm - bjn
      bjr = b(jr+j)
      bjn =  bjr
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      u7 = bjc - 0.25 * u5
      b(jc+j) = bjc + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjw = b(jw+j)
      bjo =  bjw
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jh+j) = t8 - u11
      b(jh+j) = u8 + t11
      a(jw+j) = t8 + u11
      b(jw+j) = u8 - t11
      a(jm+j) = t9 - u10
      b(jm+j) = u9 + t10
      a(jr+j) = t9 + u10
      b(jr+j) = u9 - t10
*----------------------
      ajt = a(jt+j)
      t1 = aji + ajt
      ajs = a(js+j)
      t2 = ajn + ajs
      t3 = aji - ajt
      t4 = ajn - ajs
      ajx = a(jx+j)
      ajt =  ajx
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      ajp = a(jp+j)
      t7 = ajp - 0.25 * t5
      ax = ajp + t5
      t8 = t7 + t6
      t9 = t7 - t6
      a(jp+j) = ajd
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      a(jd+j) = ax
      bjt = b(jt+j)
      u1 = bji + bjt
      bjs = b(js+j)
      u2 = bjn + bjs
      u3 = bji - bjt
      u4 = bjn - bjs
      bjx = b(jx+j)
      bjt =  bjx
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bjp = b(jp+j)
      u7 = bjp - 0.25 * u5
      bx = bjp + u5
      u8 = u7 + u6
      u9 = u7 - u6
      b(jp+j) = bjd
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      b(jd+j) = bx
      a(ji+j) = t8 - u11
      b(ji+j) = u8 + t11
      a(jx+j) = t8 + u11
      b(jx+j) = u8 - t11
      a(jn+j) = t9 - u10
      b(jn+j) = u9 + t10
      a(js+j) = t9 + u10
      b(js+j) = u9 - t10
*----------------------
      ajv = a(jv+j)
      ajy = a(jy+j)
      t1 = ajv + ajy
      t2 = ajo + ajt
      t3 = ajv - ajy
      t4 = ajo - ajt
      a(jv+j) = ajj
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aju = a(ju+j)
      t7 = aju - 0.25 * t5
      ax = aju + t5
      t8 = t7 + t6
      t9 = t7 - t6
      a(ju+j) = aje
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      a(je+j) = ax
      bjv = b(jv+j)
      bjy = b(jy+j)
      u1 = bjv + bjy
      u2 = bjo + bjt
      u3 = bjv - bjy
      u4 = bjo - bjt
      b(jv+j) = bjj
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bju = b(ju+j)
      u7 = bju - 0.25 * u5
      bx = bju + u5
      u8 = u7 + u6
      u9 = u7 - u6
      b(ju+j) = bje
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      b(je+j) = bx
      a(jj+j) = t8 - u11
      b(jj+j) = u8 + t11
      a(jy+j) = t8 + u11
      b(jy+j) = u8 - t11
      a(jo+j) = t9 - u10
      b(jo+j) = u9 + t10
      a(jt+j) = t9 + u10
      b(jt+j) = u9 - t10
      j = j + jump
  410 continue
*
      else
*
cdir$ ivdep, shortloop
      do 440 l = 1 , nvex
      ajb = a(jb+j)
      aje = a(je+j)
      t1 = ajb + aje
      ajc = a(jc+j)
      ajd = a(jd+j)
      t2 = ajc + ajd
      t3 = ajb - aje
      t4 = ajc - ajd
      ajf = a(jf+j)
      ajb =  ajf
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aja = a(ja+j)
      t7 = aja - 0.25 * t5
      a(ja+j) = aja + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajk = a(jk+j)
      ajc =  ajk
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjb = b(jb+j)
      bje = b(je+j)
      u1 = bjb + bje
      bjc = b(jc+j)
      bjd = b(jd+j)
      u2 = bjc + bjd
      u3 = bjb - bje
      u4 = bjc - bjd
      bjf = b(jf+j)
      bjb =  bjf
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bja = b(ja+j)
      u7 = bja - 0.25 * u5
      b(ja+j) = bja + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjk = b(jk+j)
      bjc =  bjk
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jf+j) = co1*(t8-u11) - si1*(u8+t11)
      b(jf+j) = si1*(t8-u11) + co1*(u8+t11)
      aje =  co4*(t8+u11) - si4*(u8-t11)
      bje =  si4*(t8+u11) + co4*(u8-t11)
      a(jk+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jk+j) = si2*(t9-u10) + co2*(u9+t10)
      ajd =  co3*(t9+u10) - si3*(u9-t10)
      bjd =  si3*(t9+u10) + co3*(u9-t10)
*----------------------
      ajg = a(jg+j)
      ajj = a(jj+j)
      t1 = ajg + ajj
      ajh = a(jh+j)
      aji = a(ji+j)
      t2 = ajh + aji
      t3 = ajg - ajj
      t4 = ajh - aji
      ajl = a(jl+j)
      ajh =  ajl
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      t7 = ajb - 0.25 * t5
      a(jb+j) = ajb + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajq = a(jq+j)
      aji =  ajq
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjg = b(jg+j)
      bjj = b(jj+j)
      u1 = bjg + bjj
      bjh = b(jh+j)
      bji = b(ji+j)
      u2 = bjh + bji
      u3 = bjg - bjj
      u4 = bjh - bji
      bjl = b(jl+j)
      bjh =  bjl
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      u7 = bjb - 0.25 * u5
      b(jb+j) = bjb + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjq = b(jq+j)
      bji =  bjq
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jg+j) = co1*(t8-u11) - si1*(u8+t11)
      b(jg+j) = si1*(t8-u11) + co1*(u8+t11)
      ajj =  co4*(t8+u11) - si4*(u8-t11)
      bjj =  si4*(t8+u11) + co4*(u8-t11)
      a(jl+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jl+j) = si2*(t9-u10) + co2*(u9+t10)
      a(jq+j) = co3*(t9+u10) - si3*(u9-t10)
      b(jq+j) = si3*(t9+u10) + co3*(u9-t10)
*----------------------
      ajo = a(jo+j)
      t1 = ajh + ajo
      ajm = a(jm+j)
      ajn = a(jn+j)
      t2 = ajm + ajn
      t3 = ajh - ajo
      t4 = ajm - ajn
      ajr = a(jr+j)
      ajn =  ajr
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      t7 = ajc - 0.25 * t5
      a(jc+j) = ajc + t5
      t8 = t7 + t6
      t9 = t7 - t6
      ajw = a(jw+j)
      ajo =  ajw
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      bjo = b(jo+j)
      u1 = bjh + bjo
      bjm = b(jm+j)
      bjn = b(jn+j)
      u2 = bjm + bjn
      u3 = bjh - bjo
      u4 = bjm - bjn
      bjr = b(jr+j)
      bjn =  bjr
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      u7 = bjc - 0.25 * u5
      b(jc+j) = bjc + u5
      u8 = u7 + u6
      u9 = u7 - u6
      bjw = b(jw+j)
      bjo =  bjw
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      a(jh+j) = co1*(t8-u11) - si1*(u8+t11)
      b(jh+j) = si1*(t8-u11) + co1*(u8+t11)
      a(jw+j) = co4*(t8+u11) - si4*(u8-t11)
      b(jw+j) = si4*(t8+u11) + co4*(u8-t11)
      a(jm+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jm+j) = si2*(t9-u10) + co2*(u9+t10)
      a(jr+j) = co3*(t9+u10) - si3*(u9-t10)
      b(jr+j) = si3*(t9+u10) + co3*(u9-t10)
*----------------------
      ajt = a(jt+j)
      t1 = aji + ajt
      ajs = a(js+j)
      t2 = ajn + ajs
      t3 = aji - ajt
      t4 = ajn - ajs
      ajx = a(jx+j)
      ajt =  ajx
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      ajp = a(jp+j)
      t7 = ajp - 0.25 * t5
      ax = ajp + t5
      t8 = t7 + t6
      t9 = t7 - t6
      a(jp+j) = ajd
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      a(jd+j) = ax
      bjt = b(jt+j)
      u1 = bji + bjt
      bjs = b(js+j)
      u2 = bjn + bjs
      u3 = bji - bjt
      u4 = bjn - bjs
      bjx = b(jx+j)
      bjt =  bjx
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bjp = b(jp+j)
      u7 = bjp - 0.25 * u5
      bx = bjp + u5
      u8 = u7 + u6
      u9 = u7 - u6
      b(jp+j) = bjd
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      b(jd+j) = bx
      a(ji+j) = co1*(t8-u11) - si1*(u8+t11)
      b(ji+j) = si1*(t8-u11) + co1*(u8+t11)
      a(jx+j) = co4*(t8+u11) - si4*(u8-t11)
      b(jx+j) = si4*(t8+u11) + co4*(u8-t11)
      a(jn+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jn+j) = si2*(t9-u10) + co2*(u9+t10)
      a(js+j) = co3*(t9+u10) - si3*(u9-t10)
      b(js+j) = si3*(t9+u10) + co3*(u9-t10)
*----------------------
      ajv = a(jv+j)
      ajy = a(jy+j)
      t1 = ajv + ajy
      t2 = ajo + ajt
      t3 = ajv - ajy
      t4 = ajo - ajt
      a(jv+j) = ajj
      t5 = t1 + t2
      t6 = c1 * ( t1 - t2 )
      aju = a(ju+j)
      t7 = aju - 0.25 * t5
      ax = aju + t5
      t8 = t7 + t6
      t9 = t7 - t6
      a(ju+j) = aje
      t10 = c3 * t3 - c2 * t4
      t11 = c2 * t3 + c3 * t4
      a(je+j) = ax
      bjv = b(jv+j)
      bjy = b(jy+j)
      u1 = bjv + bjy
      u2 = bjo + bjt
      u3 = bjv - bjy
      u4 = bjo - bjt
      b(jv+j) = bjj
      u5 = u1 + u2
      u6 = c1 * ( u1 - u2 )
      bju = b(ju+j)
      u7 = bju - 0.25 * u5
      bx = bju + u5
      u8 = u7 + u6
      u9 = u7 - u6
      b(ju+j) = bje
      u10 = c3 * u3 - c2 * u4
      u11 = c2 * u3 + c3 * u4
      b(je+j) = bx
      a(jj+j) = co1*(t8-u11) - si1*(u8+t11)
      b(jj+j) = si1*(t8-u11) + co1*(u8+t11)
      a(jy+j) = co4*(t8+u11) - si4*(u8-t11)
      b(jy+j) = si4*(t8+u11) + co4*(u8-t11)
      a(jo+j) = co2*(t9-u10) - si2*(u9+t10)
      b(jo+j) = si2*(t9-u10) + co2*(u9+t10)
      a(jt+j) = co3*(t9+u10) - si3*(u9-t10)
      b(jt+j) = si3*(t9+u10) + co3*(u9-t10)
      j = j + jump
  440 continue
*
      endif
*
*-----(end of loop across transforms)
*
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  445 continue
  450 continue
  460 continue
*-----( end of double loop for this k )
      kk = kk + 2*la
  470 continue
*-----( end of loop over values of k )
      la = 5*la
  480 continue
*-----( end of loop on type II radix-5 passes )
*-----( nvex transforms completed)
  490 continue
      istart = istart + nvex * jump
  500 continue
*-----( end of loop on blocks of transforms )
*
      return
      end
