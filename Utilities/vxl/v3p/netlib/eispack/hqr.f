      subroutine hqr(nm,n,low,igh,h,wr,wi,ierr)
C  RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG)
c
      integer i,j,k,l,m,n,en,ll,mm,na,nm,igh,itn,its,low,mp2,enm2,ierr
      double precision h(nm,n),wr(n),wi(n)
      double precision p,q,r,s,t,w,x,y,zz,norm,tst1,tst2
      logical notlas
c
c     this subroutine is a translation of the algol procedure hqr,
c     num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
c
c     this subroutine finds the eigenvalues of a real
c     upper hessenberg matrix by the qr method.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        low and igh are integers determined by the balancing
c          subroutine  balanc.  if  balanc  has not been used,
c          set low=1, igh=n.
c
c        h contains the upper hessenberg matrix.  information about
c          the transformations used in the reduction to hessenberg
c          form by  elmhes  or  orthes, if performed, is stored
c          in the remaining triangle under the hessenberg matrix.
c
c     on output
c
c        h has been destroyed.  therefore, it must be saved
c          before calling  hqr  if subsequent calculation and
c          back transformation of eigenvectors is to be performed.
c
c        wr and wi contain the real and imaginary parts,
c          respectively, of the eigenvalues.  the eigenvalues
c          are unordered except that complex conjugate pairs
c          of values appear consecutively with the eigenvalue
c          having the positive imaginary part first.  if an
c          error exit is made, the eigenvalues should be correct
c          for indices ierr+1,...,n.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated september 1989.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      norm = 0.0d0
      k = 1
c     .......... store roots isolated by balanc
c                and compute matrix norm ..........
      do 50 i = 1, n
c
         do 40 j = k, n
   40    norm = norm + dabs(h(i,j))
c
         k = i
         if (i .ge. low .and. i .le. igh) go to 50
         wr(i) = h(i,i)
         wi(i) = 0.0d0
   50 continue
c
      en = igh
      t = 0.0d0
      itn = 30*n
c     .......... search for next eigenvalues ..........
   60 if (en .lt. low) go to 1001
      its = 0
      na = en - 1
      enm2 = na - 1
c     .......... look for single small sub-diagonal element
c                for l=en step -1 until low do -- ..........
   70 do 80 ll = low, en
         l = en + low - ll
         if (l .eq. low) go to 100
         s = dabs(h(l-1,l-1)) + dabs(h(l,l))
         if (s .eq. 0.0d0) s = norm
         tst1 = s
         tst2 = tst1 + dabs(h(l,l-1))
         if (tst2 .eq. tst1) go to 100
   80 continue
c     .......... form shift ..........
  100 x = h(en,en)
      if (l .eq. en) go to 270
      y = h(na,na)
      w = h(en,na) * h(na,en)
      if (l .eq. na) go to 280
      if (itn .eq. 0) go to 1000
      if (its .ne. 10 .and. its .ne. 20) go to 130
c     .......... form exceptional shift ..........
      t = t + x
c
      do 120 i = low, en
  120 h(i,i) = h(i,i) - x
c
      s = dabs(h(en,na)) + dabs(h(na,enm2))
      x = 0.75d0 * s
      y = x
      w = -0.4375d0 * s * s
  130 its = its + 1
      itn = itn - 1
c     .......... look for two consecutive small
c                sub-diagonal elements.
c                for m=en-2 step -1 until l do -- ..........
      do 140 mm = l, enm2
         m = enm2 + l - mm
         zz = h(m,m)
         r = x - zz
         s = y - zz
         p = (r * s - w) / h(m+1,m) + h(m,m+1)
         q = h(m+1,m+1) - zz - r - s
         r = h(m+2,m+1)
         s = dabs(p) + dabs(q) + dabs(r)
         p = p / s
         q = q / s
         r = r / s
         if (m .eq. l) go to 150
         tst1 = dabs(p)*(dabs(h(m-1,m-1)) + dabs(zz) + dabs(h(m+1,m+1)))
         tst2 = tst1 + dabs(h(m,m-1))*(dabs(q) + dabs(r))
         if (tst2 .eq. tst1) go to 150
  140 continue
c
  150 mp2 = m + 2
c
      do 160 i = mp2, en
         h(i,i-2) = 0.0d0
         if (i .eq. mp2) go to 160
         h(i,i-3) = 0.0d0
  160 continue
c     .......... double qr step involving rows l to en and
c                columns m to en ..........
      do 260 k = m, na
         notlas = k .ne. na
         if (k .eq. m) go to 170
         p = h(k,k-1)
         q = h(k+1,k-1)
         r = 0.0d0
         if (notlas) r = h(k+2,k-1)
         x = dabs(p) + dabs(q) + dabs(r)
         if (x .eq. 0.0d0) go to 260
         p = p / x
         q = q / x
         r = r / x
  170    s = dsign(dsqrt(p*p+q*q+r*r),p)
         if (k .eq. m) go to 180
         h(k,k-1) = -s * x
         go to 190
  180    if (l .ne. m) h(k,k-1) = -h(k,k-1)
  190    p = p + s
         x = p / s
         y = q / s
         zz = r / s
         q = q / p
         r = r / p
         if (notlas) go to 225
c     .......... row modification ..........
         do 200 j = k, EN
            p = h(k,j) + q * h(k+1,j)
            h(k,j) = h(k,j) - p * x
            h(k+1,j) = h(k+1,j) - p * y
  200    continue
c
         j = min0(en,k+3)
c     .......... column modification ..........
         do 210 i = L, j
            p = x * h(i,k) + y * h(i,k+1)
            h(i,k) = h(i,k) - p
            h(i,k+1) = h(i,k+1) - p * q
  210    continue
         go to 255
  225    continue
c     .......... row modification ..........
         do 230 j = k, EN
            p = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
            h(k,j) = h(k,j) - p * x
            h(k+1,j) = h(k+1,j) - p * y
            h(k+2,j) = h(k+2,j) - p * zz
  230    continue
c
         j = min0(en,k+3)
c     .......... column modification ..........
         do 240 i = L, j
            p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
            h(i,k) = h(i,k) - p
            h(i,k+1) = h(i,k+1) - p * q
            h(i,k+2) = h(i,k+2) - p * r
  240    continue
  255    continue
c
  260 continue
c
      go to 70
c     .......... one root found ..........
  270 wr(en) = x + t
      wi(en) = 0.0d0
      en = na
      go to 60
c     .......... two roots found ..........
  280 p = (y - x) / 2.0d0
      q = p * p + w
      zz = dsqrt(dabs(q))
      x = x + t
      if (q .lt. 0.0d0) go to 320
c     .......... real pair ..........
      zz = p + dsign(zz,p)
      wr(na) = x + zz
      wr(en) = wr(na)
      if (zz .ne. 0.0d0) wr(en) = x - w / zz
      wi(na) = 0.0d0
      wi(en) = 0.0d0
      go to 330
c     .......... complex pair ..........
  320 wr(na) = x + p
      wr(en) = x + p
      wi(na) = zz
      wi(en) = -zz
  330 en = enm2
      go to 60
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 ierr = en
 1001 return
      end
