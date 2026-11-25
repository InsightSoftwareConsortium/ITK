      subroutine zsvdc(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info)
      integer ldx,n,p,ldu,ldv,job,info
      complex*16 x(ldx,1),s(1),e(1),u(ldu,1),v(ldv,1),work(1)
c
c
c     zsvdc is a subroutine to reduce a complex*16 nxp matrix x by
c     unitary transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         complex*16(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by zsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v
c                   (see below).
c
c         work      complex*16(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    returns the first min(n,p)
c                                  left singular vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         complex*16(mm), where mm=min(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         complex*16(p).
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         complex*16(ldu,k), where ldu.ge.n.  if joba.eq.1
c                                   then k.eq.n, if joba.ge.2 then
c
c                                   k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.gt.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         complex*16(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if jobb.eq.0.  if p.le.n,
c                   then v may be identified whth x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = ctrans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (ctrans(u)
c                   is the conjugate-transpose of u).  thus the
c                   singular values of x and b are the same.
c
c     linpack. this version dated 03/19/79 .
c              correction to shift calculation made 2/85.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     zsvdc uses the following functions and subprograms.
c
c     external zdrot
c     blas zaxpy,zdotc,zscal,zswap,dznrm2,drotg
c     fortran dabs,dmax1,cdabs,dcmplx
c     fortran dconjg,max0,min0,mod,dsqrt
c
c     internal variables
c
      integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,
     *        mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
      complex*16 zdotc,t,r
      double precision b,c,cs,el,emm1,f,g,dznrm2,scale,shift,sl,sm,sn,
     *                 smm1,t1,test,ztest
      logical wantu,wantv
c
      complex*16 csign,zdum,zdum1,zdum2
      double precision cabs1
      double precision dreal,dimag
      complex*16 zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum))
      csign(zdum1,zdum2) = cdabs(zdum1)*(zdum2/cdabs(zdum2))
c
c     set the maximum number of iterations.
c
      maxit = 1000
c
c     determine what is to be computed.
c
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min0(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
c
c     reduce x to bidiagonal form, storing the diagonal elements
c     in s and the super-diagonal elements in e.
c
      info = 0
      nct = min0(n-1,p)
      nrt = max0(0,min0(p-2,n))
      lu = max0(nct,nrt)
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
c
c           compute the transformation for the l-th column and
c           place the l-th diagonal in s(l).
c
            s(l) = dcmplx(dznrm2(n-l+1,x(l,l),1),0.0d0)
            if (cabs1(s(l)) .eq. 0.0d0) go to 10
               if (cabs1(x(l,l)) .ne. 0.0d0) s(l) = csign(s(l),x(l,l))
               call zscal(n-l+1,1.0d0/s(l),x(l,l),1)
               x(l,l) = (1.0d0,0.0d0) + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (cabs1(s(l)) .eq. 0.0d0) go to 30
c
c              apply the transformation.
c
               t = -zdotc(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call zaxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
c
c           place the l-th row of x into  e for the
c           subsequent calculation of the row transformation.
c
            e(j) = dconjg(x(l,j))
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
c
c           place the transformation in u for subsequent back
c           multiplication.
c
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
c
c           compute the l-th row transformation and place the
c           l-th super-diagonal in e(l).
c
            e(l) = dcmplx(dznrm2(p-l,e(lp1),1),0.0d0)
            if (cabs1(e(l)) .eq. 0.0d0) go to 80
               if (cabs1(e(lp1)) .ne. 0.0d0) e(l) = csign(e(l),e(lp1))
               call zscal(p-l,1.0d0/e(l),e(lp1),1)
               e(lp1) = (1.0d0,0.0d0) + e(lp1)
   80       continue
            e(l) = -dconjg(e(l))
            if (lp1 .gt. n .or. cabs1(e(l)) .eq. 0.0d0) go to 120
c
c              apply the transformation.
c
               do 90 i = lp1, n
                  work(i) = (0.0d0,0.0d0)
   90          continue
               do 100 j = lp1, p
                  call zaxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call zaxpy(n-l,dconjg(-e(j)/e(lp1)),work(lp1),1,
     *                       x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
c
c              place the transformation in v for subsequent
c              back multiplication.
c
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
c
c     set up the final bidiagonal matrix or order m.
c
      m = min0(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = (0.0d0,0.0d0)
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = (0.0d0,0.0d0)
c
c     if required, generate u.
c
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = (0.0d0,0.0d0)
  180       continue
            u(j,j) = (1.0d0,0.0d0)
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (cabs1(s(l)) .eq. 0.0d0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -zdotc(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call zaxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call zscal(n-l+1,(-1.0d0,0.0d0),u(l,l),1)
               u(l,l) = (1.0d0,0.0d0) + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = (0.0d0,0.0d0)
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = (0.0d0,0.0d0)
  260          continue
               u(l,l) = (1.0d0,0.0d0)
  270       continue
  280    continue
  290    continue
  300 continue
c
c     if it is required, generate v.
c
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (cabs1(e(l)) .eq. 0.0d0) go to 320
               do 310 j = lp1, p
                  t = -zdotc(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call zaxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = (0.0d0,0.0d0)
  330       continue
            v(l,l) = (1.0d0,0.0d0)
  340    continue
  350 continue
c
c     transform s and e so that they are double precision.
c
      do 380 i = 1, m
         if (cabs1(s(i)) .eq. 0.0d0) go to 360
            t = dcmplx(cdabs(s(i)),0.0d0)
            r = s(i)/t
            s(i) = t
            if (i .lt. m) e(i) = e(i)/r
            if (wantu) call zscal(n,r,u(1,i),1)
  360    continue
c     ...exit
         if (i .eq. m) go to 390
         if (cabs1(e(i)) .eq. 0.0d0) go to 370
            t = dcmplx(cdabs(e(i)),0.0d0)
            r = t/e(i)
            e(i) = t
            s(i+1) = s(i+1)*r
            if (wantv) call zscal(p,r,v(1,i+1),1)
  370    continue
  380 continue
  390 continue
c
c     main iteration loop for the singular values.
c
      mm = m
      iter = 0
  400 continue
c
c        quit if all the singular values have been found.
c
c     ...exit
         if (m .eq. 0) go to 660
c
c        if too many iterations have been performed, set
c        flag and return.
c
         if (iter .lt. maxit) go to 410
            info = m
c     ......exit
            go to 660
  410    continue
c
c        this section of the program inspects for
c        negligible elements in the s and e arrays.  on
c        completion the variables kase and l are set as follows.
c
c           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m
c           kase = 2     if s(l) is negligible and l.lt.m
c           kase = 3     if e(l-1) is negligible, l.lt.m, and
c                        s(l), ..., s(m) are not negligible (qr step).
c           kase = 4     if e(m-1) is negligible (convergence).
c
         do 430 ll = 1, m
            l = m - ll
c        ...exit
            if (l .eq. 0) go to 440
            test = cdabs(s(l)) + cdabs(s(l+1))
            ztest = test + cdabs(e(l))
            if (ztest .ne. test) go to 420
               e(l) = (0.0d0,0.0d0)
c        ......exit
               go to 440
  420       continue
  430    continue
  440    continue
         if (l .ne. m - 1) go to 450
            kase = 4
         go to 520
  450    continue
            lp1 = l + 1
            mp1 = m + 1
            do 470 lls = lp1, mp1
               ls = m - lls + lp1
c           ...exit
               if (ls .eq. l) go to 480
               test = 0.0d0
               if (ls .ne. m) test = test + cdabs(e(ls))
               if (ls .ne. l + 1) test = test + cdabs(e(ls-1))
               ztest = test + cdabs(s(ls))
               if (ztest .ne. test) go to 460
                  s(ls) = (0.0d0,0.0d0)
c           ......exit
                  go to 480
  460          continue
  470       continue
  480       continue
            if (ls .ne. l) go to 490
               kase = 3
            go to 510
  490       continue
            if (ls .ne. m) go to 500
               kase = 1
            go to 510
  500       continue
               kase = 2
               l = ls
  510       continue
  520    continue
         l = l + 1
c
c        perform the task indicated by kase.
c
         go to (530, 560, 580, 610), kase
c
c        deflate negligible s(m).
c
  530    continue
            mm1 = m - 1
            f = dreal(e(m-1))
            e(m-1) = (0.0d0,0.0d0)
            do 550 kk = l, mm1
               k = mm1 - kk + l
               t1 = dreal(s(k))
               call drotg(t1,f,cs,sn)
               s(k) = dcmplx(t1,0.0d0)
               if (k .eq. l) go to 540
                  f = -sn*dreal(e(k-1))
                  e(k-1) = cs*e(k-1)
  540          continue
               if (wantv) call zdrot(p,v(1,k),1,v(1,m),1,cs,sn)
  550       continue
         go to 650
c
c        split at negligible s(l).
c
  560    continue
            f = dreal(e(l-1))
            e(l-1) = (0.0d0,0.0d0)
            do 570 k = l, m
               t1 = dreal(s(k))
               call drotg(t1,f,cs,sn)
               s(k) = dcmplx(t1,0.0d0)
               f = -sn*dreal(e(k))
               e(k) = cs*e(k)
               if (wantu) call zdrot(n,u(1,k),1,u(1,l-1),1,cs,sn)
  570       continue
         go to 650
c
c        perform one qr step.
c
  580    continue
c
c           calculate the shift.
c
            scale = dmax1(cdabs(s(m)),cdabs(s(m-1)),cdabs(e(m-1)),
     *                    cdabs(s(l)),cdabs(e(l)))
            sm = dreal(s(m))/scale
            smm1 = dreal(s(m-1))/scale
            emm1 = dreal(e(m-1))/scale
            sl = dreal(s(l))/scale
            el = dreal(e(l))/scale
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2.0d0
            c = (sm*emm1)**2
            shift = 0.0d0
            if (b .eq. 0.0d0 .and. c .eq. 0.0d0) go to 590
               shift = dsqrt(b**2+c)
               if (b .lt. 0.0d0) shift = -shift
               shift = c/(b + shift)
  590       continue
            f = (sl + sm)*(sl - sm) + shift
            g = sl*el
c
c           chase zeros.
c
            mm1 = m - 1
            do 600 k = l, mm1
               call drotg(f,g,cs,sn)
               if (k .ne. l) e(k-1) = dcmplx(f,0.0d0)
               f = cs*dreal(s(k)) + sn*dreal(e(k))
               e(k) = cs*e(k) - sn*s(k)
               g = sn*dreal(s(k+1))
               s(k+1) = cs*s(k+1)
               if (wantv) call zdrot(p,v(1,k),1,v(1,k+1),1,cs,sn)
               call drotg(f,g,cs,sn)
               s(k) = dcmplx(f,0.0d0)
               f = cs*dreal(e(k)) + sn*dreal(s(k+1))
               s(k+1) = -sn*e(k) + cs*s(k+1)
               g = sn*dreal(e(k+1))
               e(k+1) = cs*e(k+1)
               if (wantu .and. k .lt. n)
     *            call zdrot(n,u(1,k),1,u(1,k+1),1,cs,sn)
  600       continue
            e(m-1) = dcmplx(f,0.0d0)
            iter = iter + 1
         go to 650
c
c        convergence.
c
  610    continue
c
c           make the singular value  positive
c
            if (dreal(s(l)) .ge. 0.0d0) go to 620
               s(l) = -s(l)
               if (wantv) call zscal(p,(-1.0d0,0.0d0),v(1,l),1)
  620       continue
c
c           order the singular value.
c
  630       if (l .eq. mm) go to 640
c           ...exit
               if (dreal(s(l)) .ge. dreal(s(l+1))) go to 640
               t = s(l)
               s(l) = s(l+1)
               s(l+1) = t
               if (wantv .and. l .lt. p)
     *            call zswap(p,v(1,l),1,v(1,l+1),1)
               if (wantu .and. l .lt. n)
     *            call zswap(n,u(1,l),1,u(1,l+1),1)
               l = l + 1
            go to 630
  640       continue
            iter = 0
            m = m - 1
  650    continue
      go to 400
  660 continue
      return
      end
