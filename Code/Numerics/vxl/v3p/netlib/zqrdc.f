      subroutine zqrdc(x,ldx,n,p,qraux,jpvt,work,job)
      integer ldx,n,p,job
      integer jpvt(1)
      complex*16 x(ldx,1),qraux(1),work(1)
c
c     zqrdc uses householder transformations to compute the qr
c     factorization of an n by p matrix x.  column pivoting
c     based on the 2-norms of the reduced columns may be
c     performed at the users option.
c
c     on entry
c
c        x       complex*16(ldx,p), where ldx .ge. n.
c                x contains the matrix whose decomposition is to be
c                computed.
c
c        ldx     integer.
c                ldx is the leading dimension of the array x.
c
c        n       integer.
c                n is the number of rows of the matrix x.
c
c        p       integer.
c                p is the number of columns of the matrix x.
c
c        jpvt    integer(p).
c                jpvt contains integers that control the selection
c                of the pivot columns.  the k-th column x(k) of x
c                is placed in one of three classes according to the
c                value of jpvt(k).
c
c                   if jpvt(k) .gt. 0, then x(k) is an initial
c                                      column.
c
c                   if jpvt(k) .eq. 0, then x(k) is a free column.
c
c                   if jpvt(k) .lt. 0, then x(k) is a final column.
c
c                before the decomposition is computed, initial columns
c                are moved to the beginning of the array x and final
c                columns to the end.  both initial and final columns
c                are frozen in place during the computation and only
c                free columns are moved.  at the k-th stage of the
c                reduction, if x(k) is occupied by a free column
c                it is interchanged with the free column of largest
c                reduced norm.  jpvt is not referenced if
c                job .eq. 0.
c
c        work    complex*16(p).
c                work is a work array.  work is not referenced if
c                job .eq. 0.
c
c        job     integer.
c                job is an integer that initiates column pivoting.
c                if job .eq. 0, no pivoting is done.
c                if job .ne. 0, pivoting is done.
c
c     on return
c
c        x       x contains in its upper triangle the upper
c                triangular matrix r of the qr factorization.
c                below its diagonal x contains information from
c                which the unitary part of the decomposition
c                can be recovered.  note that if pivoting has
c                been requested, the decomposition is not that
c                of the original matrix x but that of x
c                with its columns permuted as described by jpvt.
c
c        qraux   complex*16(p).
c                qraux contains further information required to recover
c                the unitary part of the decomposition.
c
c        jpvt    jpvt(k) contains the index of the column of the
c                original matrix that has been interchanged into
c                the k-th column, if pivoting was requested.
c
c     linpack. this version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c
c     zqrdc uses the following functions and subprograms.
c
c     blas zaxpy,zdotc,zscal,zswap,dznrm2
c     fortran dabs,dmax1,cdabs,dcmplx,cdsqrt,min0
c
c     internal variables
c
      integer j,jp,l,lp1,lup,maxj,pl,pu
      double precision maxnrm,dznrm2,tt
      complex*16 zdotc,nrmxl,t
      logical negj,swapj
c
      complex*16 csign,zdum,zdum1,zdum2
      double precision cabs1
      double precision dreal,dimag
      complex*16 zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      csign(zdum1,zdum2) = cdabs(zdum1)*(zdum2/cdabs(zdum2))
      cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum))
c
      pl = 1
      pu = 0
      if (job .eq. 0) go to 60
c
c        pivoting has been requested.  rearrange the columns
c        according to jpvt.
c
         do 20 j = 1, p
            swapj = jpvt(j) .gt. 0
            negj = jpvt(j) .lt. 0
            jpvt(j) = j
            if (negj) jpvt(j) = -j
            if (.not.swapj) go to 10
               if (j .ne. pl) call zswap(n,x(1,pl),1,x(1,j),1)
               jpvt(j) = jpvt(pl)
               jpvt(pl) = j
               pl = pl + 1
   10       continue
   20    continue
         pu = p
         do 50 jj = 1, p
            j = p - jj + 1
            if (jpvt(j) .ge. 0) go to 40
               jpvt(j) = -jpvt(j)
               if (j .eq. pu) go to 30
                  call zswap(n,x(1,pu),1,x(1,j),1)
                  jp = jpvt(pu)
                  jpvt(pu) = jpvt(j)
                  jpvt(j) = jp
   30          continue
               pu = pu - 1
   40       continue
   50    continue
   60 continue
c
c     compute the norms of the free columns.
c
      if (pu .lt. pl) go to 80
      do 70 j = pl, pu
         qraux(j) = dcmplx(dznrm2(n,x(1,j),1),0.0d0)
         work(j) = qraux(j)
   70 continue
   80 continue
c
c     perform the householder reduction of x.
c
      lup = min0(n,p)
      do 200 l = 1, lup
         if (l .lt. pl .or. l .ge. pu) go to 120
c
c           locate the column of largest norm and bring it
c           into the pivot position.
c
            maxnrm = 0.0d0
            maxj = l
            do 100 j = l, pu
               if (dreal(qraux(j)) .le. maxnrm) go to 90
                  maxnrm = dreal(qraux(j))
                  maxj = j
   90          continue
  100       continue
            if (maxj .eq. l) go to 110
               call zswap(n,x(1,l),1,x(1,maxj),1)
               qraux(maxj) = qraux(l)
               work(maxj) = work(l)
               jp = jpvt(maxj)
               jpvt(maxj) = jpvt(l)
               jpvt(l) = jp
  110       continue
  120    continue
         qraux(l) = (0.0d0,0.0d0)
         if (l .eq. n) go to 190
c
c           compute the householder transformation for column l.
c
            nrmxl = dcmplx(dznrm2(n-l+1,x(l,l),1),0.0d0)
            if (cabs1(nrmxl) .eq. 0.0d0) go to 180
               if (cabs1(x(l,l)) .ne. 0.0d0)
     *            nrmxl = csign(nrmxl,x(l,l))
               call zscal(n-l+1,(1.0d0,0.0d0)/nrmxl,x(l,l),1)
               x(l,l) = (1.0d0,0.0d0) + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               lp1 = l + 1
               if (p .lt. lp1) go to 170
               do 160 j = lp1, p
                  t = -zdotc(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                  call zaxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                  if (j .lt. pl .or. j .gt. pu) go to 150
                  if (cabs1(qraux(j)) .eq. 0.0d0) go to 150
                     tt = 1.0d0 - (cdabs(x(l,j))/dreal(qraux(j)))**2
                     tt = dmax1(tt,0.0d0)
                     t = dcmplx(tt,0.0d0)
                     tt = 1.0d0
     *                    + 0.05d0*tt
     *                      *(dreal(qraux(j))/dreal(work(j)))**2
                     if (tt .eq. 1.0d0) go to 130
                        qraux(j) = qraux(j)*cdsqrt(t)
                     go to 140
  130                continue
                        qraux(j) = dcmplx(dznrm2(n-l,x(l+1,j),1),0.0d0)
                        work(j) = qraux(j)
  140                continue
  150             continue
  160          continue
  170          continue
c
c              save the transformation.
c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue
  190    continue
  200 continue
      return
      end
