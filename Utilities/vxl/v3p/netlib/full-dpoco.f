      subroutine dpoco(a,lda,n,rcond,z,info)
      integer lda,n,info
      double precision a(lda,1),z(1)
      double precision rcond
c
c     dpoco factors a double precision symmetric positive definite
c     matrix and estimates the condition of the matrix.
c
c     if  rcond  is not needed, dpofa is slightly faster.
c     to solve  a*x = b , follow dpoco by dposl.
c     to compute  inverse(a)*c , follow dpoco by dposl.
c     to compute  determinant(a) , follow dpoco by dpodi.
c     to compute  inverse(a) , follow dpoco by dpodi.
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        rcond   double precision
c                an estimate of the reciprocal condition of  a .
c                for the system  a*x = b , relative perturbations
c                in  a  and  b  of size  epsilon  may cause
c                relative perturbations in  x  of size  epsilon/rcond .
c                if  rcond  is so small that the logical expression
c                           1.0 + rcond .eq. 1.0
c                is true, then  a  may be singular to working
c                precision.  in particular,  rcond  is zero  if
c                exact singularity is detected or the estimate
c                underflows.  if info .ne. 0 , rcond is unchanged.
c
c        z       double precision(n)
c                a work vector whose contents are usually unimportant.
c                if  a  is close to a singular matrix, then  z  is
c                an approximate null vector in the sense that
c                norm(a*z) = rcond*norm(a)*norm(z) .
c                if  info .ne. 0 , z  is unchanged.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     linpack dpofa
c     blas daxpy,ddot,dscal,dasum
c     fortran dabs,dmax1,dreal,dsign
c
c     internal variables
c
      double precision ddot,ek,t,wk,wkm
      double precision anorm,s,dasum,sm,ynorm
      integer i,j,jm1,k,kb,kp1
c
c
c     find norm of a using only upper half
c
      do 30 j = 1, n
         z(j) = dasum(j,a(1,j),1)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 20
         do 10 i = 1, jm1
            z(i) = z(i) + dabs(a(i,j))
   10    continue
   20    continue
   30 continue
      anorm = 0.0d0
      do 40 j = 1, n
         anorm = dmax1(anorm,z(j))
   40 continue
c
c     factor
c
      call dpofa(a,lda,n,info)
      if (info .ne. 0) go to 180
c
c        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
c        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e .
c        the components of  e  are chosen to cause maximum local
c        growth in the elements of w  where  trans(r)*w = e .
c        the vectors are frequently rescaled to avoid overflow.
c
c        solve trans(r)*w = e
c
         ek = 1.0d0
         do 50 j = 1, n
            z(j) = 0.0d0
   50    continue
         do 110 k = 1, n
            if (z(k) .ne. 0.0d0) ek = dsign(ek,-z(k))
            if (dabs(ek-z(k)) .le. a(k,k)) go to 60
               s = a(k,k)/dabs(ek-z(k))
               call dscal(n,s,z,1)
               ek = s*ek
   60       continue
            wk = ek - z(k)
            wkm = -ek - z(k)
            s = dabs(wk)
            sm = dabs(wkm)
            wk = wk/a(k,k)
            wkm = wkm/a(k,k)
            kp1 = k + 1
            if (kp1 .gt. n) go to 100
               do 70 j = kp1, n
                  sm = sm + dabs(z(j)+wkm*a(k,j))
                  z(j) = z(j) + wk*a(k,j)
                  s = s + dabs(z(j))
   70          continue
               if (s .ge. sm) go to 90
                  t = wkm - wk
                  wk = wkm
                  do 80 j = kp1, n
                     z(j) = z(j) + t*a(k,j)
   80             continue
   90          continue
  100       continue
            z(k) = wk
  110    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
c        solve r*y = w
c
         do 130 kb = 1, n
            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 120
               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
  120       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  130    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
c
         ynorm = 1.0d0
c
c        solve trans(r)*v = y
c
         do 150 k = 1, n
            z(k) = z(k) - ddot(k-1,a(1,k),1,z(1),1)
            if (dabs(z(k)) .le. a(k,k)) go to 140
               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
               ynorm = s*ynorm
  140       continue
            z(k) = z(k)/a(k,k)
  150    continue
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm
c
c        solve r*z = v
c
         do 170 kb = 1, n
            k = n + 1 - kb
            if (dabs(z(k)) .le. a(k,k)) go to 160
               s = a(k,k)/dabs(z(k))
               call dscal(n,s,z,1)
               ynorm = s*ynorm
  160       continue
            z(k) = z(k)/a(k,k)
            t = -z(k)
            call daxpy(k-1,t,a(1,k),1,z(1),1)
  170    continue
c        make znorm = 1.0
         s = 1.0d0/dasum(n,z,1)
         call dscal(n,s,z,1)
         ynorm = s*ynorm
c
         if (anorm .ne. 0.0d0) rcond = ynorm/anorm
         if (anorm .eq. 0.0d0) rcond = 0.0d0
  180 continue
      return
      end
      double precision function dasum(n,dx,incx)
c
c     takes the sum of the absolute values.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dtemp
      integer i,incx,m,mp1,n,nincx
c
      dasum = 0.0d0
      dtemp = 0.0d0
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dtemp = dtemp + dabs(dx(i))
   10 continue
      dasum = dtemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dabs(dx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2))
     *  + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
   50 continue
   60 dasum = dtemp
      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine dpofa(a,lda,n,info)
      integer lda,n,info
      double precision a(lda,1)
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c
c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c
c     internal variables
c
      double precision ddot,t
      double precision s
      integer j,jm1,k
c     begin block with ...exits to 40
c
c
         do 30 j = 1, n
            info = j
            s = 0.0d0
            jm1 = j - 1
            if (jm1 .lt. 1) go to 20
            do 10 k = 1, jm1
               t = a(k,j) - ddot(k-1,a(1,k),1,a(1,j),1)
               t = t/a(k,k)
               a(k,j) = t
               s = s + t*t
   10       continue
   20       continue
            s = a(j,j) - s
c     ......exit
            if (s .le. 0.0d0) go to 40
            a(j,j) = dsqrt(s)
   30    continue
         info = 0
   40 continue
      return
      end
