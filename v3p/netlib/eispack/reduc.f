      subroutine reduc(nm,n,a,b,dl,ierr)
c
      integer i,j,k,n,i1,j1,nm,nn,ierr
      double precision a(nm,n),b(nm,n),dl(n)
      double precision x,y
c
c     this subroutine is a translation of the algol procedure reduc1,
c     num. math. 11, 99-110(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971).
c
c     this subroutine reduces the generalized symmetric eigenproblem
c     ax=(lambda)bx, where b is positive definite, to the standard
c     symmetric eigenproblem using the cholesky factorization of b.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrices a and b.  if the cholesky
c          factor l of b is already available, n should be prefixed
c          with a minus sign.
c
c        a and b contain the real symmetric input matrices.  only the
c          full upper triangles of the matrices need be supplied.  if
c          n is negative, the strict lower triangle of b contains,
c          instead, the strict lower triangle of its cholesky factor l.
c
c        dl contains, if n is negative, the diagonal elements of l.
c
c     on output
c
c        a contains in its full lower triangle the full lower triangle
c          of the symmetric matrix derived from the reduction to the
c          standard form.  the strict upper triangle of a is unaltered.
c
c        b contains in its strict lower triangle the strict lower
c          triangle of its cholesky factor l.  the full upper
c          triangle of b is unaltered.
c
c        dl contains the diagonal elements of l.
c
c        ierr is set to
c          zero       for normal return,
c          7*n+1      if b is not positive definite.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      nn = iabs(n)
      if (n .lt. 0) go to 100
c     .......... form l in the arrays b and dl ..........
      do 80 i = 1, n
         i1 = i - 1
c
         do 80 j = i, n
            x = b(i,j)
            if (i .eq. 1) go to 40
c
            do 20 k = 1, i1
   20       x = x - b(i,k) * b(j,k)
c
   40       if (j .ne. i) go to 60
            if (x .le. 0.0d0) go to 1000
            y = dsqrt(x)
            dl(i) = y
            go to 80
   60       b(j,i) = x / y
   80 continue
c     .......... form the transpose of the upper triangle of inv(l)*a
c                in the lower triangle of the array a ..........
  100 do 200 i = 1, nn
         i1 = i - 1
         y = dl(i)
c
         do 200 j = i, nn
            x = a(i,j)
            if (i .eq. 1) go to 180
c
            do 160 k = 1, i1
  160       x = x - b(i,k) * a(j,k)
c
  180       a(j,i) = x / y
  200 continue
c     .......... pre-multiply by inv(l) and overwrite ..........
      do 300 j = 1, nn
         j1 = j - 1
c
         do 300 i = j, nn
            x = a(i,j)
            if (i .eq. j) go to 240
            i1 = i - 1
c
            do 220 k = j, i1
  220       x = x - a(k,j) * b(i,k)
c
  240       if (j .eq. 1) go to 280
c
            do 260 k = 1, j1
  260       x = x - a(j,k) * b(i,k)
c
  280       a(i,j) = x / dl(i)
  300 continue
c
      go to 1001
c     .......... set error -- b is not positive definite ..........
 1000 ierr = 7 * n + 1
 1001 return
      end
