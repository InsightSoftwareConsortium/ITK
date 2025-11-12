      subroutine dposl(a,lda,n,b)
      integer lda,n
      double precision a(lda,1),b(1)
c
c     dposl solves the double precision symmetric positive definite
c     system a * x = b
c     using the factors computed by dpoco or dpofa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dpoco or dpofa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dpoco(a,lda,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dposl(a,lda,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb
c
c     solve trans(r)*y = b
c
      do 10 k = 1, n
         t = ddot(k-1,a(1,k),1,b(1),1)
         b(k) = (b(k) - t)/a(k,k)
   10 continue
c
c     solve r*x = y
c
      do 20 kb = 1, n
         k = n + 1 - kb
         b(k) = b(k)/a(k,k)
         t = -b(k)
         call daxpy(k-1,t,a(1,k),1,b(1),1)
   20 continue
      return
      end
