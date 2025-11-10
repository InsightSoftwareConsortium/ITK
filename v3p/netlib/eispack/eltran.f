      subroutine eltran(nm,n,low,igh,a,int,z)
c
      integer i,j,n,kl,mm,mp,nm,igh,low,mp1
      double precision a(nm,igh),z(nm,n)
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure elmtrans,
c     num. math. 16, 181-204(1970) by peters and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
c
c     this subroutine accumulates the stabilized elementary
c     similarity transformations used in the reduction of a
c     real general matrix to upper hessenberg form by  elmhes.
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
c        a contains the multipliers which were used in the
c          reduction by  elmhes  in its lower triangle
c          below the subdiagonal.
c
c        int contains information on the rows and columns
c          interchanged in the reduction by  elmhes.
c          only elements low through igh are used.
c
c     on output
c
c        z contains the transformation matrix produced in the
c          reduction by  elmhes.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
c     .......... initialize z to identity matrix ..........
      do 80 j = 1, n
c
         do 60 i = 1, n
   60    z(i,j) = 0.0d0
c
         z(j,j) = 1.0d0
   80 continue
c
      kl = igh - low - 1
      if (kl .lt. 1) go to 200
c     .......... for mp=igh-1 step -1 until low+1 do -- ..........
      do 140 mm = 1, kl
         mp = igh - mm
         mp1 = mp + 1
c
         do 100 i = mp1, igh
  100    z(i,mp) = a(i,mp-1)
c
         i = int(mp)
         if (i .eq. mp) go to 140
c
         do 130 j = mp, igh
            z(mp,j) = z(i,j)
            z(i,j) = 0.0d0
  130    continue
c
         z(i,mp) = 1.0d0
  140 continue
c
  200 return
      end
