      function userff(myid,n,x)
c=====================================================================
c     Compute sample fitness function (2-d landscape)
c=====================================================================
      USE evaluador

      implicit none
c
c     Input:
      integer myid
      integer n
      double precision x(n)
c
c     Output
      real*8 userff
c
c     Constant
      real pi,sigma2
      integer nn
      parameter (pi=3.1415926536,sigma2=0.15,nn=9)
c
c     Local
      real rr

      CALL dispersiones(myid, x(1), x(1), x(1), x(1), userff)

      return
      end
