      subroutine cg_solve(ndim, a, b, x , PTetr)
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!     Vl Numerische Mathermatik
!     Prof.Dr. W. Piepke, 04.09.2009
!     Prof.Dr. W. Piepke, 08.06.2014
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
!     1. Versuch : Programm f"ur den konjugierten Gradienten Algorithmus
!     -->          ...
!
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Deklarationen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      implicit none

!..global variables..

!..global integer variables..
      integer           :: ndim
!..global integer arrays..
!..global real variables..
!..global arrays..
!..global real arrays..
      real(kind=8)      :: x(ndim), b(ndim)
      real(kind=8)      :: a(ndim, ndim)
      real(kind=8)      :: PTetr(ndim, 3)

!..local  variables..

!..local character variables..
#ifdef plevel2
      character(len=16) :: strg
      integer           :: istrg
      integer           :: maxprint
#endif
!..local  integer variables..
      integer           :: info
      integer           :: i
      integer           :: it
      integer           :: j
      integer           :: k
      integer           :: nrhs
!..local  real variables..
      real(kind=8)      :: delta
      real(kind=8)      :: eps
      real(kind=8)      :: beta
      real(kind=8)      :: lambda
      real(kind=8)      :: proof_l2sq
      real(kind=8)      :: dlamch
      real(kind=8)      :: temp
      real(kind=8)      :: u
      real(kind=8)      :: xnetz
      real(kind=8)      :: ynetz
      real(kind=8)      :: znetz
!..local  real arrays..
!     real(kind=8), dimension(:), allocatable ::      g_alt
!     real(kind=8), dimension(:), allocatable ::      g_neu
      real(kind=8), dimension(:), allocatable ::      p
      real(kind=8), dimension(:), allocatable ::      r
      real(kind=8), dimension(:), allocatable ::      v
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Fuer die Zeitmessung
!     integer           mclock
      real              cpu_sekunden, st0, ste
      DOUBLE PRECISION   DSECND
      EXTERNAL           DSECND
!-----|--1---------2---------3---------4---------5---------6---------7--|
      external dlamch
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Anweisungen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
#ifdef plevel2
      STRG='                 '
#endif
!
!     allocate(g_alt(ndim))
!     allocate(g_neu(ndim))
      allocate(p(ndim))
      allocate(r(ndim))
      allocate(v(ndim))
!
#ifdef plevel1
      print*,' cg_solve : ndim  = ',ndim
#endif
      eps = epsilon(0d0)
      eps = eps*100.0d+00
      print*,' cg_solve : relative machine precision epsilon ',         &
               ' eps = ',eps
!     Algorithmusanfang :
      nrhs = 1
      print*,' cg_solve : Vor Conjugate-Gradient-Algorithmus'
      call cpu_time(st0)
!
      x = 20.0d+00
      p = b - matmul(a,x)                                       ! p = Suchrichtung
      r = b - matmul(a,x)
      proof_l2sq = dot_product(r, r)                            ! r = Residuum
      it = 0
      do WHILE ( (it .le. ndim) .and. (proof_l2sq .gt. eps) )
         v   = matmul(a, p)                                     ! v(m) = A*p(m)
         lambda = proof_l2sq/dot_product(p,v)
         x      = x + lambda * p
         r      = r - lambda * v                                ! r(m+1) = r(m) - lambda(m)*u(m)
         temp   = proof_l2sq                                    ! Pruefgroesse(m)
         proof_l2sq = dot_product(r, r)                         ! Pruefgroesse(m+1)
         beta   = proof_l2sq / temp                             ! beta = [r(m+1)*r(m+1)]/[r(m)*r(m)]
         p      = r + beta * p                                  ! p(m+1) = r(m+1) + [r(m+1)*r(m+1)]/[r(m)*r(m)] * p(m)
         it = it + 1
         write(unit=6, fmt=9900) it, proof_l2sq
      end do
      call cpu_time(ste)
      print*,' cg_solve : Nach Conjugate-Gradient-Algorithmus'
      if( it .ge. ndim ) then
         print*,' cg_solve : Nicht konvergiert --> it = ',it
      else
         print*,' cg_solve :       konvergiert --> it = ',it
      end if
      cpu_sekunden = (ste - st0)
      print*,' cg_solve : Dazu wurden ',cpu_sekunden,                   &
             '            CPU--Sekunden benoetigt.'
#ifdef plevel3
      do i = 1, ndim
         print*,' cg_solve : Knotennr. = ',i,' x_i = ',x(i)
      end do
#endif
!     Algorithmusende
      k = 1
      open(unit=33, FILE='fort.33', Status='UNKNOWN')
!                                                                             h = 1.0   0.5   0.25   0.125
      do i = 1, ndim
         xnetz = PTetr(i, 1)
         ynetz = PTetr(i, 2)
         znetz = PTetr(i, 3)
         u = 20.0 - 2.0*ynetz**2                                        &
               + xnetz**3*ynetz - xnetz*ynetz**3                        &
               + znetz**3*xnetz - znetz*xnetz**3
         delta = u - x(i)
         write( unit=33, fmt=9000 )i, xnetz, ynetz, znetz, x(i), u,delta! Randtemp. unten   3     5     9
      end do
      close( unit = 33 )
      call system('gnuplot cg_Tetraed.dem')
!
!     deallocate(g_alt)
!     deallocate(g_neu)
      deallocate(p)
      deallocate(r)
      deallocate(v)
!
 9000 format(i6,' ',6(' ',e16.8))
 9900 format(' cg : it = ',i5,' proof = ',e16.8)
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Programmbeendung :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      STOP
      END
