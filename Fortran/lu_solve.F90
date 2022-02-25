      subroutine lu_solve(ndim, A, b, xLoes, Pneu)
!     subroutine lu_solve(ndim, n_tetr_pkt, 
!    &                    Tetraederzaehler_neu, n_tetr, TNneu,
!    &                    A, Aalt, b, balt, Pneu, xLoes)
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!     Vl Numerische Mathermatik
!     Prof.Dr. W. Piepke, 04.09.2009
!     Prof.Dr. W. Piepke, 15.11.2012
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
!     Version : Programm f"ur die direkte Loesung mit Gauss--Algorithmus
!     2.Versuch :  LU-Zerlegung mit LAPACK
!     -->          ...
!
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Deklarationen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      implicit none

!..global variables..

!..global integer variables..
      integer                                    :: ndim
!     integer                                    :: n_tetr_pkt
!     integer                                    :: Tetraederzaehler_neu
!     integer                                    :: n_tetr
!..global integer variables..
!     integer                                    :: TNneu(0:n_tetr, 4)
!..global real variables..
!..global arrays..

!..global integer arrays..
!..global real arrays..
      real(kind=8), dimension(:)                 :: xLoes(ndim)
      real(kind=8), dimension(:)                 :: b(ndim)
!     real(kind=8), dimension(:)                 :: balt(ndim)
      real(kind=8), dimension(:,:)               :: A(ndim, ndim)
!     real(kind=8), dimension(:,:)               :: Aalt(ndim, ndim)
      real(kind=8), dimension(:,:)               :: Pneu(ndim, 3)

!..local  variables..

!..local character variables..
      character(len=1)                           :: equed
      character(len=1)                           :: fact
      character(len=16)                          :: strg
      character(len=1)                           :: trans
!..local  integer variables..
      integer                                    :: istrg
      integer                                    :: i
      integer                                    :: ti
      integer                                    :: info
      integer                                    :: it
      integer                                    :: j
      integer                                    :: maxprint
      integer                                    :: nrhs
!..local  real variables..
      real(kind=8)                               :: delta
      real(kind=8)                               :: dlamch
      real(kind=8)                               :: eps
      real(kind=8)                               :: rcond
      real(kind=8)                               :: xnetz
      real(kind=8)                               :: u
      real(kind=8)                               :: ynetz
      real(kind=8)                               :: znetz
!     real(kind=8)                               :: Residuum_Tetr
!     real(kind=8)                               :: r1, r2, r3, r4
!..local integer arrays..
      integer     , dimension(:)  , allocatable  :: ipiv                ! Pivottauschspeicher
      integer     , dimension(:)  , allocatable  :: iwork               ! 
!..local real arrays..
      real(kind=8), dimension(:)   , allocatable :: berr                ! 
      real(kind=8), dimension(:)   , allocatable :: ferr                ! 
      real(kind=8), dimension(:)   , allocatable :: r                   ! 
      real(kind=8), dimension(:)   , allocatable :: c                   ! 
      real(kind=8), dimension(:)   , allocatable :: work                ! 
      real(kind=8), dimension(:, :), allocatable :: af                  ! L\U-Speicher
!     integer                                    :: k1
!     integer                                    :: k2
!     integer                                    :: k3
!     integer                                    :: k4
!     real(kind=8), dimension(4, 4)              :: ATetr               ! 
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Fuer die CPU-Zeitmessung
!     integer           mclock
      real(kind=8)                               :: cpu_sekunden
      real(kind=8)                               :: st0
      real(kind=8)                               :: ste
      real(kind=8)                               :: dsecnd
      external           dsecnd
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     external slamch
      external dlamch
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Anweisungen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
      allocate ( ferr(ndim) )
      allocate ( berr(ndim) )
      allocate ( ipiv(ndim) )
      allocate ( iwork(ndim) )
      allocate ( r(ndim) )
      allocate ( c(ndim) )
      allocate ( work(4*ndim) )
      allocate ( af(ndim, ndim) )
!
!     eps = 0.001d+00
!     eps = 0.0001d+00
!     eps = 0.00001d+00
!     eps = 0.000001d+00
!     eps = 0.0000001d+00
!     eps = 0.00000001d+00
!     eps    = 0.000000001d+00
!     eps    = 0.0000000001d+00
!     eps    = 0.00000000001d+00
!     eps    = 0.000000000001d+00
      STRG   ='                 '
!
#ifdef plevel1
      print*,' lu-solve : ndim  = ',ndim
#endif
      eps = epsilon(0d0)
#ifdef plevel1
      print*,' lu-solve : used averaged relative machine precision ', &
               ' epsilon eps      = ',eps
#endif
!     Algorithmusanfang :
      call cpu_time(st0)
!     Vorgabe des Anfangswertes :
      xLoes  = 0.0d+00
#ifdef plevel3
      ISTRG = 3
      STRG = ' A '
      if ( ndim .gt. 25 ) then
         maxprint = 25
      else
         maxprint = ndim
      end if
      print*,' lu-solve : A-Matrix print :'
      call dprintr(istrg, strg, a, ndim, ndim, maxprint, maxprint, &
                  1, 1, maxprint, maxprint)
      print*,' lu-solve : b-Vector print :'
      do i = 1, ndim
         write(unit = 6, fmt = 2000) i, b(i)
      end do
#endif
      nrhs = 1
      xLoes = b
!     call dgesv( ndim, nrhs, a, ndim, ipiv, xLoes, ndim, info )
      equed = 'N'
      FACT  = 'N'
      TRANS = 'N'
      call dgesvx(fact, trans, ndim, nrhs, a, ndim, af, ndim, ipiv,    &
                  equed, r, c, b, ndim, xLoes, ndim, rcond, ferr, berr,&
                  work, iwork, info )
!     Ergebnisausgabe :
      print*,' '
      print*,' lu-solve : info = ', info,' <-- LAPACK'
      print*,' '
      PRINT*,' Reciprocal condition number : 1/COND = ',RCOND
      PRINT*,' RCOND should not be smaller than epsilon = ',eps,' !'
      PRINT*,'            Condition number :   COND = ',1.0d+00/RCOND
      PRINT*,' '
      print*,' WORK(1) = ', WORK(1),' << 1 ? <-- LAPACK'
      PRINT*,' '
      IF (INFO .EQ. 0) THEN
         PRINT*,' INFO     = 0:  successful exit'
      ELSE
         PRINT*,' INFO = ',INFO
         PRINT*, &
      ' INFO<0:  if INFO = -i, the i-th argument had an illegal value'
         PRINT*, &
      ' INFO>0:  if INFO = i, and i is'
         PRINT*, &
      '          <= N:  U(i,i) is exactly zero. The factorization has'
         PRINT*, &
      '                 been completed, but the factor U is exactly'
         PRINT*, &
      '                 singular, so the solution and error bounds'
         PRINT*, &
      '                 could not be computed.'
         PRINT*, &
      '          = N+1: RCOND is less than machine precision.'
         PRINT*, &
      '                 RCOND is the condition number of matrix A.'
         PRINT*, &
      '                 The factorization has been completed, but the'
         PRINT*, &
      '                 matrix is singular to working precision, and'
         PRINT*, &
      '                 the solution and error bounds have not been'
         PRINT*,'                 computed.'
      end if
#ifdef plevel3
!                       Der Loesungsvektor
      print*,' lu-solve : Loesungsvektor X :'
      print*,' '
      do i = 1, ndim
         write(unit = 6, fmt = 2100) i, xLoes(i)
      end do
#endif
#ifdef linLoes
      print*,' lu_solve : Lineare Loesungsfunktion vorgegeben.'
#endif
#ifdef quadLoes
      print*,' lu_solve : Quadratische Loesungsfunktion vorgegeben.'
#endif
#ifdef kubLoes
      print*,' lu_solve : Kubische Loesungsfunktion vorgegeben.'
#endif
      eps     = 0.1d-06
      do 280 I = 1, Ndim
         xnetz = Pneu(i, 1)
         ynetz = Pneu(i, 2)
         znetz = Pneu(i, 3)
#ifdef linLoes
         u = xnetz + ynetz + znetz
#endif
#ifdef quadLoes
         u = 20.0d+00 + xnetz**2 + ynetz**2 + znetz**2
!        u = xnetz + ynetz + znetz + eps * xnetz**2
#endif
#ifdef kubLoes
         u = 20.0 - 2.0*ynetz**2                                        &
               + xnetz**3*ynetz - xnetz*ynetz**3                        &
               + znetz**3*xnetz - znetz*xnetz**3
#endif
         delta = u - xLoes(i)
         write( unit=33, fmt=9000 )i,xnetz,ynetz,znetz,xLoes(i),u,delta
  280 continue
!
!      Residuum pro Tetraeder
!
!      do ti = 1, Tetraederzaehler_neu
!         k1 = TNneu(ti,1)
!         k2 = TNneu(ti,2)
!         k3 = TNneu(ti,3)
!         k4 = TNneu(ti,4)
!!
!         r1 = 0.0d+00
!         r2 = 0.0d+00
!         r3 = 0.0d+00
!         r4 = 0.0d+00
!         do i = 1, ndim
!            r1 = r1 + Aalt(k1, i) * xLoes(i)
!            r2 = r2 + Aalt(k2, i) * xLoes(i)
!            r3 = r3 + Aalt(k3, i) * xLoes(i)
!            r4 = r4 + Aalt(k4, i) * xLoes(i)
!         end do
!!
!         r1 = r1 - balt(k1)
!         r2 = r2 - balt(k2)
!         r3 = r3 - balt(k3)
!         r4 = r4 - balt(k4)
!         Residuum_Tetr = r1 + r2 + r3 + r4
!!        write(unit =  6, fmt = 1000) ti, k1, k2, k3, k4, Residuum_Tetr
!         write(unit = 88, fmt = 1100) ti, k1, k2, k3, k4, Residuum_Tetr
!      end do
      call cpu_time(ste)
      cpu_sekunden = (ste - st0)
      print*,' Dazu wurden ',cpu_sekunden,' CPU--Sekunden benoetigt.'
      close( unit = 33 )
      call system('gnuplot cg_Tetraed.dem')
      deallocate ( berr )
      deallocate ( ferr )
      deallocate ( ipiv )
      deallocate ( iwork )
      deallocate ( r )
      deallocate ( c )
      deallocate ( work )
      deallocate ( af )
!-----|--1---------2---------3---------4---------5---------6---------7--|
!1000 format(' TetrNr.: ',i6,' (',i6,',',i6,',',i6,',',i6,              &
!            ') Residuum =', d16.8)
!1100 format(5(' ',i6), ' ',e16.8)
 2000 format(' b(',i6,') = ',e16.8)
 2100 format(' x(',i6,') = ',e16.8)
!3000 format('!=',i6,6(' ',d16.8))
!3100 format(' =',i6,6(' ',d16.8))
 9000 format(i6,' ',6(' ',e16.8))
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Programmbeendung :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      return
      END
