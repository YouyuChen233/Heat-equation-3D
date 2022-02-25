      subroutine bicgstab_solve(meshdim,mesh,RP,opt)
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!     Vl Numerische Mathermatik
!     Prof.Dr. W. Piepke, 04.09.2009
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
!     1. Versuch : Programm f"ur den iterativen Gauss--Seidelalgorithmus
!     -->          Determinante ungleich 0 wird nicht ueberprueft !
!     -->          Dies ist kein Algoritmus fuer schlecht 
!                  konditionierte (ill-conditioned) Gleichungssysteme.
!     -->          ...
!     2. Versuch CG-Methode
!     3. Versuch BiCGStab-Methode
!
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Deklarationen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      implicit none

!..global variables..

!..global integer variables..
      integer           :: ndim
      integer           :: pmRdim
!..global integer arrays..
!..global real variables..
!..global arrays..

!..global real arrays..
      type                                      :: Netzdim
              integer :: npkt
              integer :: nelement
      end type Netzdim
      type                                      :: Netz
              real(kind=8),dimension(:,:),allocatable :: Pkt
              integer,dimension(:,:),allocatable      :: Tetr
      end type Netz
      type                                      :: RandPkt
              integer,dimension(:),allocatable        :: IPkt
              integer,dimension(:,:),allocatable      :: RPkt
              integer                     :: indim
      end type RandPkt
      type                                      :: OptGLS
              integer                                   :: optdim
              real(kind=8),dimension(:),allocatable     :: optb
              real(kind=8),dimension(:,:),allocatable   :: opts
              real(kind=8),dimension(:),allocatable     :: temp
              real(kind=8),dimension(:,:),allocatable   :: opttmp
              real(kind=8),dimension(:),allocatable     :: optx
              real(kind=8),dimension(:,:),allocatable   :: optp
      end type OptGLS
      type(Netzdim)     ::meshdim
      type(Netz)        ::mesh
      type(OptGLS)      ::opt
      type(RandPkt)     ::RP
      real(kind=8)      :: x(opt%optdim), b(opt%optdim)
      real(kind=8)      :: a(opt%optdim,opt%optdim)
      real(kind=8)      :: PTetr(opt%optdim, 3)
      real(kind=8)      :: Pkt(meshdim%npkt, 3)
      real(kind=8)      :: Temperatur(meshdim%npkt)
      integer           :: InnerePkt(meshdim%npkt)

!..local  variables..

!..local character variables..
#ifdef plevel2
      character(len=16) :: strg
      integer    istrg
      integer    maxprint
#endif
!..local  integer variables..
      integer    i
      integer    j
      integer    it
      integer    k
      integer    nrhs
!..local  real variables..
      real(kind=8)     :: alpha
      real(kind=8)     :: beta
      real(kind=8)     :: delta
      real(kind=8)     :: omega
      real(kind=8)     :: rho
      real(kind=8)     :: rho_alt
      real(kind=8)     :: eps
      real(kind=8)     :: u
      real(kind=8)     :: xnetz
      real(kind=8)     :: ynetz
      real(kind=8)     :: znetz
      real(kind=8)     :: skalar
!..local  real arrays..
!     real(kind=8), dimension(:), allocatable ::      g_alt
!     real(kind=8), dimension(:), allocatable ::      g_neu
      real(kind=8), dimension(:), allocatable ::      p
      real(kind=8), dimension(:), allocatable ::      r
      real(kind=8), dimension(:), allocatable ::      r0
      real(kind=8), dimension(:), allocatable ::      v
      real(kind=8), dimension(:), allocatable ::      s
      real(kind=8), dimension(:), allocatable ::      t
      real(kind=8), dimension(:), allocatable ::      tmp
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Fuer die Zeitmessung
!     integer           mclock
      real              cpu_sekunden, st0, ste
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
      pmRdim=opt%optdim
      allocate(p(pmRdim))
      allocate(r(pmRdim))
      allocate(v(pmRdim))
      allocate(r0(pmRdim))
      allocate(s(pmRdim))
      allocate(t(pmRdim))
      allocate(tmp(pmRdim))
      ndim=meshdim%npkt
      a=opt%opts
      b=opt%optb
      x=opt%optx
      PTetr=opt%optp
      Temperatur=opt%temp
      InnerePkt=RP%IPkt
      Pkt=mesh%Pkt
!
#ifdef plevel1
      print*,' BiCGStab_solve : ndim  = ',pmRdim
#endif
      eps = epsilon(0d0)
      eps = eps*0.01d+00                        ! bei 924 Knoten ok
      eps = eps*0.01d+00                        ! bei 924 Knoten ok
!     eps = 0.00000001e+00
      print*,' BiCGStab_solve : relative machine precision epsilon ',&
               ' eps = ',eps
!     Algorithmusanfang :
      nrhs = 1
      print*,' BiCGStab_solve : Vor Conjugate-Gradient-Algorithmus'
      call cpu_time(st0)
#ifdef plevel3
      write( unit = 55, fmt = '(i8)' ) ndim
      do i =1, ndim
         write( unit = 55, fmt = '(e16.8)' ) b(i) 
      end do
      do i =1, ndim
         do k =1, ndim
            write( unit = 55, fmt = '(e16.8)' ) a(i, k)  
         end do
      end do
#endif
!
      x = 3.0d+00
      call matmulvkt(tmp,a,x,pmRdim,pmRdim,pmRdim)
      p=b-tmp
!      p = b - matmul(a,x)                                       ! p = Suchrichtung
      r = b - tmp
!      r = b - matmul(a,x)
      r0 = r
!      rho = dot_product(r, r)                                   ! r = Residuum
      rho=skalar(r,r,pmRdim,pmRdim)
      it = 0
!     do WHILE ( (it .le. ndim) .and. (dot_product(r,r) .gt. eps) )
#ifdef plevel1
      print*,' BiCGStab_solve : Vor der Iterationsschleife'
#endif
!      do WHILE (                      (dot_product(r,r) .gt. eps) )
      do WHILE ( (skalar(r,r,pmRdim,pmRdim) .gt. eps) )
!         v      = matmul(a, p)                                  ! v(m) = A*p(m)
         call matmulvkt(v,a,p,pmRdim,pmRdim,pmRdim)
!         alpha  = rho / dot_product(v, r0)
         alpha  = rho / skalar(v, r0, pmRdim, pmRdim)
         s      = r - alpha*v
!         t      = matmul(a, s)
         call matmulvkt(t,a,s,pmRdim,pmRdim,pmRdim)
!         omega  = dot_product(t, s) / dot_product(t, t)
         omega  = skalar(t, s, pmRdim, pmRdim) &
                  / skalar(t, t, pmRdim,pmRdim)
         x      = x + alpha * p + omega*s
         r      = s - omega * t                                 ! r(m+1) = r(m) - lambda(m)*u(m)
         rho_alt = rho
!         rho = dot_product(r, r0)                               ! Pruefgroesse(m+1)
         rho = skalar(r, r0, pmRdim, pmRdim)                         ! Pruefgroesse(m+1)
         beta   = alpha/omega * rho/rho_alt                     ! beta = [r(m+1)*r(m+1)]/[r(m)*r(m)]
         p      = r + beta * (p - omega*v)                      ! p(m+1) = r(m+1) + [r(m+1)*r(m+1)]/[r(m)*r(m)] * p(m)
         it = it + 1
!         write(unit=6, fmt=9900) it, dot_product(r,r)
         write(unit=6, fmt=9900) it, skalar(r,r,pmRdim,pmRdim)
!         write(unit = 68, fmt = *) it, dot_product(r,r)
         write(unit = 68, fmt = *) it, skalar(r,r,pmRdim,pmRdim)
	 write(67,*)alpha,beta,omega,rho,rho_alt
      end do
#ifdef plevel1
      print*,' BiCGStab_solve : Nach der Iterationsschleife'
#endif
      call cpu_time(ste)
      print*,' BiCGStab_solve : Nach Conjugate-Gradient-Algorithmus'
      if( it .ge. ndim ) then
         print*,' BiCGStab_solve : Nicht konvergiert --> it = ',it
      else
         print*,' BiCGStab_solve :       konvergiert --> it = ',it
      end if
      cpu_sekunden = (ste - st0)
      print*,' BiCGStab_solve : Dazu wurden ',cpu_sekunden,      &
             '            CPU--Sekunden benoetigt.'
#ifdef plevel3
      do i = 1, ndim
         print*,' BiCGStab_solve : Knotennr. = ',i,' x_i = ',x(i)
      end do
#endif
!
!     Algorithmusende
      k = 1
      open(unit=33, FILE='fort.33', Status='UNKNOWN')
!                                                                             h = 1.0   0.5   0.25   0.125
!      do i = 1, pmRdim
!         xnetz = PTetr(i, 1)
!         ynetz = PTetr(i, 2)
!         znetz = PTetr(i, 3)
!         u = 20.0d+00 - 2.0d+00*ynetz**2                          &
!               + xnetz**3*ynetz - xnetz*ynetz**3                  &
!               + znetz**3*xnetz - znetz*xnetz**3
!         delta = u - x(i)
!         write( unit=33, fmt=9000 )i, xnetz, ynetz, znetz, x(i), u,delta! Randtemp. unten   3     5     9
!      end do
      j=1
      do i=1,ndim
      if(InnerePkt(i) .lt. 0) then
              xnetz = Pkt(i, 1)
              ynetz = Pkt(i, 2)
              znetz = Pkt(i, 3)
              u = 20.0d+00 - 2.0d+00*ynetz**2                          &
                      + xnetz**3*ynetz - xnetz*ynetz**3                &
                      + znetz**3*xnetz - znetz*xnetz**3
              delta = u - Temperatur(i)
              write( unit=33, fmt=9000 )i, xnetz, ynetz, znetz,        &
                                        Temperatur(i), u,delta! Randtemp. unten   3     5     9
      else
              xnetz = PTetr(j, 1)
              ynetz = PTetr(j, 2)
              znetz = PTetr(j, 3)
              u = 20.0d+00 - 2.0d+00*ynetz**2                          &
                    + xnetz**3*ynetz - xnetz*ynetz**3                  &
                    + znetz**3*xnetz - znetz*xnetz**3
              delta = u - x(j)
              write( unit=33, fmt=9000 )i, xnetz, ynetz, znetz, x(j),  &
                                        u,delta
              j=j+1


      end if
      end do
      close( unit = 33 )
      call system('gnuplot cg_Tetraed.dem')
!
!     deallocate(g_alt)
!     deallocate(g_neu)
      deallocate(p)
      deallocate(r)
      deallocate(s)
      deallocate(t)
      deallocate(v)
!
 9000 format(i6,' ',6(' ',e16.8))
 9900 format(' BiCGStab_solve : it = ',i5,' proof = ',e16.8)
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Programmbeendung :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      RETURN
      END
