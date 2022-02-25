program FEM_Tetr
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
!
!.. global integer variables..
      integer                                   :: i, j, m, n
      integer                                   :: f1zaehler
      integer                                   :: f2zaehler
      integer                                   :: f3zaehler
      integer                                   :: f4zaehler
      integer                                   :: f5zaehler
      integer                                   :: f6zaehler
      integer                                   :: NRanddim
      integer                                   :: pmRdim
        
      integer                                   :: n_tetr_pkt
      integer                                   :: n_tetr
!.. global  integer arrays ..
      integer     , dimension(:)  , allocatable :: InnerePkt           ! Punktnummern im Tetraeder
      integer     , dimension(:,:), allocatable :: PNummerF            ! Punktnummern im Tetraeder
      integer     , dimension(:,:), allocatable :: T                   ! Punktnummern im Tetraeder
!.. global  double variables..
!.. global  double arrays ..
      real(kind=8), dimension(:),   allocatable :: x                   ! Temperaturverteilung
      real(kind=8), dimension(:),   allocatable :: xmR                   ! Temperaturverteilung
      real(kind=8), dimension(:),   allocatable :: bglobal             ! rechte Seite
      real(kind=8), dimension(:),   allocatable :: bmR             ! rechte Seite
      real(kind=8), dimension(:,:), allocatable :: P                   ! alle Punkte
      real(kind=8), dimension(:,:), allocatable :: PmR                   ! alle Punkte
      real(kind=8), dimension(:,:), allocatable :: Sglobal             ! SteifigkeitsMatrix
      real(kind=8), dimension(:,:), allocatable :: SmR             ! SteifigkeitsMatrix
      real(kind=8), dimension(:,:), allocatable :: tmp             ! SteifigkeitsMatrix
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

      type(Netzdim) ::meshdim
      type(Netz)    ::mesh
      type(OptGLS)  ::opt
      type(RandPkt) ::RP

#ifdef Cyy
      call tetraeder_dim1(n_tetr, n_tetr_pkt)
#endif
#ifdef Piepke
      call tetraeder_dim(n_tetr, n_tetr_pkt)
#endif
!-----|--1---------2---------3---------4---------5---------6---------7-|
      allocate ( P(n_tetr_pkt, 3) )
      allocate ( T(0:n_tetr, 4) )
      
      allocate(mesh%Pkt(n_tetr_pkt,3))
      allocate(mesh%Tetr(0:n_tetr,4))
      
      allocate ( x(n_tetr_pkt) )
      allocate ( bglobal(n_tetr_pkt) )
      allocate ( Sglobal(n_tetr_pkt, n_tetr_pkt) )

      allocate(opt%temp(n_tetr_pkt))      
!-----|--1---------2---------3---------4---------5---------6---------7-|
#ifdef Cyy      
  call tetraeder_input1(n_tetr, n_tetr_pkt, P, T)
#endif
#ifdef Piepke
  call tetraeder_input(n_tetr, n_tetr_pkt, P, T)
#endif
      call RBdimension(n_tetr_pkt, NRanddim,                           &
                       f1zaehler, f2zaehler, f3zaehler, f4zaehler,     &
                       f5zaehler, f6zaehler, P)
      print*,' main : NRanddim = ',NRanddim
      print*,' *****************************************'
      print*,' main : Randpunktdimensionen ermittelt ! *'
      print*,' *****************************************'
      allocate( PNummerF(NRanddim, 6) )                                ! Speicherfeld dimensionieren
      allocate( InnerePkt(n_tetr_pkt) )                                ! Innere Punktefeld dimensionieren
      allocate(RP%RPkt(NRanddim,6))
      allocate(RP%IPkt(n_tetr_pkt))
      call randpunkte(n_tetr_pkt, NRanddim,                            &
                      f1zaehler, f2zaehler, f3zaehler, f4zaehler,      &
                      f5zaehler, f6zaehler, PNummerF, P, InnerePkt)    ! Randpunkte speichern
      print*,' *****************************************'
      print*,' main : Randpunktnummern abgespeichert ! *'
      print*,' *****************************************'
      call SteifigkeitsMatrix(n_tetr_pkt, n_tetr,                       &
                              T, P,                                     &
                              bglobal, Sglobal)
#ifdef plevel3
      open(unit = 46, file = 'StiffMat-RB.out', Status='UNKNOWN')
      open(unit = 47, file = 'b-RB.out', Status='UNKNOWN')
      do i = 1, n_tetr_pkt
         do j = 1, n_tetr_pkt
            write( 46, fmt = 6780) i, j, sglobal(i, j)
         end do
         write( 47, fmt = 6790) i, bglobal(i)
      end do
      close( unit = 47, status = 'keep' )
      close( unit = 46, status = 'keep' )
#endif
      call Flaechen_RB(NRanddim, n_tetr_pkt, PNummerF,                  &
                       P, bglobal, InnerePkt, Sglobal)
#ifdef plevel3
      open(unit = 48, file = 'StiffMat+RB.out', Status='UNKNOWN')
      open(unit = 49, file = 'b+RB.out', Status='UNKNOWN')
      do i = 1, n_tetr_pkt
         do j = 1, n_tetr_pkt
            write( 48, fmt = 6780) i, j, sglobal(i, j)
         end do
         write( 49, fmt = 6790) i, bglobal(i)
      end do
      close( unit = 48, status = 'keep' )
      close( unit = 49, status = 'keep' )
 6780 format(' ', i8, ' ', i8,' ',e16.8)
 6790 format(' ', i8, ' ',e16.8)
#endif
      pmRdim=n_tetr_pkt-f1zaehler-f2zaehler-f3zaehler-f4zaehler-f5zaehler-f6zaehler

      print*,'pmRdim = ',pmRdim
      allocate(bmR(pmRdim))
      allocate(xmR(pmRdim))
      allocate(PmR(pmRdim,3))
      allocate(tmp(pmRdim,n_tetr_pkt))
      allocate(SmR(pmRdim,pmRdim))
      
      allocate(opt%optb(pmRdim))
      allocate(opt%optx(pmRdim))
      allocate(opt%optp(pmRdim,3))
      allocate(opt%opts(pmRdim,pmRdim))
call matopt(n_tetr_pkt,pmRdim,Sglobal,SmR,bglobal,bmR,P,PmR,x,xmR,tmp,InnerePkt)

      deallocate ( PNummerF )
      deallocate ( T )
      deallocate ( tmp )
      deallocate ( Sglobal )
      deallocate ( bglobal )

#ifdef lusolve
      call lu_solve(n_tetr_pkt, Sglobal, bglobal, x , P)
#endif
#ifdef conjgrad
      x = 1.0                                                          ! Initialisierung
      call cg_solve(n_tetr_pkt, Sglobal, bglobal, x , P)
#endif
#ifdef bicgstab
!      call bicgstab_solve(n_tetr_pkt, Sglobal, bglobal, x , P)
     xmR = 1.0d+00                                                          ! Initialisierung
     meshdim%npkt=n_tetr_pkt
     meshdim%nelement=n_tetr
     opt%opts=SmR
     opt%optb=bmR
     opt%optx=xmR
     opt%optp=PmR
     opt%optdim=pmRdim
     opt%temp=x
     do i=1,meshdim%npkt
        write(991,*)opt%temp(i)
     end do
     
     RP%IPkt=InnerePkt
     mesh%Pkt=P
     do i=1,opt%optdim
        write(990,*)opt%optp(i,1),' ',opt%optp(i,2),' ',opt%optp(i,3)
     end do
      call bicgstab_solve(meshdim,mesh,RP,opt)
      deallocate ( InnerePkt )
      deallocate ( SmR )
      deallocate ( bmR )
      deallocate ( xmR )
      deallocate ( x )
      deallocate ( P )
      deallocate ( PmR )
#endif

      !-----|--1---------2---------3---------4---------5---------6---------7-|
      stop
end program FEM_Tetr
