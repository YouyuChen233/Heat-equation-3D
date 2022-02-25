      subroutine tetraeder_input(n_tetr, n_tetr_pkt, P, T)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
!
!.. global integer variables..
      integer                                   :: n_tetr_pkt
      integer                                   :: n_tetr
!.. global  integer arrays ..
      integer     , dimension(:,:)              :: T(0:n_tetr, 4)      ! Punktnummern im Tetraeder
!.. global  double arrays ..
      real(kind=8), dimension(:,:)              :: P(n_tetr_pkt, 3)    ! alle Punkte
!.. local  integer variables..
      integer                                   :: i
      integer                                   :: iostatus
      integer                                   :: idummy1, idummy2
      integer                                   :: idummy3
!-----|--1---------2---------3---------4---------5---------6---------7-|
      T(0,1) = 1
      T(0,2) = 2
      T(0,3) = 3
      T(0,4) = 4
      i = 1
      open(unit = 33, file = 'mesh.nodes', Status='UNKNOWN')
#ifdef plevel3
      open(unit=34,File='mesh.nodes_eingelesen',status='unknown')
#endif
      iostatus = 1
      do while (i .le. n_tetr_pkt)
         read (unit = 33, fmt = *, iostat = iostatus)                  &
               idummy1, idummy2, P(i, 1), P(i, 2), P(i,3)
#ifdef plevel3
!        write ( unit = 34, fmt = 2200 ) idummy1,                       &
!              P(i, 1), P(i, 2), P(i,3)
         write ( unit = 34, fmt = 2200 ) idummy1,                       &
               P(i, 1), P(i, 2), P(i,3)
#endif
         i = i + 1
      end do
      close( unit = 33, status = 'keep' )
      close( unit = 34, status = 'keep' )
      open(unit = 33, file = 'mesh.elements', Status='UNKNOWN')
      iostatus = 1
      i = 1
      do while (i .le. n_tetr)
         read (unit = 33, fmt = *, iostat = iostatus)                  &
               idummy1, idummy2, idummy3,                              &
               T(i, 1), T(i, 2), T(i,3), T(i, 4)
#ifdef plevel3
         write(unit =  6, fmt = 2300, iostat = iostatus)               &
               idummy1,                                                &
               T(i, 1), T(i, 2), T(i,3), T(i, 4)
#endif
         i = i + 1
      end do
      close( unit = 33, status = 'keep' )
#ifdef plevel1
 2200 format(' tetraeder_input : Kn.Nr.i = ',i6,' (x, y, z) =(',3(1x,e16.8),')')
 2300 format(' tetraeder_input : El.Nr.i = ',i6,' (1, 2, 3, 4) =(',4(', ',I6),')')
#endif
#ifdef plevel1
      print*,' ***************************'
      print*,' tetraeder_input : Input eingelesen ! *'
      print*,' ***************************'
      print*,' tetraeder_input : Anzahl der sortierten Punkte : ',n_tetr_pkt
      print*,' tetraeder_input : Anzahl der sortierten Punkte : ',n_tetr
#endif
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end subroutine tetraeder_input
