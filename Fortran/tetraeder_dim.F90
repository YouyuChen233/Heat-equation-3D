      subroutine tetraeder_dim(n_tetr, n_tetr_pkt)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
!
!.. global integer variables..
      integer                                   :: n_tetr_pkt
      integer                                   :: n_tetr
!.. local  integer variables..
      integer                                   :: iostatus
      integer                                   :: idummy1, idummy2
      integer                                   :: idummy3
      integer                                   :: num1,num2,num3,num4
!.. global  double variables..
      real(kind=8)                              :: xkoor, ykoor, zkoor
!-----|--1---------2---------3---------4---------5---------6---------7-|
      open(unit = 33, file = 'mesh.nodes', Status='UNKNOWN')
      iostatus = 1
      do while (iostatus .ge. 0)
         read (unit = 33, fmt = *, iostat = iostatus)                  &
               idummy1, idummy2, xkoor, ykoor, zkoor
      end do
      n_tetr_pkt = idummy1
      print*,' tetraeder_dim : Knotenanzahl = ',n_tetr_pkt
      close( unit = 33, status = 'keep' )
      open(unit = 33, file = 'mesh.elements', Status='UNKNOWN')
      iostatus = 1
      do while (iostatus .ge. 0)
         read (unit = 33, fmt = *, iostat = iostatus)                  &
               idummy1, idummy2, idummy3, num1, num2, num3, num4
      end do
      n_tetr = idummy1
      print*,' tetraeder_dim : Elementanzahl = ',n_tetr
      close( unit = 33, status = 'keep' )
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end subroutine tetraeder_dim
