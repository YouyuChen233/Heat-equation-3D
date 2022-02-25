      subroutine RBdimension(n_tetr_pkt, NRanddim,                      &
                             f1zaehler, f2zaehler, f3zaehler, f4zaehler,&
                             f5zaehler, f6zaehler, P)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
      integer                                   :: n_tetr_pkt
      integer                                   :: f1zaehler
      integer                                   :: f2zaehler
      integer                                   :: f3zaehler
      integer                                   :: f4zaehler
      integer                                   :: f5zaehler
      integer                                   :: f6zaehler
      integer                                   :: NRanddim
      integer                                   :: i
      real(kind=8)                              :: eps
      real(kind=8)                              :: xmax
      real(kind=8)                              :: xmin
      real(kind=8)                              :: ymax
      real(kind=8)                              :: ymin
      real(kind=8)                              :: zmax
      real(kind=8)                              :: zmin
      real(kind=8), dimension(:,:)              :: P(n_tetr_pkt, 3)    ! letzte Verfeinerungsstufe Punkte
!-----|--1---------2---------3---------4---------5---------6---------7-|
      NRanddim = 0
      eps = 0.000000001d+00
      xmax = -10000.0d+00
      xmin =  10000.0d+00
      ymax = -10000.0d+00
      ymin =  10000.0d+00
      zmax = -10000.0d+00
      zmin =  10000.0d+00
      do i = 1, n_tetr_pkt
         xmax = dmax1(xmax, P(i, 1))
         xmin = dmin1(xmin, P(i, 1))
         ymax = dmax1(ymax, P(i, 2))
         ymin = dmin1(ymin, P(i, 2))
         zmax = dmax1(zmax, P(i, 3))
         zmin = dmin1(zmin, P(i, 3))
      end do
      print*,'xmax=',xmax,' xmin= ',xmin,' ymax= ',ymax,' ymin= ',ymin
      f1zaehler = 0
      f2zaehler = 0
      f3zaehler = 0
      f4zaehler = 0
      f5zaehler = 0
      f6zaehler = 0
      do i = 1, n_tetr_pkt
!------------------------------------
#ifdef WSZ_by_16
         if ( dabs( P(i, 2) - P(i, 3) ) .lt. eps ) then
            f1zaehler = f1zaehler + 1                                  ! Punkte auf Randflaeche F_1 : z = y <==> |z-y| .lt. eps
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 2) - ymax    ) .lt. eps ) then
            f1zaehler = f1zaehler + 1                                  ! Punkte auf Randflaeche F_1(ymax)
         end if
#endif
!------------------------------------
#ifdef WSZ_by_16
         if ( dabs( P(i, 3) - 0.5d+00   ) .lt. eps ) then
               f2zaehler = f2zaehler + 1                               ! Punkte auf Randflaeche F_2 : unten ==> z = 1/2
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 2) - ymin    ) .lt. eps ) then
            f2zaehler = f2zaehler + 1                                  ! Punkte auf Randflaeche F_2(ymin)
         end if
#endif
!------------------------------------
#ifdef WSZ_by_16
         if ( dabs(xmax - P(i, 1)) .lt. eps ) then
            f3zaehler = f3zaehler + 1                                  ! Punkte auf Randflaeche F_3 : vorne ==> x = 1
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 1) - xmax    ) .lt. eps ) then
            f3zaehler = f3zaehler + 1                                  ! Punkte auf Randflaeche F_3(xmax)
         end if
#endif
!------------------------------------
#ifdef WSZ_by_16
         if ( dabs(P(i, 1)+P(i, 2)+P(i, 3) - 2.25d+00)                 &
              .lt. eps ) then
            f4zaehler = f4zaehler + 1                                  ! Punkte auf Randflaeche F_4 : z = 9/4 - x - y
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 1) - xmin    ) .lt. eps ) then
            f4zaehler = f4zaehler + 1                                  ! Punkte auf Randflaeche F_4(xmin)
         end if
#endif
!------------------------------------
#ifdef WSZ_by_16
         if ( dabs(P(i, 1)-P(i, 2)) .lt. eps ) then
            f5zaehler = f5zaehler + 1                                  ! Punkte auf Randflaeche F_5 : x = y, z egal
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 3) - zmax    ) .lt. eps ) then
            f5zaehler = f5zaehler + 1                                  ! Punkte auf Randflaeche F_5(zmax)
         end if
#endif
#ifdef QUADER   
         if ( dabs( P(i, 3) - zmin    ) .lt. eps ) then
            f6zaehler = f6zaehler + 1                                  ! Punkte auf Randflaeche F_6(zmin)
         end if
#endif
!------------------------------------
      end do
      NRanddim = max0( NRanddim, f1zaehler )
      NRanddim = max0( NRanddim, f2zaehler )
      NRanddim = max0( NRanddim, f3zaehler )
      NRanddim = max0( NRanddim, f4zaehler )
      NRanddim = max0( NRanddim, f5zaehler )
#ifdef QUADER   
      NRanddim = max0( NRanddim, f6zaehler )
#endif
      print*,' RBdimension : f1zaehler = ',f1zaehler
      print*,' RBdimension : f2zaehler = ',f2zaehler
      print*,' RBdimension : f3zaehler = ',f3zaehler
      print*,' RBdimension : f4zaehler = ',f4zaehler
      print*,' RBdimension : f5zaehler = ',f5zaehler
#ifdef QUADER   
      print*,' RBdimension : f6zaehler = ',f6zaehler
#endif
      print*,' RBdimension : NRanddim = ',NRanddim
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end
