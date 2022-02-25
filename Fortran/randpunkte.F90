      subroutine randpunkte(n_tetr_pkt, NRanddim,                      &
                            f1zaehler, f2zaehler, f3zaehler, f4zaehler,&
                            f5zaehler, f6zaehler, PNummerF, P, InnerePkt)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
!..global integer variables..
      integer                                   :: f1zaehler
      integer                                   :: f2zaehler
      integer                                   :: f3zaehler
      integer                                   :: f4zaehler
      integer                                   :: f5zaehler
      integer                                   :: f6zaehler
      integer                                   :: n_tetr_pkt
      integer                                   :: NRanddim
#ifdef plevel1
      integer                                   :: innererPktZaehler
#endif
!..global integer arrays..
      integer     , dimension(:,:)              :: PNummerF(NRanddim,6)! Nummern der Randpunkte
      integer     , dimension(:)                :: InnerePkt(n_tetr_pkt)
!..global double  arrays..
      real(kind=8), dimension(:,:)              :: P(n_tetr_pkt, 3)    ! letzte Verfeinerungsstufe Punkte
!..local integer variables..
      integer                                   :: i
      integer                                   :: j
!..local real    variables..
      real(kind=8)                              :: eps
      real(kind=8)                              :: xmax
      real(kind=8)                              :: xmin
      real(kind=8)                              :: ymax
      real(kind=8)                              :: ymin
      real(kind=8)                              :: zmax
      real(kind=8)                              :: zmin
!-----|--1---------2---------3---------4---------5---------6---------7-|
      eps = 0.0000001d+00
      do i = 1, n_tetr_pkt
         InnerePkt(i) = i 
      end do
      PNummerF=0
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
      print*,' randpunkte : xmin = ',xmin
      print*,' randpunkte : xmax = ',xmax
      print*,' randpunkte : ymin = ',ymin
      print*,' randpunkte : ymax = ',ymax
      print*,' randpunkte : zmin = ',zmin
      print*,' randpunkte : zmax = ',zmax

      f1zaehler = 0
      do i = 1, n_tetr_pkt
!------------------------------------
!
!     Punkte auf Randflaeche F_1 : z = y <==> |z-y| .lt. eps
!
#ifdef WSZby16
         if ( dabs( P(i, 2) - P(i, 3) ) .lt. eps ) then
#endif
#ifdef QUADER   
         if ( dabs( P(i, 2) - ymax    ) .lt. eps ) then                ! Punkte auf Randflaeche F_1(ymax)
#endif
            if ( InnerePkt(i) .gt. 0 ) then
               f1zaehler = f1zaehler + 1
               PNummerF(f1zaehler,1) = i
               InnerePkt(i) = -1
#ifdef plevel4
               write( unit = 6, fmt = 1000)                             &
               i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
!------------------------------------
      end do
      print*,' randpunkte : ==> f1zaehler = ',f1zaehler
      print*,' '
!<--
      f2zaehler = 0
      do i = 1, n_tetr_pkt
!------------------------------------
!
!     Punkte auf Randflaeche F_2 : unten ==> z = 1/2
!
#ifdef WSZby16
         if ( dabs( P(i, 3) - 0.5d+00   ) .lt. eps ) then
#endif
#ifdef QUADER   
         if ( dabs( P(i, 2) - ymin    ) .lt. eps ) then                ! Punkte auf Randflaeche F_2(ymin)
#endif
            if ( InnerePkt(i) .gt. 0 ) then
               f2zaehler = f2zaehler + 1
               PNummerF(f2zaehler,2) = i
               InnerePkt(i) = -2
#ifdef plevel4
               write( unit = 6, fmt = 2000)                            &
               i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
!------------------------------------
      end do
      print*,' randpunkte : ==> f2zaehler = ',f2zaehler
      print*,' '
!<--
      f3zaehler = 0
      do i = 1, n_tetr_pkt
!------------------------------------
!
!     Punkte auf Randflaeche F_3 : vorne ==> x = 1
!
#ifdef WSZby16
         if ( dabs(xmax - P(i, 1)) .lt. eps ) then
#endif
#ifdef QUADER   
         if ( dabs( P(i, 1) - xmax    ) .lt. eps ) then
#endif
            if ( InnerePkt(i) .gt. 0 ) then
               f3zaehler = f3zaehler + 1
               PNummerF(f3zaehler,3) = i
               InnerePkt(i) = -3
#ifdef plevel4
            write( unit = 6, fmt = 3000)                             &
            i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
!------------------------------------
      end do
      print*,' randpunkte : ==> f3zaehler = ',f3zaehler
      print*,' '
!<--
      f4zaehler = 0
      do i = 1, n_tetr_pkt
!------------------------------------
#ifdef WSZby16
         if ( dabs(2.25d+00-P(i, 1)-P(i, 2)-P(i, 3))                   &
              .lt. eps ) then
#endif
#ifdef QUADER   
         if ( dabs( P(i, 1) - xmin    ) .lt. eps ) then
#endif
            if ( InnerePkt(i) .gt. 0 ) then
               f4zaehler = f4zaehler + 1                               ! Punkte auf Randflaeche F_4 : z = 9/4 - x - y
               PNummerF(f4zaehler,4) = i 
               InnerePkt(i) = -4
#ifdef plevel4
               write( unit = 6, fmt = 4000)                            &
               i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
!------------------------------------
      end do
      print*,' randpunkte : ==> f4zaehler = ',f4zaehler
      print*,' '
      f5zaehler = 0
      do i = 1, n_tetr_pkt
#ifdef WSZby16
         if ( dabs(P(i, 1)-P(i, 2)) .lt. eps ) then
#endif
#ifdef QUADER   
         if ( dabs( P(i, 3) - zmax    ) .lt. eps ) then
#endif
            if ( InnerePkt(i) .gt. 0 ) then
               f5zaehler = f5zaehler + 1                               ! Punkte auf Randflaeche F_5 : x = y, z egal
               PNummerF(f5zaehler,5) = i 
               InnerePkt(i) = -5
#ifdef plevel4
               write( unit = 6, fmt = 5000)                            &
               i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
      end do
      print*,' randpunkte : ==> f5zaehler = ',f5zaehler
      print*,' '
#ifdef QUADER   
      f6zaehler = 0
      do i = 1, n_tetr_pkt
         if ( dabs( P(i, 3) - zmin    ) .lt. eps ) then
            if ( InnerePkt(i) .gt. 0 ) then
               f6zaehler = f6zaehler + 1                               ! Punkte auf Randflaeche F_5 : x = y, z egal
               PNummerF(f6zaehler,6) = i 
               InnerePkt(i) = -6
#ifdef plevel4
               write( unit = 6, fmt = 6000)                            &
               i,P(i, 1),P(i, 2),P(i, 3)
#endif
            end if
         end if
      end do
      print*,' randpunkte : ==> f6zaehler = ',f6zaehler
      print*,' '
#endif
#ifdef plevel1
      innererPktZaehler = 0
      do i = 1, n_tetr_pkt
         if ( InnerePkt(i) .lt.  0 ) then
            innererPktZaehler = innererPktZaehler + 1
            write( unit =66, fmt=7000 ) innererPktZaehler, InnerePkt(i),&
                                     i,P(i, 1),P(i, 2),P(i, 3)
         end if
      end do
      do i=1,NRanddim
      write(65,*)PNummerF(i,1),PNummerF(i,2),PNummerF(i,3),&
                 PNummerF(i,4),PNummerF(i,5),PNummerF(i,6)
      end do
#endif
!<--
!-----|--1---------2---------3---------4---------5---------6---------7-|
#ifdef plevel4
 1000 format(' randpunkte : Auf der F1-Randflaeche(links)  : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
 2000 format(' randpunkte : Auf der F2-Randflaeche(unten)  : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
 3000 format(' randpunkte : Auf der F3-Randflaeche(vorn)   : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
 4000 format(' randpunkte : Auf der F4-Randflaeche(rechts) : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
 5000 format(' randpunkte : Auf der F5-Randflaeche(rechts) : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
 6000 format(' randpunkte : Auf der F6-Randflaeche(rechts) : Punkt-Nr.',&
             i6,' (x,y,z) = (',2(d16.8,', '),d16.8,')')
#endif
#ifdef plevel1
!7000 format(' randpunkte : Innerer Zaehler = ',i6,' Punkt-Nr.',i6,     &
!            ' (x,y,z)(',i6,') = (',2(d16.8,', '),d16.8,')')
 7000 format(3(' ',i8),3(' ',e16.8))
#endif
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end
