      subroutine Flaechen_RB(NRanddim, n_tetr_pkt, PNummerF, Pneu, bg, &
                             InnerePkt, Sg)
!
      implicit none
!     .. global integer variables..
      integer, intent(in)               :: NRanddim
      integer, intent(in)               :: n_tetr_pkt
!     .. global real    variables..
!     .. global integer arrays   ..
      integer     , dimension(:)        :: InnerePkt(n_tetr_pkt)
      integer     , dimension(:,:)      :: PNummerF(NRanddim, 6)
!     .. global real    arrays   ..
      real(kind=8), dimension(:,:)      :: Pneu(n_tetr_pkt, 3)         ! letzte Verfeinerungsstufe Punkte
      real(kind=8), dimension(:,:)      :: Sg(n_tetr_pkt, n_tetr_pkt)  ! globale Steifigkeitsmatrix+RBd
      real(kind=8), dimension(:)        :: bg(n_tetr_pkt)              ! globale rechte Seite
!     .. local  integer variables..
      integer                           :: f1zaehler
      integer                           :: f2zaehler
      integer                           :: f3zaehler
      integer                           :: f4zaehler
      integer                           :: f5zaehler
      integer                           :: f6zaehler
      integer                           :: i
      integer                           :: j
      integer                           :: ki
#ifdef plevel1
      character(len = 16)               :: strg
#endif
#ifdef plevel1
      integer                           :: istrg
      integer                           :: Ndim
      integer                           :: maxprint
#endif
!     .. local  real    variables..
      real(kind=8)                      :: urand
      real(kind=8)                      :: eps
      real(kind=8)                      :: x
      real(kind=8)                      :: y
      real(kind=8)                      :: z
!
!-----|--1---------2---------3---------4---------5---------6---------7-|
!     Multiplikation der Steifigkeitsmatrix mit T_k in der ki_ten Spalte
!     Randbedingung an der k. Randflaeche 
!     (k=1 : linke   Tetraederflaeche)
!     (k=2 : untere  Tetraederflaeche)
!     (k=3 : vordere Tetraederflaeche)
!     (k=4 : rechte  Tetraederflaeche)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      eps = 0.1d-06
#ifdef plevel2
      print*,' Flaechen_RB : Einarbeiten der Randbedingung : '
#endif
      f1zaehler = 0
      f2zaehler = 0
      f3zaehler = 0
      f4zaehler = 0
      f5zaehler = 0
      f6zaehler = 0
      do i = 1, n_tetr_pkt
         if ( InnerePkt(i) .lt.  0 ) then
            if ( InnerePkt(i) .eq. -1 ) then
               f1zaehler = f1zaehler + 1
               ki = PNummerF(f1zaehler,1)
            end if
            if ( InnerePkt(i) .eq. -2 ) then
               f2zaehler = f2zaehler + 1
               ki = PNummerF(f2zaehler,2)
            end if
            if ( InnerePkt(i) .eq. -3 ) then
               f3zaehler = f3zaehler + 1
               ki = PNummerF(f3zaehler,3)
            end if
            if ( InnerePkt(i) .eq. -4 ) then
               f4zaehler = f4zaehler + 1
               ki = PNummerF(f4zaehler,4)
            end if
            if ( InnerePkt(i) .eq. -5 ) then
               f5zaehler = f5zaehler + 1
               ki = PNummerF(f5zaehler,5)
            end if
            if ( InnerePkt(i) .eq. -6 ) then
               f6zaehler = f6zaehler + 1
               ki = PNummerF(f6zaehler,6)
            end if
            x = Pneu(ki, 1)
            y = Pneu(ki, 2)
            z = Pneu(ki, 3)
#ifdef linLoes
            urand = x + y + z
#endif
#ifdef quadLoes
            urand = 20.0d+00 + x**2 + y**2 + z**2
!           urand = 20.0d+00 - 2.0d+00 * y**2                         &
!                 + x**2 * y - x * y**2 + z**2 * x - z * x**2
#endif
#ifdef kubLoes
            urand = 20.0d+00 - 2.0d+00 * y**2                         &
                  + x**3 * y - x * y**3 + z**3 * x - z * x**3
!           urand =          - 2.0d+00 * y**2                         &
!                 + x**3 * y - x * y**3 + z**3 * x - z * x**3
#endif
            do j = 1, n_tetr_pkt
               bg(j)     = bg(j) - Sg(j, ki) * urand   ! Hier werden alle Komponenten belegt!
               Sg(j, ki) = 0.0d+00
               Sg(ki, j) = 0.0d+00
            end do
	       write(94,*)'ki= ',ki
            Sg(ki, ki) = 1.0d+00
         end if
      end do

      print*,' Flaechen : ==> f1zaehler = ',f1zaehler
      print*,' Flaechen : ==> f2zaehler = ',f2zaehler
      print*,' Flaechen : ==> f3zaehler = ',f3zaehler
      print*,' Flaechen : ==> f4zaehler = ',f4zaehler
      print*,' Flaechen : ==> f5zaehler = ',f5zaehler
      print*,' Flaechen : ==> f6zaehler = ',f6zaehler
      f1zaehler = 0
      f2zaehler = 0
      f3zaehler = 0
      f4zaehler = 0
      f5zaehler = 0
      f6zaehler = 0
      do i = 1, n_tetr_pkt
         if ( InnerePkt(i) .lt.  0 ) then
            if ( InnerePkt(i) .eq. -1 ) then
               f1zaehler = f1zaehler + 1
               ki = PNummerF(f1zaehler,1)
            end if
            if ( InnerePkt(i) .eq. -2 ) then
               f2zaehler = f2zaehler + 1
               ki = PNummerF(f2zaehler,2)
            end if
            if ( InnerePkt(i) .eq. -3 ) then
               f3zaehler = f3zaehler + 1
               ki = PNummerF(f3zaehler,3)
            end if
            if ( InnerePkt(i) .eq. -4 ) then
               f4zaehler = f4zaehler + 1
               ki = PNummerF(f4zaehler,4)
            end if
            if ( InnerePkt(i) .eq. -5 ) then
               f5zaehler = f5zaehler + 1
               ki = PNummerF(f5zaehler,5)
            end if
            if ( InnerePkt(i) .eq. -6 ) then
               f6zaehler = f6zaehler + 1
               ki = PNummerF(f6zaehler,6)
            end if
            x = Pneu(ki, 1)
            y = Pneu(ki, 2)
            z = Pneu(ki, 3)
#ifdef linLoes
            urand = x + y + z
#endif
#ifdef quadLoes
!           urand = x**2 - y**2
            urand = 20.0d+00 + x**2 + y**2 + z**2
!           urand = x + y + z + eps * x**2
#endif
#ifdef kubLoes
               urand = 20.0d+00 - 2.0d+00 * y**2                  &
                     + x**3 * y - x * y**3 + z**3 * x - z * x**3
!              urand =          - 2.0d+00 * y**2                  &
!                    + x**3 * y - x * y**3 + z**3 * x - z * x**3
#endif
            bg(ki) = urand   ! Hier wird nur die ki-te Komponente belegt!
         end if
      end do
      do i=1,n_tetr_pkt
           write(99,*)bg(i)
      end do
      do i=1,n_tetr_pkt
          do j=1,n_tetr_pkt
	     if(dabs(Sg(i,j)) .gt. eps) then
	     write(96,*)Sg(i,j), '(',i,',',j,')'
	     end if
	  end do
	     write(96,*)i,' *'
      end do
#ifdef plevel2
      print*,' Flaechen_RB : Einarbeiten der RB fertig : '
#endif
#ifdef plevel3
      Ndim = n_tetr_pkt
      istrg = 16
      strg  = 'Steif.Mat.mit RB'
      if ( Ndim .gt. 18 ) then
         maxprint = 18
      else
         maxprint = Ndim
      end if
      call dprintr(istrg, strg, Sg, Ndim, Ndim, maxprint, maxprint,&
                   1, 1, Ndim, maxprint)
      call dprintr(istrg, strg, Sg, Ndim, Ndim, Ndim, Ndim,        &
                   1, Ndim-maxprint+1, Ndim, maxprint)
!
      print*, ' Rechte Seite mit  Randbedingungen :'
      do i = 1, Ndim
         write( unit= 6, fmt=3000) i, bg(i)
      end do
!-----|--1---------2---------3---------4---------5---------6---------7-|
 3000 format(' b(',i6,') = ',g16.8)
#endif
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end
