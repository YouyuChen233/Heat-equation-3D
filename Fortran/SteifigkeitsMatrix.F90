      subroutine SteifigkeitsMatrix(Ndim, n_tetr,                       &
                                    TNneu, Pneu,                        &
                                    bglobal, Sglobal)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      implicit none
!..global integer variables..
      integer                                   :: Ndim
      integer                                   :: n_tetr
!..global integer arrays..
      integer     , dimension(:,:)              :: TNneu(0:n_tetr, 4)  ! neue Numerierung
!..global double  variables..
!..global double  arrays..
      real(kind=8), dimension(:,:)              :: Pneu(Ndim, 3)       ! letzte Verfeinerungsstufe Punkte
      real(kind=8), dimension(:,:)              :: Sglobal(Ndim, Ndim)
      real(kind=8), dimension(:)                :: bglobal(Ndim)
!..local  character variables..
#ifdef plevel1
      character(len = 16)                       :: strg
#endif
!..local integer variables..
#ifdef plevel1
      integer                                   :: istrg
      integer                                   :: maxprint
#endif
      integer                                   :: ti                  ! globale Tetraedernummer
      integer                                   :: i                   ! lokale Tetraederpunktnummer
      integer                                   :: j                   ! lokale Tetraederpunktnummer
      integer                                   :: l                   ! lokale Tetraederpunktnummer
      integer                                   :: r
!..local double  variables..
      real(kind=8)                              :: eps
      real(kind=8)                              :: det_Phi
      real(kind=8)                              :: one_by_Phi
      real(kind=8)                              :: a, b, c, d, e, f
      real(kind=8)                              :: x21, x31, x41
      real(kind=8)                              :: y21, y31, y41
      real(kind=8)                              :: z21, z31, z41
      real(kind=8)                              :: Pinv11,Pinv12,Pinv13
      real(kind=8)                              :: Pinv21,Pinv22,Pinv23
      real(kind=8)                              :: Pinv31,Pinv32,Pinv33
      real(kind=8)                              :: q
!..local double  arrays   ..
      real(kind=8), dimension(:,:)              :: Slocal(4,4)
      real(kind=8), dimension(:)                :: blocal(4)
!-----|--1---------2---------3---------4---------5---------6---------7-|
      eps     = 0.1d-06
      Sglobal = 0.0d+00
      bglobal = 0.0d+00
      det_Phi = 0.0d+00
      q       = 0.0d+00
#ifdef plevel3
!     print*,' Knotenkoordinaten :'
      do i = 1, Ndim
         write(unit = 77, fmt = 9000) i, Pneu(i,1), Pneu(i,2), Pneu(i,3)
      end do
!     print*,' Knotennummern der Elemente :'
      do i = 1, n_tetr
         write(unit = 78, fmt = 9100) i, TNneu(i,1), TNneu(i,2), TNneu(i,3), TNneu(i,4)
      end do
#endif
      do ti = 1, n_tetr
!-----|--1---------2---------3---------4---------5---------6---------7-|
         x21 = Pneu(TNneu(ti,2), 1) - Pneu(TNneu(ti,1), 1)             ! x_2 - x_1
         y21 = Pneu(TNneu(ti,2), 2) - Pneu(TNneu(ti,1), 2)             ! y_2 - y_1
         z21 = Pneu(TNneu(ti,2), 3) - Pneu(TNneu(ti,1), 3)             ! z_2 - z_1
         x31 = Pneu(TNneu(ti,3), 1) - Pneu(TNneu(ti,1), 1)             ! x_3 - x_1
         y31 = Pneu(TNneu(ti,3), 2) - Pneu(TNneu(ti,1), 2)             ! y_3 - y_1
         z31 = Pneu(TNneu(ti,3), 3) - Pneu(TNneu(ti,1), 3)             ! z_3 - z_1
         x41 = Pneu(TNneu(ti,4), 1) - Pneu(TNneu(ti,1), 1)             ! x_4 - x_1
         y41 = Pneu(TNneu(ti,4), 2) - Pneu(TNneu(ti,1), 2)             ! y_4 - y_1
         z41 = Pneu(TNneu(ti,4), 3) - Pneu(TNneu(ti,1), 3)             ! z_4 - z_1
!-----|--1---------2---------3---------4---------5---------6---------7-|
         write(27,3300)ti,TNneu(ti,1),TNneu(ti,2),TNneu(ti,3),TNneu(ti,4)
         write(27,3200)ti,x21,y21,z21,x31,y31,z31,x41,y41,z41
         det_Phi = x21 * y31 * z41 + x31 * y41 * z21 + x41 * y21 * z31 &
                 - x41 * y31 * z21 - x31 * y21 * z41 - x21 * y41 * z31
         if ( det_Phi .lt. 0.0d+00 ) then
            print*,' SteifigkeitsMatrix : det_Phi = ',det_Phi
            print*,' SteifigkeitsMatrix : Tetr.Nr = ',ti
            print*,' SteifigkeitsMatrix : ',                           &
                   ' Negative Determinante=negative Orientierung'
            print*,' SteifigkeitsMatrix : ==> Exit'
!           stop
         end if
!
         one_by_Phi = 1.0d+00 / det_Phi
         Pinv11 = (y31 * z41 - y41 * z31)!* one_by_Phi
         Pinv12 = (x41 * z31 - x31 * z41)!* one_by_Phi
         Pinv13 = (x31 * y41 - x41 * y31)!* one_by_Phi
         Pinv21 = (y41 * z21 - y21 * z41)!* one_by_Phi
         Pinv22 = (x21 * z41 - x41 * z21)!* one_by_Phi
         Pinv23 = (x41 * y21 - x21 * y41)!* one_by_Phi
         Pinv31 = (y21 * z31 - y31 * z21)!* one_by_Phi
         Pinv32 = (x31 * z21 - x21 * z31)!* one_by_Phi
         Pinv33 = (x21 * y31 - x31 * y21)!* one_by_Phi
         a = Pinv11**2 + Pinv12**2 + Pinv13**2
         b = Pinv21**2 + Pinv22**2 + Pinv23**2
         c = Pinv31**2 + Pinv32**2 + Pinv33**2
         d = Pinv11*Pinv21 + Pinv12*Pinv22 + Pinv13*Pinv23
         e = Pinv11*Pinv31 + Pinv12*Pinv32 + Pinv13*Pinv33
         f = Pinv21*Pinv31 + Pinv22*Pinv32 + Pinv23*Pinv33
#ifdef plevel1
         write( unit= 79, fmt=3100) ti, a, b, c, d, e, f, det_Phi
#endif
#ifdef plevel4
         print*,' SteifigkeitsMatrix : a = ',a,' b = ',b
         print*,' SteifigkeitsMatrix : c = ',c,' d = ',d
         print*,' SteifigkeitsMatrix : e = ',e,' f = ',f
#endif
         Slocal(1,1) = a+b+c+2.0d+00*(d+e+f)
         Slocal(1,2) = -a-d-e
         Slocal(1,3) = -b-d-f
         Slocal(1,4) = -c-e-f
         Slocal(2,1) = Slocal(1,2)
         Slocal(3,1) = Slocal(1,3)
         Slocal(4,1) = Slocal(1,4)
         Slocal(2,2) = a
         Slocal(2,3) = d
         Slocal(2,4) = e
         Slocal(3,2) = d
         Slocal(3,3) = b
         Slocal(3,4) = f
         Slocal(4,2) = e
         Slocal(4,3) = f
         Slocal(4,4) = c
!
!        Slocal      = Slocal * det_Phi                                ! vermeintlicher Fehler <== ELMER
         Slocal      = Slocal * one_by_Phi                             ! vermeintlicher Fehler <== ELMER
!
         blocal = 1.0d+00
#ifdef plevel4
         istrg = 16
         strg  = 'Steif.Mat.local '
         call dprintr(istrg, strg, Slocal, 4, 4, 4, 4, 1, 1, 4, 4)
#endif
         do i = 1, 4                                                   ! 4 wg.Tetraeder (hat 4 Punkte)
            l = TNneu(ti, i)
	    write(25,*)'L= ',l
            do j = 1, i
               r = TNneu(ti, j)
	       write(25,*)'R= ',r
               Sglobal(l, r) = Sglobal(l, r) + Slocal(i,j)
               Sglobal(r, l) = Sglobal(l, r)
#ifdef plevel3
               write(80,9200) l,r,i,j,Slocal(i,j),Sglobal(l,r),one_by_Phi
#endif

            end do
            bglobal(l) = bglobal(l) + det_Phi * blocal(i)
         end do
      end do
!     bglobal = 6.0d+00*4.0d+00 * det_Phi / 24.0d+00     * bglobal  ! Rechte Seite * q=4 und die 1/6 der Steifigkeitsmatrix auf die andere Seite
#ifdef linLoes
      print*,' SteifigkeitsMatrix : Lineare Loesungsfunktionsvorgabe'
      q = 0.0d+00                                                   ! u(x,y,z) = x + y + z
#endif
#ifdef quadLoes
      print*,' SteifigkeitsMatrix : Quadrat.Loesungsfunktionsvorgabe'
      q = -6.0d+00                                                  ! u(x,y,z) = 20 + x**2 + y**2 + z**2
!     q =  0.0d+00                                                  ! u(x,y,z) = x**2 - y**2 ==> nabla**2 u = 2-2 = 0
!     q = -2.0d+00 * eps                                            ! u(x,y,z) = x + y + z + eps * x**2
#endif
#ifdef kubLoes
      print*,' SteifigkeitsMatrix : Kubisch.Loesungsfunktionsvorgabe'
      q =  4.0d+00                                                  ! u(x,y,z) = 20 - 2*y**2 + x**3*y - x*y**3 + z**3*x - z*x**3
#endif
      bglobal =               q            / 24.0d+00    * bglobal  ! Rechte Seite *q=4 und die 1/6 der Steifigkeitsmatrix auf die andere Seite
      Sglobal =  1.0d+00/6.0d+00 * Sglobal
      do i=1,Ndim
         write(28,*)bglobal(i)
      end do
      do i=1,Ndim
         do j=1,Ndim
	 if(dabs(Sglobal(i,j)) .gt. eps) then
            write(29,*)Sglobal(i,j)
	    end if
         end do
	 write(29,*) i,'*'
      end do
#ifdef plevel3
      open(unit = 43, file = 'StiffMatr.out', Status='UNKNOWN')
      do i = 1, Ndim 
         do j = 1, Ndim 
            write( unit=43, fmt=3200) i, j, Sglobal(i, j)
         end do
      end do
      close( unit = 43, status = 'keep' )
      open(unit = 44, file = 'rechteSeite.out', Status='UNKNOWN')
      do i = 1, Ndim 
!        write( unit= 6, fmt=3000) i, bglobal(i)
         write( unit=44, fmt=3000) i, bglobal(i)
      end do
      close( unit = 44, status = 'keep' )
 3000 format(' bgl(',i4,') = ',g12.4)
 3200 format(' S(',i6,', ',i6,') = ',g16.8)
#endif
#ifdef plevel3
      istrg = 16
      strg  = 'Steif.Mat.ohneRB'
      if ( Ndim .gt. 18 ) then
         maxprint = 18
      else
         maxprint = Ndim
      end if
      call dprintr(istrg, strg, Sglobal, Ndim, Ndim, maxprint, maxprint,&
                   1, 1, Ndim, maxprint)
      call dprintr(istrg, strg, Sglobal, Ndim, Ndim, Ndim, Ndim,        &
                   1, Ndim-maxprint+1, Ndim, maxprint)
!
      print*, ' Rechte Seite ohne Randbedingungen :'
      do i = 1, Ndim
         write( unit= 6, fmt=3000) i, bglobal(i)
      end do
!-----|--1---------2---------3---------4---------5---------6---------7-|
 3000 format(' b(',i6,') = ',g16.8)
#endif
#ifdef plevel1
 3100 format(' ElNr.: ',i8, 7(' ',g16.8))
 3200 format(' ElNr.: ',i8, 9(' ',g16.8))
 3300 format(' TNr.: ',i8, 4(' ',g16.8))
#endif
#ifdef plevel3
 9000 format(' (x,y,z)(',i6,') = (',3(' ',g12.4),')')
 9100 format(' T(1,2,3,4)(',i6,') = (',4(' ',i8),')')
#endif
#ifdef plevel3
9200 format(4(' ',i8),3(' ',e16.8))
#endif
!-----|--1---------2---------3---------4---------5---------6---------7-|
      return
      end
