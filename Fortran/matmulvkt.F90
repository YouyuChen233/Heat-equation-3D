subroutine matmulvkt(erg,a,b,arow,acol,brow)
implicit none
        real(kind=8) ::erg(arow)
        real(kind=8) ::a(arow,acol)
        real(kind=8) ::b(brow)
        integer      ::arow
        integer      ::acol
        integer      ::brow
        integer      ::i,j
        erg=0.0d+00
        if(acol .eq. brow) then
                do i=1,arow
                do j=1,acol
                erg(i)=erg(i)+a(i,j)*b(j)
                end do
                end do 
        else 
                print*,'ERRO '
        end if
        RETURN
        END
