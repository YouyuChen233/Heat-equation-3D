subroutine matopt(ndim,pmRdim,Sglobal,SmR,bglobal,bmR,P,PmR,x,xmR,tmp,InnerePkt)
        implicit none
        integer      ::ndim
        integer      ::pmRdim

        integer      ::i, j, m, n

        real(kind=8) ::x(ndim)
        real(kind=8) ::xmR(pmRdim)
        real(kind=8) ::bglobal(ndim)
        real(kind=8) ::bmR(pmRdim)
        real(kind=8) ::Sglobal(ndim,ndim)
        real(kind=8) ::SmR(pmRdim,pmRdim)
        real(kind=8) ::tmp(pmRdim,ndim)
        real(kind=8) ::P(ndim,3)
        real(kind=8) ::PmR(pmRdim,3)
        integer      ::InnerePkt(ndim)

        real           cpu_sekunden, st0, ste
        x=0.0d+00
        j=1
        m=1
        print*,' Matrix verkueren............'
        call cpu_time(st0)
        do i=1, ndim
        if(InnerePkt(i) .lt. 0) then
                x(i)=bglobal(i)
                j=j+1
        else
                bmR(m)=bglobal(i)
                tmp(m,:)=Sglobal(i,:)
                PmR(m,:)=P(i,:)
                m=m+1
        end if
        end do
        bmR=bmR-matmul(tmp,x)
        m=1
        n=1
        do i=1,ndim
        do j=1,ndim
        if(InnerePkt(i) .gt. 0 .and. InnerePkt(j) .gt. 0) then
                SmR(m,n)=Sglobal(InnerePkt(i),InnerePkt(j))
                n=n+1
                if(n .gt. pmRdim) then
                        n=1
                        m=m+1
                end if
        end if
        end do
        end do
        call cpu_time(ste)
        cpu_sekunden = (ste - st0)
        print*,' ==> Matrix optimiert : Dazu wurden', cpu_sekunden, &
               '           CPU--Sekunden benoetigt.'
        RETURN
        END
