subroutine tetraeder_dim1(n_tetr, n_tetr_pkt)
implicit none
character(len=20)::idummy1
integer          ::n_tetr_pkt,n_element,n_tetr,iostatus
integer          ::idummy2,idummy3,idummy4,idummy5
open(unit=33,file='3D_Quader+Extrude_v2.msh',status='UNKNOWN')
do while(idummy1 .ne. '$Nodes')
read(unit=33,fmt=*) idummy1
end do

read(unit=33,fmt=*) n_tetr_pkt
do while(idummy1 .ne. '$Elements')
    read(unit=33,fmt=*) idummy1
end do
read(unit=33,fmt=*) n_element
n_tetr=0
iostatus=1
do while(iostatus .ge. 0)
    read(unit=33,fmt=*,iostat=iostatus)idummy1,idummy2
       if(idummy1 .ne. '$EndElements') then
       if(idummy2 .eq. 4) then
           n_tetr=n_tetr+1
       end if
       end if
end do
print*,'dim_tetr_pkt:',n_tetr_pkt,'dim_tetr:',n_tetr
close(unit=33,status='keep')
return
end subroutine tetraeder_dim1
