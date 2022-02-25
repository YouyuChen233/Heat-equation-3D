subroutine tetraeder_input1(n_tetr, n_tetr_pkt, P, T)
implicit none
integer    ::n_tetr_pkt
integer    ::n_tetr
integer,dimension(:,:)       ::T(0:n_tetr,4) !
real(kind=8),dimension(:,:)  ::P(n_tetr_pkt,3)   !
integer    ::iostatus,i
character(len=20)::idummy1
integer          ::idummy2,idummy3,idummy4,idummy5

      print*,' ***************************'
      print*,' tetraeder_input : Input eingelesen ! *'
      print*,' ***************************'
      print*,' tetraeder_input : Anzahl der sortierten Punkte : ',n_tetr_pkt
      print*,' tetraeder_input : Anzahl der sortierten Punkte : ',n_tetr
T(0,1)=1
T(0,2)=2
T(0,3)=3
T(0,4)=4
open(unit=33,file='3D_Quader+Extrude_v2.msh',status='UNKNOWN')
do while(idummy1 .ne. '$Nodes')
read(unit=33,fmt=*)idummy1
end do
read(unit=33,fmt=*)idummy1
i=1
iostatus=1
do while(i .le. n_tetr_pkt)
   read(unit=33,fmt=*)idummy1,&
                      P(i,1),P(i,2),P(i,3)
   i=i+1
end do
do while(idummy1 .ne. '$Elements')
read(unit=33,fmt=*)idummy1
end do
read(unit=33,fmt=*)idummy1
i=1
idummy2=0
do while(idummy2 .ne. 4)
read(unit=33,fmt=*)idummy1,idummy2
end do
backspace(unit=33)
do while(i .le. n_tetr)
read(unit=33,fmt=*)idummy1,idummy2,idummy3,idummy4,idummy5,&
                   T(i,1),T(i,2),T(i,3),T(i,4)
i=i+1
end do
close(33,status='keep')
return
end subroutine tetraeder_input1
