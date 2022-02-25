real(kind=8) function skalar(a,b,ndim,ndim2)
implicit none
integer ::ndim
integer ::ndim2
integer ::i
real(kind=8):: aus
real(kind=8) ::a(ndim)
real(kind=8) ::b(ndim)
aus=0.0d+00
do i=1, ndim
aus=aus+a(i)*b(i)
end do
skalar=aus
return
end function
