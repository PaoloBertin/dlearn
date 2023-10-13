subroutine igen(ia,m)
    !
    ! subroutine to read and generate integer nodal data
    !
    !        ia = input array
    !         m = number of rows in ia
    !         n = node number
    !        ne = end node in generation sequence
    !        ng = generation increment
    
    dimension ia(m,1),ib(13)
    common /iounit/ iin,iout,irsin,irsout

100 continue
    read(iin,1000) n,ne,ng,(ib(i),i=1,m)
    if (n.eq.0) return
    if (ng.eq.0) then
        ne = n
        ng = 1
    else
        ne = ne - mod(ne-n,ng)
    endif

    do 200 i=n,ne,ng
        call imove(ia(1,i),ib,m)
200 continue

    go to 100

1000 format(16i5)

end subroutine igen
