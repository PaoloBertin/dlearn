subroutine dhist(idhist,ndout)
    !
    !.... program to read, write and store nodal time-history input data
    !
    dimension idhist(3,1)
    common /iounit/ iin,iout,irsin,irsout
    common /labels/ labeld(3),label1(16),label2(3),label3(24)
    !
    do 100 n=1,ndout
        read(iin,2000) node,idof,idva
        if (mod(n,50).eq.1) write(iout,1000) ndout
        write(iout,3000) node,idof,labeld(idva)
        idhist(1,n) = node
        idhist(2,n) = idof
        idhist(3,n) = idva
100 continue
    !
    return
    !
1000 format('1',' n o d a l   t i m e - h i s t o r y  ', ' i n f o r m a t i o n'//                                               &
        5x, ' number of nodal time histories  . . . . . . (ndout ) = ',i5 //                                                       &
        5x,'    node      dof     kinematic ',/                                                                                    &
        5x,'   number    number     type    ',/ )
2000 format(3i5)
3000 format(7x,i5,5x,i5,7x,a4)
    !
end subroutine dhist
    