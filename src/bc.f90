subroutine bc(id,ndof,numnp,neq,iprtin)
    !
    ! subroutine to read, generate and write boundary condition data
    ! and establish equation numbers
    !
    dimension id(ndof,1)
    
    common /iounit/ iin,iout,irsin,irsout
    logical pflag
    
    call iclear(id,ndof*numnp)
    call igen(id,ndof)
    
    if (iprtin.eq.0) then
        nn=0
        do 200 n=1,numnp
            pflag = .false.
            
            do 100 i=1,ndof
                if (id(i,n).ne.0) pflag = .true.
100         continue
    
            if (pflag) then
                nn = nn + 1
                if (mod(nn,50).eq.1) write(iout,1000) (i,i=1,ndof)
                write(iout,2000) n,(id(i,n),i=1,ndof)
            endif
200     continue
    endif
    
    ! establish equation numbers
    neq = 0
    
    do 400 n=1,numnp
        do 300 i=1,ndof
            if (id(i,n).eq.0) then
                neq = neq + 1
                id(i,n) = neq
            else
                id(i,n) = 1 - id(i,n)
            endif
        300 continue
    400 continue
    
    return
    
1000 format('1',' n o d a l   b o u n d a r y   c o n d i t i o n   c o d e s'//                                                   &
         5x,' node no.',3x,6(6x,'dof',i1:)//)
     
2000 format(6x,i5,5x,6(5x,i5))

end subroutine bc