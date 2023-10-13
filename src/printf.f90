subroutine printf(f,ndof,numnp,nlv)
    !
    !.... program to print prescribed force and boundary condition data
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    logical lzero
    dimension f(ndof,numnp,1)
    common /iounit/ iin,iout,irsin,irsout
    
    nn = 0
    do 100 n=1,numnp
        call ztest(f(1,n,nlv),ndof,lzero)
        if (.not.lzero) then
            nn = nn + 1
            if (mod(nn,50).eq.1) write(iout,1000) nlv,(i,i=1,ndof)
            write(iout,2000) n,(f(i,n,nlv),i=1,ndof)
        endif
100 continue
    
    return
    
1000 format('1',                                                                                                                   &
         &' p r e s c r i b e d   f o r c e s   a n d   k i n e m a t i c ',                                                       &
         &'  b o u n d a r y   c o n d i t i o n s'//5x,                                                                           &
         &' load vector number = ',i5///5x,                                                                                        &
         &' node no.',6(13x,'dof',i1,:)/ )
2000 format(6x,i5,10x,6(1pe15.8,2x))
end subroutine printf
