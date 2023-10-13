subroutine ltimef(g,nptslf,nltftn,iprtin)
    !
    !.... program to read, write and store load-time functions
    !
    !        g(i,1,l) = time i for load-time function l
    !        g(i,2,l) = load factor at time i for load-time function l
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    !
    dimension g(nptslf,2,nltftn)
    common /iounit/ iin,iout,irsin,irsout
    
    do 200 l=1,nltftn
    
        do 100 i=1,nptslf
            read(iin,1000) g(i,1,l),g(i,2,l)
100     continue
    
200 continue
    
    if (iprtin.eq.1) return
    
    write(iout,2000) nltftn
    do 400 l=1,nltftn
    
        do 300 i=1,nptslf
            if (mod(i,50).eq.1) write(iout,3000) l
            write(iout,4000) g(i,1,l),g(i,2,l)
300     continue
    
400 continue
    
    return
    
1000 format(2f10.0)
2000 format('1',' l o a d - t i m e   f u n c t i o n   d a t a ',//5x,                                                            &
                ' number of load-time funtions . . . . . (nltftn  ) = ',i5)
3000 format(///5x,' function number ',i5,//16x,'time',13x,'load factor'/ )
4000 format(5x,2(1pe20.8))
    
end subroutine ltimef
    