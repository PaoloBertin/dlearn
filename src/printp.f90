subroutine printp(a,idiag,neq,nsq,*)
    !
    !.... program to print array d after Crout factorization
    !        a = u(transpose) * d * u
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension a(1),idiag(1)
    common /iounit/ iin,iout,irsin,irsout
    
    do 100 n=1,neq
        if (mod(n,50).eq.1) write(iout,1000) nsq
        i = idiag(n)
        write(iout,2000) n,a(i)
100 continue
    
    return 1
    
1000 format('1',' array d of factorization',                                                                                       &
            ' a = u(transpose) * d * u ',                                //5x,                                                     &
            ' time sequence number   . . . . . . . . . . . . (nsq) = ',i5//5x)

2000 format(1x,i5,4x,1pe20.8)

end subroutine printp
    