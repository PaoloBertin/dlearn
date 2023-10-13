subroutine pivots(a,idiag,neq,nsq,*)
    !
    !.... program to determine the number of zero and negative terms in
    !        array d of factorization a = u(transpose) * d * u
    !
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single-precision operation
    dimension a(1),idiag(1)
    common /iounit/ iin,iout,irsin,irsout

    iz = 0
    in = 0

    do 100 n=1,neq
        i = idiag(n)
        if (a(i).eq.0.) iz = iz + 1
        if (a(i).lt.0.) in = in + 1
100 continue

    write(iout,1000) nsq,iz,in

    return 1

1000 format(' ',                                                                                                                   &
            ' zero and/or negative pivots encountered                ', ///5x,                                                     &
            ' time sequence number   . . . . . . . . . . . (nsq  ) = ',i5//5x,                                                     &
            ' number of zeroes . . . . . . . . . . . . . . . . . . = ',i5//5x,                                                     &
            ' number of negatives  . . . . . . . . . . . . . . . . = ',i5//5x)
    !
end subroutine pivots

