subroutine eqset(neq,nalhs)
    !
    !.... program to allocate storage for global equation system
    !
    character*4 title

    common /bpoint/ mfirst,mlast,mtot,iprec
    common /iounit/ iin,iout,irsin,irsout
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp,mpalhs,mpbrhs
    common /titlec/ title(20)
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    dimension iadum(MAX_SIZE)
    equivalence(a(1),iadum(1))
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    
    !.... determine addresses of diagonals in left-hand-side matrix
    !cZACE MODIF (RD 30 mar 1987) -------------------------------------------
    call diag(iadum(mpdiag),neq,nalhs)
    mpalhs = mpoint('alhs    ',nalhs,0,0,iprec)
    mpbrhs = mpoint('brhs    ',neq  ,0,0,iprec)
    meanbw = nalhs/neq
    nwords = mtot - mlast + mfirst - 1

    ! write equation system data!
    write(iout,1000) title,neq,nalhs,meanbw,nwords
    
    return
    
1000 format('1',20a4//                                                                                                             &
            ' e q u a t i o n    s y s t e m    d a t a              ',  //5x,                                                     &
            ' number of equations . . . . . . . . . . . . (neq   ) = ',i8//5x,                                                     &
            ' number of terms in left-hand-side matrix  . (nalhs ) = ',i8//5x,                                                     &
            ' mean half bandwidth . . . . . . . . . . . . (meanbw) = ',i8//5x,                                                     &
            ' total length of blank common required . . . (nwords) = ',i8    )

end subroutine eqset