function mpoint(name,ndim1,ndim2,ndim3,ipr)
    !
    !.... program to calculate storage pointer
    !
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    character*4 name
    dimension name(2)
    common /bpoint/ mfirst,mlast,mtot,iprec
    
    mpoint = mfirst
    if ( iprec.eq.2 .and. mod(mpoint,2).eq.0 ) mpoint = mpoint + 1
    call dctnry(name,ndim1,ndim2,ndim3,mpoint,ipr,mlast)
    mfirst = mpoint + ndim1*max0(1,ndim2)*max0(1,ndim3)*ipr
    if (mfirst.ge.mlast) call serror(name,mfirst-mlast)
    
    return
    
end function mpoint
    