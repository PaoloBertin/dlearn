subroutine dctnry(name,ndim1,ndim2,ndim3,mpoint,ipr,mlast)
    !
    !.... program to store pointer information in dictionary
    !
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    character*4 name
    dimension name(2)
    !-ZACE-2005.08
    include 'memory_size.inc'
    common ia(max_size)
    
    mlast = mlast - 7
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    !      ia(mlast+1) = name(1)
    !      ia(mlast+2) = name(2)
    call chrint(ia(mlast+1),name(1))
    call chrint(ia(mlast+2),name(2))
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    ia(mlast+3) = mpoint
    ia(mlast+4) = ndim1
    ia(mlast+5) = ndim2
    ia(mlast+6) = ndim3
    ia(mlast+7) = ipr
    
    return
    
end subroutine dctnry