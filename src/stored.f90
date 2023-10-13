subroutine stored(idhist,d,v,a,dout,ndof,ndout,ndout1)
    !
    !.... program to store nodal time histories as single-precision data
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    ! ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    real dout(ndout1,1)
    dimension idhist(3,1),d(ndof,1),v(ndof,1),a(ndof,1)
    common /hplotc/ nplpts,locplt,time
    
    dout(1,locplt) = real(time)
    
    do 100 i=1,ndout
        node = idhist(1,i)
        idof = idhist(2,i)
        idva = idhist(3,i)
        if (idva .eq. 1) dout(i+1,locplt) = real(d(idof,node))
        if (idva .eq. 2) dout(i+1,locplt) = real(v(idof,node))
        if (idva .eq. 3) dout(i+1,locplt) = real(a(idof,node))
100 continue
    
    return
end

