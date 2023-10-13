subroutine trust5(ishist,sout  ,mat   ,ien   ,d     ,dl    ,                                                                       &
                  x     ,xl    ,det   ,shl   ,shg   ,xs    ,                                                                       &
                  b     ,strain,c     ,stress,force ,area  ,                                                                       &
                  work  ,                                                                                                          &
                  nsout ,nen   ,ndof  ,ned   ,nsd   ,nesd  ,                                                                       &
                  nrowsh,nint  ,neg   ,nrowb ,nee   ,nsout1)
    !
    !.... program to calculate and store element time-histories for the
    !        three-dimensional, elastic truss element
    
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single-precision operation
    
    real sout
    logical lnode3
    dimension ishist(3,1),sout(nsout1,1),mat(1),ien(nen,1),d(ndof,1),                                                              &
              dl(ned,1),x(nsd,1),xl(nesd,1),det(1),shl(nrowsh,nen,1),                                                              &
              shg(nrowsh,nen,1),xs(nesd,1),b(nrowb,1),strain(1),                                                                   &
              c(nrowb,nrowb,1),stress(1),force(1),area(1),work(1)
    common /hplotc/ nplpts,locplt,time
    
    sout(1,locplt) = real(time)
    
    do 100 i=1,nsout
        
        nel   = ishist(1,i)
        intpt = ishist(2,i)
        ncomp = ishist(3,i)
        
        m = mat(nel)
        call local(ien(1,nel),d,dl,nen,ndof,ned)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        lnode3 = .true.
        if ( nen .eq. 3 .and. ien(2,nel).eq.ien(3,nel) ) lnode3 = .false.
        call trushg(xl,det,shl,shg,xs,nen,nint,nel,neg,lnode3)
        
        call trustr(shg(1,1,intpt),xs(1,intpt),b,dl,strain,c(1,1,m),                                                               &
                    stress,force,area(m),nrowsh,nesd,nrowb,nen,nee)
        
        work(1) = stress(1)
        work(2) = force(1)
        work(3) = strain(1)
        sout(i+1,locplt) = real(work(ncomp))
        
100 continue
    
    return
    
end subroutine trust5
