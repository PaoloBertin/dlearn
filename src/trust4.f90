subroutine trust4(mat   ,ien   ,d     ,dl    ,x     ,xl    ,                                                                       &
                  det   ,shl   ,shg   ,xs    ,xint  ,b     ,                                                                       &
                 strain,c     ,stress,force ,area  ,                                                                               &
                 nint  ,numel ,nen   ,ndof  ,ned   ,nsd   ,                                                                        &
                 nesd  ,nrowsh,neg   ,nrowb ,nee   )
    !
    !.... program to calculate and print stress, strain and force for the
    !        three-dimensional, elastic truss element
    
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single-precision operation
    
    logical lnode3
    dimension mat(1),ien(nen,1),d(ndof,1),dl(ned,1),x(nsd,1),                                                                      &
              xl(nesd,1),det(1),shl(nrowsh,nen,1),shg(nrowsh,nen,1),                                                               &
              xs(nesd,1),xint(nesd,1),b(nrowb,1),strain(1),                                                                        &
              c(nrowb,nrowb,1),stress(1),force(1),area(1)
    
    nntot = 24
    nn = 0
    
    do 300 nel=1,numel
        
        m = mat(nel)
        call local(ien(1,nel),d,dl,nen,ndof,ned)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        lnode3 = .true.
        if ( nen .eq. 3 .and. ien(2,nel).eq.ien(3,nel) ) lnode3 = .false.
        call trushg(xl,det,shl,shg,xs,nen,nint,nel,neg,lnode3)
        
        ! loop over integration points
        do 200 l=1,nint
            
            ! calculate coordinates of integration points
            do 100 i=1,nesd
                xint(i,l) = rowdot(shg(nrowsh,1,l),xl(i,1),nrowsh,nesd,nen)
100         continue
            
            !.... calculate stress, strain and force
            call trustr(shg(1,1,l),xs(1,l),b,dl,strain,c(1,1,m),stress,                                                            &
                        force,area(m),nrowsh,nesd,nrowb,nen,nee)
            
            !.... print stress, strain and force
            call truspt(xint(1,l),stress,force,strain,nn,nntot,neg,nel,l)
200     continue

300 continue

    return
    end subroutine trust4
