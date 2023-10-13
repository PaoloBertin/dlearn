subroutine qdck(shgbar,w,det,r,shg,b,c,dmat,db,elstif,constk,                                                                      &
                 ibbar,nen,nint,iopt,nesd,nrowsh,nrowb,nstr,nee)
    !
    !.... program to form stiffness matrix for a continuum element
    !        with "nen" nodes
    !
    !        note: the b-bar option is restricted to the mean-dilatation
    !              formulation. to generalize to other formulations,
    !              redimension array "shgbar", and replace routines
    !              "meansh" and "qdcb".
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension shgbar(3,nen,1),w(1),det(1),r(1),shg(nrowsh,nen,1),                                                                  &
              b(nrowb,1),c(nrowb,1),dmat(nrowb,1),db(nrowb,1),                                                                     &
              elstif(nee,1)
                  
    !.... calculate mean values of shape function global derivatives
    !        for mean-dilatational b-bar formulation    
    if (ibbar.eq.1) call meansh(shgbar,w,det,r,shg,nen,nint,iopt,nesd,nrowsh)
    
    !.... loop on integration points
    do 100 l=1,nint
        temp = constk*w(l)*det(l)
        
        !.... set up the strain-displacement matrix
        call qdcb(shg(1,1,l),shgbar,b,r(l),iopt,nrowsh,nrowb,nen,ibbar)
        
        !.... set up the constitutive matrix
        call setupd(c,dmat,temp,nstr,nrowb)
        
        !.... multiply d*b
        call multab(dmat,b,db,nrowb,nrowb,nrowb,nstr,nstr,nee,1)
        
        !.... multiply b(transpose) * db, taking account of symmetry,
        !        and accumulate in elstif
        call btdb(elstif,b,db,nee,nrowb,nstr)
        !
100 continue
    
    return
end subroutine qdck
