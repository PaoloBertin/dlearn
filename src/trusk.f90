subroutine trusk(w,det,shg,xs,xl,b,c,dmat,db,elstif,constk,                                                                        &
                 nen,nint,nesd,nrowsh,nrowb,nstr,nee)
    !
    !.... program to form stiffness matrix for the
    !        three-dimensional, elastic truss element
    
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single precision operation
    dimension w(1),det(1),shg(nrowsh,nen,1),xs(nesd,1),xl(nesd,1),                                                                 &
              b(nrowb,1),db(nrowb,1),elstif(nee,1)
    
    ! loop on integration points
    
    do 100 l=1,nint
        temp1 = constk*w(l)*det(l)
        
        ! set up the strain-displacement matrix
        call trusb(b,shg(1,1,l),xs(1,l),nen,nesd,nrowb,nrowsh)
        
        ! set up the constitutive "matrix"
        dmat = c*temp1
        
        ! multiply dmat * b
        call smult(dmat,b,db,nrowb,nrowb,nstr,nee,1)
        
        ! multiply b(transpose) * db, taking account of symmetry, and accumulate in elstif
        call btdb(elstif,b,db,nee,nrowb,nstr)
        
100 continue
    
    return

    end subroutine trusk
