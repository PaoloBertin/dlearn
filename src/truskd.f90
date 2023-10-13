subroutine truskd(w,det,shg,xs,xl,b,dl,strain,c,dmat,stress,work,                                                                  &
                  elresf,constk,nen,nint,nrowsh,nesd,nrowb,nee)
    !
    ! program to form internal force ("-k*d") for the
    ! three-dimensional, elastic truss element
    implicit double precision (a-h,o-z)
    !
    ! deactivate above card(s) for single-precision operation
    !
    dimension w(1),det(1),shg(nrowsh,nen,1),xs(nesd,1),xl(nesd,1),                                                                 &
              b(nrowb,1),dl(1),work(1),elresf(1)
    !
    ! loop on integration points
    !
    do 100 l=1,nint
        temp = constk*w(l)*det(l)
        
        ! set up the strain-displacement matrix
        call trusb(b,shg(1,1,l),xs,nen,nesd,nrowb,nrowsh)
        
        ! calculate strain
        strain = rcdot(b,dl,nrowb,nee)
        
        ! calculate stress
        stress = c*strain
        
        ! calculate element internal force
        stress = temp*stress
        call smult(stress,b,work,nrowb,1,1,nee,1)
        call matadd(elresf,work,elresf,nee,nee,nee,nee,1,1)
100 continue
    
    return

end subroutine truskd
