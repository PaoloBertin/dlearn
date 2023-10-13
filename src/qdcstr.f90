subroutine qdcstr(shg,shgbar,b,r,dl,strain,c,stress,pstrn,pstrs,                                                                   &
                  nrowsh,nesd,nrowb,ibbar,nen,ned,nee,nstr,iopt)
    !
    !.... program to calculate stress, strain and principal values at an
    !        integration point for a two-dimensional continuum element
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single precision operation
    !
    dimension shg(nrowsh,1),shgbar(3,1),b(nrowb,1),dl(ned,1),                                                                      &
    &         strain(1),c(nrowb,1),stress(1),pstrn(1),pstrs(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    ! set up strain-displacement matrix
    call qdcb(shg,shgbar,b,r,iopt,nrowsh,nrowb,nen,ibbar)
    
    !.... calculate strains
    call multab(b,dl,strain,nrowb,nee,nstr,nee,nstr,1,1)
    
    ! calculate stresses
    call multab(c,strain,stress,nrowb,nstr,nstr,nstr,nstr,1,1)
    
    ! calculate principal strains; account for engineering shear strain
    strain(3) = pt5*strain(3)
    call princ(nesd,strain,pstrn)
    strain(3) = two*strain(3)
    pstrn(3)  = two*pstrn(3)
    
    ! calculate principal stress
    call princ(nesd,stress,pstrs)
    if (iopt.eq.0) then
        stress(4) = zero
        strain(4) = - ( c(4,1)*strain(1) + c(4,2)*strain(2) + c(4,3)*strain(3) )/c(4,4)
    endif
    
    if ( (iopt.eq.1) .and. (ibbar.eq.0) ) then
        strain(4) = zero
        stress(4) = c(4,1)*strain(1) + c(4,2)*strain(2) + c(4,3)*strain(3)
    endif
    
    return
    
end subroutine qdcstr
