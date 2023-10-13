subroutine bkcstr(shg,shgbar,b,dl,strain,c,stress,pstrn,pstrs,nrowsh,nesd,nrowb,ibbar,nen,ned,nee,nstr)
    !
    !    subroutine to calculate stress, strain and principal values at an
    !    integration point for a three-dimensional continuum element
    !
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single precision operation
    dimension shg(nrowsh,1),shgbar(nrowsh,1),b(nrowb,1),dl(ned,1),                                                                &
              strain(1),c(nrowb,1),stress(1),pstrn(1),pstrs(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    ! set up strain-displacement matrix
    
    call bkcb(shg,shgbar,b,nrowsh,nrowb,nen,ibbar)

    ! calculate strains
    call multab(b,dl,strain,nrowb,nee,nstr,nee,nstr,1,1)

    ! calculate stresses
    call multab(c,strain,stress,nrowb,nstr,nstr,nstr,nstr,1,1)

    ! calculate principal strains; account for engineering shear strain
    strain(4) = pt5*strain(4)
    strain(5) = pt5*strain(5)
    strain(6) = pt5*strain(6)
    call princ(nesd,strain,pstrn)
    strain(4) = two*strain(4)
    strain(5) = two*strain(5)
    strain(6) = two*strain(6)

    ! calculate principal stress
    call princ(nesd,stress,pstrs)
    
    return
    
end subroutine bkcstr
