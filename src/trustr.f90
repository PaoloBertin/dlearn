subroutine trustr(shg,xs,b,dl,strain,c,stress,force,area,nrowsh,nesd,nrowb,nen,nee)
    !
    !.... program to calculate stress, strain and force at an integration
    !     point for the three-dimensional, elastic truss element
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension shg(nrowsh,1),xs(nesd,1),b(nrowb,1),dl(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    !.... set up strain-displacement matrix
    call trusb(b,shg,xs,nen,nesd,nrowb,nrowsh)
    
    !.... calculate strain
    strain = rcdot(b,dl,nrowb,nee)
    
    !.... calculate stress
    stress = c*strain
    
    !.... calculate forces
    force = area*stress
    
    return
    
end subroutine trustr
