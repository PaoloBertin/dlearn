subroutine iterup(id,d,dpred,vpred,a,brhs,ndof,numnp,ldyn)
    !
    !.... program to perform intermediate update of displacements,
    !        velocities and accelerations during iterative loop in
    !        predictor/corrector algorithm
    !
          implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    logical ldyn
    dimension id(ndof,1),d(ndof,1),dpred(ndof,1),vpred(ndof,1),a(ndof,1),brhs(1)
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    
    if (ldyn) then
    
        do 200 i=1,ndof
        
            do 100 j=1,numnp
                k = id(i,j)
                if (k.gt.0) then
                    dpred(i,j) = dpred(i,j) + coeff5*brhs(k)
                    vpred(i,j) = vpred(i,j) + coeff4*brhs(k)
                    a(i,j) = a(i,j) + brhs(k)
                endif
100         continue
        
200     continue
    
    else
    
        do 400 i=1,ndof

            do 300 j=1,numnp
                k = id(i,j)
                if (k.gt.0) d(i,j) = brhs(k)
300         continue
    
400     continue
    
    endif
    
    return
    
end subroutine iterup
    