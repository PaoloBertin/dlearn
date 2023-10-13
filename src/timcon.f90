subroutine timcon(nsq,nstep ,ndprt ,nsprt ,nhplt ,niter ,                                                                          &
                      nstep1,ndprt1,nsprt1,nhplt1,niter1,                                                                          &
                      alpha ,beta  ,gamma ,dt    )
    !
    !.... program to compute current time sequence parameters
    !        and time-integration coefficients
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single precision operation
    dimension nstep(1),ndprt(1),nsprt(1),nhplt(1),niter(1),                                                                        &
              alpha(1),beta(1) ,gamma(1),dt(1)
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,                                                                     &
                    coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    nstep1 = nstep(nsq)
    ndprt1 = ndprt(nsq)
    nsprt1 = nsprt(nsq)
    nhplt1 = nhplt(nsq)
    niter1 = niter(nsq)
    alpha1 = alpha(nsq)
    beta1  = beta(nsq)
    gamma1 = gamma(nsq)
    dt1    = dt(nsq)
    
    coeff1 = one + alpha1
    coeff2 = gamma1*dt1
    coeff3 = beta1*dt1*dt1
    coeff4 = coeff1*coeff2
    coeff5 = coeff1*coeff3
    coeff6 = coeff1*dt1
    coeff7 = pt5*coeff1*(one - two*beta1)*dt1*dt1
    coeff8 = coeff1*(one - gamma1)*dt1
    
    return

end subroutine timcon
