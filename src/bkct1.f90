subroutine bkct1(shl   ,w     ,rho   ,rdampm,rdampk,                                                                               &
                 c     ,grav  ,ien   ,mat   ,id    ,lm    ,                                                                        &
                 idiag ,ielno ,iside ,press ,ishist,                                                                               &
                 ntype ,numel ,numat ,nsurf ,nsout ,                                                                               &
                 istprt,lfsurf,lfbody,nicode,nint  ,ibbar ,                                                                        &
                 imass ,impexp,nrowsh,nrowb ,nesd  ,nen   ,                                                                        &
                 ndof  ,ned   ,iprtin,ldyn  )
    !
    ! subroutine to read, generate and write data for the eight-node brick, elastic element
    !
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single-precision operation
    logical ldyn
    dimension shl(nrowsh,nen,1),w(1),rho(1),rdampm(1),rdampk(1),                                                                   &
              c(nrowb,nrowb,1),grav(nesd),ien(nen,1),mat(1),                                                                       &
              id(ndof,1),lm(ned,nen,1),idiag(1),ielno(1),iside(1),                                                                 &
              press(4,1),ishist(3,1)                               
    common /iounit/ iin,iout,irsin,irsout

     write(iout,1000) ntype,numel,numat,nsurf,nsout,iopt,istprt,lfsurf,lfbody
     write(iout,2000) nicode,ibbar
     if (ldyn) write(iout,3000) imass,impexp

     call bkcshl(shl,w,nint)

     call prop3d(rho,rdampm,rdampk,c,numat,nrowb)

     read (iin,4000) grav
     write (iout,5000) grav

     call genel(ien,mat,nen)

     if (iprtin.eq.0) call prntel(mat,ien,nen,numel)

     call formlm(id,ien,lm,ndof,ned,nen,numel)

     if ( (.not.ldyn) .or. (impexp.eq.0) .or. (imass.eq.0) ) call colht(idiag,lm,ned,nen,numel)

     if (nsurf.gt.0) call bkcrsf(ielno,iside,press,nsurf)

     if (nsout.gt.0) call shist(ishist,nsout,ntype)

     return

1000 format(' ',' e i g h t - n o d e  ',                                                                                          &
     ' b r i c k   e l e m e n t s',                              //5x,                                                            &
     ' element type number . . . . . . . . . . . . (ntype ) = ',i5//5x,                                                            &
     ' number of elements  . . . . . . . . . . . . (numel ) = ',i5//5x,                                                            &
     ' number of element material sets . . . . . . (numat ) = ',i5//5x,                                                            &
     ' number of surface force cards . . . . . . . (nsurf ) = ',i5//5x,                                                            &
     ' number of stress/strain time histories  . . (nsout ) = ',i5//5x,                                                            &
     ' free option . . . . . . . . . . . . . . . . (iopt  ) = ',i5//5x,                                                            &
     ' stress output print code  . . . . . . . . . (istprt) = ',i5//5x,                                                            &
     '    eq.0, stress output printed                         ',   /5x,                                                            &
     '    eq.1, stress output not printed                     ',  //5x,                                                            &
     ' surface force load-time function number . . (lfsurf) = ',i5//5x,                                                            &
     ' body force load-time function number  . . . (lfbody) = ',i5 /5x)                                                            
2000 format(5x,                                                                                                                    &
    &' numerical integration code  . . . . . . . . (nicode) = ',i5//5x,                                                            &
    &'    eq.0, 2 x 2 x 2 gaussian quadrature                 ',   /5x,                                                            &
    &'    eq.1, 1-point   gaussian quadrature                 ',  //5x,                                                            &
    &' strain-displacement option  . . . . . . . . (ibbar ) = ',i5//5x,                                                            &
    &'    eq.0, standard formulation                          ',   /5x,                                                            &
    &'    eq.1, b-bar formulation                             ',   /5x)
3000 format(5x,                                                                                                                    &
    &' mass type code  . . . . . . . . . . . . . . (imass ) = ',i5//5x,                                                            &
    &'    eq.0, consistent mass matrix                        ',   /5x,                                                            &
    &'    eq.1, lumped mass matrix                            ',   /5x,                                                            &
    &'    eq.2, no mass matrix                                ',  //5x,                                                            &
    &' implicit/explicit code  . . . . . . . . . . (impexp) = ',i5//5x,                                                            &
    &'    eq.0, implicit element group                        ',   /5x,                                                            &
    &'    eq.1, explicit element group                        ',  //5x)                  
4000 format(8f10.0)
5000 format(////' ',                                                                                                               &
    &' g r a v i t y   v e c t o r   c o m p o n e n t s      ',  //5x,                                                            &
    &' x-1 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x,                                                            &
    &' x-2 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x,                                                            &
    &' x-3 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x)
!
     end
