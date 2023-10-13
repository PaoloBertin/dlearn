subroutine trust1(shl   ,w     ,rho   ,rdampm,rdampk,area  ,                                                                       &
    c     ,grav  ,ien   ,mat   ,id    ,lm    ,                                                                       &
    idiag ,ishist,                                                                                                   &
    ntype ,numel ,numat ,nen   ,nsout ,istprt,                                                                       &
    lfbody,nint  ,imass ,impexp,nrowsh,nrowb ,                                                                       &
    nesd  ,ndof  ,ned   ,iprtin,ldyn  )
    !
    !.... program to read, generate and write element data for the
    !     three-dimensional, elastic truss element
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single precision operation

    logical ldyn
    dimension shl(nrowsh,nen,1),w(1),rho(1),rdampm(1),rdampk(1),                                                                   &
        area(1),c(nrowb,nrowb,1),grav(nesd),ien(nen,1),mat(1),                                                               &
        id(ndof,1),lm(ned,nen,1),idiag(1),ishist(3,1)
    common /iounit/ iin,iout,irsin,irsout

    write(iout,1000) ntype,numel,numat,nen,nsout,istprt,lfbody,nint
    if (ldyn) write(iout,2000) imass,impexp

    call trushl(shl,w,nint,nen)

    call truspr(rho,rdampm,rdampk,area,c,numat)

    read(iin,3000) grav
    write(iout,4000) grav

    call genel(ien,mat,nen)

    if (iprtin.eq.0) call prntel(mat,ien,nen,numel)

    call formlm(id,ien,lm,ndof,ned,nen,numel)

    if ( (.not.ldyn) .or. (impexp.eq.0) .or. (imass.eq.0) ) call colht(idiag,lm,ned,nen,numel)

    if (nsout.gt.0) call shist(ishist,nsout,ntype)

    return

1000 format(' ',                                                                                                                   &
        ' t w o / t h r e e - n o d e   t r u s s   e l e m e n t s',//5x,                                                         &
        ' element type number . . . . . . . . . . . . (ntype ) = ',i5//5x,                                                         &
        ' number of elements  . . . . . . . . . . . . (numel ) = ',i5//5x,                                                         &
        ' number of element material sets . . . . . . (numat ) = ',i5//5x,                                                         &
        ' number of element nodes . . . . . . . . . . (nen   ) = ',i5//5x,                                                         &
        ' number of stress/strain time histories  . . (nsout ) = ',i5//5x,                                                         &
        ' stress output print code  . . . . . . . . . (istprt) = ',i5//5x,                                                         &
        '    eq.0, stress output printed                         ',   /5x,                                                         &
        '    eq.1, stress output not printed                     ',  //5x,                                                         &
        ' body force load-time function number  . . . (lfbody) = ',i5//5x,                                                         &
        ' integration code  . . . . . . . . . . . . . (nint  ) = ',i5//5x,                                                         &
        '    eq.1, 1-point gaussian quadrature                   ',   /5x,                                                         &
        '    eq.2, 2-point gaussian quadrature                   ',   /5x,                                                         &
        '    eq.3, 3-point gaussian quadrature                   ',   /5x)

2000 format(' ',/5x,                                                                                                               &
        ' mass type code  . . . . . . . . . . . . . . (imass ) = ',i5//5x,                                                         &
        '    eq.0, consistent mass matrix                        ',   /5x,                                                         &
        '    eq.1, lumped mass matrix                            ',   /5x,                                                         &
        '    eq.2, no mass matrix                                ',  //5x,                                                         &
        ' implicit/explicit code  . . . . . . . . . . (impexp) = ',i5//5x,                                                         &
        '    eq.0, implicit element group                        ',   /5x,                                                         &
        '    eq.1, explicit element group                        ',  //5x)

3000 format(8f10.0)

4000 format(////' ',                                                                                                               &
    ' g r a v i t y   v e c t o r   c o m p o n e n t s      ',  //5x,                                                             &
    ' x-1 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x,                                                             &
    ' x-2 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x,                                                             &
    ' x-3 direction . . . . . . . . . . . . . . = ',      1pe15.8//5x)

end subroutine trust1
