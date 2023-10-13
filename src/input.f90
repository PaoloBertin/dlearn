subroutine input(f,ndof,numnp,j,nlvect,iprtin,time)
    !
    ! subroutine to read, generate and write nodal input data
    !
    ! f(ndof,numnp,nlvect) = prescribed forces/kinematic data (j=0)
    !                      = initial displacements (j=1)
    !                      = initial velocities(j=2)
    !                      = initial accelerations (j=3)
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    logical lzero
    dimension f(ndof,numnp,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    call clear(f,nlvect*numnp*ndof)
    
    do 100 nlv=1,nlvect
        call genfl(f(1,1,nlv),ndof)
        call ztest(f(1,1,nlv),ndof*numnp,lzero)
    
        if (iprtin.eq.0) then
    
            if (lzero) then
                if (j.eq.0) write(iout,1000) nlv
                if (j.eq.1) write(iout,2000)
                if (j.eq.2) write(iout,3000)
                if (j.eq.3) write(iout,4000)
             else
                if (j.eq.0) call printf(f,ndof,numnp,nlv)
    
                if (j.eq.1) call printd(' i n i t i a l   d i s p l a c e m e n t s  ', f,ndof,numnp,0,time)
    
                if (j.eq.2) call printd(' i n i t i a l   v e l o c i t i e s        ', f,ndof,numnp,0,time)
    
                if (j.eq.3) call printd(' i n i t i a l   a c c e l e r a t i o n s  ', f,ndof,numnp,0,time)
             endif
          endif
100 continue
    
    return
1000 format('1'//,' there are no nonzero prescribed forces and kinematic boundary conditions for load vector number ',i5)
2000 format('1'//,' there are no nonzero initial displacements')
3000 format('1'//,' there are no nonzero initial velocities   ')
4000 format('1'//,' there are no nonzero initial accelerations')

end subroutine input
    