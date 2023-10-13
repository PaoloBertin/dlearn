subroutine timlog
    !
    !.... program to print log of execution times
    !
    character*4 title
    common /etimec/ etime(7)
    common /iounit/ iin,iout,irsin,irsout
    common /titlec/ title(20)
    
    subtot = 0.0
    do 100 i=3,7
        subtot = subtot + etime(i)
100 continue
    
    write(iout,1000) title,etime,subtot
    
    return
    
1000 format('1',20a4///5x,                                                                                                         &
    &' e x e c u t i o n   t i m i n g   i n f o r m a t i o n'  ///5x,                                                            &
    &' i n i t i a l i z a t i o n   p h a s e        = ',1pe10.3///5x,                                                            &
    &' s o l u t i o n   p h a s e                    = ',1pe10.3///5x,                                                            &
    &'     formation of left-hand-side matrices       = ',1pe10.3 //5x,                                                            &
    &'     factorizations                             = ',1pe10.3 //5x,                                                            &
    &'     formation of right-hand-side vectors       = ',1pe10.3 //5x,                                                            &
    &'     forward reductions/back substitutions      = ',1pe10.3 //5x,                                                            &
    &'     calculation of element output              = ',1pe10.3  /5x,                                                            &
    &51x,'_________',//5x,                                                                                                         &
    &'     subtotal                                   = ',1pe10.3     )

end subroutine timlog

