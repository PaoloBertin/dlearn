subroutine serror(name,i)
    !
    !.... program to print error message if available storage is exceeded
    !
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    character*4 name
    dimension name(2)
    common /iounit/ iin,iout,irsin,irsout
    
    call prtdc
    write(iout,1000) i,name(1),name(2)
    stop
    
1000 format(1x,5('*'),'storage exceeded by ',i10, ' words in attempting to store array ',2a4)

end subroutine serror

