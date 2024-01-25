      program tarefa5b
            
      real*8 :: ln_x = 0.d0, x = 0.d0, aux = 0.d0, eps = 1.d-16
      
      error = 1.d0
      n = 1
      
      read(*,*) x
      
      do while (abs(error).ge.eps)
         
         ln_x = ln_x + (1 - x)**real(n, 8)/ real(n, 8)
         error = ln_x - aux
         
         aux = ln_x
         n = n + 1
      end do
      
      ln_x = -1 * ln_x
      
      write(*,*) "Taylor: ", ln_x
      write(*,*) "Fortran log(x): ", dlog(x)
      write(*,*) "Erro absoluto", abs(ln_x - dlog(x))
      
      end program tarefa5b
      
