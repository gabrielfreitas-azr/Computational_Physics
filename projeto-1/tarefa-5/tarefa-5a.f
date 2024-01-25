      program tarefa5a
      
      real*4 :: ln_x = 0.e0, x = 0.e0, aux = 0.e0
      
      eps = 1.e-5
      error = 1.e0
      n = 1
      
      read(*,*) x
      
      do while (abs(error).ge.eps)
         
         ln_x = ln_x + (1 - x)**real(n, 4) / real(n, 4)
         
         error = ln_x - aux
         aux = ln_x
         n = n + 1
      end do
      
      ln_x = -1 * ln_x
      
      write(*,*) "Taylor: ", ln_x
      write(*,*) "Fortran log(x): ", log(x)
      write(*,*) "Erro absoluto", abs(ln_x - log(x))
      
      end program tarefa5a
      
