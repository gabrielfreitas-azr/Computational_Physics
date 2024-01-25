      program tarefa3c
      
      real*8 :: f
      real*8 :: x_i = 0.d0, x_i_1 = 0.d0, x_n = 0.d0, x_n_plus_1 = 0.d0,
     &          x_n_minus_1 = 0.d0
      real*8 :: eps = 1.d-3, h = 0.1d0, tol = 1.d-6
      integer*8 :: ia = -10, ib = 10, icount = 0
      
      open(11, file = "tarefa-3c-saida-1.dat")
      
      N =  (ib - ia) / h
      
      do i = 1, N
      
         ! loop para busca direta
      
         x_i = real(ia + (i - 1) * h, 8) 
         x_i_1 = x_i + eps + h
         
         if (f(x_i) * f(x_i_1).lt.0) then
         
            ! aplica o método da secante
           
            icount = 0
            
            x_n_minus_1 = x_i
            x_n = x_i_1
            
            x_n_plus_1 = x_n - f(x_n) * (x_n - x_n_minus_1) / 
     &      (f(x_n) - f(x_n_minus_1))
            
            do while(abs(x_n_plus_1 - x_n).gt.tol)
            
               icount = 1 + icount
            
               x_n_minus_1 = x_n
               x_n = x_n_plus_1
               
               x_n_plus_1 = x_n - f(x_n) * (x_n - x_n_minus_1) / 
     &         (f(x_n) - f(x_n_minus_1))
     
               write(11,*) icount, x_n_plus_1
           
            end do      
         end if
      end do
      
      close(11)
      
      end program tarefa3c
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      
      f = x**3 -4 * x**2 -59 * x + 126
      
      return 
      end function f
