      program tarefa3b
      
      real*8 :: f, df_dx
      real*8 :: x_i = 0.d0, x_i_1 = 0.d0, x_n = 0.d0, x_n_1 = 0.d0
      real*8 :: eps = 1.d-3, h = 0.1d0, tol = 1.d-6
      integer*8 :: ia = -10, ib = 10, icount = 0
      
      open(11, file = "tarefa-3b-saida-1.dat")
      
      N = (ib - ia) / h
      
      do i = 1, N
      
         ! loop para a busca direta
      
         x_i = real(ia + (i - 1) * h, 8) 
         x_i_1 = x_i + h + eps
         
         if (f(x_i) * f(x_i_1).lt.0) then
         
            ! aplica o método de newton-raphson
           
            icount = 0
            
            ! utiliza o extremo x_i como "chute" inicial para o método
            
            x_n = x_i
            x_n_1 = x_n - f(x_n) / df_dx(x_n)
            
            do while(abs(x_n_1 - x_n).gt.tol)
            
               icount = 1 + icount
            
               x_n = x_n_1
               x_n_1 = x_n - f(x_n) / df_dx(x_n)
               
               write(11,*) icount, x_n_1
               
            end do           
         end if
      end do
      
      close(11)
      
      end program tarefa3b
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      
      f = x**3 -4 * x**2 -59 * x + 126
      
      return 
      end function f
      
      function df_dx(x)
      
      real*8 :: df_dx
      real*8 :: x
      
      df_dx = 3 * x**2 -8 * x - 59
      
      return
      end function df_dx
