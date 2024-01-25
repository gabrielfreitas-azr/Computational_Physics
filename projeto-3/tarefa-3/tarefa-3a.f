      program tarefa3a
      
      real*8 :: f
      real*8 :: tol = 1.d-6, x_i = 0.d0, x_i_1 = 0.d0, x_m = 0.d0
      real*8 :: eps = 1.d-3, h = 0.1d0
      integer*8 :: ia = -10, ib = 10, icount = 0
      
      open(11, file = "tarefa-3a-saida-1.dat")
      
      N =  (ib - ia) / h
      
      do i = 1, N
      
         ! loop para busca direta
      
         x_i = real(ia + (i - 1) * h, 8) 
         x_i_1 = x_i + eps + h
         
         
         if (f(x_i) * f(x_i_1).lt.0) then
           
            icount = 0
            
            ! primeiro intervalo a = x_i, b = x_i_1
            
            do while(abs(x_i_1 - x_i).gt.tol)
            
               icount = 1 + icount
            
               x_m = (x_i_1 + x_i) / 2
               
               if(f(x_m) * f(x_i_1).gt.(0.d0)) then
                  x_i_1 = x_m
               else
                  x_i = x_m
               end if
               
               write(11,*) icount, x_i         
            end do   
            
         end if
      end do
      
      close(11)
      
      end program tarefa3a
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      
      f = x**3 -4 * x**2 -59 * x + 126
      
      return 
      end function f
