      program tarefa2c
      
      real*8 :: f
       
      real*8 :: h(11) = 0.d0     
      real*8 :: dint_f = 0.d0, x_i = 0.d0
      
      integer*8 :: ia = 0, ib = 1
      
      open(11, file = "tarefa-2c-saida-1.dat")
      
      ! inicializa o vetor com os passos (h)
      
      do i = 2, 12     
         h(i - 1) = 1.d0 / (3.d0 * 2.d0 ** real(i, 8))
      end do
      
      ! loop para todos os passos
      
      do j = 1, 11
         
         N = (ib - ia) / h(j)
         dint_f = 0.d0
         
         do i = 3, N - 1, 4
         
            x_i = real(ia, 8) + real(i - 1, 8) * h(j)
            
            dint_f = dint_f + 
     &      (7.d0 * f(x_i + 2.d0 * h(j)) + 32.d0 * f(x_i + h(j)) 
     &      + 12.d0 * f(x_i) 
     &      + 7.d0 * f(x_i - 2.d0 * h(j)) + 32.d0 * f(x_i - h(j)))
     
         end do  
         
         dint_f = dint_f * 2.d0 * h(j) / 45.d0
         
         write(11,*) dint_f
         
      end do
      
      close(11)
 
      end program tarefa2c
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      real*8 :: pi = dacos(-1.d0)
      
      f = dexp(-x) * dcos(2.d0 * pi * x)
      
      return 
      end function f
