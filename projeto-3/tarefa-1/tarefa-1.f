      program tarefa1
      
      real*8 :: f
      real*8 :: x = 0.5d0, df_2f = 0.d0, df_2t = 0.d0, df_3s = 0.d0,
     &   df_5s = 0.d0, d2f_3s = 0.d0, d2f_5s = 0.d0, d3f_5a = 0.d0
      
      ! definição do vetor com os passos
      real*8 :: h(14) = (/5.d-1, 1.d-1, 5.d-2, 1.d-2, 5.d-3, 1.d-3, 
     &   5.d-4, 1.d-4, 5.d-5, 1.d-5, 5.d-6, 1.d-6, 5.d-7, 1.d-8/)
     
      ! valor teórico das derivadas 
      real*8 :: df_dx = 9.79678201384d0, df2_dx = 64.09832454947d0, 
     &   df3_dx = 671.51461345787d0
     
      open(11, file = "tarefa-1-saida-1.dat")
      
      do i = 1, 14
         ! loop para cada passo
         
         ! calcula a derivada para frente de 2 pontos
         df_2f = (f(x + h(i)) - f(x)) / h(i)
         
         ! calcula a derivada para trás de 2 pontos
         df_2t = (f(x) - f(x - h(i))) / h(i)
         
         ! calcula a derivada simétrica de 3 pontos
         df_3s = (f(x + h(i)) - f(x - h(i))) / (2.d0 * h(i))
         
         ! calcula a derivada simétrica de 5 pontos
         df_5s = (f(x - 2 * h(i)) - 8 * f(x - h(i)) + 8 * f(x + h(i)) - 
     &   f(x + 2 * h(i))) / (12 * h(i))
     
         ! calcula a segunda derivada simétrica de 3 pontos
         d2f_3s = (f(x + h(i)) - 2 * f(x) + f(x - h(i))) / (h(i)**2)
         
         ! calcula a segunda derivada simétrica de 5 pontos
         d2f_5s = (-f(x - 2 * h(i)) + 16 * f(x - h(i)) -30 * f(x) + 16 *
     &   f(x + h(i)) - f(x + 2 * h(i))) / (12 * h(i)**2)

         ! calcula a segunda derivada simétrica de 5 pontos     
         d3f_5a = (-f(x - 2 * h(i)) + 2 * f(x - h(i)) - 2 * f(x + h(i))
     &   + f(x + 2 * h(i))) / (2 * h(i)**3)
         
         ! escreve nos arquivos o desvio em módulo do valor analítico
         ! definido no início do programa e o calculado numericamente
         write(11,*) h(i), abs(df_2f - df_dx), abs(df_2t - df_dx), 
     &   abs(df_3s - df_dx), abs(df_5s - df_dx), abs(d2f_3s - df2_dx), 
     &   abs(d2f_5s - df2_dx), abs(d3f_5a - df3_dx)
         
      end do
      
      close(11)
      
      end program tarefa1
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      
      f = dexp(x / 2.d0) * dtan(2.d0 * x)
      
      return 
      end function f
