      program tarefa2a
      implicit real*8(a-h, o-z)
      
      real*8 :: pi = dacos(-1.d0)
      
      !     Raio de órbita dos planetas.
                 
      real*8 :: a(8) = (/0.39d0, 0.72d0, 1.d0, 1.52d0, 5.20d0, 9.58d0,
     &               19.2d0, 30.1d0/)
     
      !     Arquivos de saída para o método de Velvet.

      open(11, file = 'tarefa-2a-saida-1.dat')
      open(12, file = 'tarefa-2a-saida-2.dat')
      open(13, file = 'tarefa-2a-saida-3.dat')      
      open(14, file = 'tarefa-2a-saida-4.dat')      
      open(15, file = 'tarefa-2a-saida-5.dat')      
      open(16, file = 'tarefa-2a-saida-6.dat')      
      open(17, file = 'tarefa-2a-saida-7.dat')      
      open(18, file = 'tarefa-2a-saida-8.dat') 
      
      !     Arquivos de saída para o método de Euler-Cromer.
      
      open(19, file = 'tarefa-2a-saida-9.dat')
      open(20, file = 'tarefa-2a-saida-10.dat')
      open(21, file = 'tarefa-2a-saida-11.dat')      
      open(22, file = 'tarefa-2a-saida-12.dat')      
      open(23, file = 'tarefa-2a-saida-13.dat')      
      open(24, file = 'tarefa-2a-saida-14.dat')      
      open(25, file = 'tarefa-2a-saida-15.dat')      
      open(26, file = 'tarefa-2a-saida-16.dat')   
      
      !     Loop que aplica o método de velvet para cada 
      !     condição inicial.        
      
      do i = 1, 5
                 
         call velvet_method(i + 10, a(i), 1.d-4, 50.d0)
         close(i + 10)
         
      end do  
            
      end program tarefa2a
      
      !     Função que calcula o módulo de um vetor de
      !     duas dimensões.
      
      function vec_mod(x, y)
      
      real*8 :: vec_mod, x, y
      
      vec_mod = dsqrt(x**2.d0 + y**2.d0)
      
      return
      end function vec_mod
      
      !     Subrotina que implementa o método de velvet.
      !  
      !     nfile = número do arquivo de saída
      !     x0 = posição x inicial do planeta
      !     dtau = passo de iteração
      !     tau_m = tempo máximo de iterações
            
      subroutine velvet_method(nfile, x0, dtau, tau_m)     
      implicit real*8(a-h, o-z)
      
      write(nfile,*) "tau x y rho vx vy v"
      
      tau = 0.d0
      
      !     Condições inicias 
      !
      !     rho = (a, 0), 
      !     v = (0, 1 / sqrt(a)).
            
      x_aux = x0
      y_aux = 0.d0
      
      rho = vec_mod(x_aux, y_aux)
      
      vx = 0.d0
      !vy = 1.d0 / dsqrt(x_aux)
      vy = dsqrt(2.d0 / rho) + 1.d-4
      v = vec_mod(vx, vy)
        
      !     Escreve as condições inicias.
      
      write(nfile,*) tau, x_aux, y_aux, rho, vx, vy, v
     
      !     Realiza uma iteração com o método de Euler-Cromer.
      
      tau = tau + dtau
      
      vx = vx - dtau * x_aux / rho**(3.d0)
      x = x_aux + dtau * vx
      
      vy = vy - dtau * y_aux / rho**(3.d0)
      y = y_aux + dtau * vy
     
      rho = vec_mod(x, y)
      v = vec_mod(vx, vy)
            
      write(nfile,*) tau, x, y, rho, vx, vy, v
      
      !     Itera o método de Verlet
      
      do while (tau .lt. tau_m)
      
         tau = tau + dtau
         
         x_aux = 2.d0 * x - x_aux - dtau**2.d0 * x / rho**(3.d0)
         y_aux = 2.d0 * y - y_aux - dtau**2.d0 * y / rho**(3.d0)
         
         rho = vec_mod(x_aux, y_aux)
         
         vx = (x_aux - x) / dtau
         vy = (y_aux - y) / dtau
         
         v = vec_mod(vx, vy)
         
         write(nfile,*) tau, x_aux, y_aux, rho, vx, vy, v
         
         aux = x_aux  
         x_aux = x
         x = aux
         
         aux = y_aux  
         y_aux = y
         y = aux
             
      end do
      
      return
      end subroutine velvet_method
