      program tarefa2b
      implicit real*8(a-h, o-z)
           
      real*8 :: vec_mod
      real*8 :: a(8) = (/0.39d0, 0.72d0, 1.d0, 1.52d0, 5.20d0, 9.58d0,
     &               19.2d0, 30.1d0/)
      real*8 :: pi = dacos(-1.d0)

      open(11, file = 'tarefa-2b-saida-1.dat')
      open(12, file = 'tarefa-2b-saida-2.dat')
      open(13, file = 'tarefa-2b-saida-3.dat')      
      open(14, file = 'tarefa-2b-saida-4.dat')      
      open(15, file = 'tarefa-2b-saida-5.dat')      
      open(16, file = 'tarefa-2b-saida-6.dat')      
      open(17, file = 'tarefa-2b-saida-7.dat')      
      open(18, file = 'tarefa-2b-saida-8.dat')            
      
      do i = 1, 8
                 
         call velvet_method(i + 10, a(i), 1.d-4, i * 50.d0)
         close(i + 10)
         
      end do  
      
      end program tarefa2b
      
      function vec_mod(x, y)
      
      real*8 :: vec_mod, x, y
      
      vec_mod = dsqrt(x**2.d0 + y**2.d0)
      
      return
      end function vec_mod
      
      subroutine velvet_method(nfile, x0, dtau, tau_m)
      
      implicit real*8(a-h, o-z)
      
      write(nfile,*) "tau x y vx vy a b dA_dt"
      
      tau = 0.d0
      tn = 0.d0
      
      a = 0.d0
      b = 0.d0
      
      !     Condições inicias rho = (a, 0), 
      !                       v = (0, 1 / sqrt(a)).
            
      x_aux = x0
      y_aux = 0.d0
      
      rho = vec_mod(x_aux, y_aux)
      
      vx = 0.d0
      vy = 1.d0 / dsqrt(x_aux) * 0.8d0
        
      v = vec_mod(vx, vy)
      
      !     Escreve as condições inicias.
      
      write(nfile,*) tau, x_aux, y_aux, vx, vy, a, b, 
     &               dA_dt(x_aux, y_aux, vx, vy)
     
      !     Realiza uma iteração com o método de Euler-Cromer.
      
      tau = tau + dtau
      
      vx = vx - dtau * x_aux / rho**(3.d0)
      x = x_aux + dtau * vx
      
      vy = vy - dtau * y_aux / rho**(3.d0)
      y = y_aux + dtau * vy
     
      rho = vec_mod(x, y)
            
      write(nfile,*) tau, x, y, vx, vy, a, b,
     &               dA_dt(x, y, vx, vy)
           
      !     Itera o método de Verlet
      
      do while (tau .lt. tau_m)
      
         tau = tau + dtau
         
         x_aux = 2.d0 * x - x_aux - dtau**2.d0 * x / rho**(3.d0)
         y_aux = 2.d0 * y - y_aux - dtau**2.d0 * y / rho**(3.d0)
         
         rho = vec_mod(x_aux, y_aux)
         
         vx = (x_aux - x) / dtau
         vy = (y_aux - y) / dtau
         
         if (y_aux .gt. b) then
            b = y_aux
         end if
         
         if ((abs(y_aux) .lt. 1.d-4).and.(x_aux .lt. 0.d0).and.
     &    (a .eq. 0.d0)) then
         
            tn = tau
            a = (x0 - x_aux) / 2.d0
         
         end if
                  
         write(nfile,*) tau, x_aux, y_aux, vx, vy, a, b,
     &                  dA_dt(x_aux, y_aux, vx, vy)         
         aux = x_aux  
         x_aux = x
         x = aux
         
         aux = y_aux  
         y_aux = y
         y = aux
             
      end do
      
      write(*,*) "T = ", tn * 2.d0
      
      return
      end subroutine velvet_method
      
      function dA_dt(x, y, vx, vy)
      real*8 :: x, y, vx, vy, dA_dt
      
      dA_dt = 0.5d0 * abs(x * vy - vx * y)
      
      return
      end function dA_dt
