      program tarefa1b
      implicit real*8(a-h, o-z)
      
      real*8 :: mod_vec 
      
      ! MÉTODO DE EULER-CROMER
      
      open(11, file = 'tarefa-1c-saida-1.dat')
      write(11,*) 'dtau delta'
      
      eps = 1.d-5
           
      tau_m = 25.d0  
      dtau = 1.d-1 + eps
      delta = 1.d0
      
      rho_max = 0.d0
      rho_min = 0.d0 
      
      do while(delta .gt. 1.d-3)
      
         tau = 0.d0
         dtau = dtau - eps
        
         x = 0.39d0
         y = 0.d0
         rho = mod_vec(x, y)
         
         vx = 0.d0
         vy = 1.d0 / dsqrt(x)
         
         rho_max = rho
         rho_min = rho
         
         do while (tau .lt. tau_m)
            
            vx = vx - dtau * x / rho**(3.d0)
            x = x + dtau * vx
            
            vy = vy - dtau * y / rho**(3.d0)
            y = y + dtau * vy
            
            rho = mod_vec(x, y)
            
            if (rho .gt. rho_max) then
               rho_max = rho
            end if
            
            if (rho .lt. rho_min) then
               rho_min = rho
            end if
            
            tau = tau + dtau
         end do
         
         delta = rho_max / rho_min - 1.d0
         
         write(11,*) dtau, delta
      end do
      
      close(11)
      
      ! MÉTODO DE VERLET
      
      open(12, file = 'tarefa-1c-saida-2.dat')      
      write(12,*) 'dtau delta'
      
      tau = 0.d0
      dtau = 1.d-1 + eps
      delta = 1.d0

      do while(delta .gt. 1.d-3)
         
         dtau = dtau - eps
                  
         x_aux = 0.39d0
         y_aux = 0.d0
         rho = mod_vec(x_aux, y_aux)
         
         rho_max = rho
         rho_min = rho
         
         vx = 0.d0
         vy = 1.d0 / dsqrt(x_aux)
         
         x = x_aux + dtau * vx
         y = y_aux + dtau * vy
         
         rho = mod_vec(x, y)
         
         if (rho .gt. rho_max) then
            rho_max = rho
         end if
         
         if (rho .lt. rho_min) then
            rho_min = rho
         end if
         
         tau = tau + dtau
         
         delta = rho_max / rho_min - 1.d0
         
         do while (tau .lt. tau_m)
            
            x_aux = 2.d0 * x - x_aux - dtau**(2.d0) * x / rho**(3.d0)
            y_aux = 2.d0 * y - y_aux - dtau**(2.d0) * y / rho**(3.d0)
            
            rho = mod_vec(x_aux, y_aux)
            
            if (rho .gt. rho_max) then
               rho_max = rho
            end if
            
            if (rho .lt. rho_min) then
               rho_min = rho
            end if
            
            tau = tau + dtau
            
            aux = x_aux  
            x_aux = x
            x = aux
            
            aux = y_aux  
            y_aux = y
            y = aux
                
         end do
         
         delta = rho_max / rho_min - 1.d0
         write(12,*) dtau, delta
         
      end do
      
      close(12)
      
      end program tarefa1b
      
      function mod_vec(x, y)
      
      real*8 mod_vec, x, y
      
      mod_vec = dsqrt(x**2.d0 + y**2.d0)
      
      return
      end function mod_vec
