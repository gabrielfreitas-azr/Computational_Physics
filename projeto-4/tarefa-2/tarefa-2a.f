      program tarefa2a
      implicit real*8 (a-h, o-z)
      
      real*8, parameter :: pi = dacos(-1.d0)
      real*8, parameter :: tau_max = 25.d0
      
      real*8 :: theta, omega
      
      open(11, file = "tarefa-2a-saida-1.dat")
      
      write(11,*) "tau theta omega theta_i omega_i epsilon_i epsilon"
      
      dtau = 1.d-2
      tau = 0.d0
      
      theta_i = pi / 3.d0
      omega_i = 0.d0
      
      epsilon = 0.d0
      epsilon_i = 0.d0
      
      do while (tau.lt.tau_max)
         
         epsilon_i = 1 - dcos(theta_i) + 0.5d0 * (omega_i**2.d0)
         epsilon = 1 - dcos(theta(tau)) + 0.5d0 * (omega(tau)**2.d0)
         
         write(11,*) tau, theta(tau), omega(tau), theta_i, omega_i, 
     &               epsilon_i, epsilon
         
         omega_i = omega_i - dsin(theta_i) * dtau
         theta_i = theta_i + omega_i * dtau

         
         tau = tau + dtau
      end do
      
      close(11)
      end program tarefa2a
      
      ! Valores calculados para condição inicial:
      !     theta = pi / 12, omega = 0
      
      function theta(tau)      
      
      real*8, parameter :: pi = dacos(-1.d0)
      real*8 :: tau, theta
      
      theta = pi / 3.d0 * dcos(tau)
      
      return
      end function theta
      
      function omega(tau)
      
      real*8, parameter :: pi = dacos(-1.d0)
      real*8 :: tau, omega  
      
      omega = - pi / 3.d0 * dsin(tau) 
      
      return
      end function omega
