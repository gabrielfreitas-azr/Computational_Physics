      program tarefa7
      
      real*8 :: random_vec(4), R, fun_gamma = 1.d0, vd_formula = 0.d0
      real*8, parameter :: pi = acos(-1.d0)
      
      read(*,*) m, R, id
      
      random_vec(1) = rand(1) ! inicializa o rand()
      icont = 0
     
      do i = 1,m
      
         dist = 0.d0
         
         random_vec(1) = rand() * R
         random_vec(2) = rand() * R
         random_vec(3) = rand() * R
         random_vec(4) = rand() * R
         
         do j = 1,id
            dist = dist + random_vec(j)**2
         end do
         
         if (dist.le.R) then
            icont = icont + 1
         end if
         
      end do
      
      write(*,*) 'To d = ', id, ' and R = ', R
      
      write(*,*) 'Monte Carlo:'
      write(*,*) real(icont, 8) / real(m, 8) * (2 * R)**id
      
      ! PELA FÓRMULA:
      
      x = real(id, 8) / 2.d0 + 1.d0
      
      do while(x.ne.(0.5d0).and.x.ne.(1.d0))
         fun_gamma = (x - 1.d0) * fun_gamma
         x = x - 1.d0
      end do
      
      if (x.eq.(0.5d0)) then
         fun_gamma = fun_gamma * dsqrt(pi)
      end if
  
      vd_formula = pi**(real(id, 8) / 2.d0)* R**real(id, 8) / fun_gamma
      
      write(*,*) 'Formula:'
      write(*,*) vd_formula
     
      end program tarefa7
      
      
      
