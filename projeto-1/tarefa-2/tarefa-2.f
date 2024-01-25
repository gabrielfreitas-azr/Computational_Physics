      program tarefa2
		
      real*8 :: v1(3), v2(3)
		
      write(*,*) "Digite o primeiro vetor (v1): "
      read(*,*) v1
		
      write(*,*) "Digite o segundo vetor (v2): "
      read(*,*) v2
      
      abs_crossp = dsqrt((v1(2)*v2(3) - v2(2)*v1(3))**2 + (v1(3)*v2(1)
     *   - v2(3)*v1(1))**2 + (v1(1)*v2(2) - v2(1)*v1(2))**2) 
      
      write(*,*) "A = ", abs_crossp / 2.d0
		
      end program tarefa2
    
    

