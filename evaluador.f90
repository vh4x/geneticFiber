MODULE evaluador
      !USE nrtype; USE nr; USE nrutil, ONLY : assert,swap
USE GenerarMpb


!IMPLICIT NONE
      
      INTEGER ierr,my_rank,size,partner
      REAL*8 :: col1, col2, col3, col4, col5
      CHARACTER*50 greeting
      CHARACTER*55 mpbExec
      CHARACTER*55 runBetasExec
      CHARACTER*20 paramFile
      CHARACTER*7 fileFormat
      CHARACTER*15 betaFile
      CHARACTER*50 nanReplace
      CHARACTER*50 mpbExecFormat
      CHARACTER*50 runBetasExecFormat
      CHARACTER*20 createLocal
      CHARACTER*20 cdLocal
      LOGICAL :: file_exists


CONTAINS 

SUBROUTINE dispersiones(my_rank, r1, r2, rg, re, dispersion)
  INTEGER, INTENT(IN) :: my_rank
  REAL*8, INTENT(IN) :: r1
  REAL*8, INTENT(IN)  :: r2
  REAL*8, INTENT(IN)  :: rg
  REAL*8, INTENT(IN)  :: re
  REAL*8, INTENT(OUT) :: dispersion

!************************************************************
! Nodo maestro
!************************************************************
      IF(my_rank.eq.0) THEN
         write(6,*) '***Calculando dispersiones***'

!************************************************************
! Nodos esclavos. Cada uno resuelve un cálculo
!************************************************************         
      ELSE
      	 IF (my_rank.lt.10) THEN
      	 	write (fileFormat, "(A7)") "(A7,I1)"
      	 	write (mpbExecFormat, *) "(A11, I1, A5, I1, A8, I1, A4)"
          write (runBetasExecFormat, *) "(A12, I1, A8, I1, A16, I1, A4)"
          write(betaFile, "(A5, I1, A4)") "betas", my_rank, ".out"
          write(createLocal, "(A6, I1)") "mkdir ", my_rank
          write(cdLocal, "(A3, I1)") "cd ", my_rank
      	 ELSE IF (my_rank.gt.9) THEN
      	 	write (fileFormat, "(A7)") "(A7,I2)"
      	 	write (mpbExecFormat, *) "(A11, I2, A5, I2, A8, I2, A4)"
          write (runBetasExecFormat, *) "(A12, I2, A8, I2, A16, I2, A4)"
          write(betaFile, "(A5, I2, A4)") "betas", my_rank, ".out"
          write(createLocal, "(A6, I2)") "mkdir ", my_rank
          write(cdLocal, "(A3, I2)") "cd ", my_rank
      	 ELSE IF (my_rank.gt.99) THEN
      	 	write (fileFormat, "(A7)") "(A7,I3)"
      	 	write (mpbExecFormat, *) "(A11, I3, A5, I3, A8, I3, A4)"
      	 	write (runBetasExecFormat, *) "(A12, I3, A8, I3, A16, I3, A4)"
          write(betaFile, "(A5, I3, A4)") "betas", my_rank, ".out"
          write(createLocal, "(A6, I3)") "mkdir ", my_rank
          write(cdLocal, "(A3, I3)") "cd ", my_rank
      	 END IF

         write(nanReplace, *) "sed -i 's/NaN/-0.9999999E+99/g' ", betaFile

         write (paramFile, fileFormat) "mpbFile", my_rank
         INQUIRE(FILE=paramFile, EXIST=file_exists) 
         ! Si no existe el archivo, no se procesa
         !IF (.NOT.file_exists) THEN
          !  write (*, *) "No existe el archivo de entrada ", paramFile
         !ELSE
            CALL SYSTEM (createLocal)
            !CALL SYSTEM (cdLocal)
            ! Llamada al procedimiento para generar el archivo MPB y luego llamadas para correrlo
            CALL escribirMpb(my_rank, r1, r2, rg, re)
            file = ""
            write(mpbExec, mpbExecFormat) "mpb mpbFile", my_rank, " > ./", my_rank, "/anillos", my_rank, ".out"
            CALL SYSTEM(mpbExec)
            write(*, *) mpbExec

            write(runBetasExec, runBetasExecFormat) "run_betas ./", my_rank,"/anillos", my_rank, ".out 0.5 > betas", my_rank, ".out"
            CALL SYSTEM(runBetasExec)
            write(*, *) runBetasExec
            CALL SYSTEM(nanReplace)

            OPEN(666, FILE=betaFile, STATUS='UNKNOWN', FORM = 'FORMATTED', ACTION='READ')
            DO J=1, 4
			      read (666, *) 
			      END DO

            DO I=1, 10
			         read (666, '(5E18.4)') col1, col2, col3, col4, col5
               write(*,*) col5;
               dispersion = dispersion + col5;
			      END DO

      END IF

END SUBROUTINE dispersiones


END MODULE evaluador