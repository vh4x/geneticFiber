MODULE GenerarMpb


IMPLICIT NONE
CHARACTER(LEN=50)  :: file
!REAL(DP) :: r1, r2, rg, re

CONTAINS

SUBROUTINE escribirMpb(rank, r1, r2, rg, re)
  INTEGER, INTENT(IN) :: rank
  REAL*8, INTENT(IN) :: r1
  REAL*8, INTENT(IN)  :: r2
  REAL*8, INTENT(IN)  :: rg
  REAL*8, INTENT(IN)  :: re
  file = ""
  write (file, "(A7,I1)") "mpbFile", rank
  OPEN(UNIT=1, FILE=file, ACTION="WRITE", STATUS='REPLACE')
  CALL header
  CALL functions
  CALL constants(r1, r2, rg, re)
  CALL media
  CALL parameters
  CALL run
  CLOSE(1)
END SUBROUTINE escribirMpb

SUBROUTINE header
  write(1, *) ";;; PCF TRIANGULAR LATTICE"
  write(1, *) ";;;"
END SUBROUTINE header

SUBROUTINE functions
  write(1, *) ""
  write(1, *) ";;; ### FUNCTIONS ###########"
END SUBROUTINE functions

SUBROUTINE constants(r1, r2, rg, re)
  REAL*8, INTENT(IN)  :: r1
  REAL*8, INTENT(IN)  :: r2
  REAL*8, INTENT(IN)  :: rg
  REAL*8, INTENT(IN)  :: re
  write(1, *) ""
  write(1, *) ";;; ### CONSTANTS ###########"
  write(1, *) "(define n 1.4491)"
  write(1, *) "(define r1 ",r1 ,")     ;Radio de huecos en primer anillo" 
  write(1, *) "(define r2 ",r2 ,")      ;Radio de huecos en segundo anillo"
  write(1, *) "(define rg ",rg ,")      ;Radio general de huecos de la red triangular"
  write(1, *) "(define re ",re ,")      ;Radio de 2 esfuerzos extremos al nucleo" 

END SUBROUTINE constants

SUBROUTINE media

  write(1, *) ";;; #############################"
  write(1, *) ";;; DEFINITION OF MEDIA  write(1, *) "
  write(1, *) ""

  write(1, *) ";;;                                                                       ; Configuracion triangular"
  write(1, *) "(set! geometry-lattice (make lattice (size 8 8 no-size)"
  write(1, *) "                       (basis1 (/ (sqrt 3) 2) 0.5)"
  write(1, *) "                       (basis2 (/ (sqrt 3) 2) -0.5)))"

  write(1, *) ";;; Define radio y la constante dielectrica del bulk"
  write(1, *) "(define-param eps 2.1) ; the dielectric constant of the background"
  write(1, *) "(define-param r 0.25) ; the hole radius                                   ;Polli pagina 115  "

  write(1, *) "(set! default-material (make dielectric (epsilon eps)))                   ; Huecos"
  write(1, *) "(set! geometry (list (make cylinder (center 0) (material air)"
  write(1, *) "                   (radius r) (height infinity))))"
  write(1, *) "(set! geometry ( geometric-objects-lattice-duplicates geometry))"

  write(1, *) "(set! geometry (append geometry                                           ; Defecto central"
  write(1, *) "                (list (make cylinder (center 0 0 0)"
  write(1, *) "                (radius 0.25) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";####################################################################################################"
  write(1, *) ";;; ###################################################################   ; Defecto primer anillo"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 1 Anillo 1"
  write(1, *) "                (list (make cylinder (center 1 -1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 2 Anillo 1"
  write(1, *) "                (list (make cylinder (center 0 1 0)"
  write(1, *) "                (radius re) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 3 Anillo 1"
  write(1, *) "                (list (make cylinder (center -1 0 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Defecto 4 Anillo 1"
  write(1, *) "                (list (make cylinder (center -1 1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Defecto 5 Anillo 1"
  write(1, *) "                (list (make cylinder (center 0 -1 0)                       ;;;; (0 -1 0)"
  write(1, *) "                (radius re) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 6 Anillo 1"
  write(1, *) "                (list (make cylinder (center 1 0 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) ";;; ###################################################################   ; Huecos primer anillo"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 1 Anillo 1"
  write(1, *) "                (list (make cylinder (center 1 -1 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 2 Anillo 1"
  write(1, *) "                (list (make cylinder (center 0 1 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 3 Anillo 1"
  write(1, *) "                (list (make cylinder (center -1 0 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 4 Anillo 1"
  write(1, *) "                (list (make cylinder (center -1 1 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 5 Anillo 1"
  write(1, *) "                (list (make cylinder (center 0 -1 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 6 Anillo 1"
  write(1, *) "                (list (make cylinder (center 1 0 0)"
  write(1, *) "                (radius r1) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;; ###################################################################   ; Defecto segundo anillo"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 1 Anillo 2"
  write(1, *) "                (list (make cylinder (center -1 2 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 2 Anillo 2"
  write(1, *) "                (list (make cylinder (center -2 2 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 3 Anillo 2"
  write(1, *) "                (list (make cylinder (center -2 1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 4 Anillo 2"
  write(1, *) "                (list (make cylinder (center -2 0 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Defecto 5 Anillo 2"
  write(1, *) "                (list (make cylinder (center -1 -1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 6 Anillo 2"
  write(1, *) "                (list (make cylinder (center 0 -2 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 7 Anillo 2"
  write(1, *) "                (list (make cylinder (center 1 -2 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 8 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 -2 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 9 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 -1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Defecto 10 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 0 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Defecto 11 Anillo 2"
  write(1, *) "                (list (make cylinder (center 1 1 0)"
  write(1, *) "                (radius rg) (height infinity)"
  write(1, *) "                (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                     ; Defecto 12 Anillo 2"
  write(1, *) "                 (list (make cylinder (center 0 2 0)"
  write(1, *) "                 (radius rg) (height infinity)"
  write(1, *) "                 (material (make dielectric (epsilon eps)))))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) ";;; ###################################################################   ; Huecos segundo anillo"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 1 Anillo 2"
  write(1, *) "                (list (make cylinder (center -1 2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 2 Anillo 2"
  write(1, *) "                (list (make cylinder (center -2 2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 3 Anillo 2;"
  write(1, *) "                (list (make cylinder (center -2 1 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Hueco 4 Anillo 2"
  write(1, *) "                (list (make cylinder (center -2 0 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Hueco 5 Anillo 2"
  write(1, *) "                (list (make cylinder (center -1 -1 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 6 Anillo 2"
  write(1, *) "                (list (make cylinder (center 0 -2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 7 Anillo 2"
  write(1, *) "                (list (make cylinder (center 1 -2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 8 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 -2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 9 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 -1 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Hueco 10 Anillo 2"
  write(1, *) "                (list (make cylinder (center 2 0 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) "(set! geometry (append geometry                                           ; Hueco 11 Anillo 2"
  write(1, *) "                (list (make cylinder (center 1 1 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) " (set! geometry (append geometry                                           ; Hueco 12 Anillo 2"
  write(1, *) "                (list (make cylinder (center 0 2 0)"
  write(1, *) "                (radius r2) (height infinity)"
  write(1, *) "                (material air)))))"
  write(1, *) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
  write(1, *) ";;; ###################################################################"
  write(1, *) ";###########################################################################################"

END SUBROUTINE media

SUBROUTINE parameters
  write(1, *) ""
  write(1, *) ";;; #############################"
  write(1, *) ";;; PARAMETERS"
  write(1, *) "(set-param! resolution 8)"
  write(1, *) "(set! num-bands 2)"
  write(1, *) "(set! k-points (list (vector3 0.0 0.0 0.0) (vector3 0. 0. 10)))"
  write(1, *) "(set! k-points (interpolate 100 k-points))"
END SUBROUTINE parameters

SUBROUTINE run
  write(1, *) ""
  write(1, *) "(run)"
  write(1, *) "; (run (output-at-kpoint current-k fix-efield-phase output-efield))"
  write(1, *) "(quit)"
END SUBROUTINE run

END MODULE GenerarMpb

