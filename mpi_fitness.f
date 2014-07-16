      subroutine mpi_fitness (num_jobs, npar, oldph, fitness)
c ---------------------------------------------
c     parallel fitness evaluation using MPI
c ---------------------------------------------
      implicit none

      include 'mpif.h'
      
      integer ndone, nproc, ierr, nspawn
      integer msgtype, job, trial, slave
      integer par, npar, status(MPI_STATUS_SIZE), num_jobs
      double precision data(32), result
      real oldph(32,1024), fitness(1024)
      logical receiving

c ---------------------------------------------
c     initialize counter
c ---------------------------------------------
      ndone = 0

c ---------------------------------------------
c     determine number of processors
c ---------------------------------------------
      call mpi_comm_size( MPI_COMM_WORLD, nproc, ierr )

c ---------------------------------------------
c     run jobs on slave nodes only
c ---------------------------------------------
      nspawn = nproc-1

c ---------------------------------------------
c     send an initial job to each node
c ---------------------------------------------
      do job=1,nspawn
         trial = job
         slave = job
         call sendjob(trial,slave,npar,oldph)
      enddo

      do job=1,num_jobs
c ---------------------------------------------
c     listen for responses
c ---------------------------------------------
 25      msgtype  = 2 
         call mpi_iprobe( MPI_ANY_SOURCE, msgtype, MPI_COMM_WORLD,
     +                    receiving, status, ierr )

         if (receiving) then
c ---------------------------------------------
c     get data from responding node
c ---------------------------------------------
            call mpi_recv( slave, 1, MPI_INTEGER, MPI_ANY_SOURCE,
     +                     msgtype, MPI_COMM_WORLD, status, ierr )
            call mpi_recv( trial, 1, MPI_INTEGER, slave,
     +                     msgtype, MPI_COMM_WORLD, status, ierr )
            call mpi_recv( result, 1, MPI_DOUBLE_PRECISION, slave,
     +                     msgtype, MPI_COMM_WORLD, status, ierr )

            fitness(trial) = result
            ndone = ndone + 1

c ---------------------------------------------
c     send new job to responding node
c ---------------------------------------------
 140        if (ndone .LE. (num_jobs-nspawn)) then
               trial = job + nspawn
               call sendjob(trial,slave,npar,oldph)
            endif
            goto 100
         endif

c ---------------------------------------------
c     return to listen again or move on
c ---------------------------------------------
         if (.NOT. receiving) goto 25

         goto 199
 100     continue
      enddo

c ---------------------------------------------
c     ready for next generation of jobs
c ---------------------------------------------
 199  return
      end

c**********************************************************************
      subroutine sendjob(trial,slave,npar,oldph)

      implicit none

      include 'mpif.h'

      integer trial, slave, par, npar, ierr, msgtype
      double precision data(32)
      real oldph(32,1024)

      do par=1,npar
         data(par) = oldph(par,trial)
      enddo

      msgtype = 1
      call mpi_send( trial, 1, MPI_INTEGER, slave,
     +               msgtype, MPI_COMM_WORLD, ierr )
      call mpi_send( npar, 1, MPI_INTEGER, slave,
     +               msgtype, MPI_COMM_WORLD, ierr )
      call mpi_send( data, npar, MPI_DOUBLE_PRECISION, slave,
     +               msgtype, MPI_COMM_WORLD, ierr )

      return
      end

c**********************************************************************
