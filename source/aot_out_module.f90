!> First draft of incomplete output module!
module aot_out_module
  use mpi

  implicit none

  type put_conf_type
    integer :: outunit
    integer :: indent
    integer :: stack(100)
    integer :: level
    integer :: comm
  end type 

  interface aot_put_val
    module procedure aot_put_val_int
    module procedure aot_put_val_char
    module procedure aot_put_val_real
  end interface

  contains

!******************************************************************************!
!> open the file, set the filename and the values of put_conf
!!
  subroutine aot_open_put(filename, put_conf, comm)
    !------------------------------------------------------------------------ 
    character(len=labelLen), intent(in) :: filename
    type(put_conf_type), intent(inout) :: put_conf
    integer, intent(in) :: comm
    !------------------------------------------------------------------------ 
    ! defining local variables
    integer :: rank
    integer :: iError
    !------------------------------------------------------------------------ 

    put_conf%comm = comm
    call MPI_COMM_RANK(comm, rank, iError) 
    if (rank .eq. 0) then
      put_conf%outunit = newunit()
      open (unit = put_conf%outunit, file = trim(filename))
      put_conf%indent = 0
      put_conf%stack(:) = 0 
      put_conf%level = 0
    end if
  end subroutine aot_open_put
!******************************************************************************!

    
!******************************************************************************!
!> Append the table start format { to table 
!!
  subroutine aot_table_open_out(put_conf, tname)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    character(len=labelLen), intent(in) :: tname
    !------------------------------------------------------------------------ 
    
    if (present(tname)) then
      write(put_conf%outunit,*) 'Initial Conditions = {' 
    else
      write(put_conf%outunit,*) '{'
    end if
    put_conf%level = put_conf%level + 1
    put_conf%indent = put_conf%indent + 4
  end subroutine aot_table_open_out
!******************************************************************************!

    
!******************************************************************************!
!>  Append } to the table and decrease the level and indent 
!!
  subroutine aot_table_close_out(put_conf)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    !------------------------------------------------------------------------ 
    put_conf%indent = put_conf%indent - 4
    put_conf%level = put_conf%level - 1
    write(put_conf%outunit,*) '}'            
  end subroutine aot_table_close_out
!******************************************************************************!
    

!******************************************************************************!
!>  Close the table 
!!
  subroutine aot_close_put(put_conf)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    !------------------------------------------------------------------------ 
    close( put_conf%outunit )
  end subroutine aot_close_put
!******************************************************************************!

    
!******************************************************************************!
!>  Put integer variables in the table 
!!
  subroutine aot_put_val_int(put_conf, vname, val)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    character(len=labelLen), intent(in) :: vname
    integer, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,*) ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(level .ne. 0) then
        write(put_conf%outunit,*,advance ='no') trim(vname)//"=", val
      else
        write(put_conf%outunit,*) trim(vname)//"=", val
      end if 
    else
      if(level .ne. 0) then
        write(put_conf%outunit,*, advance ='no') val
      else
        call tem_write('Plain values shall onlz be used for tables!!!!!')
      end if 
    end if
  end subroutine aot_put_val_int
!******************************************************************************!

    
!******************************************************************************!
!>  Put character variables in the table 
!!
  subroutine aot_put_val_char(put_conf, vname, val)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    character(len=labelLen), optional, intent(in) :: vname
    character, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,*) ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(level .ne. 0) then
        write(put_conf%outunit,*,advance ='no') trim(vname)//"=", val
      else
        write(put_conf%outunit,*) trim(vname)//"=", val
      end if 
    else
      if(level .ne. 0) then
        write(put_conf%outunit,*, advance ='no') val
      else
        call tem_write('Plain values shall onlz be used for tables!!!!!')
      end if 
    end if
  end subroutine aot_put_val_char
!******************************************************************************!

  
!******************************************************************************!
!>  Put real variables in the table 
!!
  subroutine aot_put_val_real(put_conf, vname, val)
    !------------------------------------------------------------------------ 
    type(put_conf_type), intent(inout)  :: put_conf
    character(len=labelLen), intent(in) :: vname
    real, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,*) ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(level .ne. 0) then
        write(put_conf%outunit,*,advance ='no') trim(vname)//"=", val
      else
        write(put_conf%outunit,*) trim(vname)//"=", val
      end if 
    else
      if(level .ne. 0) then
        write(put_conf%outunit,*, advance ='no') val
      else
        call tem_write('Plain values shall onlz be used for tables!!!!!')
      end if 
    end if
  end subroutine aot_put_val_real


end module aot_out_module
