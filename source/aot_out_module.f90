!> First draft of incomplete output module!
!!
!! This module eases the output of readable Lua scripts.
!! It takes care of indentation with nested tables, and
!! provides a concise interface to output Fortran data
!! into Lua scripts.
!! Therefore this module is somehow the counter-part to
!! the reading functions, however it is almost completely
!! independent and relies purely on Fortran output methods.
module aot_out_module

  implicit none

  public :: aot_out_type
  public :: aot_put_val
  public :: aot_open_put
  public :: aot_table_open_out
  public :: aot_table_close_out
  public :: aot_close_put

  type aot_out_type
    integer :: outunit
    integer :: indent
    integer :: stack(0:100)
    integer :: level
  end type 

  interface aot_put_val
    module procedure aot_put_val_int
    module procedure aot_put_val_char
    module procedure aot_put_val_real
  end interface

  private

  integer, parameter :: labelLen = 80

contains

!******************************************************************************!
!> open the file, set the filename and the values of put_conf
!!
!! Will overwrite the given file, if it already exists.
  subroutine aot_open_put(put_conf, filename, outUnit)
    !------------------------------------------------------------------------ 
    character(len=*), optional, intent(in) :: filename
    integer, optional, intent(in) :: outUnit
    type(aot_out_type), intent(out) :: put_conf
    !------------------------------------------------------------------------ 
    if (present(filename)) then
      put_conf%outunit = newunit()
      open(unit = put_conf%outunit, file = trim(filename), action = 'write', &
        &  status='replace', recl=360)
    else if ( present(outUnit) ) then
      put_conf%outunit = outUnit
    else
        write(*,*) 'Error, no unit or filename specified for aot_open_put'
       stop
    end if

      put_conf%indent = 0
      put_conf%stack(:) = 0 
      put_conf%level = 0
  end subroutine aot_open_put
!******************************************************************************!

    
!******************************************************************************!
!>  Close the table 
!!
  subroutine aot_close_put(put_conf)
    !------------------------------------------------------------------------ 
    type(aot_out_type), intent(inout)  :: put_conf
    !------------------------------------------------------------------------ 
    close( put_conf%outunit )
    put_conf%stack(put_conf%level) = 0                                         
  end subroutine aot_close_put
!******************************************************************************!


!******************************************************************************!
!> Append the table start format { to table 
!!
  subroutine aot_table_open_out(put_conf, tname)
    !------------------------------------------------------------------------ 
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: tname
    character(len=1) :: separator
    !------------------------------------------------------------------------ 
    separator = ''
    if(put_conf%level .gt. 0)  then
      if( put_conf%stack( put_conf%level ) .gt. 0) then
        separator = ','
      else
        separator = ''
      endif
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1 
    end if
      if (present(tname)) then
        write(put_conf%outunit,fmt="(a)") trim(separator)//trim(tname)//' = {' 
      else
        write(put_conf%outunit,fmt="(a)") trim(separator)//'{'
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
    type(aot_out_type), intent(inout)  :: put_conf
    !------------------------------------------------------------------------ 
    put_conf%indent = put_conf%indent - 4
    put_conf%stack(put_conf%level) = 0
    put_conf%level = put_conf%level - 1
    write(put_conf%outunit,fmt="(a)") '}'            
  end subroutine aot_table_close_out
!******************************************************************************!
    

    
!******************************************************************************!
!>  Put integer variables in the table 
!!
  subroutine aot_put_val_int(put_conf, val, vname)
    !------------------------------------------------------------------------ 
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(a,i2)",advance ='no') trim(vname)//" = ", val
      else
        write(put_conf%outunit,fmt="(a,i2)") trim(vname)//" = ", val
      end if 
    else
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(a,i2)", advance ='no') val
      end if 
    end if
  end subroutine aot_put_val_int
!******************************************************************************!

    
!******************************************************************************!
!>  Put character variables in the table 
!!
  subroutine aot_put_val_char(put_conf, val, vname)
    !------------------------------------------------------------------------ 
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    character, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(a,a)",advance ='no') trim(vname)//" = ", "'"//val//"'"
      else
        write(put_conf%outunit,fmt="(a,a)") trim(vname)//" = ", "'"//val//"'"
      end if 
    else
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(a,a)", advance ='no'),"'",val//"'"
      end if 
    end if
  end subroutine aot_put_val_char
!******************************************************************************!

  
!******************************************************************************!
!>  Put real variables in the table 
!!
  subroutine aot_put_val_real(put_conf, val, vname)
    !------------------------------------------------------------------------ 
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    real, intent(in) :: val
    !------------------------------------------------------------------------ 
    if ( put_conf%level .gt. 0 ) then
      if ( put_conf%stack(put_conf%level) .gt. 0) then
        ! commata for previous line maybe use advance = no ????
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) =              &
        &      put_conf%stack(put_conf%level) + 1
    end if
    if (present(vname)) then
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(a,f8.2)",advance ='no') trim(vname)//" = ", val
      else
        write(put_conf%outunit,fmt="(a,f8.2)") trim(vname)//" = ", val
      end if 
    else
      if(put_conf%level .ne. 0) then
        write(put_conf%outunit,fmt="(f8.2)", advance ='no') val
      end if 
    end if
  end subroutine aot_put_val_real
 

  !> Helper function to provide new unit, as long as F2008 newunit argument
  !! in open statement is not commonly available.
  !! To be used right in front of the open statement like this:
  !!  myUnit = newunit()
  !!  open(myUnit, ...)
  function newunit() result(nu)
    integer :: nu

    logical :: connected

    nu = 21
    inquire(unit=nu, opened=connected)
    do while(connected)
      nu = nu + 1
      inquire(unit=nu, opened=connected)
    end do
  end function newunit

end module aot_out_module
