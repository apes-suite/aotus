!> A module to produce Lua scripts with nested tables.
!!
!! This module eases the output of readable Lua scripts.
!! It takes care of indentation with nested tables, and
!! provides a concise interface to output Fortran data
!! into Lua scripts.
!! Therefore this module is somehow the counter-part to
!! the reading functions, however it is almost completely
!! independent and relies purely on Fortran output methods.
module aot_out_module

  use aot_kinds_module

  implicit none

  public :: aot_out_type
  public :: aot_out_val
  public :: aot_out_open
  public :: aot_out_close
  public :: aot_out_open_table
  public :: aot_out_close_table

  !> This type provides the internal representation of the opened Lua script.
  !!
  !! It is used to keep track of the state in the script internally.
  type aot_out_type
    integer :: outunit !< Unit to write to
    integer :: indent !< Indentation level (number of spaces)
    integer :: stack(100) !< Number of entries on each level
    integer :: level !< Current nesting level in tables
    logical :: externalOpen !< Flag if file opened outside the aot_out scope
  end type

  !> Put Fortran intrinsic types into the script.
  interface aot_out_val
    module procedure aot_out_val_int
    module procedure aot_out_val_long
    module procedure aot_out_val_real
    module procedure aot_out_val_double
    module procedure aot_out_val_logical
    module procedure aot_out_val_string
    module procedure aot_out_val_arr_int
    module procedure aot_out_val_arr_long
    module procedure aot_out_val_arr_real
    module procedure aot_out_val_arr_double
  end interface

  private

  integer, parameter :: indentation = 4

contains

!******************************************************************************!
!> Open the file to write to and return a handle (put_conf) to it.
!!
!! This will overwrite the given file, if it already exists.
!! Either filename of outUnit has to be specified, use outUnit to write to a
!! pre-connected file.
!! If both are given, the file will be opened and connected to a new unit,
!! outUnit is ignored in this case.
  subroutine aot_out_open(put_conf, filename, outUnit)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(out) :: put_conf !< Handle for the file
    character(len=*), optional, intent(in) :: filename !< File to open
    integer, optional, intent(in) :: outUnit !< Pre-connected unit to write to
    !------------------------------------------------------------------------

    if (present(filename)) then
      put_conf%outunit = newunit()
      open(unit = put_conf%outunit, file = trim(filename), action = 'write', &
        &  status='replace', recl=360)
      put_conf%externalOpen = .false.
    else if ( present(outUnit) ) then
      put_conf%externalOpen = .true.
      put_conf%outunit = outUnit
!HK!    else
!HK!       write(*,*) 'Error, no unit or filename specified for aot_open_put'
!HK!       stop
!HK: Return an error code instead.
    end if

    put_conf%indent = 0
    put_conf%stack(:) = 0
    put_conf%level = 0

  end subroutine aot_out_open
!******************************************************************************!


!******************************************************************************!
!>  Close the script again.
!!
  subroutine aot_out_close(put_conf)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    !------------------------------------------------------------------------
    if( .not. put_conf%externalOpen ) close( put_conf%outunit )
  end subroutine aot_out_close
!******************************************************************************!


!******************************************************************************!
!> Start a new table to write to.
!!
  subroutine aot_out_open_table(put_conf, tname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: tname
    logical, optional, intent(in) :: advance_previous
    !------------------------------------------------------------------------

    call aot_out_breakline(put_conf, advance_previous)

    if (present(tname)) then
      write(put_conf%outunit, fmt='(a)', advance='no') trim(tname)//' = {'
    else
      write(put_conf%outunit, fmt='(a)', advance='no') '{'
    end if

    put_conf%level = put_conf%level + 1
    put_conf%indent = put_conf%indent + indentation

  end subroutine aot_out_open_table
!******************************************************************************!


!******************************************************************************!
!>  Close the current table.
!!
  subroutine aot_out_close_table(put_conf, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    logical, optional, intent(in) :: advance_previous
    !------------------------------------------------------------------------
    logical :: loc_adv_prev
    character(len=max(put_conf%indent-indentation,0)) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (present(advance_previous)) then
      loc_adv_prev = advance_previous
    else
      loc_adv_prev = .true.
    end if

    put_conf%indent = max(put_conf%indent - indentation, 0)
    put_conf%stack(put_conf%level) = 0
    put_conf%level = max(put_conf%level - 1, 0)

    if (put_conf%level > 0) then
      ! Do not advance, to let the next entry append the separator to the line.
      adv_string = 'no'
    end if

    ! Close last entry without separator.
    if (loc_adv_prev) then
      ! Closing brace should be on new line.
      write(put_conf%outunit,*) ''
      write(put_conf%outunit, fmt="(a)", advance=adv_string) indent//'}'
    else
      ! Closing brace on same line as last entry.
      write(put_conf%outunit, fmt="(a)", advance=adv_string) ' }'
    end if

  end subroutine aot_out_close_table
!******************************************************************************!



!******************************************************************************!
!>  Put integer variables into the Lua script.
!!
  subroutine aot_out_val_int(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, optional, intent(in) :: advance_previous
    integer, intent(in) :: val
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) &
        & trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(i0)", advance=adv_string) val
    end if

  end subroutine aot_out_val_int
!******************************************************************************!


!******************************************************************************!
!>  Put long variables into the Lua script.
!!
  subroutine aot_out_val_long(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, optional, intent(in) :: advance_previous
    integer(kind=long_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) &
        & trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(i0)", advance=adv_string) val
    end if

  end subroutine aot_out_val_long
!******************************************************************************!


!******************************************************************************!
!>  Put real variables into the Lua script.
!!
  subroutine aot_out_val_real(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, optional, intent(in) :: advance_previous
    real(kind=single_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) &
        & trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(f0.9)", advance=adv_string) val
    end if

  end subroutine aot_out_val_real
!******************************************************************************!


!******************************************************************************!
!>  Put double variables into the Lua script.
!!
  subroutine aot_out_val_double(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, optional, intent(in) :: advance_previous
    real(kind=double_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) &
        & trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(f0.9)", advance=adv_string) val
    end if

  end subroutine aot_out_val_double
!******************************************************************************!


!******************************************************************************!
!>  Put logical variables into the Lua script.
!!
  subroutine aot_out_val_logical(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, intent(in) :: val
    logical, optional, intent(in) :: advance_previous
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    character(len=5) :: valstring
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (val) then
      valstring = 'true'
    else
      valstring = 'false'
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        &   trim(vname)//" = "//trim(valstring)
    else
      write(put_conf%outunit, fmt="(a)", advance=adv_string) trim(valstring)
    end if

  end subroutine aot_out_val_logical
!******************************************************************************!


!******************************************************************************!
!>  Put string variables into the Lua script.
!!
  subroutine aot_out_val_string(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    character(len=*), intent(in) :: val
    logical, optional, intent(in) :: advance_previous
    !------------------------------------------------------------------------
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    if (put_conf%level > 0) then
      ! Leave the advancing to the next entry in the table.
      adv_string = 'no'
    else
      ! Not within a table, finalize the global definition with a newline.
      adv_string = 'yes'
    end if

    call aot_out_breakline(put_conf, advance_previous)

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        & trim(vname)//" = '"//trim(val)//"'"
    else
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        &  "'"//trim(val)//"'"
    end if

  end subroutine aot_out_val_string
!******************************************************************************!


!******************************************************************************!
!>  Put integer array variables into the Lua script.
!!
  subroutine aot_out_val_arr_int(put_conf, val, vname, advance_previous)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer, intent(in) :: val(:)
    logical, optional, intent(in) :: advance_previous
    !------------------------------------------------------------------------
    integer :: i
    !------------------------------------------------------------------------

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    ! Opening the table(subtable for array actually)
    call aot_out_open_table(put_conf, vname, & 
     &                      advance_previous = advance_previous)

    
    ! Looping over val which is a one dimensional array
    do i = 1, size(val)
      if (mod(i, 10) .ne. 0) then
        ! Call the aot_out_val_int routine to write integer values within array
        call aot_out_val(put_conf, val(i), &
          &              advance_previous = .false.)
      else      
        call aot_out_val(put_conf, val(i), &
          &              advance_previous = .true.)
      end if
    end do

    ! Close the table
    call aot_out_close_table(put_conf, advance_previous = .false.)

  end subroutine aot_out_val_arr_int
!******************************************************************************!

!******************************************************************************!
!>  Put long array variables into the Lua script.
!!
  subroutine aot_out_val_arr_long(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer(kind=long_k), intent(in) :: val(:)
    !------------------------------------------------------------------------
    integer :: i
    !------------------------------------------------------------------------

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    ! Opening the table(subtable for array actually)
    call aot_out_open_table(put_conf, vname, & 
     &                      advance_previous = .false.)

    
    ! Looping over val which is a one dimensional array
    do i = LBOUND(val,1), UBOUND(val,1) 
      if(mod((i-1), 10) .ne. 0)then
        ! Call the aot_out_val_int routine to write integer values within array
        call aot_out_val(put_conf, val(i), &
         &                   advance_previous = .false.)
       else      
         call aot_out_val(put_conf, val(i), &
          &                   advance_previous = .true.)
      end if
    end do

    ! Close the table
    call aot_out_close_table(put_conf, advance_previous = .false.)

  end subroutine aot_out_val_arr_long
!******************************************************************************!

!******************************************************************************!
!>  Put real array variables into the Lua script.
!!
  subroutine aot_out_val_arr_real(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    real, intent(in) :: val(:)
    !------------------------------------------------------------------------
    integer :: i
    !------------------------------------------------------------------------

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    ! Opening the table(subtable for array actually)
    call aot_out_open_table(put_conf, vname, & 
     &                      advance_previous = .false.)

    
    ! Looping over val which is a one dimensional array
    do i = LBOUND(val,1), UBOUND(val,1) 
      if(mod((i-1), 10) .ne. 0)then
        ! Call the aot_out_val_int routine to write integer values within array
        call aot_out_val(put_conf, val(i), &
         &                   advance_previous = .false.)
       else      
         call aot_out_val(put_conf, val(i), &
          &                   advance_previous = .true.)
      end if
    end do

    ! Close the table
    call aot_out_close_table(put_conf, advance_previous = .false.)

  end subroutine aot_out_val_arr_real
!******************************************************************************!


!******************************************************************************!
!>  Put real array variables into the Lua script.
!!
  subroutine aot_out_val_arr_double(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    real(kind=double_k), intent(in) :: val(:)
    !------------------------------------------------------------------------
    integer :: i
    !------------------------------------------------------------------------

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    ! Opening the table(subtable for array actually)
    call aot_out_open_table(put_conf, vname, & 
     &                      advance_previous = .false.)

    
    ! Looping over val which is a one dimensional array
    do i = LBOUND(val,1), UBOUND(val,1) 
      if(mod((i-1), 10) .ne. 0)then
        ! Call the aot_out_val_int routine to write integer values within array
        call aot_out_val(put_conf, val(i), &
         &                   advance_previous = .false.)
       else      
         call aot_out_val(put_conf, val(i), &
          &                   advance_previous = .true.)
      end if
    end do

    ! Close the table
    call aot_out_close_table(put_conf, advance_previous = .false.)

  end subroutine aot_out_val_arr_double
!******************************************************************************!


!******************************************************************************!
!> This subroutine takes care of the proper linebreaking in Lua-Tables.
!!
!! It takes care of a proper line-continuation, depending on the optional
!! advance_previous flag and increases the count of elements in the current
!! table.
  subroutine aot_out_breakline(put_conf, advance_previous)
    type(aot_out_type), intent(inout)  :: put_conf
    logical, optional, intent(in) :: advance_previous

    character(len=put_conf%indent) :: indent
    character :: sep
    logical :: loc_adv_prev

    indent = ''
    if (present(advance_previous)) then
      loc_adv_prev = advance_previous
    else
      loc_adv_prev = .true.
    end if
    
    lev_if: if (put_conf%level > 0) then

      if (put_conf%stack(put_conf%level) > 0) then
        ! Use the separator to close the previous entry.
        sep = ','
      else
        ! First entry, nothing to separate yet.
        sep = ''
      end if

      if (loc_adv_prev) then
        write(put_conf%outunit, fmt='(a)') trim(sep)
        write(put_conf%outunit, fmt='(a)', advance='no') indent
      else
        write(put_conf%outunit, fmt='(a)', advance='no') trim(sep)//" "
      end if

      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1

    end if lev_if

  end subroutine aot_out_breakline
!******************************************************************************!



  
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
