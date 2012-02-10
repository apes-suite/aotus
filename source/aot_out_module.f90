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
    module procedure aot_out_val_arr_1d
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
  subroutine aot_out_open_table(put_conf, tname, linearize)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: tname
    logical, optional :: linearize
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------
    if(present(linearize) .and. linearize )then
      adv_string = 'no'
    else
      adv_string = 'yes'
    end if

    indent = ''
    if(put_conf%level .gt. 0)  then
      if( put_conf%stack( put_conf%level ) .gt. 0) then
        ! Not the first entry in the parent table, close previous entry with
        ! a separator.
        write(put_conf%outunit,fmt="(a)") ','
      endif
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(tname)) then
      write(put_conf%outunit, fmt="(a)", advance=adv_string) indent//trim(tname)//' = {'
    else
      write(put_conf%outunit, fmt="(a)", advance=adv_string) indent//'{'
    end if

    put_conf%level = put_conf%level + 1
    put_conf%indent = put_conf%indent + indentation

  end subroutine aot_out_open_table
!******************************************************************************!


!******************************************************************************!
!>  Close the current table.
!!
  subroutine aot_out_close_table(put_conf, linearize)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    logical, optional, intent(in) :: linearize
    !------------------------------------------------------------------------
    character(len=max(put_conf%indent-indentation,0)) :: indent

    indent = ''
    put_conf%indent = max(put_conf%indent - indentation, 0)
    put_conf%stack(put_conf%level) = 0
    put_conf%level = max(put_conf%level - 1, 0)

    if(present(linearize) .and. linearize)then
      ! put closing brace in the same line
      write(put_conf%outunit,fmt="(a)", advance='no') '}'
    else if (put_conf%level == 0) then
      ! Close last entry without separator and put closing brace on a separate
      ! line.
      write(put_conf%outunit,*) ''
      write(put_conf%outunit,fmt="(a)") indent//'}'
    else
      ! Close last entry without separator and put closing brace on a separate
      ! line.
      write(put_conf%outunit,*) ''
      ! Do not advance, to let the next entry append the separator, to the line
      write(put_conf%outunit,fmt="(a)", advance='no') indent//'}'
    end if

  end subroutine aot_out_close_table
!******************************************************************************!



!******************************************************************************!
!>  Put integer variables into the Lua script.
!!
  subroutine aot_out_val_int(put_conf, val, vname, linearize)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer, intent(in) :: val
    logical, optional :: linearize
    !------------------------------------------------------------------------
    character(len=:), allocatable :: indent
    character(len=3) :: adv_string
    integer :: i
    !------------------------------------------------------------------------
    adv_string = 'yes'

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        if(present(linearize) .and. linearize )then
          write(put_conf%outunit, fmt="(a)", advance=adv_string) ","
        else
          write(put_conf%outunit, fmt="(a)") ","
        end if
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    ! if the output shall be linearized advance = 'no', indentation = ' '
    if(present(linearize) .and. linearize )then
      adv_string = 'no'
      allocate(character(len=1) :: indent)
      indent = ' '
    else
      allocate(character(len=put_conf%indent) :: indent)
      indent = ''
      do i=1, put_conf%indent
        indent = indent//' '
      end do
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) &
        & indent//trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) indent, val
    end if

    deallocate(indent)

  end subroutine aot_out_val_int
!******************************************************************************!


!******************************************************************************!
!>  Put long variables into the Lua script.
!!
  subroutine aot_out_val_long(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer(kind=long_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) &
        & indent//trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(a,i0)", advance=adv_string) indent, val
    end if

  end subroutine aot_out_val_long
!******************************************************************************!


!******************************************************************************!
!>  Put real variables into the Lua script.
!!
  subroutine aot_out_val_real(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    real(kind=single_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) &
        & indent//trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) indent, val
    end if

  end subroutine aot_out_val_real
!******************************************************************************!


!******************************************************************************!
!>  Put double variables into the Lua script.
!!
  subroutine aot_out_val_double(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    real(kind=double_k), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) &
        & indent//trim(vname)//" = ", val
    else
      write(put_conf%outunit, fmt="(a,f0.9)", advance=adv_string) indent, val
    end if

  end subroutine aot_out_val_double
!******************************************************************************!


!******************************************************************************!
!>  Put logical variables into the Lua script.
!!
  subroutine aot_out_val_logical(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    logical, intent(in) :: val
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    character(len=5) :: valstring
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (val) then
      valstring = 'true'
    else
      valstring = 'false'
    end if

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        & indent//trim(vname)//" = "//trim(valstring)
    else
      write(put_conf%outunit, fmt="(a)", advance=adv_string) indent &
        &                                                    //trim(valstring)
    end if

  end subroutine aot_out_val_logical
!******************************************************************************!


!******************************************************************************!
!>  Put string variables into the Lua script.
!!
  subroutine aot_out_val_string(put_conf, val, vname)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    character(len=*), intent(in) :: val
    !------------------------------------------------------------------------
    character(len=put_conf%indent) :: indent
    character(len=3) :: adv_string
    !------------------------------------------------------------------------

    indent = ''
    adv_string = 'yes'

    if (put_conf%level .gt. 0) then
      ! Do not advance after writing this value, in order to allow
      ! subsequent entries, to append the separator!
      adv_string = 'no'
      if (put_conf%stack(put_conf%level) .gt. 0) then
        ! This is not the first entry in the current table, append a ',' to the
        ! previous entry.
        write(put_conf%outunit,fmt="(a)") ","
      end if
      put_conf%stack(put_conf%level) = put_conf%stack(put_conf%level) + 1
    end if

    if (present(vname)) then
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        & indent//trim(vname)//" = '"//trim(val)//"'"
    else
      write(put_conf%outunit, fmt="(a)", advance=adv_string) &
        &  indent//"'"//trim(val)//"'"
    end if

  end subroutine aot_out_val_string
!******************************************************************************!


!******************************************************************************!
!>  Put array variables into the Lua script.
!!
  subroutine aot_out_val_arr_1d(put_conf, val, vname, linearize)
    !------------------------------------------------------------------------
    type(aot_out_type), intent(inout)  :: put_conf
    character(len=*), optional, intent(in) :: vname
    integer, intent(in) :: val(:)
    logical, optional :: linearize
    !------------------------------------------------------------------------
    integer :: i
    logical :: local_linearize
    !------------------------------------------------------------------------

    local_linearize = .false.
    if(present(linearize)) &
      local_linearize = linearize
    !Looping over val which is a one dimensional array
    do i = LBOUND(val,1), UBOUND(val,1) 
      !If the vname is specified then it will be printed followed by = {
      !otherwise only { will be printed marking start of subtable
      if(i .eq. LBOUND(val,1) ) then
        if (present(vname)) then
          call aot_out_open_table( put_conf = put_conf, tname = trim( vname ), &
            &                      linearize = local_linearize)
        else 
          call aot_out_open_table( put_conf = put_conf, linearize = local_linearize)
        end if
      end if
      call aot_out_val(put_conf = put_conf, val = val(i), linearize = local_linearize)
    end do
    call aot_out_close_table(put_conf = put_conf, linearize = .true.)

  end subroutine aot_out_val_arr_1d
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
