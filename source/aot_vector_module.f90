!> This module provides some convenience functions to access complete vectors
!! from a lua table at once.
module aot_vector_module
  use flu_binding
  use aotus_module, only: get_top_val, aoterr_NonExistent
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_table_module, only: aot_table_open, aot_table_top, &
    &                         aot_table_length, aot_table_getval, &
    &                         aot_table_first

  implicit none

  private

  public :: get_table_val

  interface get_table_val
    module procedure get_table_real_vvect
!!    module procedure get_table_double_vect
!!    module procedure get_table_integer_vect
!!    module procedure get_table_long_vect
!!    module procedure get_table_string_vect
!!    module procedure get_table_logical_vect
  end interface get_table_val

contains

  !> This routine obtains a vectorial quantity with variable length from a Lua
  !! table as a whole.
  !!
  !! It is intented to ease the reading of vectors on the Fortran side by
  !! capsulating the parsing of the Lua table internally.
  !! For the dynamically sized array, which will be allocated, a upper limit
  !! to allocate has to be specified.
  subroutine get_table_real_vvect(conf, thandle, tab_val, ErrCode, var, &
    &                            pos, maxlength, default)
    type(flu_State) :: conf !< Handle to the lua script
    integer, intent(in) :: thandle !< Handle of the parent table

    !> Vector read from the Lua table, will have the same length as the table
    !! but not exceed maxlength, if provided.
    real(kind=single_k), intent(out), allocatable :: tab_val(:)

    !> Error code describing problems encountered in each of the components.
    !! Will be allocated with the same length as the returned vector.
    !! If the complete vector is not given in the Lua script, and no default
    !! is provided, an zerosized array will be returned.
    integer, intent(out), allocatable :: ErrCode(:)

    !> Maximal length to allocate for the vector.
    integer, intent(in) :: maxlength

    !> Name of the variable (vector) to read.
    character(len=*), intent(in), optional :: var

    !> Position of the (vector) to read.
    integer, intent(in), optional :: pos

    !> A default vector to use, if no proper definition is found.
    !! Components will be filled with the help of this default definition.
    real(kind=single_k), intent(in), optional :: default(:)

    integer :: vect_handle
    integer :: table_len, vect_len, def_len
    integer :: vect_lb
    integer :: iComp

    ! Get the requeseted value from the provided table
    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)

    ! Try to interpret it as table
    vect_handle = aot_table_top(L=conf)
    table_len = aot_table_length(L=conf, thandle=vect_handle)

    ! The size of the vector is limited by maxlength.
    vect_len = min(maxlength, table_len)

    ! Find the length of the default value, if it is not provided, its 0.
    def_len = 0
    if (present(default)) def_len = size(default)

    ! Now parse the table with the vector entries.
    if (aot_table_first(conf, vect_handle)) then
      allocate(tab_val(vect_len))
      allocate(errCode(vect_len))

      ! Only if the vector table actually exists, and has at least one entry,
      ! this parsing has to be done.
      if (present(default).and.(def_len > 0)) then
        call get_top_val(conf, tab_val(1), ErrCode(1), default(1))
      else
        call get_top_val(conf, tab_val(1), ErrCode(1))
      end if

      ! Up to the length of the default value, provide the default settings.
      do iComp=2,def_len
        if (.not. flu_next(conf, vect_handle)) exit
        call get_top_val(conf, tab_val(iComp), ErrCode(iComp), default(iComp))
      end do

      vect_lb = max(2, def_len)
      ! After def_len entries no default values for the components are
      ! available anymore, proceed without a default setting for the rest.
      do iComp=vect_lb,vect_len
        if (.not. flu_next(conf, vect_handle)) exit
        call get_top_val(conf, tab_val(iComp), ErrCode(iComp))
      end do
    else
      ! No vector definition found in the Lua script, use the default.
      if (present(default)) then
        allocate(tab_val(def_len))
        allocate(errCode(vect_len))
        tab_val = default
        ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      else
        ! No vector definition in the Lua script and no default provided,
        ! return an empty array.
        allocate(tab_val(0))
        allocate(errCode(0))
      end if
    end if

  end subroutine get_table_real_vvect


end module aot_vector_module
