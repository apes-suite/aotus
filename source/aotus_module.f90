!> This module provides high level Fortran interfaces to retrieve values from a
!! Lua script.
!!
!! Its central interface is aot_get_val, which is a generic interface that
!! allows access to scalars and vectors in global Lua variables as well as
!! nested tables.
module aotus_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val, aoterr_Fatal, aoterr_NonExistent, &
    &                       aoterr_WrongType
  use aot_table_module, only: aot_get_val
  use aot_vector_module, only: aot_get_val, aot_top_get_val

  implicit none

  private

  public :: aot_get_val
  public :: open_config_file, close_config
  public :: open_config_chunk

  ! Entities inherited from aot_top_module, published here to
  ! allow most functionality by "use aotus_module".
  public :: aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
  public :: aot_top_get_val

  ! Inherited from the flu_binding module, publish for convenience.
  public :: flu_State

  !> Get a global configuration value from the script.
  !!
  !! This provides a convenient direct access to
  !! global variables from the Lua script.
  interface aot_get_val
    module procedure get_config_real
    module procedure get_config_double
    module procedure get_config_integer
    module procedure get_config_long
    module procedure get_config_string
    module procedure get_config_logical
  end interface

contains

  subroutine open_config_file(L, filename, ErrCode, ErrString)
    type(flu_State) :: L
    character(len=*), intent(in) :: filename
    integer, intent(out), optional :: ErrCode
    character(len=*), intent(out), optional :: ErrString

    integer :: err

    if (.not.flu_isopen(L)) L = fluL_newstate()

    err = fluL_loadfile(L, filename)
    call aot_config_err_handler(L, err, 'Cannot load configuration file:', ErrString, ErrCode)

    call fluL_openlibs(L)

    err = flu_pcall(L, 0, 0, 0)
    call aot_config_err_handler(L, err, 'Cannot run configuration file:', ErrString, ErrCode)

  end subroutine open_config_file


  subroutine close_config(L)
    type(flu_State) :: L

    call flu_close(L)

  end subroutine close_config


  subroutine get_config_real(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    real(kind=single_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    real(kind=single_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_real


  subroutine get_config_double(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    real(kind=double_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    real(kind=double_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_double


  subroutine get_config_integer(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    integer, intent(out) :: val
    integer, intent(out) :: ErrCode
    integer, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_integer


  subroutine get_config_long(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    integer(kind=long_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    integer(kind=long_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_long


  subroutine get_config_logical(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    logical, intent(out) :: val
    integer, intent(out) :: ErrCode
    logical, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_logical


  subroutine get_config_string(val, ErrCode, L, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    character(len=*) :: val
    integer, intent(out) :: ErrCode
    character(len=*), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_string


  subroutine open_config_chunk(L, chunk, ErrCode, ErrString)
    type(flu_State) :: L
    character(len=*), intent(in) :: chunk
    integer, intent(out), optional :: ErrCode
    character(len=*), intent(out), optional :: ErrString

    integer :: err

    if (.not.flu_isopen(L)) L = fluL_newstate()

    err = fluL_loadstring(L, chunk)

    call aot_config_err_handler(L, err, 'Cannot load chunk:', ErrString, ErrCode)

    call fluL_openlibs(L)

    err = flu_pcall(L, 0, 0, 0)

    call aot_config_err_handler(L, err, 'Cannot run chunk:', ErrString, ErrCode)

  end subroutine open_config_chunk

  subroutine aot_config_err_handler(L, err, msg, ErrString, ErrCode)

    type(flu_State) :: L
    integer, intent(in) :: err
    character(len=*), intent(in) :: msg
    character(len=*), intent(out), optional :: ErrString
    integer, intent(out), optional :: ErrCode

    logical :: stop_on_error
    character, pointer, dimension(:) :: string
    integer :: str_len
    integer :: i

    stop_on_error = .not.(present(ErrString) .or. present(ErrCode))

    if (present(ErrCode)) then
      ErrCode = err
    end if

    if (err .ne. 0) then

      string => flu_tolstring(L, -1, str_len)
      if (present(ErrString)) then
        do i=1,min(str_len, len(ErrString))
          ErrString(i:i) = string(i)
        end do
      end if

      if (stop_on_error) then
        write(*,*) msg, string
        STOP
      end if

    end if

  end subroutine aot_config_err_handler


end module aotus_module
