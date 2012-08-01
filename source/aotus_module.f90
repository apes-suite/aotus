!> This module provides high level Fortran interfaces to retrieve values from a
!! Lua script.
!!
!! Its central interface is aot_get_val, which is a generic interface that
!! allows access to scalars and vectors in global Lua variables as well as
!! nested tables.
module aotus_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val, aot_err_handler, &
    &                       aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
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
  public :: aot_err_handler
  public :: aot_top_get_val

  ! Inherited from the flu_binding module, publish for convenience.
  public :: flu_State

  !> Get a global configuration value from the script.
  !!
  !! This provides a convenient direct access to
  !! global variables from the Lua script.
  !! The interface is also used for other values than the global ones, and the
  !! general shape of it looks like
  !! <tt>call aot_{top}_get_val(<outputs>, <id>, default)</tt>.
  !! Where the "outputs" are <tt>val</tt> and <tt>errCode</tt>. While "id" is
  !! at least the Lua context <tt>L</tt>. For the global variables there has to
  !! be a <tt>key</tt> for the identification of the variable.
  !!
  !! The <tt>errCode</tt> returns an error code with various bits set for
  !! different errors, that might happen while retrieving the variable.
  !! They can be checked by <tt>btest</tt> and the different error codes are:
  !!- aoterr_fatal: Something went irrecoverably wrong
  !!- aoterr_nonExistent: The requested variable is not set in the Lua script
  !!- aoterr_wrongType: The requested variable in the Lua script does not meet
  !!                    the requested data type
  !!
  !! For example a check for a fatal error can be done by
  !! <tt>btest(errCode, aoterr_fatal)</tt>.
  !!
  !! For the access to global variables in the Lua script the interface
  !! therefore looks like:
  !! <tt>call aot_get_val(val, errCode, L, key, default)</tt>.
  !! See for example aotus_module#get_config_real for a more detailed
  !! description of the parameters.
  interface aot_get_val
    module procedure get_config_real
    module procedure get_config_double
    module procedure get_config_integer
    module procedure get_config_long
    module procedure get_config_string
    module procedure get_config_logical
  end interface

contains

  !> Subroutine to load and execute a script from a file.
  subroutine open_config_file(L, filename, ErrCode, ErrString)
    type(flu_State) :: L !< Handle to the Lua script

    !> Name of file to load the Lua code from
    character(len=*), intent(in) :: filename

    !> Error code returned by Lua during loading or executing the file.
    !!
    !! This optional parameter might be used to react on errors in the calling
    !! side. If neither ErrCode nor ErrString are given, this subroutine will
    !! stop the program execution and print the error message from Lua to the
    !! stdout.
    integer, intent(out), optional :: ErrCode

    !> Obtained error description from the Lua stack.
    !!
    !! This optional argument holds the Lua error message in case somehting
    !! went wrong. It can be used to provide some feedback to the user in the
    !! calling routine. If neither ErrCode nor ErrString are provided,
    !! open_config() will print the error message and stop program execution.
    character(len=*), intent(out), optional :: ErrString

    integer :: err

    if (.not.flu_isopen(L)) L = fluL_newstate()

    err = fluL_loadfile(L, filename)
    call aot_err_handler(L, err, 'Cannot load configuration file:', ErrString, &
      &                  ErrCode)

    if (err == 0) then
      call fluL_openlibs(L)

      err = flu_pcall(L, 0, 0, 0)
      call aot_err_handler(L, err, 'Cannot run configuration file:',  &
        &                  ErrString, ErrCode)
    end if

  end subroutine open_config_file


  !> Subroutine to load and execute a script given in a string.
  subroutine open_config_chunk(L, chunk, ErrCode, ErrString)
    type(flu_State) :: L !< Handle to the Lua script

    !> String with Lua code to load.
    character(len=*), intent(in) :: chunk

    !> Error code returned by Lua during loading or executing the file.
    !!
    !! This optional parameter might be used to react on errors in the calling
    !! side. If neither ErrCode nor ErrString are given, this subroutine will
    !! stop the program execution and print the error message from Lua to the
    !! stdout.
    integer, intent(out), optional :: ErrCode

    !> Obtained error description from the Lua stack.
    !!
    !! This optional argument holds the Lua error message in case somehting
    !! went wrong. It can be used to provide some feedback to the user in the
    !! calling routine. If neither ErrCode nor ErrString are provided,
    !! open_config() will print the error message and stop program execution.
    character(len=*), intent(out), optional :: ErrString

    integer :: err

    if (.not.flu_isopen(L)) L = fluL_newstate()

    err = fluL_loadstring(L, chunk)

    call aot_err_handler(L, err, 'Cannot load chunk:', ErrString, ErrCode)

    if (err == 0) then
      call fluL_openlibs(L)

      err = flu_pcall(L, 0, 0, 0)

      call aot_err_handler(L, err, 'Cannot run chunk:', ErrString, ErrCode)
    end if

  end subroutine open_config_chunk


  !> Close an opened Lua script again.
  subroutine close_config(L)
    type(flu_State) :: L !< Handle to the Lua script to close.

    call flu_close(L)

  end subroutine close_config


  subroutine get_config_real(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    real(kind=single_k), intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    real(kind=single_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_real


  subroutine get_config_double(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    real(kind=double_k), intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    real(kind=double_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_double


  subroutine get_config_integer(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    integer, intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    integer, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_integer


  subroutine get_config_long(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    integer(kind=long_k), intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    integer(kind=long_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_long


  subroutine get_config_logical(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    logical, intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    logical, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_logical


  subroutine get_config_string(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    character(len=*) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    character(len=*), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_string

end module aotus_module
