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
  use aot_table_module, only: aot_get_val, aot_table_set_val, &
    &                         aot_table_open, aot_table_close
  use aot_vector_module, only: aot_get_val, aot_top_get_val

  ! The following module enables an interface for quadruple precision numbers,
  ! if the compiler supports them. However, you should be aware, that this is
  ! merely a convenience interface, as the values provided by Lua are only
  ! double precision.
  use aot_quadruple_module

  ! Support for extdouble precision.
  use aot_extdouble_module

  implicit none

  private

  public :: aot_get_val
  public :: open_config_file, close_config
  public :: open_config_chunk, open_config_buffer
  public :: aot_require_buffer
  public :: aot_file_to_buffer

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
  !! `btest(errCode, aoterr_fatal)`.
  !!
  !! For the access to global variables in the Lua script the interface
  !! therefore looks like:
  !! `call aot_get_val(val, errCode, L, key, default)`.
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


  !> Subroutine to load and execute a script given in a buffer
  !! (might be bytecode).
  subroutine open_config_buffer(L, buffer, bufName, ErrCode, ErrString)
    type(flu_State) :: L !< Handle to the Lua script

    !> String with Lua code to load.
    character, intent(in) :: buffer(:)

    !> Name for the buffer to use in debug messages.
    character(len=*), intent(in), optional :: bufName

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

    err = fluL_loadbuffer(L, buffer, bufName)

    call aot_err_handler(L, err, 'Cannot load buffer:', ErrString, ErrCode)

    if (err == 0) then
      call fluL_openlibs(L)

      err = flu_pcall(L, 0, 0, 0)

      call aot_err_handler(L, err, 'Cannot run buffer:', ErrString, ErrCode)
    end if

  end subroutine open_config_buffer


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


  !> Subroutine to load a script from a file and put it into a character buffer.
  subroutine aot_file_to_buffer(filename, buffer, ErrCode, ErrString)
    !> Name of file to load the Lua code from
    character(len=*), intent(in) :: filename

    !> Buffer to store the script in the given file in
    character, pointer :: buffer(:)

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

    type(flu_State) :: L
    integer :: err
    integer :: buflen

    L = fluL_newstate()

    err = fluL_loadfile(L, filename)
    call aot_err_handler(L, err, 'Cannot load configuration file:', ErrString, &
      &                  ErrCode)

    if (err == 0) then

      call flu_dump(L = L, buf = buffer, length = buflen, iError = err)

      if (err /= 0) then
        if (present(ErrCode)) then
           ErrCode = err
           if (present(ErrString)) then
             ErrString = 'Error while dumping the Lua script into a buffer!'
           end if
        else
          write(*,*) 'Error while dumping the Lua script into a buffer!'
          write(*,*) 'STOPPING'
          STOP
        end if
      end if

    end if

    call close_config(L)

  end subroutine aot_file_to_buffer


  !> Load and execute a given buffer and register it in the package table as
  !! the given module name.
  subroutine aot_require_buffer(L, buffer, modname)
    type(flu_State) :: L !< Lua State to set load the buffer into.
    character, intent(in) :: buffer(:) !< Buffer to load.
    character(len=*), intent(in) :: modname !< Module name to set.

    integer :: pac_handle
    integer :: ld_handle

    call open_config_buffer(L = L, buffer = buffer, bufName = trim(modname))
    call aot_table_open(L, thandle = pac_handle, key = "package")
    call aot_table_open(L, parent = pac_handle, &
      &                 thandle = ld_handle, key = "loaded")
    call aot_table_set_val(val = .true., L = L, thandle = ld_handle, &
      &                    key = trim(modname))
    call aot_table_close(L, ld_handle)
    call aot_table_close(L, pac_handle)
  end subroutine aot_require_buffer

end module aotus_module

!> \page Aotus
!!
!! Aotus stands for *Advanced Options in Tables and Universal Scripting*.
!!
!! It is a Fortran wrapper for the [Lua](http://www.lua.org/) scripting
!! language.
!! The aim of this wrapper is to provide flexible configuration files to Fortran
!! applications with the full user experience provided by Lua.
!! Aotus is also known as the
!! [night monkey](http://en.wikipedia.org/wiki/Night_monkey), living in south
!! america.
!! Thus we saw the name as fitting as it interacts with the moon (Lua, provided
!! by the Pontifical Catholic University of Rio de Janeiro in Brazil).
!!
!! The most prominent data structure in Lua are
!! [tables](http://www.lua.org/manual/5.2/manual.html#2), which provide the
!! possibility to store complex data structures.
!! Thus the configuration is mainly done in global variables in the lua script
!! or tables.
!!
!! Aotus provides several layers, encapsulating the bare
!! [Lua C-API](http://www.lua.org/manual/5.2/manual.html#4):
!! - Lua_fif: this just provides the
!!   [ISO_C_Binding](http://www.fortran.bcs.org/2002/interop.htm)
!!   interface declarations.
!! - Flu_binding: this the actural Fortran binding wrapped around Lua_fif, to
!!   provide a more Fortran like interface.
!!   Especially the Flu_binding::flu_state type is declared which maintains the
!!   handle for the
!!   [Lua state](http://www.lua.org/manual/5.2/manual.html#lua_state).
!! - AOT_table_module: provides some convenience functions to work on Lua tables
!!   in Fortran.
!! - AOT_fun_module: provides some convenience functions to work with Lua
!!   functions in Fortran.
!! - Aotus_module: provides the high end level to easily retrieve data from a
!!   Lua script.
!! - On top of those there is an additional AOT_vector_module, which allows the
!!   concise reading of values into arrays of rank one.
!! - Finally there is and additional AOT_out_module, that allows output of
!!   Fortran values into nested Lua tables.
!!
!! The library can be compiled by various modern Fortran compilers as described
!! in \ref compiler_support "Compiler Support".
!!
!! An example showing the usage of the library in a Fortran application is given
!! in sample/aotus_sample.f90 in the Aotus main directory.
!! The corresponding Lua script used as input is given in sample/config.lua.
!!
!! *Please see also the README.rst.*
!!
!! *Sources are available at <https://bitbucket.org/haraldkl/aotus/overview>.*
