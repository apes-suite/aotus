!> This module provides some convenient functions to act on Lua tables.
module aot_table_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val

  implicit none

  private

  public :: aot_table_top, aot_table_length, aot_table_first, aot_table_push
  public :: aot_table_open, aot_table_close, aot_table_get_val
  public :: aot_get_val

  !> This routine provides a way to open a table
  !! either as a globally defined one, are as a
  !! table within another table.
  !! After the table is opened, the returned
  !! handle can be used to access its components.
  interface aot_table_open
    module procedure aot_table_global
    module procedure aot_table_table
  end interface

  !> Get a value from a table.
  !!
  !! First the given key is looked up, if this fails, the value
  !! at the given position is looked up, and if this also fails,
  !! the default value is returned.
  !! Positional addressing is only valid, as long,
  !! as no value was provided by an explicit key
  !! in the list before the entry in question.
  interface aot_table_get_val
    module procedure get_table_real
    module procedure get_table_double
    module procedure get_table_integer
    module procedure get_table_long
    module procedure get_table_string
    module procedure get_table_logical
  end interface

  !> Get a value from a table.
  !!
  !! First the given key is looked up, if this fails, the value
  !! at the given position is looked up, and if this also fails,
  !! the default value is returned.
  !! Positional addressing is only valid, as long,
  !! as no value was provided by an explicit key
  !! in the list before the entry in question.
  !!
  !! The interface to access table values looks like:
  !! `call aot_get_val(val, errCode, L, thandle, key, pos, default)`.
  !! Position pos and key are both optional, but one of them has to be provided.
  !! If both are provided the key takes precedence over the pos, and the pos
  !! will only be tried if the access to the key fails.
  !! See for example get_table_real() for a more detailed
  !! description of the parameters.
  !!
  !! Note that positional addressing only works intuitively as long as there
  !! have been no entries specified by keys in the table.
  !! This kind of resembles the behavior of Fortran interfaces with named or
  !! unnamed arguments, as soon as you provide a name, all following arguments
  !! have to be given by key also.
  !! Just stick to this rule for the Lua tables as well to avoid too much
  !! headache.
  !!
  !! The reason for this is, that positional addressing in Lua refers only to
  !! the unnamed entries of the tables.
  interface aot_get_val
    module procedure get_table_real
    module procedure get_table_double
    module procedure get_table_integer
    module procedure get_table_long
    module procedure get_table_string
    module procedure get_table_logical
  end interface


contains

  !> Return the position at the top of the stack as a
  !! table handle.
  !!
  !! If it actually exists and is a table, this handle can be used
  !! for further operations on that table.
  !! Otherwise a 0 will be returned.
  function aot_table_top(L) result(thandle)
    type(flu_state) :: L !< Handle for the Lua script.

    !> A handle for the table on the top of the stack to access it.
    integer :: thandle

    if (flu_isNoneOrNil(L, -1) .or. (.not. flu_isTable(L, -1))) then
      thandle = 0
      call flu_pop(L)
    else
      thandle = flu_gettop(L)
    end if
  end function aot_table_top


  !> Load a globally defined table into the top of the stack.
  !!
  !! Return its position in the stack as a handle for this
  !! table. If it does not exist or the global variable is not
  !! a table, the handle will be set to 0.
  subroutine aot_table_global(L, thandle, key)
    type(flu_state) :: L !< Handle for the Lua script.

    !> A handle for the table to access it, 0 if no table available.
    integer, intent(out) :: thandle

    !> Name of the global table to access.
    character(len=*), intent(in) :: key

    call flu_getglobal(L, key)

    thandle = aot_table_top(L)
  end subroutine aot_table_global


  !> This subroutine tries to get a table in a table, and
  !! return a handle for it.
  !!
  !! Return its position in the stack as a handle for this
  !! table. If it does not exist or the table entry is not
  !! a table itself, the handle will be set to 0.
  !! The table can be looked up either by position or name.
  subroutine aot_table_table(L, parent, thandle, key, pos)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle of the table containing the requested table.
    integer, intent(in) :: parent

    !> A handle for the table to access it, 0 if no table available.
    integer, intent(out) :: thandle

    !> Name of the entry in the parent table to access.
    !!
    !! The key takes precedence over the position, if both are provided.
    !! In this case the positional address is only tried, if the access to the
    !! key failed.
    character(len=*), intent(in), optional :: key

    !> Position of the entry in the parent table to access.
    integer, intent(in), optional :: pos

    call aot_table_push(L, parent, key, pos)
    thandle = aot_table_top(L)
  end subroutine aot_table_table


  !> Close a table again.
  !!
  !! This is done by popping all values above and itself from the stack.
  subroutine aot_table_close(L, thandle)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle of the table to close.
    integer, intent(in) :: thandle

    if (thandle > 0) call flu_settop(L, thandle-1)
  end subroutine aot_table_close


  !> This subroutine tries to push the value of the entry given by key or pos
  !! within the table thandle on the lua stack.
  !!
  !! If no corresponding value is found, a nil value is pushed to the stack.
  !! Key and pos are both optional, but one of them has to be supplied. If one
  !! is supplied, the key is checked first and only if this fails the entry at
  !! pos will be looked up.
  subroutine aot_table_push(L, thandle, key, pos)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle to the table to look in.
    integer :: thandle

    !> Name of the entry to push to the stack.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to push to the stack.
    integer, intent(in), optional :: pos

    if (thandle /= 0) then
      ! Only proceed if thandle is actually a table
      ! (Should be received with aot_table_global or aot_table_top)

      if (present(key)) then
        ! Try to look up the given key first
        call flu_getfield(L, thandle, key)
        if (flu_isNoneOrNil(L, -1)) then
          ! If this is not found, try to retrieve
          ! the value at the given position
          if (present(pos)) then
            call flu_pop(L)
            call flu_pushInteger(L, pos)
            call flu_getTable(L, thandle)
          end if
        end if
      else
        ! No key to look up, just check the given position
        if (present(pos)) then
          call flu_pushInteger(L, pos)
          call flu_getTable(L, thandle)
        else
          ! Neither key nor pos present, nothing to look up
          ! Just push a NIL onto the stack as a result
          call flu_pushnil(L)
        end if
      end if

    else

      call flu_pushnil(L)

    end if

  end subroutine aot_table_push


  !> Load the first key-value pair of table thandle on the
  !! stack.
  !!
  !! This serves as an entry point, further traversal
  !! can be done by flu_next(L, thandle).
  !! If there are no entries in the table the function
  !! returns false, otherwise the result will be true.
  function aot_table_first(L, thandle) result(exists)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle to the table to get the first entry of.
    integer, intent(in) :: thandle

    !> The return value signals, if there actually is such a first entry.
    logical :: exists

    if (thandle /= 0) then
      call flu_pushnil(L)
      exists = flu_next(L, thandle)
    else
      exists = .false.
    end if
  end function aot_table_first


  !> Count the entries in a lua table.
  function aot_table_length(L, thandle) result(length)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle of the table to count the enries in.
    integer, intent(in) :: thandle

    !> Returns the number of entries in the table.
    integer :: length

    length = 0
    if (aot_table_first(L, thandle)) then
      do
        length = length + 1
        call flu_pop(L)
        if (.not. flu_next(L, thandle)) exit
      end do
    end if
  end function aot_table_length


  !> Retrieve a single precision real value from a table.
  subroutine get_table_real(val, ErrCode, L, thandle, key, pos, default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    real(kind=single_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    real(kind=single_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_real


  !> Retrieve a double precision real value from a table.
  subroutine get_table_double(val, ErrCode, L, thandle, key, pos, &
    &                         default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    real(kind=double_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    real(kind=double_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_double


  !> Retrieve a default integer value from a table.
  subroutine get_table_integer(val, ErrCode, L, thandle, key, pos, &
    &                          default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    integer, intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode


    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    integer, intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_integer


  !> Retrieve a long integer value from a table.
  subroutine get_table_long(val, ErrCode, L, thandle, key, pos, default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    integer(kind=long_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    integer(kind=long_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_long


  !> Retrieve a logical value from a table.
  subroutine get_table_logical(val, ErrCode, L, thandle, key, pos, &
    &                          default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    logical, intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    logical, intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_logical


  !> Retrieve a string from a table.
  subroutine get_table_string(val, ErrCode, L, thandle, key, pos, &
    &                         default)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    character(len=*) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Name of the entry to look for.
    !!
    !! Key and pos are both optional, however at least one of them has to be
    !! supplied.
    !! The key takes precedence over the pos if both are given.
    character(len=*), intent(in), optional :: key

    !> Position of the entry to look for in the table.
    !!
    !! It allows the access to unnamed arrays in the Lua tables.
    integer, intent(in), optional :: pos

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    character(len=*), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_table_string


end module aot_table_module
