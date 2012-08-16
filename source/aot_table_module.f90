!> This module provides some convenient functions to act on Lua tables.
module aot_table_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val

  implicit none

  private

  public :: aot_table_top, aot_table_length, aot_table_first, aot_table_push
  public :: aot_table_open, aot_table_close, aot_table_get_val
  public :: aot_table_from_1Darray
  public :: aot_table_set_val, aot_table_set_top
  public :: aot_get_val

  !> This routine provides a way to open a table either as a globally defined
  !! one, a table within another table or a newly defined one.
  !!
  !! After the table is opened, the returned handle can be used to access its
  !! components.
  interface aot_table_open
    module procedure aot_table_global
    module procedure aot_table_table
    module procedure aot_table_new
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

  !> Set a value in a table.
  !!
  !! The given value will be put at the entry named by key into the table
  !! provided in thandle.
  !! Alternatively you can also put the value by position into the table by
  !! providing the pos argument.
  !! If both, pos and key are provided, the key will be used.
  !! Though, both of them are optional, at least one of them has to be provided.
  interface aot_table_set_val
    module procedure set_table_real
    module procedure set_table_double
    module procedure set_table_integer
    module procedure set_table_long
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

  !> This interface enables the simple creation of uniform one dimensional
  !! arrays as tables in the Lua context.
  !!
  !! It takes an one dimensional array of values and returns a thandle to
  !! identify the newly generated table.
  interface aot_table_from_1Darray
    module procedure create_1Darray_real
    module procedure create_1Darray_double
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


  !> Open a new, empty table to fill it subsequently.
  !!
  !! Return its position in the stack as a handle for this
  !! table.
  subroutine aot_table_new(L, thandle)
    type(flu_state) :: L !< Handle for the Lua script.

    !> A handle for the table to access it.
    integer, intent(out) :: thandle

    call flu_createtable(L, 0, 0)
    thandle = flu_gettop(L)
  end subroutine aot_table_new

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


  !===========================================================================!


  !> Put the top of the stack into a table.
  subroutine aot_table_set_top(L, thandle, key, pos)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

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

    integer :: indpos

    if (thandle > 0) then
      if (present(key)) then
        ! Now put it into the table
        call flu_setField(L, thandle, trim(key))
      else
        ! No key given, try to put the value by position
        if (present(pos)) then
          ! First store the current top of the stack for later reference, to
          ! move the desired position infront of it.
          indpos = flu_gettop(L)
          ! First put the index, where to write the value into the table, on the
          ! stack.
          call flu_pushInteger(L, pos)
          ! Now move this position infront of the actual argument, which was
          ! at the top previously..
          call flu_insert(L, indpos)
          ! Get the two entries from the stack into the table.
          call flu_setTable(L, thandle)
        end if
      end if
    end if

  end subroutine aot_table_set_top


  !> Put a single precision real value into a table.
  subroutine set_table_real(val, L, thandle, key, pos)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    real(kind=single_k), intent(in) :: val

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

    if (thandle > 0) then
      if (present(key)) then
        ! If there is a key, use that.
        ! First put the value on the top of the stack
        call flu_pushNumber(L, val)
        ! Now put it into the table
        call flu_setField(L, thandle, trim(key))
      else
        ! No key given, try to put the value by position
        if (present(pos)) then
          ! First put the index, where to write the value into the table, on the
          ! stack.
          call flu_pushInteger(L, pos)
          ! Now put the actual value on the top of the stack.
          call flu_pushNumber(L, val)
          ! Get the two entries from the stack into the table.
          call flu_setTable(L, thandle)
        end if
      end if
    end if

  end subroutine set_table_real


  !> Put a double precision real value into a table.
  subroutine set_table_double(val, L, thandle, key, pos)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    real(kind=double_k), intent(in) :: val

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

    if (thandle > 0) then
      if (present(key)) then
        ! If there is a key, use that.
        ! First put the value on the top of the stack
        call flu_pushNumber(L, val)
        ! Now put it into the table
        call flu_setField(L, thandle, trim(key))
      else
        ! No key given, try to put the value by position
        if (present(pos)) then
          ! First put the index, where to write the value into the table, on the
          ! stack.
          call flu_pushInteger(L, pos)
          ! Now put the actual value on the top of the stack.
          call flu_pushNumber(L, val)
          ! Get the two entries from the stack into the table.
          call flu_setTable(L, thandle)
        end if
      end if
    end if

  end subroutine set_table_double


  !> Put a default integer value into a table.
  subroutine set_table_integer(val, L, thandle, key, pos)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    integer, intent(in) :: val

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

    if (thandle > 0) then
      if (present(key)) then
        ! If there is a key, use that.
        ! First put the value on the top of the stack
        call flu_pushInteger(L, val)
        ! Now put it into the table
        call flu_setField(L, thandle, trim(key))
      else
        ! No key given, try to put the value by position
        if (present(pos)) then
          ! First put the index, where to write the value into the table, on the
          ! stack.
          call flu_pushInteger(L, pos)
          ! Now put the actual value on the top of the stack.
          call flu_pushInteger(L, val)
          ! Get the two entries from the stack into the table.
          call flu_setTable(L, thandle)
        end if
      end if
    end if

  end subroutine set_table_integer


  !> Put a long integer value into a table.
  subroutine set_table_long(val, L, thandle, key, pos)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to the table to look the value up in.
    integer, intent(in) :: thandle

    !> Value of the table entry if it exists.
    integer(kind=long_k), intent(in) :: val

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

    if (thandle > 0) then
      if (present(key)) then
        ! If there is a key, use that.
        ! First put the value on the top of the stack
        call flu_pushInteger(L, int(val))
        ! Now put it into the table
        call flu_setField(L, thandle, trim(key))
      else
        ! No key given, try to put the value by position
        if (present(pos)) then
          ! First put the index, where to write the value into the table, on the
          ! stack.
          call flu_pushInteger(L, pos)
          ! Now put the actual value on the top of the stack.
          call flu_pushInteger(L, int(val))
          ! Get the two entries from the stack into the table.
          call flu_setTable(L, thandle)
        end if
      end if
    end if

  end subroutine set_table_long


  !> This subroutine takes a one dimensional array, and puts it as a table
  !! into the Lua context.
  !!
  !! The returned thandle provides the index to access this newly created
  !! table.
  subroutine create_1Darray_real(L, thandle, val)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to access the newly created table.
    integer, intent(out) :: thandle

    !> Values to put into the new table.
    real(kind=single_k), intent(in) :: val(:)

    integer :: tab
    integer :: nvals
    integer :: i

    nVals = size(val)
    call flu_createtable(L, nVals, 0)
    thandle = flu_gettop(L)
    tab = thandle

    do i=1,nVals
      call flu_pushInteger(L, i)
      call flu_pushNumber(L, val(i))
      call flu_settable(L, tab)
    end do

  end subroutine create_1Darray_real


  !> This subroutine takes a one dimensional array, and puts it as a table
  !! into the Lua context.
  !!
  !! The returned thandle provides the index to access this newly created
  !! table.
  subroutine create_1Darray_double(L, thandle, val)
    type(flu_State) :: L !< Handle to the Lua script.

    !> Handle to access the newly created table.
    integer, intent(out) :: thandle

    !> Values to put into the new table.
    real(kind=double_k), intent(in) :: val(:)

    integer :: tab
    integer :: nvals
    integer :: i

    nVals = size(val)
    call flu_createtable(L, nVals, 0)
    thandle = flu_gettop(L)
    tab = thandle

    do i=1,nVals
      call flu_pushInteger(L, i)
      call flu_pushNumber(L, val(i))
      call flu_settable(L, tab)
    end do

  end subroutine create_1Darray_double

end module aot_table_module
