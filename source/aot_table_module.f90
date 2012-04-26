!> This module provides some convenient functions to act
!! on lua tables.
module aot_table_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val

  implicit none

  private

  public :: aot_table_top, aot_table_length, aot_table_first, aot_table_push
  public :: aot_table_open, aot_table_close, aot_table_get_val

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
  !! First the given !! key is looked up, if this fails, the value
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


contains

  !> Return the position at the top of the stack as a
  !! table handle.
  !!
  !! If it actually exists and is a table, this handle can be used
  !! for further operations on that table.
  !! Otherwise a 0 will be returned.
  function aot_table_top(L) result(thandle)
    type(flu_state) :: L
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
    type(flu_state) :: L
    integer, intent(out) :: thandle
    character(len=*), intent(in) :: key

    call flu_getglobal(L, key)

    thandle = aot_table_top(L)
  end subroutine aot_table_global


  !> This subroutine tries to get a table in a table, and
  !! return a handle for it.
  subroutine aot_table_table(L, parent, thandle, key, pos)
    type(flu_state) :: L
    integer, intent(in) :: parent
    integer, intent(out) :: thandle
    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos

    call aot_table_push(L, parent, key, pos)
    thandle = aot_table_top(L)
  end subroutine aot_table_table


  !> Close a table again.
  !!
  !! This is done by popping all values above and itself from the stack.
  subroutine aot_table_close(L, thandle)
    type(flu_state) :: L
    integer, intent(in) :: thandle

    if (thandle > 0) call flu_settop(L, thandle-1)
  end subroutine aot_table_close


  !> This subroutine tries to push the value of table thandle on
  !! the lua stack, or if this fails, the entry at position pos
  !! of the table. If no corresponding value is found, a nil
  !! value is pushed to the stack.
  subroutine aot_table_push(L, thandle, key, pos)
    type(flu_state) :: L
    integer :: thandle
    character(len=*), intent(in), optional :: key
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
    type(flu_state) :: L
    integer, intent(in) :: thandle
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
    type(flu_state) :: L
    integer, intent(in) :: thandle
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


  subroutine get_table_real(L, thandle, val, ErrCode, key, pos, default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    real(kind=single_k), intent(out) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    real(kind=single_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_real


  subroutine get_table_double(L, thandle, val, ErrCode, key, pos, &
    &                         default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    real(kind=double_k), intent(out) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    real(kind=double_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_double


  subroutine get_table_integer(L, thandle, val, ErrCode, key, pos, &
    &                          default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    integer, intent(out) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    integer, intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_integer

  subroutine get_table_long(L, thandle, val, ErrCode, key, pos, default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    integer(kind=long_k), intent(out) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    integer(kind=long_k), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_long

  subroutine get_table_logical(L, thandle, val, ErrCode, key, pos, &
    &                          default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    logical, intent(out) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    logical, intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_logical


  subroutine get_table_string(L, thandle, val, ErrCode, key, pos, &
    &                         default)
    type(flu_State) :: L
    integer, intent(in) :: thandle
    character(len=*) :: val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: default

    call aot_table_push(L=L, thandle=thandle, &
      &                   key=key, pos=pos)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_table_string


end module aot_table_module
