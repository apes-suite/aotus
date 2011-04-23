!> This module provides some convenient functions to act
!! on lua tables.
module aot_table_module
  use flu_binding

  implicit none

contains

  !> Return the position at the top of the stack as a
  !! table handle for further operations on that table,
  !! if it actually exists and is a table. Otherwise
  !! 0 will be returned.
  function aot_table_top(L) result(thandle)
    type(flu_state) :: L
    integer :: thandle

    if (flu_isNoneOrNil(L, -1) .or. (.not. flu_isTable(L, -1))) then
      thandle = 0
    else
      thandle = flu_gettop(L)
    end if
  end function aot_table_top

  !> Load a globally defined table into the top of the stack
  !! and return its position in the stack as a handle for this
  !! table. If it does not exist or the global variable is not
  !! a table, the handle will be set to 0.
  function aot_table_global(L, table_name) result(thandle)
    type(flu_state) :: L
    character(len=*), intent(in) :: table_name
    integer :: thandle

    call flu_getglobal(L, table_name)

    thandle = aot_table_top(L)
  end function aot_table_global

  !> Load the first key-value pair of table thandle on the
  !! stack. This serves as an entry point, further traversal
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

end module aot_table_module
