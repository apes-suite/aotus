!> A module providing access to Lua functions
!!
!! Intended usage:
!! First open a function with aot_fun_open
!! Then put required parameters inot it with
!! aot_fun_put
!! Execute the function with aot_fun_do
!! Retrieve the possibly multiple results with
!! aot_top_get_val.
!! Repeat putting and retrieving if needed.
!! Close the function again with aot_fun_close.
module aot_fun_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k
  use aot_table_module, only: aot_table_push
  use aot_top_module, only: aot_err_handler

  implicit none

  private

  public :: aot_fun_type, aot_fun_open, aot_fun_close, aot_fun_put, aot_fun_do

  type aot_fun_type
    integer :: handle = 0
    integer :: arg_count = 0
  end type

  !> Open a Lua function for evaluation.
  !!
  !! After it is opened, arguments might be
  !! put into the function, and it might be
  !! executed. This might be repeated for
  !! an arbitrary number to retrieve more than
  !! one evaluation of a single function, before
  !! closing it again with aot_fun_close.
  interface aot_fun_open
    module procedure aot_fun_global
    module procedure aot_fun_table
  end interface aot_fun_open

  !> Put an argument into the lua function.
  !!
  !! Arguments have to be in order, first put
  !! the first argument then the second and so on.
  interface aot_fun_put
    module procedure aot_fun_put_double
  end interface aot_fun_put

contains

  !> Return the stack of the top as a function.
  !!
  !! If it actually is not a Lua function, its handle
  !! will be set to 0.
  function aot_fun_top(L) result(fun)
    type(flu_state) :: L
    type(aot_fun_type) :: fun

    fun%handle = 0
    fun%arg_count = 0
    if (flu_isFunction(L, -1)) then
      fun%handle = flu_gettop(L)
      call flu_pushvalue(L, -1)
    end if
  end function aot_fun_top

  !> Get a globally defined function.
  subroutine aot_fun_global(L, fun, key)
    type(flu_state) :: L
    type(aot_fun_type), intent(out) :: fun
    character(len=*), intent(in) :: key

    call flu_getglobal(L, key)
    fun = aot_fun_top(L)
  end subroutine aot_fun_global

  !> Get a function defined as component of a table.
  subroutine aot_fun_table(L, parent, fun, key, pos)
    type(flu_state) :: L
    integer, intent(in) :: parent
    type(aot_fun_type), intent(out) :: fun
    character(len=*), intent(in), optional :: key
    integer, intent(in), optional :: pos

    call aot_table_push(L, parent, key, pos)
    fun = aot_fun_top(L)
  end subroutine aot_fun_table

  !> Close the function again (pop everything
  !! above from the stack).
  subroutine aot_fun_close(L, fun)
    type(flu_state) :: L
    type(aot_fun_type) :: fun

    if (fun%handle > 0) call flu_settop(L, fun%handle-1)
    fun%handle = 0
    fun%arg_count = 0
  end subroutine aot_fun_close

  !> Put an argument of type double into the list
  !! of arguments for the function.
  subroutine aot_fun_put_double(L, fun, arg)
    type(flu_state) :: L
    type(aot_fun_type) :: fun
    real(kind=double_k), intent(in) :: arg

    if (fun%handle /= 0) then
      if (fun%arg_count == -1) then
        call flu_settop(L, fun%handle)
        call flu_pushvalue(L, fun%handle)
        fun%arg_count = fun%arg_count+1
      end if
      call flu_pushNumber(L, arg)
      fun%arg_count = fun%arg_count+1
    end if
  end subroutine aot_fun_put_double

  !> Execute a given function and put its results
  !! on the stack, where it is retrievable with
  !! aot_top_get_val.
  subroutine aot_fun_do(L, fun, nresults, ErrCode, ErrString)
    type(flu_state) :: L
    type(aot_fun_type) :: fun
    integer, intent(in) :: nresults
    integer, intent(out), optional :: ErrCode
    character(len=*), intent(out), optional :: ErrString

    integer :: err

    if (fun%handle /= 0) then
      err = flu_pcall(L, fun%arg_count, nresults, 0)
      call aot_err_handler(L=L, err=err, msg="Failed aot_fun_do! ", &
        &                  ErrCode = ErrCode, ErrString = ErrString)
      fun%arg_count = -1
    end if
  end subroutine aot_fun_do

end module aot_fun_module
