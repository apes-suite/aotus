!> Global definitions of some handy kind declarations
!! with the help of the intrinsic selected_*_kind
!! functions.
module aot_kinds_module
  implicit none

  integer, parameter :: double_k = selected_real_kind(15)
  integer, parameter :: single_k = selected_real_kind(6)
  integer, parameter :: long_k = selected_int_kind(15)

end module aot_kinds_module
