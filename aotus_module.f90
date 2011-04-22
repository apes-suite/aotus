module aotus_module
  use flu_binding

  implicit none

  integer, parameter :: aoterr_Fatal = 0
  integer, parameter :: aoterr_NonExistent = 1
  integer, parameter :: aoterr_WrongType = 2

  interface get_config_val
    module procedure get_config_real
  end interface

contains

  subroutine open_config(conf, filename)
    type(flu_State) :: conf
    character(len=*), intent(in) :: filename
    integer :: str_len

    conf = fluL_newstate()

    if (fluL_loadfile(conf, filename) .ne. 0) then
      write(*,*) "cannot load configuration file: ", flu_tolstring(conf, -1, str_len)
      STOP
    end if

    if (flu_pcall(conf, 0, 0, 0) .ne. 0) then
      write(*,*) "cannot run configuration file: ", flu_tolstring(conf, -1, str_len)
      STOP
    end if

  end subroutine open_config


  subroutine close_config(conf)
    type(flu_State) :: conf

    call flu_close(conf)

  end subroutine close_config


  subroutine get_config_real(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    real, intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    real, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        conf_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_real

end module aotus_module
