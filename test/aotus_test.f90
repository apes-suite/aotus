program aotus_test
  use aotus_module

  implicit none

  real :: width
  type(flu_State) :: conf
  integer :: iError

  call open_config(conf = conf, filename = 'config.lua')

  call get_config_val(conf = conf, var = 'width', &
    &                 conf_val = width, ErrCode = iError)

  if (btest(iError, aoterr_Fatal)) then
    write(*,*) 'FATAL Error occured, while retrieving width:'
    if (btest(iError, aoterr_NonExistent)) write(*,*) 'Variable not existent!'
    if (btest(iError, aoterr_WrongType)) write(*,*) 'Variable has wrong type!'
  else
    write(*,*) 'width =', width
  end if

  call get_config_val(conf = conf, var = 'height', &
    &                 conf_val = width, ErrCode = iError, &
    &                 default = 100.0)

  if (btest(iError, aoterr_Fatal)) then
    write(*,*) 'FATAL Error occured, while retrieving height:'
    if (btest(iError, aoterr_NonExistent)) write(*,*) 'Variable not existent!'
    if (btest(iError, aoterr_WrongType)) write(*,*) 'Variable has wrong type!'
  else
    if (btest(iError, aoterr_NonExistent)) write(*,*) 'Variable not set in config,' &
      &                                            // ' Using default value!'
    write(*,*) 'height =', width
  end if

  call close_config(conf)

end program aotus_test
