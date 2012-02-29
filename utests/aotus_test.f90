program aotus_test
  use flu_binding, only: flu_State

  use aot_kinds_module, only: double_k, long_k
  use aotus_module, only: open_config, close_config, get_config_val, &
    &                     aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType

  implicit none

  type(flu_State) :: conf
  integer :: iError
  integer :: glob_int
  integer(kind=long_k) :: glob_long

  call create_script('aotus_test_config.lua')
  write(*,*)
  write(*,*) 'Running aotus_test...'
  write(*,*) ' * open_config (aotus_test_config.lua)'
  call open_config(conf = conf, filename = 'aotus_test_config.lua')
  write(*,*) '  : success.'

  ! Testing for global INTEGER
  write(*,*) ' * reading a global integer'
  call get_config_val(conf = conf, var = 'int_test', &
    &                 conf_val = glob_int, ErrCode = iError)

  if (btest(iError, aoterr_Fatal)) then
    write(*,*) '  : unexpected FATAL Error occured !!!'
    if (btest(iError, aoterr_NonExistent)) &
      &   write(*,*) '  : Variable not existent!'
    if (btest(iError, aoterr_WrongType)) &
      &   write(*,*) '  : Variable has wrong type!'
  else
    if (glob_int == 5) then
      write(*,*) '  : success.'
    else
      write(*,*) '  : unexpected ERROR, value mismatch, got: ', glob_int
      write(*,*) '  :                             should be: ', 5
    end if
  end if
  ! -------------------------------- !


  ! Testing for global LONG
  write(*,*) ' * reading a global long'
  call get_config_val(conf = conf, var = 'long_test', &
    &                 conf_val = glob_long, ErrCode = iError)

  if (btest(iError, aoterr_Fatal)) then
    write(*,*) '  : unexpected FATAL Error occured !!!'
    if (btest(iError, aoterr_NonExistent)) &
      &   write(*,*) '  : Variable not existent!'
    if (btest(iError, aoterr_WrongType)) &
      &   write(*,*) '  : Variable has wrong type!'
  else
    if (glob_long == 5000000000_long_k) then
      write(*,*) '  : success.'
    else
      write(*,*) '  : unexpected ERROR, value mismatch, got: ', glob_long
      write(*,*) '  :                             should be: ', &
        &                                         5000000000_long_k
    end if
  end if
  ! -------------------------------- !

  write(*,*) ' * close_conf'
  call close_config(conf)
  write(*,*) '  : success.'
  write(*,*) '... Done with aotus_test.'

contains

  subroutine create_script(filename)
    character(len=*) :: filename

    open(file=trim(filename), unit=22, action='write', status='replace')
    write(22,*) '-- test script for aotus_test'
    write(22,*) 'int_test = 5'
    write(22,*) 'long_test = 5000000000'
    write(22,*) 'real_test = 0.5'
    write(22,*) 'log_test = true'
    write(22,*) "string_test = 'last words'"
    close(22)
  end subroutine create_script

end program aotus_test
