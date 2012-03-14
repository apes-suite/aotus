program aot_table_test
  use flu_binding, only: flu_State

  use aot_kinds_module, only: double_k, long_k
  use aotus_module, only: open_config, close_config, get_table_val, &
    &                     aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
  use aot_table_module, only: aot_table_open, aot_table_close, &
    &                         aot_table_length

  implicit none

  integer, parameter :: primes(5) = [ 1, 2, 3, 5, 7 ]

  type(flu_State) :: conf
  integer :: globhandle
  integer :: tabint
  integer :: tablen
  integer :: iError
  integer :: i

  call create_script('aot_table_test_config.lua')
  write(*,*)
  write(*,*) 'Running aot_table_test...'
  call open_config(conf = conf, filename = 'aot_table_test_config.lua')

  ! Testing for global Table
  write(*,*) ' * opening a global table'
  call aot_table_open(L = conf, thandle = globhandle, key = 'primes')
  if (globhandle == 0) then
    write(*,*) '  : unexpected FATAL Error occured !!!'
    write(*,*) '  : could not open global table primes.'
  else
    write(*,*) '  : success.'
    write(*,*) ' * getting the length of the table'
    tablen = aot_table_length(L = conf, thandle = globhandle)
    if (tablen /= 5) then
      write(*,*) '  : unexpected FATAL Error occured !!!'
      write(*,*) '  : found a table length of ', tablen
      write(*,*) '  :    but should have been ', 5
    else
      write(*,*) '  : success.'
      write(*,*) ' * retrieving entries of table by position'
      do i=1,5
        call get_table_val(conf = conf, thandle = globhandle, &
          &                pos = i, &
          &                tab_val = tabint, ErrCode = iError)
        if (btest(iError, aoterr_Fatal)) then
          write(*,*) '  : unexpected FATAL Error occured !!!'
          if (btest(iError, aoterr_NonExistent)) &
            &   write(*,*) '  : Variable not existent!'
          if (btest(iError, aoterr_WrongType)) &
            &   write(*,*) '  : Variable has wrong type!'
          exit
        else
          if (tabint /= primes(i)) then
            write(*,*) '  : unexpected ERROR, value mismatch, got: ', tabint
            write(*,*) '  :                             should be: ', primes(i)
            iError = 42
            exit
          end if
        end if
      end do
      if (iError == 0) write(*,*) '  : success.'
      write(*,*) ' * Attempting read of nonexistent entry'
      call get_table_val(conf = conf, thandle = globhandle, &
        &                pos = 10, &
        &                tab_val = tabint, ErrCode = iError)
      if (btest(iError, aoterr_Fatal)) then
        write(*,*) '  : success.'
      else
        write(*,*) '  : ERROR, unexpected success in reading nonexistent entry'
        if (btest(iError, aoterr_NonExistent)) &
          &   write(*,*) '  : Variable not existent, but should be fatal!'
        if (btest(iError, aoterr_WrongType)) &
          &   write(*,*) '  : Variable has wrong type but should not exist!'
      end if
    end if
  end if

  write(*,*) ' * Closing table'
  call aot_table_close(L = conf, thandle = globhandle)
  write(*,*) '  : success.'

  call close_config(conf)
  write(*,*) '... Done with aot_table_test.'

contains

  subroutine create_script(filename)
    character(len=*) :: filename

    open(file=trim(filename), unit=22, action='write', status='replace')
    write(22,*) '-- test script for aot_table_test'
    write(22,*) 'primes = { 1, 2, 3, 5, 7 }'
    write(22,*) 'tabtab = { origin = {0.0, 0.0, 0.0} }'
    write(22,*) 'unnamed = { { kind = 1 } }'
    close(22)
  end subroutine create_script

end program aot_table_test
