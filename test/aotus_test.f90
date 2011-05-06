program aotus_test
  use aotus_module
  use aot_table_module

  implicit none

  real :: width
  type(flu_State) :: conf
  integer :: iError
  integer :: stl_table
  integer :: stl_tab_len
  integer :: desc_table
  character(len=80) :: buffer
  character(len=20) :: keys(3)
  integer :: i

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

  flush(6)

      keys(1) = 'filename'
      keys(2) = 'bctype'
      keys(3) = 'fileformat'

  call aot_table_open(L = conf, thandle = stl_table, key = 'stl_files')
  if (stl_table /= 0) then
    write(*,*) 'There are ', aot_table_length(L=conf, thandle=stl_table), ' entries'
    if (aot_table_first(conf, stl_table)) then
      do
        write(*,*) 'Process entry in stl_table'
        flush(6)
        desc_table = aot_table_top(conf)
        stl_tab_len = aot_table_length(L=conf, thandle=desc_table)
        do i=1,min(3, stl_tab_len)
          call get_table_val(conf = conf, thandle = desc_table, &
            &                tab_val = buffer, ErrCode = iError, &
            &                var = trim(keys(i)), pos = i)
          if (btest(iError, aoterr_Fatal)) then
            write(*,*) 'FATAL Error occured, while retrieving'//trim(keys(i))
            if (btest(iError, aoterr_NonExistent)) write(*,*) 'Variable not existent!'
            if (btest(iError, aoterr_WrongType)) write(*,*) 'Variable has wrong type!'
          else
            if (btest(iError, aoterr_NonExistent)) write(*,*) 'Variable not set in' &
              &                                            // ' config, Using default' &
              &                                            // '  value!'
          write(*,*) trim(keys(i))//' = ', trim(buffer)
        end if
        end do
        call flu_pop(conf)

        if (.not. flu_next(conf, stl_table)) exit
      end do
    end if
  end if
  call aot_table_close(L = conf, thandle = stl_table)

  call close_config(conf)

end program aotus_test
