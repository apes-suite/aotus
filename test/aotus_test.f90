program aotus_test
  use aotus_module
  use aot_table_module

  implicit none

  real :: width
  type(flu_State) :: conf
  integer :: iError, strlen
  integer :: stl_table
  integer :: desc_table
  character, pointer :: cstring(:)
  character(len=80) :: buffer
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

  stl_table = aot_table_global(L = conf, table_name = 'stl_files')
  if (stl_table /= 0) then
    if (aot_table_first(L=conf, thandle=stl_table)) then
      do
        write(*,*) 'Process entry in stl_table'
        flush(6)
        desc_table = aot_table_top(conf)
        if (aot_table_first(L=conf, thandle=desc_table)) then
          do
            cstring => flu_toLString(L=conf, index=-1, len=strLen)
            buffer = ''
            do i=1,strLen
              buffer(i:i) = cstring(i)
            end do
            call flu_pop(conf)
            write(*,*) trim(buffer)
            if (.not. flu_next(conf, desc_table)) exit
          end do
        end if
        call flu_pop(conf)
        if (.not. flu_next(conf, stl_table)) exit
      end do
    end if
    call flu_pop(conf)
  end if

  call close_config(conf)

end program aotus_test
