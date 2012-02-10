program aotus_test
  use aot_kinds_module, only: double_k
  use aotus_module
  use aot_table_module
  use aot_fun_module
  use aot_vector_module
  use aot_out_module

  implicit none

  real :: width
  type(flu_State) :: conf
  type(aot_fun_type) :: foo 
  type(aot_out_type) :: dummyOut
  integer :: iError
  integer :: vErr(3)
  integer :: stl_table
  integer :: stl_tab_len
  integer :: desc_table
  character(len=80) :: buffer
  character(len=20) :: keys(3)
  integer :: i, stl
  integer :: nSTLs
  real(kind=double_k) :: coord(3)
  real(kind=double_k) :: results

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

  keys(1) = 'filename'
  keys(2) = 'bctype'
  keys(3) = 'fileformat'

  call aot_table_open(L = conf, thandle = stl_table, key = 'stl_files')
  if (stl_table /= 0) then
    nSTLs = aot_table_length(L=conf, thandle=stl_table)
    write(*,*) 'There are ', nSTLs, ' entries'
    do stl=1,nSTLs
      write(*,*) 'Process entry in stl_table'
      call aot_table_open(L=conf, parent=stl_table, thandle=desc_table, &
        &                 pos=stl)
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
      call aot_table_close(L=conf, thandle=desc_table)
    end do
  end if
  call aot_table_close(L = conf, thandle = stl_table)

  !> Get a vector, describing the coordinate from the script
  call get_config_val(conf = conf, var = 'coord', conf_val = coord, &
    &                 ErrCode = vErr, &
    &                 default = [0.0_double_k, 0.0_double_k, 0.0_double_k])

  !> First open a function with aot_fun_open
  call aot_fun_open(L = conf, fun = foo, key = 'ic_density')
  
  !> Then put required parameters into it with
  !! aot_fun_put
  call aot_fun_put_double(L = conf, fun = foo, arg = coord(1))
  call aot_fun_put_double(L = conf, fun = foo, arg = coord(2))
  call aot_fun_put_double(L = conf, fun = foo, arg = coord(3))


  !> Execute the function with aot_fun_do
  call aot_fun_do(L = conf, fun = foo, nresults = 1) 

  !> Retrieve the possibly multiple results with
  !! get_top_val.
  call get_top_val(conf = conf, top_val = results , ErrCode = iError)

  write(*,"(a,3f5.2,a)") "result of ic_density at ",  coord, ':'
  write(*,"(EN16.7)") results



  !> Repeat putting and retrieving if needed.
  !! Close the function again with aot_fun_close.
  call aot_fun_close(L = conf, fun = foo) 

  call close_config(conf)

  call aot_out_open(put_conf = dummyOut, filename = 'dummy.lua')
  call aot_out_open_table(dummyOut, 'screen')
  call aot_out_val(dummyOut, 123, 'width')
  call aot_out_val(dummyOut, 456, 'height')

  call aot_out_open_table(dummyOut, 'origin')  
  call aot_out_val(dummyOut, 100)
  call aot_out_val(dummyOut, 0)
  call aot_out_close_table(dummyOut)
  call aot_out_close_table(dummyOut)

  call aot_out_val(dummyOut, (/0,1,2,3/), vname='testarray')


  call aot_out_close(dummyOut)

end program aotus_test
