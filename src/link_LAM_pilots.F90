!/* 
! * Copyright (C) 2017-2019  UQAM centre ESCER
! *
! * This software is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This software is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this software; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
module file_index
  use ISO_C_BINDING
  implicit none

  interface
    subroutine f_exit(code) BIND(C,name='exit')
      import :: C_INT
      integer(C_INT), intent(IN), value :: code
    end subroutine f_exit
    function f_mkdir(path,mode) result(status) bind(C,name='mkdir')   ! interface to libc mkdir
      import :: C_CHAR, C_INT
      character(C_CHAR), dimension(*), intent(IN) :: path
      integer(C_INT), intent(IN), value :: mode
      integer(C_INT) :: status
    end function f_mkdir
    function f_unlink(path) result(status) bind(C,name='unlink')   ! interface to libc unlink (rm)
      import :: C_CHAR, C_INT
      character(C_CHAR), dimension(*), intent(IN) :: path
      integer(C_INT) :: status
    end function f_unlink
    function f_link(oldpath,newpath) result(status) bind(C,name='link')   ! interface to libc link (hard link)
      import :: C_CHAR, C_INT
      character(C_CHAR), dimension(*), intent(IN) :: oldpath, newpath
      integer(C_INT) :: status
    end function f_link
    function f_symlink(oldpath,newpath) result(status) bind(C,name='symlink')   ! interface to libc symlink (soft link)
      import :: C_CHAR, C_INT
      character(C_CHAR), dimension(*), intent(IN) :: oldpath, newpath
      integer(C_INT) :: status
    end function f_symlink
  end interface
  integer, parameter :: PAGESIZE = 128
  type :: indexpage
    type(indexpage), pointer :: next
    integer(KIND=8), dimension(0:PAGESIZE-1) :: date
    character(len=128), dimension(0:PAGESIZE-1) :: name
  end type
  save
  type(indexpage), pointer :: dtbl=>null()
  type(indexpage), pointer :: curp=>null()   ! current page
  character(len=4096) :: indexname = ' '
  integer :: ntbl = 0
  integer :: curi = 0
  contains
    subroutine find_file(date1, date2, name) ! find name of file where YYYYMMDD:hhmmss00 (date1:date2) will be found
      integer, intent(IN) :: date1, date2
      character(len=*), intent(OUT) :: name
      type(indexpage), pointer :: p
      integer(KIND=8) :: date
      integer :: i, j

      date = date1
      date = ishft(date,32) + date2
      name = ""                       ! blank name returned if search fails
      p => curp
      do i = curi, ntbl-1
        j = mod(i,PAGESIZE)           ! index in page
        if(p%date(j) == date) then    ! date matches 
          name = p%name(j)
          write(0,*)'INFO: from',curi,' to',j
          curi = j
          exit                        ! done, return name to caller
        endif
        if(j == PAGESIZE-1 .and. associated(p%next)) then
          p => p%next
          curp =>p
        endif
      enddo
      if(name == ' ') then
        write(0,1)'ERROR: time frame ',date1,':',date2,' NOT FOUND, ABORTING'
1       format(A,I8,A1,I8.8,A)
        call f_exit(1)
      endif
    end subroutine find_file

    subroutine build_index(indexfile)     ! from indexfile, build index after deallocating precious index if it exists
      character(len=*), intent(IN) :: indexfile
      integer :: date1, date2
      character(len=128) :: name
      type(indexpage), pointer :: p, t
      integer :: iun, status, ix
      integer, external :: fnom, fclos

      if( trim(indexfile) == trim(indexname) ) then
        write(0,*) 'INFO: reusing index file '//trim(indexname)
        return
      else
        write(0,*) 'INFO: using index file '//trim(indexfile)
      endif
      p => dtbl
      do while(associated(p))  ! deallocate current index pages if they exist
        t => p%next            ! pointer to next page
        deallocate(p)
        p => t
      enddo
      allocate(dtbl)           ! allocate first page
      ntbl = 0                 ! table is empty
      curi = 0
      p => dtbl
      curp => dtbl
      p%next => null()
      iun = 0
      status = fnom(iun,trim(indexfile),'FTN+SEQ+FMT',0)  ! open index file
      do while(.true.)
        read(iun,*,end=1) date1, date2, name  ! read index file to the end
!         write(0,*) date1,date2
        ix = mod(ntbl,PAGESIZE)
        p%date(ix) = date1
        p%date(ix) = ishft(p%date(ix),32) + date2
        p%name(ix) = name
        ntbl = ntbl + 1
        if(ix == PAGESIZE-1)then  ! current page is full, allocate a new page
          allocate(p%next)
          p=>p%next
        endif
      enddo
1     status = fclos(iun)
      indexname = indexfile
    end subroutine build_index
end module file_index

program print_date_range
  use ISO_C_BINDING
  use file_index
  implicit none
#include "clib_interface.cdk"
#define CLIB_OK 1
  integer, external :: newdate
  integer :: stamp, p1, p2, stamp1, stamp2, p3, p4, stamp3
  integer :: dt1, dt2
  integer, dimension(2) :: printable1, printable2, printable3
  real *8 :: delta, diff
  integer :: status
  character(len=128) :: date1, date2, interval, name, sym
  character(len=32) :: arg1, arg2, key
  character(len=4096) :: oldpath, newpath, dirpath, option, oldmonth, month_name, val, indexfile, targetname
  character(len=1024) :: set_pattern
  character(C_CHAR), dimension(4096) :: oldp, newp, dirp
  character(len=4096) :: nest_rept, set_name, anal, statusfile, name_to_index
  integer(C_INT) :: mode
  logical :: use_anal, first_in_month, sub_daily, indexmode
  integer :: cur_arg, nargs, arg_len, ntimes
  integer :: month_is_file = 0
  integer :: index_file_found = 0
  character(len=128) :: version = 'version 1.0.11 2019/04/24'
  integer, parameter :: MAXGLOB=2
  character(len=4096), dimension(MAXGLOB) :: globs
  integer :: nglob, arg2_nc, errors, iun, keyrec, ni,nj,nk, datev
  integer :: dateo,deet,npas,nbits,datyp,ip1,ip2,ip3,ig1,ig2,ig3,ig4
  integer :: swa,lng,dltf,ubc,extra1,extra2,extra3
  real*8 :: hours, file_span
  character(len=1) :: grtyp
  character(len=2) :: typvar
  character(len=4) :: nomvar
  character(len=12) :: etiket
  character(len=16) :: template
  integer, external :: fnom, fstouv, fstinf, fstsui

  CALL get_command_argument(0, name)   ! program name as seen by OS
  statusfile = '/dev/null'

  mode = o'0777'  ! to be "anded" with user's umask for mkdir
  oldmonth = ' '
  set_pattern = '*'  ! default filename pattern for set name "globbing"

  errors = 0
  iun = 0
  cur_arg = 1
  nargs = command_argument_count()
  if(nargs == 0) goto 777
  printable1 = -1
  printable2 = -1
  printable3 = -1
  delta = -1
  anal = ''
  set_name = ''
  nest_rept = ''
  arg2_nc = 8
  first_in_month = .true.
  sub_daily = .false.
  indexmode = .false.
  template = 'YYYYMM????????'
  file_span = 24.00   ! default is daily files if not monthly

  do while(cur_arg <= nargs)      ! process command line options
    call get_command_argument(cur_arg,option,arg_len,status)
    call optkey(key,option)
    call optval(val,option)
    if(trim(key) == '--start_date=' ) then       ! YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS
      date1 = trim(val)//'000000'
      if(date1(9:9) == '.') then
        read(date1,11,err=777)printable1(1),printable1(2)  ! start date in YYYYMMDD.HHMMSS format
      else
        read(date1,12,err=777)printable1(1),printable1(2)  ! start date in YYYYMMDDHHMMSS format
      endif
    else if(trim(key) == '--end_date=' ) then         ! YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS
      date2 = trim(val)//'000000'
      if(date2(9:9) == '.') then
        read(date2,11,err=777)printable2(1),printable2(2)  ! end date in YYYYMMDD.HHMMSS format
      else
        read(date2,12,err=777)printable2(1),printable2(2)  ! end date in YYYYMMDDHHMMSS format
      endif
    else if(trim(key)  == '--nhours=' ) then           ! hours
      interval = val
      read(interval,*,err=777)delta                      ! interval in hours
    else if(trim(key) == '--nseconds=' ) then         ! seconds
      interval = val
      read(interval,*,err=777)delta                      ! interval in seconds
      delta = delta/3600.0                               ! interval in hours
    else if(trim(key) == '--start_sym=' ) then        ! YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS
      sym = trim(val)//'000000'
      if(sym(9:9) == '.') then
        read(sym,11,err=777)printable3(1),printable3(2)  ! start of simulation in YYYYMMDD.HHMMSS format
      else
        read(sym,12,err=777)printable3(1),printable3(2)  ! start of simulation in YYYYMMDDHHMMSS format
      endif
    else if(trim(key) == '--sub_daily' ) then        ! sub daily time resolution for driving data files
      sub_daily = .true.
      file_span = 1.0                                ! default for sub daily is hourly
      if(val(1:1) .ne. ' ') then
        if(val(1:1) == 'm' .or. val(1:1) == 'M') file_span = file_span / 60.0_8     ! minutely files
        if(val(1:1) == 's' .or. val(1:1) == 'S') file_span = file_span / 3600.0_8   ! secondly files
      endif
    else if(trim(key) == '--start_anal=' ) then       ! initial analysis (only necessary if start_sym == start_date)
      anal = val
    else if(trim(key) == '--index=' ) then       ! filename to index (look for P0)
      name_to_index = val
      indexmode = .true.
    else if(trim(key) == '--status=' ) then       ! initial analysis (only necessary if start_sym == start_date)
      statusfile = val
    else if(trim(key) == '--pilot_data=' ) then       ! directory for boundary conditions
      nest_rept = val
    else if(trim(key) == '--set_name=' ) then         ! experiment name
      set_name = val
    else if(trim(key) == '--set_pattern=' ) then      ! disambiguation pattern for file "globbing", default is '*'
      set_pattern = val
    else if(trim(key)  == '--year=' ) then             ! calendar option (optional)
      call NewDate_Options(trim(option(3:4096)),'set')       ! set calendar option
      write(0,*)'INFO: using calendar option '//trim(option(3:4096))
    else if(trim(key)  == '--version' ) then              ! version option
      write(0,*)version
      stop
    else if(trim(key)  == '--help' ) then              ! help option
      goto 777
    else if(trim(key)  == '-h' ) then                  ! help option
      goto 777
    else 
       write(0,*)"ERROR: unrecognized argument '"//trim(option)//"'"
      if( trim(statusfile) .ne. '/dev/null' ) call set_status(statusfile,'status="ABORT"')
      errors = errors + 1
    endif
    cur_arg = cur_arg + 1
  enddo
  if(errors > 0) then
    write(0,*)errors,' errors in argument parsing'
    goto 777
  endif
  if( trim(statusfile) .ne. '/dev/null' ) call set_status(statusfile,'status="ABORT"')
  if(indexmode) then  ! file indexing mode
    status = fnom(iun,trim(name_to_index),'STD+RND+OLD+R/O',0)   ! connect file)
    if(status < 0) goto 555
    status = fstouv(iun,'RND')
    if(status < 0) goto 555
    keyrec = fstinf(iun,ni,nj,nk,-1,"",-1,-1,-1,"","TT")  ! first TT record
    call fstprm(keyrec,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
                typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
                swa,lng,dltf,ubc,extra1,extra2,extra3)
    keyrec = fstinf(iun,ni,nj,nk,-1,"",ip1,-1,-1,"","TT")  ! first TT record with ip1 forced
    do while(keyrec >= 0)
      call fstprm(keyrec,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3, &
                  typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4, &
                  swa,lng,dltf,ubc,extra1,extra2,extra3)
      hours = deet
      hours = hours * npas / 3600.0_8
      call incdatr(datev,dateo,hours)
      status =  newdate(datev,printable1(1),printable1(2),-3)
      write(6,33)printable1(1)," ,",printable1(2)," ,'"//trim(name_to_index)//"'"
33    format(I8.8,A2,I8.8,A)
      keyrec = fstsui(iun,ni,nj,nk)        ! find subsequent matches if any
    enddo
    call fstfrm(iun)
    goto 666      ! set status to success
555  write(0,*)"ERROR: while opening file '"//trim(name_to_index)//"'"
    call f_exit(1)
  endif
  use_anal = (printable3(1) == printable1(1)) .and. (printable3(2) == printable1(2))
  if(printable1(1) == -1 .or. printable2(1) == -1) then
    write(0,*)'ERROR: missing start/end date(s)'
    goto 777
  endif
  if(printable1(1) == -1 .or. printable1(1) == -1) then
    write(0,*)'ERROR: bad start date'
    goto 777
  endif
  if(printable2(1) == -1 .or. printable2(1) == -1) then
    write(0,*)'ERROR: bad end date'
    goto 777
  endif
  if(use_anal .and. trim(anal) == '' ) then
     write(0,*)'ERROR: initial conditions missing'
    goto 777
  endif
  if(trim(nest_rept) == '' ) then
    write(0,*)'ERROR: boundary conditions directory missing'
    goto 777
  endif
  if(trim(set_name) == '' ) then
    write(0,*)'ERROR: dataset name missing'
    goto 777
  endif
  write(0,*)'INFO: from ',trim(date1),' to ',trim(date2),' every',delta,' hours'
  write(0,*)"INFO: boundary conditions data in '"//trim(nest_rept)//"'"
  if(use_anal) write(0,*)"INFO: using initial conditions file '"//trim(anal)//"'"

  open(unit=11,file='content',form='FORMATTED')
  write(11,'(A/A)')'1','GEM_input_file_0001'   ! there will be 1 file in the directory and it will be called GEM_input_file_0001
  close(unit=11)

  status = newdate(stamp1,printable1(1),printable1(2)*100,3) ! stamp for start date
  status = newdate(stamp2,printable2(1),printable2(2)*100,3) ! stamp for end date
  call difdatr(stamp2,stamp1,diff)                           ! end - start in hours

  ntimes = 0
  do while(diff >= 0)                                 ! end date - next date
    status = newdate(stamp1,p1,p2,-3)                 ! convert to printable
    status = newdate(stamp1,dt1,dt2,-3)
    p3 = p1                                           ! YYYYMMDD
    if(p2 == 0 .and. (.not. use_anal) .and. (.not. sub_daily)) then   ! hhmmss = 0, use previous day, except if use_anal or sub_daily
      call incdatr(stamp3,stamp1,-file_span)          ! subtract file contents interval
      status = newdate(stamp3,p3,p4,-3)
    endif
!     print 102,p1,'.',p2/100,p3                        ! print itname
    write(arg1,'(I8.8,A1,I6.6)')p1,'.',p2/100          ! YYYYMMDD.hhmmss
    write(arg2,'(2I8.8)')p3,p2                         ! YYYYMMDDhhmmss00
!     print *,trim(arg1),' ',trim(arg2)
    write(dirpath,'(A)')'VALID_'//trim(arg1)           ! VALID_YYYYMMDD.hhmmss
    dirp = transfer(trim(dirpath)//achar(0),dirp)      ! C null terminated string from Fortran string
    status = f_mkdir( dirp, mode )                     ! directory containing boundary files for this time interval

    oldpath = 'content'
    oldp = transfer(trim(oldpath)//achar(0),oldp)      ! C null terminated string from Fortran string
    newpath = trim(dirpath)//'/content'                ! content file in directory created above
    newp = transfer(trim(newpath)//achar(0),newp)      ! C null terminated string from Fortran string
    status = f_unlink( newp )                          ! in case file/link named ....../content already exists
    status = f_link( oldp, newp )                      ! hard link to file named 'content' in upper directory

    month_name = trim(nest_rept) // '/' // trim(set_name) // '_' // arg2(1:6)   ! month part can be a file or a directory
    if(trim(oldmonth) .ne. month_name) then      ! new month name
      oldmonth = month_name
      first_in_month = .true.
      month_is_file = clib_isfile( month_name )  ! it is a file name
!       write(0,*)month_name,month_is_file
      if(month_is_file == 1) then
        write(0,*)'INFO: using monthly boundary file '//trim(oldmonth)
      else
        if(clib_isdir( month_name ) .ne. 1) then ! is it a directory name
          write(0,*)'ERROR: '//trim(oldmonth)//' is neither a directory nor a file, ABORTING'
          stop
        endif
        indexfile = trim(month_name)//'/index_file'
        index_file_found = clib_isfile( indexfile )
        if(1 == index_file_found ) then ! it is a file name) 
!           write(0,*) 'INFO: index file found'
          call build_index(indexfile)
        endif
!         write(0,*)'INFO: using monthly boundary files directory '//trim(oldmonth)
      endif
    endif

    if(use_anal) then        ! use initial conditions file instead of boundary conditions file
      oldpath = trim(anal)
    else
      if(month_is_file == 1) then   ! monthly boundary contitions file, may get linked to multiple times
        oldpath = month_name
      else    ! same month, another day
        if(1 == index_file_found ) then                                        ! WITH INDEX FILE
          call find_file(dt1,dt2,targetname)
          oldpath = trim(month_name) // '/'// trim(targetname)
          write(0,*)'INFO: target is',dt1,dt2," '" // trim(oldpath) // "'"
        else                                                                   ! NO INDEX FILE FOUND
          if(first_in_month) then  ! see if name ends in YYYYMMDDhh, if so use 10 chars from arg2
            arg2_nc = 8
            if(sub_daily) arg2_nc = 10    ! sub_daily mode, hh MUST be present
            do while(arg2_nc <= 14)
              oldpath = trim(month_name) // '/' // trim(set_pattern) // arg2(1:arg2_nc)   ! look for 'pattern'YYYYMMDD[hh][mm][ss] file name
  !              write(0,*)'DEBUG: trying ' // trim(oldpath)
              status = clib_glob(globs,nglob,trim(oldpath),MAXGLOB)            ! find file name match(es)
              if(status == CLIB_OK .and. nglob == 1) exit                      ! found unique match , exit loop
              arg2_nc = arg2_nc + 2                                            ! try longer match
              if( .not. sub_daily) then
                write(0,*)'ERROR: --sub_daily flag is absent and no daily file has been found'
                write(0,*)'      '//trim(oldpath)
                stop
              endif
            enddo
            write(0,*)'INFO: using boundary files pattern '//trim(oldmonth)//'/'//trim(set_pattern)//arg2(1:6)//template(7:arg2_nc)
            if(arg2_nc > 14) then  ! OOPS
              write(0,*)'ERROR: no file was found matching ' // trim(oldpath)
              write(0,*)'       date = '//arg2(1:4)//'/'//arg2(5:6)//'/'//arg2(7:8)//'-'//arg2(9:10)//':'//arg2(11:12)//':'//arg2(13:14)
              stop
            endif
          endif
          oldpath = trim(month_name) // '/' // trim(set_pattern) // arg2(1:arg2_nc) ! look for 'pattern'YYYYMMDD[hh[mmdd]]  ( default pattern is * )
          globs(1) = 'UnknownFile'
  !         write(0,*)'INFO: looking for '//trim(oldpath)
          status = clib_glob(globs,nglob,trim(oldpath),MAXGLOB)            ! find file name match(es)
          if(status .ne. CLIB_OK .or. nglob > 1) then                      ! there must be one and only one match
            write(0,*)'ERROR: '//trim(oldpath)//' is ambiguous or does not exist'
            stop
          endif
          oldpath = globs(1)     ! use file name that matches pattern
        endif                                                                  ! WITH INDEX FILE
      endif
    endif

    oldp = transfer(trim(oldpath)//achar(0),oldp)
    newpath = 'VALID_' // trim(arg1) // '/GEM_input_file_0001'
    newp = transfer(trim(newpath)//achar(0),newp)     ! C null terminated string from Fortran string
    status = f_unlink( newp )                         ! in case ....../GEM_input_file_0001 already exists
    status = f_symlink( oldp, newp )                  ! soft link to initial/boundary conditions file

    call incdatr(stamp,stamp1,delta)                  ! increment next date
    stamp1 = stamp
    call difdatr(stamp2,stamp1,diff)                  ! end date - next date

    first_in_month = .false.
    if(use_anal) first_in_month = .true.
    use_anal = .false.                                ! use_anal can only be true for the first time frame
    ntimes = ntimes + 1                               ! counter for time frames
  enddo
  write(0,*)"INFO: ",ntimes," directory/link sets created"
666 continue
  if( trim(statusfile) .ne. '/dev/null' ) call set_status(statusfile,'status="SUCCESS"')
  call f_exit(0)
  stop
11  format(I8,1x,I6)
12  format(I8,I6)
777 continue
  write(0,*)'USAGE: '//trim(name)//' [-h|--help] --start_date= --end_date= --nhours= --nseconds= --set_name= \'
  write(0,*)'        [--start_sym=] [--status=statusfile] [--sub-daily] [--start_anal=] --pilot_data= \'
  write(0,*)'        [--set_pattern] [--year=gregorian|360_day|365_day] [--index=] [--version]'
  write(0,*)''
  write(0,*)'       '//version
  write(0,*)''
  write(0,*)'       statusfile : path to status file that will contain(status="SUCCESS" or status="ABORT")'
  write(0,*)'       start_date : YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS , start of this simulation slice'
  write(0,*)'       end_date   : YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS , end of this simulation slice'
  write(0,*)'       nseconds   : interval in seconds between boundary condition files'
  write(0,*)'       nhours     : interval in hours between boundary condition files'
  write(0,*)'       start_sym  : YYYYMMDD.HHMMSS or YYYYMMDDHHMMSS , start of entire simulation'
  write(0,*)'       start_anal : initial conditions (only used if start_date == start_sym)'
  write(0,*)'       pilot_data : directory containing the boundary condition files'
  write(0,*)'       set_name   : dataset/experiment name'
  write(0,*)'       set_pattern: disambiguation pattern for file "globbing"'
  write(0,*)'       year=...   : (optional) argument ,  calendar to be used (gregorian by default)'
  write(0,*)'       sub_daily  : driving data files contain less than a day of data (hh/hhmm/hhmmss in file names)'
  write(0,*)'       index      : filename to  index (looking for record P0)'
  write(0,*)'       version    : print version and quit'
  write(0,*)'       arguments between [] are optional'
  write(0,*)'       only one of --nhours/--nseconds is necessary'
  write(0,*)'       for date parameters, the trailing 0s in the HHMMSS part may be omitted'
  call f_exit(1)
  stop
end program
subroutine set_status(filename,message)
  implicit none
  character (len=*), intent(IN) :: filename,message
  integer :: iun, status
  integer, external :: fnom

  iun = 0
  status = fnom(iun,trim(filename),'FTN+FMT',0)
  if(iun > 0) then
    write(iun,1) trim(message)
1     format(A)
    call fclos(iun)
  endif
  return
end subroutine set_status
subroutine optkey(key,option)
  implicit none
  character(len=*), intent(OUT) :: key
  character(len=*), intent(IN) :: option
  integer :: l, i
  l = len(option)
  i = index(option,'=')
  key = option(1:len(key))
  if(i > 0) key = option(1:i)
end subroutine optkey
subroutine optval(val,option)
  implicit none
  character(len=*), intent(OUT) :: val
  character(len=*), intent(IN) :: option
  integer :: l, i
  l = len(option)
  i = index(option,'=')
  val = ' '
  if(i > 0) val = option(i+1:l)
end subroutine optval
