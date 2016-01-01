module mod_mathexp
  implicit none

  public :: MathExp
  public :: newMathExp

  private

  type MathExp
    character(:), allocatable :: exp
    type(MathExp), pointer    :: left
    type(MathExp), pointer    :: right
    logical                   :: is_static
    real(8)                   :: value

    contains
      procedure :: evaluate
      procedure :: clear
  end type MathExp

  integer, parameter :: normal = 3
  integer, parameter :: middle = 1
  integer, parameter :: low    = 0

  character(10), allocatable, save :: names(:)
  real(8), allocatable, save       :: values(:)

  contains
    recursive subroutine clear(self)
      implicit none
      class(MathExp), intent(inout) :: self

      if (associated(self % left)) then
        call self % left % clear()
        deallocate(self % left)
      endif

      if (associated(self % right)) then
        call self % right % clear()
        deallocate(self % right)
      endif

      deallocate(self % exp)
    end subroutine clear


    recursive function evaluate(self, succeeded, vnames, vvalues) result(ret)
      implicit none
      class(MathExp), intent(inout)      :: self
      logical, intent(out)               :: succeeded
      character(5), intent(in), optional :: vnames(:)
      real(8), intent(in), optional      :: vvalues(:)
      real(8)                            :: ret
      integer iostat, i
      logical ls, rs

      if (self % is_static) then
        ret = self % value
        succeeded = .true.
        return
      endif

      ret = 0

      if (present(vnames)) then
        names = [vnames]
        values = [vvalues]
      endif

      if (allocated(names)) then
        do i=1, size(names)
          if (names(i) == self % exp) then
            ret = values(i)
            if (present(vnames)) deallocate(names, values)
            succeeded = .true.
            return
          endif
        enddo
      endif

      succeeded = .false.
      ls = .false.
      rs = .false.

      if ((associated(self % left) .and. associated(self % right))) then
        if (self % exp == "+") then
          ret = self % left % evaluate(ls) + self % right % evaluate(rs)
        elseif (self % exp == "-") then
          ret = self % left % evaluate(ls) - self % right % evaluate(rs)
        elseif (self % exp == "*") then
          ret = self % left % evaluate(ls) * self % right % evaluate(rs)
        elseif (self % exp == "/") then
          ret = self % left % evaluate(ls) / self % right % evaluate(rs)
        elseif (self % exp == "^") then
          ret = self % left % evaluate(ls) ** self % right % evaluate(rs)
        endif
        if (ls .and. rs .and. self % left % is_static .and. self % right % is_static) then
          self % is_static = .true.
          self % value = ret
        endif
      else
        if (checklog(self % exp, ret)) then
          succeeded = .true.
        elseif (checklog10(self % exp, ret)) then
          succeeded = .true.
        else
          read(self % exp, *, iostat=iostat) ret
          if (iostat == 0) then
            succeeded = .true.
          endif
        endif

        if (succeeded) then
          self % is_static = .true.
          self % value = ret
        endif
      endif

      succeeded = succeeded .or. (ls .and. rs)

      if (present(vnames)) deallocate(names, values)
    end function evaluate


    function checklog(string, ret) result(is_log)
      implicit none
      character(*), intent(in) :: string
      real(8), intent(out)     :: ret
      logical                  :: is_log
      integer l, iostat

      l = len(string)
      ret = 0
      is_log = .false.
      if (l < 5) return

      if (string(1:3) == "ln(" .and. string(l:l) == ")") then
        read(string(4:l-1), *, iostat=iostat) ret
        if (iostat == 0) then
          ret = log(ret)
          is_log = .true.
        endif
      endif
    end function checklog


    function checklog10(string, ret) result(is_log)
      implicit none
      character(*), intent(in) :: string
      real(8), intent(out)     :: ret
      logical                  :: is_log
      integer l, iostat

      l = len(string)
      ret = 0
      is_log = .false.
      if (l < 6) return

      if (string(1:4) == "log(" .and. string(l:l) == ")") then
        read(string(5:l-1), *, iostat=iostat) ret
        if (iostat == 0) then
          ret = log10(ret)
          is_log = .true.
        endif
      endif
    end function checklog10


    function newMathExp(string) result(m)
      implicit none
      character(*), intent(in)           :: string
      type(MathExp)                      :: m

      m % exp = trim(remove_spaces(string))
      call parse(m)
    end function newMathExp


    recursive subroutine parse(parent)
      implicit none
      type(MathExp)  :: parent
      integer start, last, l

      parent % is_static = .false.
      l = len(parent % exp)
      if (l == 0) return

      call operator_pos(parent % exp, start, last)

      if (start == l .and. last == l) return

      allocate(parent % left, parent % right)

      parent % left  % exp = parent % exp(1:start-1)
      parent % right % exp = parent % exp(last+1:l)
      parent % exp = parent % exp(start:last)

      if (start == 1 .and. last == 1) then
        if (parent % exp(1:1) == "+" .or. parent % exp(1:1) == "-") then
          parent % left % exp = "0"
        endif
      endif

      call parse(parent % left)
      call parse(parent % right)
    end subroutine parse


    subroutine operator_pos(string, start, last)
      implicit none
      character(*), intent(inout) :: string
      integer, intent(out)     :: start
      integer, intent(out)     :: last
      integer i, l, nest, high, priority, lowest_priority, iostat
      real(8) testreal

      high = 2
      nest = 0
      lowest_priority = normal

      string = remove_outer_brackets(string)
      l = len(string)
      if (l <= 1) then
        start = l
        last = l
        return
      endif

      do i=1, len(string)
        priority = normal

        if (string(i:i) == "(") then
          nest = nest + 1
        elseif (string(i:i) == ")") then
          nest = nest - 1
        elseif (string(i:i) == "+" .or. string(i:i) == "-") then
          if (2 < i .and. i <= l-1) then
            read(string(i-2:i+1), *, iostat=iostat) testreal
            if (iostat == 0) cycle
          endif
          priority = low
        elseif (string(i:i) == "*" .or. string(i:i) == "/") then
          priority = middle
        elseif (string(i:i) == "^") then
          priority = high
          high = 3
        endif

        if (nest == 0 .and. priority <= lowest_priority) then
          lowest_priority = priority
          start = i
          last = i
        endif

      enddo
    end subroutine operator_pos


    recursive function remove_outer_brackets(string) result(removed)
      implicit none
      character(*), intent(in) :: string
      character(len(string))   :: removed
      integer l

      removed = adjustl(string)
      l = len(trim(removed))

      do
        if (removed(1:1) == "(" .and. removed(l:l) == ")") then
          removed = removed(2:l-1) // "  "
          l = len(trim(removed))
        else
          exit
        endif
      enddo
    end function remove_outer_brackets


    function remove_spaces(string) result(removed)
      implicit none
      character(*), intent(in) :: string
      character(len(string))   :: removed
      integer i, l

      removed = adjustl(string)
      l = len(string)
      do i=l, 1, -1
        if (removed(i:i) == " ") removed(i:l) = removed(i+1:l) // " "
      enddo
    end function remove_spaces

  
end module mod_mathexp
