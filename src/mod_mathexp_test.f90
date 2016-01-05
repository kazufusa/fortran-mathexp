program eval_test
  use mod_mathexp
  implicit none

  type(MathExp) m
  integer ret
  real(8) testreal
  logical succeeded
  character(10) a, b, c, d
  a = "a         "
  b = "b         "
  c = "c         "
  d = "d         "
  ret = 0

  m = newMathExp("((1+1))")
  if (m%evaluate(succeeded) /= 1+1) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 1+1) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("1+2*2")
  if (m%evaluate(succeeded) /= 1+2*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 1+2*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("(1+2)*2")
  if (m%evaluate(succeeded) /= (1+2)*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= (1+2)*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("(1-2)*2")
  if (m%evaluate(succeeded) /= (1-2)*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= (1-2)*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("1/2")
  if (m%evaluate(succeeded) /= 1D0/2D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 1D0/2D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("(1+1)/2")
  if (m%evaluate(succeeded) /= (1+1)/2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= (1+1)/2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("2*2")
  if (m%evaluate(succeeded) /= 2*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 2*2) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("2 + (1+2)*2")
  if (m%evaluate(succeeded) /= 8D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 8D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("2 + (1+2)^2")
  if (m%evaluate(succeeded) /= 11D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 11D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("b * (2 + a)^c")
  if (m%evaluate(succeeded, [a, b, c], [4D0, 3D0, 2D0]) /= 108D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded, [a, b, c], [4D0, 3D0, 2D0]) /= 108D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded, [c, b, a], [4D0, 2D0, 3D0]) /= 1250D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded, [c, b, a], [4D0, 2D0, 3D0]) /= 1250D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("*3")
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("/3")
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("^3")
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  testreal = m%evaluate(succeeded)
  if (succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("+3")
  if (m%evaluate(succeeded) /= 3D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 3D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("-3")
  if (m%evaluate(succeeded) /= -3D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= -3D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("2^2*3")
  if (m%evaluate(succeeded) /= 12D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  if (m%evaluate(succeeded) /= 12D0) ret = ret + 1
  if (.not. succeeded) ret = ret + 1
  call m%clear()

  m = newMathExp("4^3^2")
  if (m%evaluate(succeeded) /= 4**3**2) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  if (m%evaluate(succeeded) /= 4**3**2) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  call m%clear()

  m = newMathExp("2 * ln(110-12)")
  if (m%evaluate(succeeded) /= 2*log(110D0-12D0)) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  if (m%evaluate(succeeded) /= 2*log(110D0-12D0)) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  call m%clear()

  m = newMathExp("2 - ln(10*10/a+2)")
  if (m%evaluate(succeeded, [a], [2D0]) /= 2-log(10D0*10D0/2D0+2)) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  if (m%evaluate(succeeded, [a], [2D0]) /= 2-log(10D0*10D0/2D0+2)) ret = ret + 1
  if (succeeded .neqv. .true.) ret = ret + 1
  call m%clear()

  if (ret>0) stop 1

end program eval_test
