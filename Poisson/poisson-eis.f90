MODULE shared_data

  IMPLICIT NONE

  !Information about the global array
  INTEGER, TARGET :: nx=-1, ny=-1
  REAL, DIMENSION(:,:), ALLOCATABLE :: phi, temp, rho
  REAL, DIMENSION(:), ALLOCATABLE :: x, y
  REAL, TARGET :: current_x, current_y

  CONTAINS

  !This routine displays the output. It isn't a part of this course. Annotation
  !is only for general interest. Note that normally you'd use a library like
  !ncurses (https://en.wikipedia.org/wiki/Ncurses) to do the terminal trickery
  !that I'm doing here. This is just for maximum compatability
  SUBROUTINE display_result(array)

    REAL, DIMENSION(0:,0:), INTENT(IN) :: array
    CHARACTER(LEN=3) :: clrstr = '[2J'
    CHARACTER(LEN=5), DIMENSION(3) :: colours = (/'[34m', '[39m', '[31m'/)
    CHARACTER(LEN=1), DIMENSION(3) :: vals = (/'*', '.', '+'/)
    INTEGER, DIMENSION(2) :: sizes
    INTEGER :: ix, iy, index

    !Special string to clear screen using VT100 terminal codes, see
    !(http://wiki.bash-hackers.org/scripting/terminalcodes)
    WRITE(*,'(A)') CHAR(27) // TRIM(clrstr)

    sizes = SHAPE(array)
    !Outer array is flipped because screen indexes work from top left
    !Everything else works from bottom left
    DO iy = sizes(2) - 2, 1, -1
      DO ix = 1, sizes(1) - 2
        !Get the symbol and colour for the value in this cell
        !Colours are more VT100 terminal codes
        index = NINT((array(ix,iy)-MINVAL(array)) &
            / (MAXVAL(array) - MINVAL(array)) &
            * REAL(SIZE(vals)-1))+1

        !Write out the special VT100 colour control code and symbol
        WRITE(*,'(A,A)', ADVANCE='NO') ACHAR(27) // TRIM(colours(index)) , &
            vals(index) // " "
        !Version without colour code
!        WRITE(*,'(A)', ADVANCE='NO') vals(index) // " "
      END DO
      WRITE(*,*) ""
    END DO
    !Set terminal colours back to default
    WRITE(*,'(A)', ADVANCE='NO') ACHAR(27) // TRIM('[39m')

  END SUBROUTINE display_result

END MODULE shared_data

MODULE deck_control

  USE shared_data
  USE eis_deck_header

  TYPE(eis_deck_definition) :: deck_definition
  TYPE(eis_text_deck_parser) :: deck_parser
  TYPE(eis_parser), TARGET :: maths_parser
  TYPE(eis_error_handler), TARGET :: err_handler
  TYPE(eis_stack) :: rho_stack

  CONTAINS

  SUBROUTINE initialise_deck

    TYPE(eis_parser), POINTER :: mp_p
    INTEGER(eis_error) :: errcode
    TYPE(eis_deck_block_definition), POINTER :: root
    TYPE(eis_error_handler), POINTER :: epointer
    INTEGER, POINTER :: iptr
    REAL, POINTER :: rptr
    CHARACTER(LEN=:), ALLOCATABLE :: str
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: results

    epointer => err_handler

    mp_p => maths_parser
    CALL maths_parser%init(errcode, physics = eis_physics_si, &
        err_handler = epointer)
    rptr => current_x
    CALL maths_parser%add_pointer_variable('x', rptr, errcode)
    rptr => current_y
    CALL maths_parser%add_pointer_variable('y', rptr, errcode)

    CALL deck_parser%init(err_handler = epointer)

    root => deck_definition%init(parser = mp_p)
    iptr => nx
    CALL root%add_key('nx', i32value = iptr)
    iptr => ny
    CALL root%add_key('ny', i32value = iptr)

    CALL root%add_key('rho', key_stack_fn = rho_stack_fn)

    CALL deck_parser%parse_deck_file('input.deck',deck_definition, errcode, &
        allow_root_keys = .TRUE.)

    IF (errcode /= eis_err_none) THEN
      DO ierr = 1, err_handler%get_error_count()
        CALL err_handler%get_error_report(ierr, str)
        PRINT *, str
      END DO
      STOP
    END IF

  END SUBROUTINE initialise_deck

  SUBROUTINE rho_stack_fn(key_text, value_stack, pass_number, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    TYPE(eis_stack), INTENT(INOUT) :: value_stack
    INTEGER, INTENT(IN) :: pass_number
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    rho_stack = value_stack

  END SUBROUTINE rho_stack_fn

END MODULE deck_control

PROGRAM serial

  USE shared_data
  USE deck_control

  IMPLICIT NONE

  INTEGER :: ix, iy, icycle, ierr
  REAL :: dx, dy
  CHARACTER(LEN=:), ALLOCATABLE :: str
  REAL(eis_num), ALLOCATABLE, DIMENSION(:) :: results
  INTEGER(eis_error) :: errcode
  INTEGER :: ct

  CALL initialise_deck()

  PRINT *, 'nx = ', nx
  PRINT *, 'ny = ', ny

  IF (nx < 1 .OR. ny < 1) THEN
    PRINT *,'Need to specify both nx and ny in the deck'
    STOP
  END IF

  IF (.NOT. rho_stack%init) THEN
    PRINT *,'Must specify a rho function'
    STOP
  END IF

  ALLOCATE(phi(0:nx+1,0:ny+1), rho(0:nx+1,0:ny+1), temp(0:nx+1,0:ny+1))
  ALLOCATE(x(0:nx+1), y(0:ny+1))

  !This applies global boundaries to all edges
  phi = 5.5
  rho = 0.0

  dx = 2.0 / REAL(nx-1)
  dy = 2.0 / REAL(ny-1)

  x(0) = -1.0 - dx
  DO ix = 1, nx+1
    x(ix) = x(ix-1) + dx
  END DO

  y(0) = -1.0 - dy
  DO iy = 1, ny+1
    y(iy) = y(iy-1) + dy
  END DO

  DO iy = 1, ny
    current_y = y(iy)
    DO ix = 1, nx
      current_x = x(ix)
      ct = maths_parser%evaluate(rho_stack, results, errcode)
      rho(ix,iy) = results(1)
!      rho(ix,iy) = EXP(-(x(ix)/0.1)**2 - (y(iy)/0.1)**2)
    END DO
  END DO

  PRINT *,'Please press a key to start iterating'
  READ(*,*)

  CALL display_result(phi)
  PRINT *,'Please press a key to advance'
  READ(*,*)
  !Now iterate
  DO icycle = 1, 500
    DO iy = 1, ny
      DO ix = 1, nx
        temp(ix,iy) = -rho(ix, iy) + 0.25 * (phi(ix+1,iy) + &
             phi(ix,iy+1) + phi(ix-1,iy) + phi(ix,iy-1))
      END DO
    END DO
    phi(1:nx,1:ny) = temp(1:nx,1:ny)

    IF (MOD(icycle,50) == 0) THEN
      CALL display_result(phi)
      PRINT *,'Please press a key to advance'
      READ (*,*)
    END IF
  END DO

END PROGRAM serial


