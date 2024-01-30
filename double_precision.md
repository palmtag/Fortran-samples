# Understanding Double Precision Numbers in Fortran
Fall 2017

Fortran has several different types of floating point numbers.  These include real (4 byte), double (8 byte) and quad precision (16 byte).  I will not discuss quad precision in this document.

When using floating point numbers in your programming projects, I recommend always using double precision instead of single precision.  This includes variables, constants, and intrinsic functions.

## Declarations
One thing to note about Fortran is that the syntax has evolved over the years, so unfortunately, there are several ways of declaring and using floating numbers. 

To declare a single precision real number, use one of the following (the 4 refers to 4 bytes):

    real    :: xs
    real(4) :: xs

To declare a double precision number, there are several ways to do it.  The first method is preferred

    real(8):: xd
    real*8 :: xd                ! obsolete
    double precision :: xd      ! obsolete

You can also use the new Fortran “kind” functions. Kind functions use bits instead of bytes, so remember that 4 bytes equals 32 bits, and 8 bytes equals 64 bits.

    integer, parameter :: DRK = selected_real_kind(64)
    real (kind=DRK) :: xd

A single precision “kind” declaration is

    integer, parameter :: SRK = selected_real_kind(32)
    real (kind=SRK) :: xs

I’ve never actually found the kind functions useful in practice because you have to consistently define SRK and DRK everywhere.  This makes it hard to share code.

## Constants
Programs usually use some kind of constants in them.  The compiler needs to know what data type the constants are.

Integer constants have no decimal point

    i=3
    x=y**2        ! 2 is an integer constant
    z=sin(2*x)    ! 2 is an integer constant

Real constants have a decimal point or “e” exponent.

    x=4.0
    avag=6.02214e24
    x=y**1.4
    z=sin(2.0*x)

Double precision constants are declared by replacing the “e” exponent with a “d”, or adding “d0” if there is no exponent.

    x=4.0d0
    avag=6.02214d24
    x=y**1.4d0
    z=sin(2.0d0*x)
    pi=3.1415926535897932384d0

When using double precision numbers, always remember to include a “d” exponent on constants!

You can also define single and double precision constants using “kind” declarations.  In this example, SRK and DRK must be previously defined.

    x=2.0_SRK    ! single precision
    y=4.0_DRK    ! double precision

Defining the correct constant is important for accuracy!  You should always try to be consistent in your arithmetic, for example multiply singles by singles, doubles by doubles, etc.

If you do arithmetic with a single and a double, or an integer with a double, it will promote the lower precision number to the higher precision number.  However, the lower precision number may not have the accuracy you think it does.

**Watch out for integer division!**   Having something like "1/2" will evaluate to 0 with integer division.

## Intrinsic Functions

In modern Fortran, intrinsic functions (sin, cos, log, etc.) will return the precision that is passed in. If you pass in a single, it will return single precision.  If you pass in a double, it will return double precision.

    ln2 = log(2.0)    ! returns single precision number
    ln2 = log(2.0d0)  ! returns double precision number

## Common Mistakes
In this example, “pi” is declared with many digits, but it is stored in a single precision number. Therefore, it will only store 5-6 digits of accuracy.   When you use it, you are not getting the accuracy you want.

    real(8) :: x, z
    real    :: pi=3.1415926535897932384
    z=cos(pi*x)     ! may not have the accuracy you desire

In this similar example, you have two variables that are double precision, but they are set equal to a single log2 function and a double log2 function. 

    real(8) :: ln2_sgl, ln2_dbl
    ln2_sgl=log(2.0)    ! single precision constant
    ln2_dbl=log(2.0d0)  ! double precision constant

The results of this are:

    ln2_sgl   0.69314718246459961
    ln2_dbl   0.69314718055994529

Both numbers are double precision, but they have different levels of accuracy because the log function returned different levels of accuracy.

