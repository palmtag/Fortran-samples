# Creating CSV file in Fortran
Fall 2021

You often want to run a Fortran code and export the data to Excel (or some other spreadsheet) to make plots.

The easiest way to do this workflow is to have your program create a CSV file (CSV stands for "Comma Separated Value")
that can easily be read by the spreadsheet program.
A CSV file is just a text file that has data separated by commas (just like you would expect from the name).

An example of a CSV file might look like this:

    n, A, B, C
    1, 1.10, 1.23, 0.12
    2, 1.23, 2.42, 2.23
    3, 1.36, 3.51, 8.59
    4, 1.49, 3.89, 6.24
    
Notice that the CSV file can include strings as well as numbers.

## Example 1

Assume you have an array with of size "n(niso,nstep)" which may be concentrations of nuclides vs. time.
The code to write the CSV file might look like the following:

    open (20,file="output.csv")
    write (20,'(a)') "i, A, B"    ! write header line
    do i=1, nstep
      write (20,100) i, n(:,i)
    enddo
    close (20)
    100 format (i8,100(',',f12.8))
    
Note that this code will produce an extra comma at the end of the line and will only work for up to 100 values in the first dimension because of the "100" in the format statement.

If you want to get fancier, you can do something like the following:

    open (20,file="output.csv")
    write (20,"(a)",advance="no") "i"
    do j=1, niso
       write (20,'(a,i0)',advance="no") ", N",j
    enddo
    write (20,*)    ! write end of line for header
    do i=1, nstep
      write (20,"(i8)",advance="no") i
      do j=1, niso
        write (20,"(',',f12.8)",advance="no") n(j,i)
      enddo
      write (20,*)     ! write end of line
    enddo
    close (20)

    
# Importing File

If you are running on Windows, you should be able to just double click on the CSV file and it will open in Excel
with the data arranged in rows and columns.

This workflow will also work if you are using Linux and using the spreadsheet LibreCalc.



