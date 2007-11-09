This directory should only contain FindXXX.cmake modules and other CMake configuration files specific to VXL.
In particular it should contain specialised files to deal with the libraries provided in v3p.

In the past the this directory and NewCMake were used to store FindXXX.cmake modules
that were not yet available from CMake. Initially VXL needed many more FindXXX.cmake modules, (or several vastly
better written modules) than were available from CMake. Now however, CMake provides all the module files needed
by VXL, and has access to many more bug reports for its module files than VXL has.

It no longer makes any sense in all but a few rare and temporary cases for VXL to attempt to maintain ordinary
module files. Trying to maintain parallel copies in VXL and CMake is unnecessary and difficult work.

If the CMake provided module file is buggy then please submit a fix to CMake at cmake@www.cmake.org
If you need a new FindXXX.cmake file for your private code, then please do not add it to VXL. You
can submit it to CMake at cmake@www.cmake.org

If you are adding some new functionality to VXL and you need a third party library, (and VXL-maintainers agrees
that VXL can depend on this additional library) then you can temporarily add the file to the NewCMake subdirectory.
Please also submit it to CMake at cmake@www.cmake.org, and delete it from the VXL repository when the next
version of CMake is widely available.


