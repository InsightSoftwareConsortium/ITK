This directory should only contain FindXXX.cmake modules specific to VXL.
In particular It should contain specialised files to deal with the libraries provided in v3p.

If you want to add a new Module or functionality, check that it isn't already provided in the current version of CMake.
Then add it to NewCMake/FindXXX.cmake. Make sure that it fits the format used for example by FindQt.cmake. As well as
putting it in the VXL repository, submit it to the CMake repository so that it will appear in the next version. You can
email it to cmake@public.kitware.com.