FEM classes
===========

These classes provide the ability to implement the low-level Finite Element Modeling. When using them in your code, make sure that you include "itkFEM.h" header file. This will include all files required for FEM. You will also need to add this folder to your INCLUDE folders, and link to both "VXLNumerics" and "FEM" library.

When compiling the toolkit, you and optionally choose to include visualization member functions that draw elements and nodes on the device context in Windows using MFC classes by defining the macro "FEM_BUILD_VISUALIZATION". In this case you should also define this macro before including any FEM header files. By default the visualization support is off.

Another option you have is to specify what kind pointers will be used within the FEM classes. You can use standard C++ pointers (default) or SmartPointer's from itk. If you want SmartPointer support, define macro "FEM_USE_SMART_POINTERS", before compiling the itk and when using the FEM classes in your code.

You can put #define's for the above two macros in file "itkFEMMacro.h", or specify them on compiler's command line.

When FEM project is compiled, it creates the library FEM. If you're using Visual Studio, all FEM source files are included in the FEM.dsp project file and grouped into several groups for easier access.
