%include exception.i


%{
#include "vtkImageImport.h"
#include "vtkImageExport.h"
#include "itkImageToImageFilter.h"
#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"
#include "vtkImageData.h"
%}

#ifdef SWIGTCL
%{
#include "vtkTclUtil.h"
%}

%typemap(in) vtkImageExport* {
  int cerror=0;
  $1 = NULL;
  $1 = (vtkImageExport*) vtkTclGetPointerFromObject(Tcl_GetString($input),
                                                    "vtkImageExport", interp,
                                                    cerror );
  if ( cerror ) { SWIG_fail; }
}

%typemap(in) vtkImageImport* {
  int cerror=0;
  $1 = NULL;
  $1 = (vtkImageImport*) vtkTclGetPointerFromObject(Tcl_GetString($input),
                                                    "vtkImageImport", interp,
                                                    cerror );
  if ( cerror ) { SWIG_fail; }
}

#endif

#ifdef SWIGPYTHON
%{
#include "vtkPythonUtil.h"
%}

%typemap(out) vtkImageExport* {
  PyImport_ImportModule("vtk");
  $result = vtkPythonGetObjectFromPointer ( (vtkImageExport*)$1 );
}

%typemap(out) vtkImageImport* {
  PyImport_ImportModule("vtk");
  $result = vtkPythonGetObjectFromPointer ( (vtkImageImport*)$1 );
}

%typemap(out) vtkImageData* {
  PyImport_ImportModule("vtk");
  $result = vtkPythonGetObjectFromPointer ( (vtkImageData*)$1 );
}

%typemap(in) vtkImageData* {
  $1 = NULL;
  $1 = (vtkImageData*) vtkPythonGetPointerFromObject ( $input, "vtkImageData" );
  if ( $1 == NULL ) { SWIG_fail; }
}

#endif
