/* This file declares various VTK datatype to SWIG so they can be refered as*/
/* something else than SwigObject*/
/* Python bindings have been tested successfully.*/
/* TODO: Test TCL bindings*/

%include exception.i

%{
// VTK also includes a Py_hash_t typedef definition for Python 2 that clashes
// with SWIG's preprocessor macro
#if PY_VERSION_HEX < 0x3020000
#ifdef Py_hash_t
#undef Py_hash_t
#endif
#endif

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
%module VtkGluePython

%{
#include "vtkPythonUtil.h"
#include "vtkVersion.h"
#if (VTK_MAJOR_VERSION > 5 ||((VTK_MAJOR_VERSION == 5)&&(VTK_MINOR_VERSION > 6)))
#define vtkPythonGetObjectFromPointer vtkPythonUtil::GetObjectFromPointer
#define vtkPythonGetPointerFromObject vtkPythonUtil::GetPointerFromObject
#endif
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

%typemap(out) vtkPolyData* {
  PyImport_ImportModule("vtk");
  $result = vtkPythonGetObjectFromPointer ( (vtkPolyData*)$1 );
}

%typemap(in) vtkPolyData* {
  $1 = NULL;
  $1 = (vtkPolyData*) vtkPythonGetPointerFromObject ( $input, "vtkPolyData" );
  if ( $1 == NULL ) { SWIG_fail; }
}
#endif

#ifdef SWIGJAVA
%{
#include "vtkJavaUtil.h"
%}

/*// TODO: The java typemaps seem to only work in java.i, they are harmless in that file but misplaces.*/
#endif
