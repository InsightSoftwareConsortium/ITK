/**
 * ConnectVTKITK.i - Connect VTK and ITK pipelines in wrapped code.
 *
 * Based on work and ideas by Daniel Blezek, integrated and refined
 * by Charl P. Botha.  If ITK_CSWIG_CONNECTVTKITK has been selected via
 * Insight CMake, NoCable Swig will be ran on this interface code to generate
 * functions with which ITK and VTK pipelines can be connected.
 */

%module ConnectVTKITK

%{
#include "vtkImageImport.h"
#include "vtkImageExport.h"
#include "itkImageToImageFilter.h"
#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"
%}

#ifdef SWIGTCL
%{
#include "vtkTclUtil.h"
%}

%typemap(in) vtkImageExport* {
  int cerror;
  $1 = NULL;
  $1 = (vtkImageExport*) vtkTclGetPointerFromObject(Tcl_GetString($input),
                                                    "vtkImageExport", interp, 
                                                    cerror );
  if ( cerror ) { SWIG_fail; }
}

%typemap(in) vtkImageImport* {
  int cerror;
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

%typemap(in) vtkImageExport* {
  $1 = NULL;
  $1 = (vtkImageExport*) vtkPythonGetPointerFromObject ( $input, "vtkImageExport" );
  if ( $1 == NULL ) { SWIG_fail; }
}

%typemap(in) vtkImageImport* {
  $1 = NULL;
  $1 = (vtkImageImport*) vtkPythonGetPointerFromObject ( $input, "vtkImageImport" );
  if ( $1 == NULL ) { SWIG_fail; }
}

#endif

%{
#include "ConnectVTKITK.h"
%}

%include "ConnectVTKITK.h"

%template(ConnectVTKToITKUC2) ConnectVTKToITK<itk::Image<unsigned char, 2> >;
%template(ConnectITKUC2ToVTK) ConnectITKToVTK<itk::Image<unsigned char, 2> >;
%template(ConnectVTKToITKUC3) ConnectVTKToITK<itk::Image<unsigned char, 3> >;
%template(ConnectITKUC3ToVTK) ConnectITKToVTK<itk::Image<unsigned char, 3> >;

%template(ConnectVTKToITKUS2) ConnectVTKToITK<itk::Image<short unsigned int, 2> >;
%template(ConnectITKUS2ToVTK) ConnectITKToVTK<itk::Image<short unsigned int, 2> >;
%template(ConnectVTKToITKUS3) ConnectVTKToITK<itk::Image<short unsigned int, 3> >;
%template(ConnectITKUS3ToVTK) ConnectITKToVTK<itk::Image<short unsigned int, 3> >;

%template(ConnectVTKToITKF2) ConnectVTKToITK<itk::Image<float, 2> >;
%template(ConnectITKF2ToVTK) ConnectITKToVTK<itk::Image<float, 2> >;
%template(ConnectVTKToITKF3) ConnectVTKToITK<itk::Image<float, 3> >;
%template(ConnectITKF3ToVTK) ConnectITKToVTK<itk::Image<float, 3> >;



