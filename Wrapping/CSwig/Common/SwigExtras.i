%module(directors="1") SwigExtras
%include "typemaps.i"
%include "carrays.i"
%array_functions(unsigned long, ULArray);
%array_functions(int, IArray);
%array_functions(float, FArray);
%array_functions(double, DArray);
%array_class(unsigned long, ULArrayClass);
%array_class(int, IArrayClass);
%array_class(float, FArrayClass);
%array_class(double, DArrayClass);
#ifdef SWIGTCL
Tcl_Interp* GetInterp(Tcl_Interp* interp);
%{
Tcl_Interp* GetInterp(Tcl_Interp* interp)
{
  return interp;
}
%}
#endif

/* See wrap_SwigExtras.cxx. */
%include stl.i
%template(StringVector) std::vector<std::string>;
#ifdef SWIGJAVA
%feature("director") itkJavaCommand;
%{
#include "itkJavaCommand.h"
%}

// import fake itk command
// because itkCommand will be wrapped elsewhere by cableswig
%import "itkCommand.i"

//  create an itkJavaCommand that has an Execute method that
// can be overriden in java, and used as an itkCommand
class itkJavaCommand : public itkCommand
{
public: 
  virtual void Execute();
};

%pragma(java) jniclasscode=%{
  static { InsightToolkit.itkbase.LoadLibrary("ITKCommonJava"); }
%}
#endif
