%module SwigExtras
%include "typemaps.i"
%include "carrays.i"
%array_functions(unsigned long, ULArray);
%array_functions(int, IArray);
%array_functions(float, FArray);
%array_functions(double, DArray);
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
%pragma(java) jniclasscode=%{
  static { InsightToolkit.itkbase.LoadLibrary("ITKCommonJava"); }
%}
#endif
