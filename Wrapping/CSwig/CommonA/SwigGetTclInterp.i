%module SwigGetTclInterp
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
#endif
%}
