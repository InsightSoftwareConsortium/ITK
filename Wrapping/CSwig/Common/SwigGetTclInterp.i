%module SwigGetTclInterp
%include "typemaps.i"
Tcl_Interp* GetInterp(Tcl_Interp* interp);
%{
Tcl_Interp* GetInterp(Tcl_Interp* interp)
{
  return interp;
}
%}
