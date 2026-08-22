/* This file declares various VTK datatype to SWIG so they can be referred as*/
/* something else than SwigObject*/
/* Python bindings have been tested successfully.*/
/* TODO: Test TCL bindings*/

%include exception.i

%{
// VTK also includes a Py_hash_t type definition for Python 2 that clashes
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
%module(package="itk",threads="1") VtkGluePython

%{
#include <cinttypes>
#include <cstdio>
#include <cstring>

// Pointer exchange with VTK's Python layer using only the Limited API, so this
// module stays abi3 and needs no link against VTK::WrappingPythonCore.
namespace itkVtkGlueABI3
{

inline PyObject *
ImportClass(const char * moduleName, const char * className)
{
  PyObject * mod = PyImport_ImportModule(moduleName);
  if (!mod)
  {
    return nullptr;
  }
  PyObject * cls = PyObject_GetAttrString(mod, className);
  Py_DECREF(mod);
  return cls;
}

// Parses the `_<hex>_p_<ClassName>` encoding VTK publishes as `__this__`. The
// isinstance() gate is what makes trusting that string safe: without it any
// object exposing a forged `__this__` would be cast to a native pointer.
inline void *
GetPointerFromObject(PyObject * obj, const char * moduleName, const char * className)
{
  PyObject * cls = ImportClass(moduleName, className);
  if (!cls)
  {
    return nullptr;
  }
  const int isInstance = PyObject_IsInstance(obj, cls);
  Py_DECREF(cls);
  if (isInstance < 0)
  {
    return nullptr;
  }
  if (isInstance == 0)
  {
    PyErr_Format(PyExc_TypeError, "expected a VTK %s instance", className);
    return nullptr;
  }

  PyObject * thisStr = PyObject_GetAttrString(obj, "__this__");
  if (!thisStr)
  {
    PyErr_Clear();
    PyErr_Format(PyExc_TypeError, "expected a VTK %s instance", className);
    return nullptr;
  }

  void *      ptr = nullptr;
  Py_ssize_t  len = 0;
  const char * s = PyUnicode_AsUTF8AndSize(thisStr, &len);
  if (s && len > 4 && s[0] == '_' && std::strlen(s) == static_cast<size_t>(len))
  {
    const char * sep = std::strstr(s + 1, "_p_");
    if (sep && std::strcmp(sep + 3, className) == 0)
    {
      std::uintptr_t addr = 0;
      // '_' is not a hex digit, so the conversion stops at the separator.
      if (std::sscanf(s + 1, "%" SCNxPTR, &addr) == 1 && addr != 0)
      {
        ptr = reinterpret_cast<void *>(addr);
      }
    }
  }
  Py_DECREF(thisStr);

  if (!ptr)
  {
    PyErr_Format(PyExc_TypeError, "expected a VTK %s instance", className);
  }
  return ptr;
}

// Reconstructs through VTK's own `Addr=0x...` path so the IsA() check, the
// object map, and reference counting all stay VTK's responsibility.
// `__new__` is called explicitly rather than `cls(addr)`: vtkmodules.util.data_model
// registers keyword-only `override` subclasses for the data-model classes, whose
// __init__ would reject the positional address string.
inline PyObject *
GetObjectFromPointer(void * ptr, const char * moduleName, const char * className)
{
  if (!ptr)
  {
    Py_RETURN_NONE;
  }

  PyObject * cls = ImportClass(moduleName, className);
  if (!cls)
  {
    return nullptr;
  }

  char addr[64];
  std::snprintf(addr, sizeof(addr), "Addr=0x%" PRIxPTR, reinterpret_cast<std::uintptr_t>(ptr));
  PyObject * obj = PyObject_CallMethod(cls, "__new__", "Os", cls, addr);
  Py_DECREF(cls);
  return obj;
}

} // namespace itkVtkGlueABI3
%}

%typemap(out) vtkImageExport* {
  $result = itkVtkGlueABI3::GetObjectFromPointer($1, "vtkmodules.vtkIOImage", "vtkImageExport");
  if (!$result) { SWIG_fail; }
}

%typemap(out) vtkImageImport* {
  $result = itkVtkGlueABI3::GetObjectFromPointer($1, "vtkmodules.vtkIOImage", "vtkImageImport");
  if (!$result) { SWIG_fail; }
}

%typemap(out) vtkImageData* {
  $result = itkVtkGlueABI3::GetObjectFromPointer($1, "vtkmodules.vtkCommonDataModel", "vtkImageData");
  if (!$result) { SWIG_fail; }
}

%typemap(in) vtkImageData* {
  $1 = static_cast<vtkImageData *>(itkVtkGlueABI3::GetPointerFromObject($input, "vtkmodules.vtkCommonDataModel", "vtkImageData"));
  if (!$1) { SWIG_fail; }
}

%typemap(out) vtkPolyData* {
  $result = itkVtkGlueABI3::GetObjectFromPointer($1, "vtkmodules.vtkCommonDataModel", "vtkPolyData");
  if (!$result) { SWIG_fail; }
}

%typemap(in) vtkPolyData* {
  $1 = static_cast<vtkPolyData *>(itkVtkGlueABI3::GetPointerFromObject($input, "vtkmodules.vtkCommonDataModel", "vtkPolyData"));
  if (!$1) { SWIG_fail; }
}
#endif

#ifdef SWIGJAVA
%{
#include "vtkJavaUtil.h"
%}

/*// TODO: The java typemaps seem to only work in java.i, they are harmless in that file but misplaces.*/
#endif
