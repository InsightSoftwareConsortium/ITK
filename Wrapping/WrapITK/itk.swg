/* This is an itk-specific typemap used by CableSwig.  
 *   -- Charl P. Botha <cpbotha AT ieee.org>
 */

// Exception support. May be conditionally turned off by defining
// NO_EXCEPTIONS.
#ifndef NO_EXCEPTIONS

%include exception.i

#ifdef SWIGPYTHON

// check if exception is an 'index out of range' exception. If so, throw a python IndexError.
// It allow to use itk objects with __getitem__ method like standard python list
// for example, to format a size object of dim=3 in [1, 2, 3] we can use
// s = "[%s]" % ", ".join(map(str, sizeObject))
// or to get a real python list object
// l = list(sizeObject)
%typemap(throws) std::exception {
   if ( strstr(const_cast<char*>(_e.what()), ": index out of range") == NULL ) {
       SWIG_exception(SWIG_RuntimeError, const_cast<char*>(_e.what()));
   }
   else {
       PyErr_SetString(PyExc_IndexError, const_cast<char*>(_e.what()));
       return NULL;
   }
}
   
#else  // SWIGPYTHON

/* A "throws" attribute with the "std::exception" type is added synthetically
 * to each method node by CableSwig.cxx.  When gcc_xml starts passing through
 * correct throws types, this typemap could be optionally extended to
 * account for more different types.  For now this is sufficient though.
 */
 
%typemap(throws) std::exception {
   SWIG_exception(SWIG_RuntimeError, const_cast<char*>(_e.what()));
}

#endif // SWIGPYTHON

#endif // NO_EXCEPTIONS




#ifdef SWIGPYTHON

/* ------------------------------------------------------------
 * PyObject *  - Just pass straight through unmodified
 * This is default behaviour for python.swg, but Cable passes
 * a PyObject * through as a "p._object", so we redo the typemap
 * ------------------------------------------------------------ */
  
%typemap(in)   p._object "$1 = $input;";
%typemap(out)  p._object "$result = $1;";


// a implementation of %extend in cableswig would also be great to move back __getitem__ method 
// added in some files in this one


// basic type pointers typemap

%typemap(out) unsigned char &, const unsigned char &
  {$result = SWIG_FromUnsignedChar( *$1 );}

%typemap(out)  signed char &, const signed char &
  {$result = SWIG_FromSignedChar( *$1 );}

%typemap(out)  unsigned short &, const unsigned short &
  {$result = SWIG_FromUnsignedShort( *$1 );}

%typemap(out)  signed short &, const signed short &
  {$result = SWIG_FromShort( *$1 );}

%typemap(out)  unsigned int &, const unsigned int &, signed int &, const signed int &
  {$result = SWIG_FromInt( *$1 );}

%typemap(out)  signed long &, const signed long &, unsigned long &, const unsigned long &
  {$result = SWIG_FromLong( *$1 );}

%typemap(out) float &, const float &, double &, const double &
  {$result = SWIG_FromDouble( *$1 );}

#endif // SWIGPYTHON




%include std_string.i

/* disabling this c linkage warning on windows is now handled in ConfigureWrapping.cmake
TODO: If that disablement works, delete this block entirely.

%{
#ifdef _WIN32
#pragma warning ( disable : 4190 )
#pragma warning ( disable : 4049 )
#pragma warning ( disable : 4101 )
#endif
%}

*/
