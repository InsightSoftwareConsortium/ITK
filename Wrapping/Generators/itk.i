// Common SWIG support applied to all generators.

//######################################################################
// The ignore list

// win32
// TODO: this would be wrapped between #ifdefs
%ignore itkLightObject::operator new(size_t n);
%ignore itkLightObject::operator new[](size_t n);
%ignore itkLightObject::operator delete(void* m);
%ignore itkLightObject::operator delete[](void* m, size_t);
