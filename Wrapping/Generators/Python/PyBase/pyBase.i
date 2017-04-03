%module pyBasePython

%begin %{
// To address Python 2.7 hypot bug, https://bugs.python.org/issue11566
#include "PatchedPython27pyconfig.h"
%}

%include <exception.i>
%include <typemaps.i>

%include <std_string.i>
%include <std_vector.i>
%include <std_map.i>
%include <std_list.i>
%include <std_set.i>

// must be included in the end to avoid wrong std::string typemaps
%include std_iostream.i
// broken for now when used after std_string.i: error: ‘basic_string’ has not been declared
// TODO: make a bug report to swig
// %include std_sstream.i

// let str be usable as a template parameter instead of std::string
%pythoncode {
str = str
}

%exception {
  try {
    $action
  } catch (const std::out_of_range& e) {
    SWIG_exception_fail(SWIG_IndexError, e.what());
//  } catch (Swig::DirectorException &e) {
//    SWIG_fail;
  } catch (const std::exception& e) {
    SWIG_exception_fail(SWIG_RuntimeError, e.what());
  }
}

//%feature("director:except") {
//    if ($error != NULL) {
//        throw Swig::DirectorMethodException();
//    }
//}

%typemap(out)  unsigned char &, const unsigned char &, signed char &, const signed char &, unsigned short &, const unsigned short &, signed short &, const signed short &, unsigned int &, const unsigned int &, signed int &, const signed int &, signed long &, const signed long &, unsigned long &, const unsigned long &
  {$result = PyInt_FromLong( *$1 );}

%typemap(out) float &, const float &, double &, const double &
  {$result = PyFloat_FromDouble( *$1 );}

// ignore reference count management
%ignore Delete;
%ignore SetReferenceCount;
%ignore Register;
%ignore UnRegister;


// This macro replaces the use of itk::SmartPointer.
// class_name is class name without namespace qualifiers.
// Reference: http://www.nabble.com/attachment/16653644/0/SwigRefCount.i
%define DECLARE_REF_COUNT_CLASS(class_name)

        // pointers and references
        %typemap(out) class_name *, class_name & {
                // always tell SWIG_NewPointerObj we're the owner
                $result = SWIG_NewPointerObj((void *) $1, $1_descriptor, 1);
                if ($1) {
                        $1->Register();
                }
        }

  // transform smart pointers in raw pointers
        %typemap(out) class_name##_Pointer {
          // get the raw pointer from the smart pointer
          class_name * ptr = $1;
                // always tell SWIG_NewPointerObj we're the owner
                $result = SWIG_NewPointerObj((void *) ptr, $descriptor(class_name *), 1);
                // register the object, it it exists
                if (ptr) {
                        ptr->Register();
                }
        }

  // transform smart pointers in raw pointers
        %typemap(out) class_name##_Pointer & {
          // get the raw pointer from the smart pointer
          class_name * ptr = *$1;
                // always tell SWIG_NewPointerObj we're the owner
                $result = SWIG_NewPointerObj((void *) ptr, $descriptor(class_name *), 1);
                // register the object, it it exists
                if (ptr) {
                        ptr->Register();
                }
        }

        // make "deletion" in scripting language just decrement ref. count
        %extend class_name {
                public:
                ~class_name() {self->UnRegister();};
        }

        %ignore class_name::~class_name;

        %ignore class_name##_Pointer;

        // a cast() static method to downcast objects
        %extend class_name {
                public:
                static class_name * cast( itkLightObject * obj ) {
                  if( obj == NULL ) {
                    return NULL;
                  }
                  class_name * cast_obj = dynamic_cast<class_name *>(obj);
                  if( cast_obj == NULL ) {
                    throw std::bad_cast();
                  }
                  return cast_obj;
                };
        }

        // a GetPointer() method for backward compatibility with older wrapitk
        %extend class_name {
                public:
                class_name * GetPointer() {
                  std::cerr << "WrapITK warning: GetPointer() is now deprecated for 'class_name'." << std::endl;
                  return self;
                };
        }

  // some changes in the New() method
  %rename(__New_orig__) class_name::New;
  %extend class_name {
    %pythoncode {
      def New(*args, **kargs):
          """New() -> class_name

          Create a new object of the class class_name and set the input and the parameters if some
          named or non-named arguments are passed to that method.

          New() tries to assign all the non named parameters to the input of the new objects - the
          first non named parameter in the first input, etc.

          The named parameters are used by calling the method with the same name prefixed by 'Set'.

          Ex:

            class_name.New( reader, Threshold=10 )

          is (most of the time) equivalent to:

            obj = class_name.New()
            obj.SetInput( 0, reader.GetOutput() )
            obj.SetThreshold( 10 )
          """
          obj = class_name.__New_orig__()
          import itkTemplate
          itkTemplate.New(obj, *args, **kargs)
          return obj
      New = staticmethod(New)
    }
  }
  %pythoncode {
    def class_name##_New():
      return class_name.New()
  }
%enddef

// convert the only known SmartPointerForwardReference to a raw pointer
%typemap(out) itk::SmartPointerForwardReference< itk::ProcessObject > {
  itk::ProcessObject * ptr = $1;
  $result = SWIG_NewPointerObj((void *) ptr, $descriptor(itkProcessObject *), 1);
  if (ptr) {
        ptr->Register();
  }
}

%extend itkMetaDataDictionary {
    std::string __str__() {
        std::ostringstream msg;
        self->Print( msg );
        return msg.str();
    }
    %pythoncode {
        def __setitem__(self,key,item):
            import itk
            if isinstance(item, str):
                object = itk.MetaDataObject.S.New()
            elif isinstance(item, int):
                object = itk.MetaDataObject.SI.New()
            elif isinstance( item, float):
                object = itk.MetaDataObject.F.New()
            elif isinstance( item, bool):
                object = itk.MetaDataObject.B.New()
            else:
                object = None
            if object != None:
                object.SetMetaDataObjectValue(item)
                self.Set(key, object)
        def __getitem__(self,key):
            import itk
            obj = self.Get(key)
            return itk.down_cast(obj).GetMetaDataObjectValue()
        def __len__(self):
            return self.GetKeys().size()
        def __iter__(self):
            keys = self.GetKeys()
            for key in keys:
                yield self.Get(key)
    }
}

%extend itkLightObject {
    std::string __str__() {
        std::ostringstream msg;
        self->Print( msg );
        return msg.str();
    }
    bool __eq__( itkLightObject* obj ) {
        return self == obj;
    }
    size_t __hash__() {
        return reinterpret_cast<size_t>(self);
    }
}

// swig generates invalid code with that simple typemap
//     %typemap(in) itkLightObject##_Pointer const & {
//       // tralala
//       itkLightObject* ptr;
//     //          if ((SWIG_ConvertPtr($input,(void **) &ptr, $descriptor(itkLightObject), 0)) == -1) return NULL;
//    $1 = ptr;
//    }


%extend itkComponentTreeNode {
    %pythoncode {
        def __len__(self):
            return self.GetChildren().size()
        def __getitem__(self, i):
            return self.GetNthChild(i)
        def __iter__(self):
            for child in self.GetChildren():
                yield child
    }
    std::string __str__() {
        std::ostringstream msg;
        self->Print( msg );
        return msg.str();
    }
}

// Macros for template classes

%define DECL_PYTHON_SPATIALOBJECTPPOINT_CLASS(swig_name)

    %extend swig_name {
        std::string __str__() {
            std::ostringstream msg;
            self->Print( msg );
            return msg.str();
        }
    }

%enddef


%define DECL_PYTHON_LABELMAP_CLASS(swig_name)

    %extend swig_name {
        %pythoncode {
            def __len__(self):
                return self.GetNumberOfLabelObjects()
            def __getitem__(self, label):
                return self.GetLabelObject(label)
            def __iter__(self):
                labels = self.GetLabels()
                for label in labels:
                    yield self.GetLabelObject(label)
        }
    }

%enddef


%define DECL_PYTHON_IMAGEREGION_CLASS(swig_name)

    %extend swig_name {
        std::string __repr__() {
            std::ostringstream msg;
            msg << "swig_name(" << self->GetIndex() << ", " << self->GetSize()  << ")";
            return msg.str();
    }
}

%enddef


%define DECL_PYTHON_OBJECT_CLASS(swig_name)

    %pythonprepend itkObject::AddObserver %{
        import itk
        if len(args) == 3 and not issubclass(args[2].__class__, itk.Command) and callable(args[2]):
            args = list(args)
            pycommand = itk.PyCommand.New()
            pycommand.SetCommandCallable( args[2] )
            args[2] = pycommand
            args = tuple(args)
        elif len(args) == 2 and not issubclass(args[1].__class__, itk.Command) and callable(args[1]):
            args = list(args)
            pycommand = itk.PyCommand.New()
            pycommand.SetCommandCallable( args[1] )
            args[1] = pycommand
            args = tuple(args)
    %}

%enddef

%define DECL_PYTHON_PROCESSOBJECT_CLASS(swig_name)

    %extend itkProcessObject {
        %pythoncode {
            def __len__(self):
                """Returns the number of outputs of that object.
                """
                return self.GetNumberOfIndexedOutputs()

            def __getitem__(self, item):
                """Returns the outputs of that object.

                The outputs are casted to their real type.
                Several outputs may be returned by using the slice notation.
                """
                import itk
                if isinstance(item, slice):
                    indices = item.indices(len(self))
                    return [itk.down_cast(self.GetOutput(i)) for i in range(*indices)]
                else:
                    return itk.down_cast(self.GetOutput(item))

            def __call__(self, *args, **kargs):
                """Change the inputs and attributes of the object and update it.

                The syntax is the same as the one used in New().
                UpdateLargestPossibleRegion() is ran once the input are changed, and
                the current object is returned, to make is easier to get one of the
                outputs. Something like 'filter(newInput, Threshold=10)[0]' would
                return the first output of the filter up to date.
                """
                import itk
                itk.set_inputs( self, args, kargs )
                self.UpdateLargestPossibleRegion()
                return self
            }
    }

%enddef


%define DECL_PYTHON_IMAGEBASE_CLASS(swig_name, template_params)
    %inline %{
    #include "itkContinuousIndexSwigInterface.h"
    %}

    %extend swig_name {
        itkIndex##template_params TransformPhysicalPointToIndex( itkPointD##template_params & point ) {
            itkIndex##template_params idx;
            self->TransformPhysicalPointToIndex<double>( point, idx );
            return idx;
         }

        itkContinuousIndexD##template_params TransformPhysicalPointToContinuousIndex( itkPointD##template_params & point ) {
            itkContinuousIndexD##template_params idx;
            self->TransformPhysicalPointToContinuousIndex<double>( point, idx );
            return idx;
        }

        itkPointD##template_params TransformContinuousIndexToPhysicalPoint( itkContinuousIndexD##template_params & idx ) {
            itkPointD##template_params point;
            self->TransformContinuousIndexToPhysicalPoint<double>( idx, point );
            return point;
        }

        itkPointD##template_params TransformIndexToPhysicalPoint( itkIndex##template_params & idx ) {
            itkPointD##template_params point;
            self->TransformIndexToPhysicalPoint<double>( idx, point );
            return point;
        }

        // TODO: also add that method. But with which types?
        //  template<class TCoordRep>
        //  void TransformLocalVectorToPhysicalVector(
        //    const FixedArray<TCoordRep, VImageDimension> & inputGradient,
        //          FixedArray<TCoordRep, VImageDimension> & outputGradient ) const
    }

%enddef


%define DECL_PYTHON_STD_COMPLEX_CLASS(swig_name)

%extend swig_name {
    %pythoncode {
        def __repr__(self):
            return "swig_name (%s, %s)" % (self.real(), self.imag())

        def __complex__(self):
            return complex(self.real(), self.imag())
    }
}

%enddef

%define DECL_PYTHON_VEC_TYPEMAP(swig_name, type, dim)

    %typemap(in) swig_name & (swig_name itks) {
        if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {
            PyErr_Clear();
            if (PySequence_Check($input) && PyObject_Length($input) == dim) {
                for (int i =0; i < dim; i++) {
                    PyObject *o = PySequence_GetItem($input,i);
                    if (PyInt_Check(o)) {
                        itks[i] = PyInt_AsLong(o);
                    } else if (PyFloat_Check(o)) {
                        itks[i] = (type)PyFloat_AsDouble(o);
                    } else {
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                        return NULL;
                    }
                }
                $1 = &itks;
            }else if (PyInt_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = PyInt_AsLong($input);
                }
                $1 = &itks;
            }else if (PyFloat_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = (type)PyFloat_AsDouble($input);
                }
                $1 = &itks;
            } else {
                PyErr_SetString(PyExc_TypeError,"Expecting an swig_name, an int, a float, a sequence of int or a sequence of float.");
                SWIG_fail;
            }
        }
    }

    %typemap(typecheck) swig_name & {
        void *ptr;
        if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1
            && ( !PySequence_Check($input) || PyObject_Length($input) != dim )
            && !PyInt_Check($input) && !PyFloat_Check($input) ) {
            _v = 0;
            PyErr_Clear();
        } else {
            _v = 1;
        }
    }

    %typemap(in) swig_name (swig_name itks) {
        swig_name * s;
        if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(swig_name *), 0)) == -1) {
            PyErr_Clear();
            if (PySequence_Check($input) && PyObject_Length($input) == dim) {
                for (int i =0; i < dim; i++) {
                    PyObject *o = PySequence_GetItem($input,i);
                    if (PyInt_Check(o)) {
                        itks[i] = PyInt_AsLong(o);
                    } else if (PyFloat_Check(o)) {
                        itks[i] = (type)PyFloat_AsDouble(o);
                    } else {
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                        return NULL;
                    }
                }
                $1 = itks;
            }else if (PyInt_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = PyInt_AsLong($input);
                }
                $1 = itks;
            }else if (PyFloat_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = (type)PyFloat_AsDouble($input);
                }
                $1 = itks;
            } else {
                PyErr_SetString(PyExc_TypeError,"Expecting an swig_name, an int, a float, a sequence of int or a sequence of float.");
                SWIG_fail;
            }
        } else if( s != NULL ) {
            $1 = *s;
        } else {
            PyErr_SetString(PyExc_ValueError, "Value can't be None");
            SWIG_fail;
        }
    }

    %typemap(typecheck) swig_name {
        void *ptr;
        if (SWIG_ConvertPtr($input, &ptr, $descriptor(swig_name *), 0) == -1
            && ( !PySequence_Check($input) || PyObject_Length($input) != dim )
            && !PyInt_Check($input) && !PyFloat_Check($input) ) {
            _v = 0;
            PyErr_Clear();
        } else {
            _v = 1;
        }
    }

    %extend swig_name {
        type __getitem__(unsigned long d) {
            if (d >= dim) { throw std::out_of_range("swig_name index out of range."); }
            return self->operator[]( d );
        }
        void __setitem__(unsigned long d, type v) {
            if (d >= dim) { throw std::out_of_range("swig_name index out of range."); }
            self->operator[]( d ) = v;
        }
        static unsigned int __len__() {
            return dim;
        }
        std::string __repr__() {
            std::ostringstream msg;
            msg << "swig_name (" << *self << ")";
            return msg.str();
        }
    }

%enddef


%define DECL_PYTHON_VARLEN_SEQ_TYPEMAP(type, value_type)

    %typemap(in) type& (type itks) {
        if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {
            PyErr_Clear();
            itks = type( PyObject_Length($input) );
            for (unsigned int i =0; i < itks.Size(); i++) {
                PyObject *o = PySequence_GetItem($input,i);
                if (PyInt_Check(o)) {
                    itks[i] = (value_type)PyInt_AsLong(o);
                } else if (PyFloat_Check(o)) {
                    itks[i] = (value_type)PyFloat_AsDouble(o);
                } else {
                    PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                    return NULL;
                }
            }
            $1 = &itks;
        }
}

%typemap(typecheck) type & {
    void *ptr;
    if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1
        && !PySequence_Check($input) ) {
        _v = 0;
        PyErr_Clear();
    } else {
        _v = 1;
    }
}

%typemap(in) type (type itks) {
    type * s;
    if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(type *), 0)) == -1) {
        PyErr_Clear();
        itks = type( PyObject_Length($input) );
        for (unsigned int i =0; i < itks.Size(); i++) {
            PyObject *o = PySequence_GetItem($input,i);
            if (PyInt_Check(o)) {
                itks[i] = (value_type)PyInt_AsLong(o);
            } else if (PyFloat_Check(o)) {
                itks[i] = (value_type)PyFloat_AsDouble(o);
            } else {
                PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                return NULL;
            }
        }
        $1 = itks;
    }
}

%typemap(typecheck) type {
    void *ptr;
    if (SWIG_ConvertPtr($input, &ptr, $descriptor(type *), 0) == -1
        && !PySequence_Check($input) ) {
        _v = 0;
        PyErr_Clear();
    } else {
        _v = 1;
    }
}

%extend type {
    value_type __getitem__(unsigned long dim) {
        if (dim >= self->Size()) { throw std::out_of_range("type index out of range."); }
        return self->operator[]( dim );
    }
    void __setitem__(unsigned long dim, value_type v) {
        if (dim >= self->Size()) { throw std::out_of_range("type index out of range."); }
        self->operator[]( dim ) = v;
    }
    unsigned int __len__() {
        return self->Size();
    }
    std::string __repr__() {
        std::ostringstream msg;
        msg << "swig_name (" << *self << ")";
        return msg.str();
    }
}

%enddef


%define DECL_PYTHON_SEQ_TYPEMAP(swig_name, dim)

    %typemap(in) swig_name & (swig_name itks) {
        if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {
            PyErr_Clear();
            if (PySequence_Check($input) && PyObject_Length($input) == dim) {
                for (int i =0; i < dim; i++) {
                    PyObject *o = PySequence_GetItem($input,i);
                    if (PyInt_Check(o) || PyLong_Check(o)) {
                        itks[i] = PyInt_AsLong(o);
                    } else {
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int (or long)");
                        return NULL;
                    }
                }
                $1 = &itks;
            }else if (PyInt_Check($input) || PyLong_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = PyInt_AsLong($input);
                }
                $1 = &itks;
            } else {
                PyErr_SetString(PyExc_TypeError,"Expecting an swig_name, an int or sequence of int (or long)");
                SWIG_fail;
            }
        }
    }

    %typemap(typecheck) swig_name & {
        void *ptr;
        if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1
            && ( !PySequence_Check($input) || PyObject_Length($input) != dim )
            && !PyInt_Check($input) ) {
            _v = 0;
            PyErr_Clear();
        } else {
            _v = 1;
        }
    }

    %typemap(in) swig_name (swig_name itks) {
        swig_name * s;
        if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(swig_name *), 0)) == -1) {
            PyErr_Clear();
            if (PySequence_Check($input) && PyObject_Length($input) == dim) {
                for (int i =0; i < dim; i++) {
                    PyObject *o = PySequence_GetItem($input,i);
                    if (PyInt_Check(o) || PyLong_Check(o)) {
                        itks[i] = PyInt_AsLong(o);
                    } else {
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int (or long)");
                        return NULL;
                    }
                }
                $1 = itks;
            }else if (PyInt_Check($input) || PyLong_Check($input)) {
                for (int i =0; i < dim; i++) {
                    itks[i] = PyInt_AsLong($input);
                }
                $1 = itks;
            } else {
                PyErr_SetString(PyExc_TypeError,"Expecting an swig_name, an int or sequence of int (or long)");
                SWIG_fail;
            }
        }else if( s != NULL ) {
            $1 = *s;
        } else {
            PyErr_SetString(PyExc_ValueError, "Value can't be None");
            SWIG_fail;
        }
    }

    %typemap(typecheck) swig_name {
        void *ptr;
        if (SWIG_ConvertPtr($input, &ptr, $descriptor(swig_name *), 0) == -1
            && ( !PySequence_Check($input) || PyObject_Length($input) != dim )
            && !(PyInt_Check($input) || PyLong_Check($input)) ) {
            _v = 0;
            PyErr_Clear();
        } else {
            _v = 1;
        }
    }

    %extend swig_name {
        long __getitem__(unsigned long d) {
            if (d >= dim) { throw std::out_of_range("swig_name index out of range."); }
            return self->operator[]( d );
        }
        void __setitem__(unsigned long d, long int v) {
            if (d >= dim) { throw std::out_of_range("swig_name index out of range."); }
            self->operator[]( d ) = v;
        }
        static unsigned int __len__() {
            return dim;
        }
        std::string __repr__() {
            std::ostringstream msg;
            msg << "swig_name (" << *self << ")";
            return msg.str();
        }
    }

%enddef

// some code from stl

%template(vectorstring)   std::vector< std::string >;
%template(liststring)     std::list< std::string >;

%template(mapULD)         std::map< unsigned long, double >;
%template(mapBB)          std::map< bool, bool >;
%template(mapUCUC)        std::map< unsigned char, unsigned char >;
%template(mapUSUS)        std::map< unsigned short, unsigned short >;
%template(mapULUL)        std::map< unsigned long, unsigned long >;
%template(mapSCSC)        std::map< signed char, signed char >;
%template(mapSSSS)        std::map< signed short, signed short >;
%template(mapSLSL)        std::map< signed long, signed long >;
%template(mapFF)          std::map< float, float >;
%template(mapDD)          std::map< double, double >;

%template(vectorB)        std::vector< bool >;
%template(vectorvectorB)  std::vector< std::vector< bool > >;
%template(vectorUC)       std::vector< unsigned char >;
%template(vectorvectorUC) std::vector< std::vector< unsigned char > >;
%template(vectorUS)       std::vector< unsigned short >;
%template(vectorvectorUS) std::vector< std::vector< unsigned short > >;
%template(vectorUL)       std::vector< unsigned long >;
%template(vectorvectorUL) std::vector< std::vector< unsigned long > >;
%template(vectorSC)       std::vector< signed char >;
%template(vectorvectorSC) std::vector< std::vector< signed char > >;
%template(vectorSS)       std::vector< signed short >;
%template(vectorvectorSS) std::vector< std::vector< signed short > >;
%template(vectorSL)       std::vector< signed long >;
%template(vectorvectorSL) std::vector< std::vector< signed long > >;
%template(vectorF)        std::vector< float >;
%template(vectorvectorF)  std::vector< std::vector< float > >;
%template(vectorD)        std::vector< double >;
%template(vectorvectorD)  std::vector< std::vector< double > >;

%template(listB)          std::list< bool >;
%template(listUC)         std::list< unsigned char >;
%template(listUS)         std::list< unsigned short >;
%template(listUL)         std::list< unsigned long >;
%template(listSC)         std::list< signed char >;
%template(listSS)         std::list< signed short >;
%template(listSL)         std::list< signed long >;
%template(listF)          std::list< float >;
%template(listD)          std::list< double >;

%template(setB)          std::set< bool, std::less< bool > >;
%template(setUC)         std::set< unsigned char, std::less< unsigned char > >;
%template(setUS)         std::set< unsigned short, std::less< unsigned short > >;
%template(setUL)         std::set< unsigned long, std::less< unsigned long > >;
%template(setULL)        std::set< unsigned long long, std::less< unsigned long long > >;
%template(setSC)         std::set< signed char, std::less< signed char > >;
%template(setSS)         std::set< signed short, std::less< signed short > >;
%template(setSL)         std::set< signed long, std::less< signed long > >;
%template(setSLL)        std::set< signed long long, std::less< signed long long > >;
%template(setF)          std::set< float, std::less< float > >;
%template(setD)          std::set< double, std::less< double > >;

%template(vectorsetUL)   std::vector< std::set< unsigned long, std::less< unsigned long > > >;
%template(mapsetUL)      std::map< unsigned long, std::set< unsigned long, std::less< unsigned long > > >;
