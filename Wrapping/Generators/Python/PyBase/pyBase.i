%module pyBasePython

%include <exception.i>
%include <typemaps.i>

%include <std_string.i>
%include <std_pair.i>
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

  // some changes in the New() method
  %rename(__New_orig__) class_name::New;
  %extend class_name {
    %pythoncode %{
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
    %}
  }
  %pythoncode %{
    def class_name##_New():
      return class_name.New()
  %}
%enddef


%extend itkMetaDataDictionary {
    %ignore Find;
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
        %pythoncode %{
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
                """Deprecated procedural interface function.

                Use snake case function instead. This function is now
                merely a wrapper around the snake case function.

                Create a process object, update with the inputs and
                attributes, and return the result.

                The syntax is the same as the one used in New().
                UpdateLargestPossibleRegion() is ran once the input are changed, and
                the current output, or tuple of outputs, if there is more than
                one, is returned. Something like 'filter(input_image, threshold=10)[0]' would
                return the first up-to-date output of a filter with multiple
                outputs.
                """
                import itkHelpers
                import warnings

                name = self.GetNameOfClass()
                snake = itkHelpers.camel_to_snake_case(name)

                warnings.warn("WrapITK warning: itk.%s() is deprecated for procedural"
                " interface. Use snake case function itk.%s() instead."
                % (name, snake), DeprecationWarning)

                filt = self.New(*args, **kargs)
                return filt.__internal_call__()


            def __internal_call__(self):
                """Create a process object, update with the inputs and
                attributes, and return the result.

                The syntax is the same as the one used in New().
                UpdateLargestPossibleRegion() is ran once the input are changed, and
                the current output, or tuple of outputs, if there is more than
                one, is returned. Something like 'filter(input_image, threshold=10)[0]' would
                return the first up-to-date output of a filter with multiple
                outputs.
                """
                self.UpdateLargestPossibleRegion()
                try:
                    import itk
                    if self.GetNumberOfIndexedOutputs() == 0:
                        result = None
                    elif self.GetNumberOfIndexedOutputs() == 1:
                        result = itk.down_cast(self.GetOutput())
                    else:
                        result = tuple([itk.down_cast(self.GetOutput(idx)) for idx in range(self.GetNumberOfIndexedOutputs())])
                    return result
                except AttributeError as e:
                    # In theory, filters should declare that they don't return any output
                    # and therefore the `GetOutput()` method should not be called. However,
                    # there is no garranty that this is always the case.
                    print("This filter cannot be called functionally. Use Object call instead.")
            %}
    }

%enddef


%define DECL_PYTHON_IMAGEBASE_CLASS(swig_name, template_params)
    %inline %{
    #include "itkContinuousIndexSwigInterface.h"
    %}

    %rename(__SetDirection_orig__) swig_name::SetDirection;
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

        %pythoncode %{
            def _SetBase(self, base):
                """Internal method to keep a reference when creating a view of a NumPy array."""
                self.base = base

            @property
            def ndim(self):
                """Equivalant to the np.ndarray ndim attribute when converted
                to an image with itk.array_view_from_image."""
                spatial_dims = self.GetImageDimension()
                if self.GetNumberOfComponentsPerPixel() > 1:
                    return spatial_dims + 1
                else:
                    return spatial_dims

            @property
            def shape(self):
                """Equivalant to the np.ndarray shape attribute when converted
                to an image with itk.array_view_from_image."""
                itksize = self.GetLargestPossibleRegion().GetSize()
                dim = len(itksize)
                result = [int(itksize[idx]) for idx in range(dim)]

                if(self.GetNumberOfComponentsPerPixel() > 1):
                    result = [self.GetNumberOfComponentsPerPixel(), ] + result
                result.reverse()
                return tuple(result)

            @property
            def dtype(self):
                """Equivalant to the np.ndarray dtype attribute when converted
                to an image with itk.array_view_from_image."""
                import itk
                first_template_arg = itk.template(self)[1][0]
                if hasattr(first_template_arg, 'dtype'):
                    return first_template_arg.dtype
                else:
                    # Multi-component pixel types, e.g. Vector,
                    # CovariantVector, etc.
                    return itk.template(first_template_arg)[1][0].dtype

            def SetDirection(self, direction):
                import itkHelpers
                if itkHelpers.is_arraylike(direction):
                    import itk
                    import numpy as np

                    array = np.asarray(direction).astype(np.float64)
                    dimension = self.GetImageDimension()
                    for dim in array.shape:
                        if dim != dimension:
                            raise ValueError('Array does not have the expected shape')
                    matrix = itk.matrix_from_array(array)
                    self.__SetDirection_orig__(matrix)
                else:
                    self.__SetDirection_orig__(direction)
            %}

        // TODO: also add that method. But with which types?
        //  template<class TCoordRep>
        //  void TransformLocalVectorToPhysicalVector(
        //    const FixedArray<TCoordRep, VImageDimension> & inputGradient,
        //          FixedArray<TCoordRep, VImageDimension> & outputGradient ) const
    }

%enddef

%define DECL_PYTHON_IMAGE_CLASS(swig_name)
  %extend swig_name {
      %pythoncode {
          def __array__(self, dtype=None):
              import itk
              import numpy as np
              array = itk.array_from_image(self)
              return np.asarray(array, dtype=dtype)
      }
  }
%enddef


%define DECL_PYTHON_ITK_MATRIX(class_name)
  %rename(__GetVnlMatrix_orig__) class_name::GetVnlMatrix;
  %extend class_name {
        %pythoncode %{
            def GetVnlMatrix(self):
                vnl_reference = self.__GetVnlMatrix_orig__()
                vnl_copy = type(vnl_reference)(vnl_reference)
                return vnl_copy
            %}
        }

  %pythoncode %{
    def class_name##_New():
      return class_name.New()
  %}
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

%define DECL_PYTHON_OUTPUT_RETURN_BY_VALUE_CLASS(swig_name, function_name)
    %rename(__##function_name##_orig__) swig_name::function_name;
    %extend swig_name {
        %pythoncode {
            def function_name(self):
                var = self.__##function_name##_orig__()
                var_copy = type(var)(var)
                return var_copy
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
        %pythoncode %{
    def __eq__(self, other):
        return tuple(self) == tuple(other)
        %}
    }

%enddef


%define DECL_PYTHON_STD_VEC_RAW_TO_SMARTPTR_TYPEMAP(swig_name, swig_name_ptr)

   %typemap(in) std::vector< swig_name_ptr >::value_type const & (swig_name_ptr smart_ptr) {
      swig_name * img;
      if( SWIG_ConvertPtr($input,(void **)(&img),$descriptor(swig_name *), 0) == 0 )
        {
        smart_ptr = img;
        $1 = &smart_ptr;
        }
      else
        {
        PyErr_SetString(PyExc_TypeError, "Expecting argument of type " #swig_name ".");
        SWIG_fail;
        }
    }

    %typemap(in) std::vector<swig_name_ptr> (std::vector< swig_name_ptr> vec_smartptr,
                                             std::vector< swig_name_ptr> *vec_smartptr_ptr) {
        if ((SWIG_ConvertPtr($input,(void **)(&vec_smartptr_ptr),$descriptor(std::vector<swig_name_ptr> *), 0)) == -1) {
            PyErr_Clear();
            if (PySequence_Check($input)) {
                for (Py_ssize_t i =0; i < PyObject_Length($input); i++) {
                    PyObject *o = PySequence_GetItem($input,i);
                    swig_name * raw_ptr;
                    if(SWIG_ConvertPtr(o,(void **)(&raw_ptr),$descriptor(swig_name *), 0) == 0) {
                        vec_smartptr.push_back(raw_ptr);
                    } else {
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of raw pointers (" #swig_name ")." );
                        SWIG_fail;
                    }
                }
                $1 = vec_smartptr;
            }
            else {
                PyErr_SetString(PyExc_ValueError,"Expecting a sequence of raw pointers (" #swig_name ") or a std::vector of SmartPointers (" #swig_name_ptr ").");
                SWIG_fail;
            }
        } else if( vec_smartptr_ptr != NULL ) {
            $1 = *vec_smartptr_ptr;
        } else {
            PyErr_SetString(PyExc_ValueError, "Value can't be None");
            SWIG_fail;
        }
    }
%enddef


// some code from stl

%template(mapULD)         std::map< unsigned long, double >;
%template(mapBB)          std::map< bool, bool >;
%template(mapII)          std::map< int, int >;
%template(mapUCUC)        std::map< unsigned char, unsigned char >;
%template(mapUIUI)        std::map< unsigned int, unsigned int >;
%template(mapUSUS)        std::map< unsigned short, unsigned short >;
%template(mapULUL)        std::map< unsigned long, unsigned long >;
%template(mapSCSC)        std::map< signed char, signed char >;
%template(mapSSSS)        std::map< signed short, signed short >;
%template(mapSLSL)        std::map< signed long, signed long >;
%template(mapFF)          std::map< float, float >;
%template(mapDD)          std::map< double, double >;

%template(pairI)          std::pair< int, int >;
%template(pairUI)         std::pair< unsigned int, unsigned int >;

%template(vectorB)        std::vector< bool >;
%template(vectorvectorB)  std::vector< std::vector< bool > >;
%template(vectorI)        std::vector< int >;
%template(vectorvectorI)  std::vector< std::vector< int > >;
%template(vectorUC)       std::vector< unsigned char >;
%template(vectorvectorUC) std::vector< std::vector< unsigned char > >;
%template(vectorUS)       std::vector< unsigned short >;
%template(vectorvectorUS) std::vector< std::vector< unsigned short > >;
%template(vectorUI)       std::vector< unsigned int >;
%template(vectorvectorUI) std::vector< std::vector< unsigned int > >;
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
%template(vectorstring)   std::vector< std::string >;

%template(listB)          std::list< bool >;
%template(listI)          std::list< int >;
%template(listUC)         std::list< unsigned char >;
%template(listUS)         std::list< unsigned short >;
%template(listUI)         std::list< unsigned int >;
%template(listUL)         std::list< unsigned long >;
%template(listSC)         std::list< signed char >;
%template(listSS)         std::list< signed short >;
%template(listSL)         std::list< signed long >;
%template(listF)          std::list< float >;
%template(listD)          std::list< double >;
%template(liststring)     std::list< std::string >;

%template(setB)          std::set< bool, std::less< bool > >;
%template(setI)          std::set< int, std::less< int > >;
%template(setUC)         std::set< unsigned char, std::less< unsigned char > >;
%template(setUS)         std::set< unsigned short, std::less< unsigned short > >;
%template(setUI)         std::set< unsigned int, std::less< unsigned int > >;
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
