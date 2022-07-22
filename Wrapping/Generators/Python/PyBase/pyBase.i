%module(package="itk") pyBasePython

%pythonbegin %{
from . import _ITKPyBasePython
import collections

from sys import version_info as _version_info
if _version_info < (3, 7, 0):
    raise RuntimeError("Python 3.7 or later required")

from . import _ITKCommonPython
%}

//By including pyabc.i and using the -py3 command line option when calling SWIG,
//the proxy classes of the STL containers will automatically gain an appropriate
//abstract base class.
%include <pyabc.i>

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
// Reference: https://www.nabble.com/attachment/16653644/0/SwigRefCount.i
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

%define DECL_PYIMAGEFILTER_CLASS(swig_name)
    %extend swig_name {
        %pythoncode {
            def Update(self):
                """Internal method to pass a pointer to the Python object wrapper, then call Update() on the filter."""
                self._SetSelf(self)
                super().Update()
            def UpdateLargestPossibleRegion(self):
                """Internal method to pass a pointer to the Python object wrapper, then call UpdateLargestPossibleRegion() on the filter."""
                self._SetSelf(self)
                super().UpdateLargestPossibleRegion()
            def UpdateOutputInformation(self):
                """Internal method to pass a pointer to the Python object wrapper, then call UpdateOutputInformation() on the filter."""
                self._SetSelf(self)
                super().UpdateOutputInformation()
        }
    }
%enddef

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


%define DECL_PYTHON_VARIABLELENGTHVECTOR_CLASS(swig_name, type)

    %extend swig_name {
        swig_name __add__(swig_name v2) {
            return *self + v2;
        }
        swig_name __sub__(swig_name v2) {
            return *self - v2;
        }
        type __getitem__(unsigned long d) {
            if (d >= self->GetNumberOfElements()) { throw std::out_of_range("swig_name index out of range."); }
            return self->operator[]( d );
        }
        void __setitem__(unsigned long d, type v) {
            if (d >= self->GetNumberOfElements()) { throw std::out_of_range("swig_name index out of range."); }
            self->operator[]( d ) = v;
        }
        unsigned int __len__() {
            return self->GetNumberOfElements();
        }
        std::string __repr__() {
            std::ostringstream msg;
            msg << "swig_name (" << *self << ")";
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
                from itk.support import helpers
                import warnings

                name = self.GetNameOfClass()
                snake = helpers.camel_to_snake_case(name)

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


%define DECL_PYTHON_TRANSFORMBASETEMPLATE_CLASS(swig_name)
    %extend swig_name {
        %pythoncode %{
            def keys(self):
                """
                Return keys related to the transform's metadata.
                These keys are used in the dictionary resulting from dict(transform).
                """
                result = ['name', 'inputDimension', 'outputDimension', 'inputSpaceName', 'outputSpaceName', 'numberOfParameters', 'numberOfFixedParameters', 'parameters', 'fixedParameters']
                return result

            def __getitem__(self, key):
                """Access metadata keys, see help(transform.keys), for string keys."""
                import itk
                if isinstance(key, str):
                    state = itk.dict_from_transform(self)
                    return state[0][key]

            def __setitem__(self, key, value):
                if isinstance(key, str):
                    import numpy as np
                    if key == 'name':
                        self.SetObjectName(value)
                    elif key == 'inputSpaceName':
                        self.SetInputSpaceName(value)
                    elif key == 'outputSpaceName':
                        self.SetOutputSpaceName(value)
                    elif key == 'fixedParameters' or key == 'parameters':
                        if key == 'fixedParameters':
                            o1 = self.GetFixedParameters()
                        else:
                            o1 = self.GetParameters()

                        o1.SetSize(value.shape[0])
                        for i, v in enumerate(value):
                            o1.SetElement(i, v)

                        if key == 'fixedParameters':
                            self.SetFixedParameters(o1)
                        else:
                            self.SetParameters(o1)


            def __getstate__(self):
                """Get object state, necessary for serialization with pickle."""
                import itk
                state = itk.dict_from_transform(self)
                return state

            def __setstate__(self, state):
                """Set object state, necessary for serialization with pickle."""
                import itk
                import numpy as np
                deserialized = itk.transform_from_dict(state)
                self.__dict__['this'] = deserialized
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
                """Equivalent to the np.ndarray ndim attribute when converted
                to an image with itk.array_view_from_image."""
                spatial_dims = self.GetImageDimension()
                if self.GetNumberOfComponentsPerPixel() > 1:
                    return spatial_dims + 1
                else:
                    return spatial_dims

            @property
            def shape(self):
                """Equivalent to the np.ndarray shape attribute when converted
                to an image with itk.array_view_from_image."""
                itksize = self.GetLargestPossibleRegion().GetSize()
                dim = len(itksize)
                result = [int(itksize[idx]) for idx in range(dim)]

                if(self.GetNumberOfComponentsPerPixel() > 1):
                    result = [self.GetNumberOfComponentsPerPixel(), ] + result
                # ITK is C-order. The shape needs to be reversed unless we are a view on
                # a NumPy array that is Fortran-order.
                reverse = True
                base = self
                while hasattr(base, 'base'):
                    if hasattr(base, 'flags'):
                        reverse = not base.flags.f_contiguous
                        break
                    base = base.base
                if reverse:
                    result.reverse()
                return tuple(result)

            @property
            def dtype(self):
                """Equivalent to the np.ndarray dtype attribute when converted
                to an image with itk.array_view_from_image."""
                import itk
                first_template_arg = itk.template(self)[1][0]
                if hasattr(first_template_arg, 'dtype'):
                    return first_template_arg.dtype
                else:
                    # Multi-component pixel types, e.g. Vector,
                    # CovariantVector, etc.
                    return itk.template(first_template_arg)[1][0].dtype

            def astype(self, pixel_type):
                """Cast the image to the provided itk pixel type or equivalent NumPy dtype."""
                import itk
                import numpy as np
                from itk.support import types

                # if both a numpy dtype and a ctype exist, use the latter.
                if type(pixel_type) is type:
                    c_pixel_type = types.itkCType.GetCTypeForDType(pixel_type)
                    if c_pixel_type is not None:
                        pixel_type = c_pixel_type

                # input_image_template is Image or VectorImage
                (input_image_template, (input_pixel_type, input_image_dimension)) = itk.template(self)

                if input_pixel_type is pixel_type:
                    return self
                OutputImageType = input_image_template[pixel_type, input_image_dimension]
                cast = itk.cast_image_filter(self, ttype=(type(self), OutputImageType))
                return cast

            def SetDirection(self, direction):
                from itk.support import helpers
                if helpers.is_arraylike(direction):
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

            def keys(self):
                """Return keys related to the image's metadata.

                These keys are used in the dictionary resulting from dict(image).

                These keys include MetaDataDictionary keys along with
                'origin', 'spacing', and 'direction' keys, which
                correspond to the image's Origin, Spacing, and Direction. However,
                they are in (z, y, x) order as opposed to (x, y, z) order to
                correspond to the indexing of the shape of the pixel buffer
                array resulting from np.array(image).
                """
                meta_keys = self.GetMetaDataDictionary().GetKeys()
                # Ignore deprecated, legacy members that cause issues
                result = list(filter(lambda k: not k.startswith('ITK_original'), meta_keys))
                result.extend(['origin', 'spacing', 'direction'])
                return result

            def __getitem__(self, key):
                """Access metadata keys, see help(image.keys), for string
                keys, otherwise provide NumPy indexing to the pixel buffer
                array view. The index order follows NumPy array indexing
                order, i.e. [z, y, x] versus [x, y, z]."""
                import itk
                if isinstance(key, str):
                    import numpy as np
                    if key == 'origin':
                        return np.flip(np.asarray(self.GetOrigin()), axis=None)
                    elif key == 'spacing':
                        return np.flip(np.asarray(self.GetSpacing()), axis=None)
                    elif key == 'direction':
                        return np.flip(itk.array_from_matrix(self.GetDirection()), axis=None)
                    else:
                        return self.GetMetaDataDictionary()[key]
                else:
                    return itk.array_view_from_image(self).__getitem__(key)

            def __setitem__(self, key, value):
                """Set metadata keys, see help(image.keys), for string
                keys, otherwise provide NumPy indexing to the pixel buffer
                array view. The index order follows NumPy array indexing
                order, i.e. [z, y, x] versus [x, y, z]."""
                if isinstance(key, str):
                    import numpy as np
                    if key == 'origin':
                        self.SetOrigin(np.flip(value, axis=None))
                    elif key == 'spacing':
                        self.SetSpacing(np.flip(value, axis=None))
                    elif key == 'direction':
                        self.SetDirection(np.flip(value, axis=None))
                    else:
                        self.GetMetaDataDictionary()[key] = value
                else:
                    import itk
                    itk.array_view_from_image(self).__setitem__(key, value)

            def __getstate__(self):
                """Get object state, necessary for serialization with pickle."""
                import itk
                state = itk.dict_from_image(self)
                return state

            def __setstate__(self, state):
                """Set object state, necessary for serialization with pickle."""
                import itk
                import numpy as np
                deserialized = itk.image_from_dict(state)
                self.__dict__['this'] = deserialized
                self.SetOrigin(state['origin'])
                self.SetSpacing(state['spacing'])
                direction = np.asarray(self.GetDirection())
                self.SetDirection(direction)

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

%define DECL_PYTHON_POINTSET_CLASS(swig_name)
    %extend swig_name {
        %pythoncode %{
            def keys(self):
                """
                Return keys related to the pointset's metadata.
                These keys are used in the dictionary resulting from dict(pointset).
                """
                result = ['name', 'dimension', 'numberOfPoints', 'points', 'numberOfPointPixels', 'pointData']
                return result

            def __getitem__(self, key):
                """Access metadata keys, see help(pointset.keys), for string keys."""
                import itk
                if isinstance(key, str):
                    state = itk.dict_from_pointset(self)
                    return state[key]

            def __setitem__(self, key, value):
                if isinstance(key, str):
                    import numpy as np
                    if key == 'name':
                        self.SetObjectName(value)
                    elif key == 'points':
                        self.SetPoints(itk.vector_container_from_array(value))
                    elif key == 'pointData':
                        self.SetPointData(itk.vector_container_from_array(value))

            def __getstate__(self):
                """Get object state, necessary for serialization with pickle."""
                import itk
                state = itk.dict_from_pointset(self)
                return state

            def __setstate__(self, state):
                """Set object state, necessary for serialization with pickle."""
                import itk
                import numpy as np
                deserialized = itk.pointset_from_dict(state)
                self.__dict__['this'] = deserialized
            %}
    }

%enddef

%define DECL_PYTHON_MESH_CLASS(swig_name)
    %extend swig_name {
        %pythoncode %{
            def keys(self):
                """
                Return keys related to the mesh's metadata.
                These keys are used in the dictionary resulting from dict(mesh).
                """
                result = ['meshType', 'name', 'dimension', 'numberOfPoints', 'points', 'numberOfPointPixels', 'pointData',
                            'numberOfCells', 'cells', 'numberOfCellPixels', 'cellData', 'cellBufferSize']
                return result

            def __getitem__(self, key):
                """Access metadata keys, see help(mesh.keys), for string keys."""
                import itk
                if isinstance(key, str):
                    state = itk.dict_from_mesh(self)
                    return state[key]

            def __setitem__(self, key, value):
                """Set metadata keys, see help(image.keys), for string
                keys, otherwise provide NumPy indexing to the pixel buffer
                array view. The index order follows NumPy array indexing
                order, i.e. [z, y, x] versus [x, y, z]."""
                if isinstance(key, str):
                    import numpy as np
                    if key == 'name':
                        self.SetObjectName(value)
                    elif key == 'points':
                        self.SetPoints(itk.vector_container_from_array(value))
                    elif key == 'cells':
                        self.SetCellsArray(itk.vector_container_from_array(value))
                    elif key == 'pointData':
                        self.SetPointData(itk.vector_container_from_array(value))
                    elif key == 'cellData':
                        self.SetCellData(itk.vector_container_from_array(value))

            def __getstate__(self):
                """Get object state, necessary for serialization with pickle."""
                import itk
                state = itk.dict_from_mesh(self)
                return state

            def __setstate__(self, state):
                """Set object state, necessary for serialization with pickle."""
                import itk
                import numpy as np
                deserialized = itk.mesh_from_dict(state)
                self.__dict__['this'] = deserialized
            %}
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

            def __repr__(self):
                vnl_mat = self.GetVnlMatrix()
                python_list_mat = [
                  [vnl_mat.get(i, j) for j in range(vnl_mat.cols())]
                  for i in range(vnl_mat.rows())
                ]
                return type(self).__name__ + " (" + repr(python_list_mat) + ")"

            def __array__(self, dtype=None):
                import itk
                import numpy as np
                array = itk.array_from_matrix(self)
                return np.asarray(array, dtype=dtype)

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
                        Py_DECREF(o);
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                        return NULL;
                    }
                    Py_DECREF(o);
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
                        Py_DECREF(o);
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                        return NULL;
                    }
                    Py_DECREF(o);
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
                    Py_DECREF(o);
                    PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                    return NULL;
                }
                Py_DECREF(o);
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
                Py_DECREF(o);
                PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int or float");
                return NULL;
            }
            Py_DECREF(o);
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
                        Py_DECREF(o);
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int (or long)");
                        return NULL;
                    }
                    Py_DECREF(o);
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
                        Py_DECREF(o);
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of int (or long)");
                        return NULL;
                    }
                    Py_DECREF(o);
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
                        Py_DECREF(o);
                        PyErr_SetString(PyExc_ValueError,"Expecting a sequence of raw pointers (" #swig_name ")." );
                        SWIG_fail;
                    }
                    Py_DECREF(o);
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
