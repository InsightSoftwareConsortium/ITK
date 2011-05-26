%module pyBasePython

%include <exception.i>
%include <typemaps.i>

%include std_iostream.i
%include std_sstream.i
%include <std_vector.i>
%include <std_map.i>
%include <std_list.i>
%include <std_set.i>

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

// some code from stl

%template(vectorstring)   std::vector< std::string >;
%template(liststring)     std::list< std::string >;

%template(mapULD)         std::map< unsigned long, double >;
// %template(mapBB)          std::map< bool, bool >;
%template(mapUCUC)        std::map< unsigned char, unsigned char >;
%template(mapUSUS)        std::map< unsigned short, unsigned short >;
%template(mapULUL)        std::map< unsigned long, unsigned long >;
%template(mapSCSC)        std::map< signed char, signed char >;
%template(mapSSSS)        std::map< signed short, signed short >;
%template(mapSLSL)        std::map< signed long, signed long >;
%template(mapFF)          std::map< float, float >;
%template(mapDD)          std::map< double, double >;

// %template(vectorB)        std::vector< bool >;
// %template(vectorvectorB)  std::vector< std::vector< bool > >;
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

// %template(listB)          std::list< bool >;
%template(listUC)         std::list< unsigned char >;
%template(listUS)         std::list< unsigned short >;
%template(listUL)         std::list< unsigned long >;
%template(listSC)         std::list< signed char >;
%template(listSS)         std::list< signed short >;
%template(listSL)         std::list< signed long >;
%template(listF)          std::list< float >;
%template(listD)          std::list< double >;

// %template(setB)          std::set< bool, std::less< bool > >;
%template(setUC)         std::set< unsigned char, std::less< unsigned char > >;
%template(setUS)         std::set< unsigned short, std::less< unsigned short > >;
%template(setUL)         std::set< unsigned long, std::less< unsigned long > >;
%template(setSC)         std::set< signed char, std::less< signed char > >;
%template(setSS)         std::set< signed short, std::less< signed short > >;
%template(setSL)         std::set< signed long, std::less< signed long > >;
%template(setF)          std::set< float, std::less< float > >;
%template(setD)          std::set< double, std::less< double > >;

%template(vectorsetUL)   std::vector< std::set< unsigned long, std::less< unsigned long > > >;
%template(mapsetUL)      std::map< unsigned long, std::set< unsigned long, std::less< unsigned long > > >;
