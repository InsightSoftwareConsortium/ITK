%module rbBaseRuby

%include <exception.i>
%include <typemaps.i>

/*
%include std_alloc.i
%include std_basic_string.i
%include std_char_traits.i
%include std_common.i
%include std_complex.i
%include std_container.i
%include std_deque.i
%include std_except.i
%include std_functors.i
%include std_ios.i
%include std_iostream.i
%include std_list.i
%include std_map.i
%include std_multimap.i
%include std_multiset.i
%include std_pair.i
%include std_queue.i
%include std_set.i
%include std_sstream.i
%include std_stack.i
%include std_streambuf.i
%include std_string.i
%include std_vector.i
// %include std_vectora.i
// %include std_wstring.i
*/

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

%enddef


// some code from stl
/*
%template(VectorString)   std::vector< std::string >;
%template(ListString)     std::list< std::string >;

%template(MapULD)         std::map< unsigned long, double >;
%template(MapBB)          std::map< bool, bool >;
%template(MapUCUC)        std::map< unsigned char, unsigned char >;
%template(MapUSUS)        std::map< unsigned short, unsigned short >;
%template(MapULUL)        std::map< unsigned long, unsigned long >;
%template(MapSCSC)        std::map< signed char, signed char >;
%template(MapSSSS)        std::map< signed short, signed short >;
%template(MapSLSL)        std::map< signed long, signed long >;
%template(MapFF)          std::map< float, float >;
%template(MapDD)          std::map< double, double >;

%template(VectorB)        std::vector< bool >;
%template(VectorvectorB)  std::vector< std::vector< bool > >;
%template(VectorUC)       std::vector< unsigned char >;
%template(VectorvectorUC) std::vector< std::vector< unsigned char > >;
%template(VectorUS)       std::vector< unsigned short >;
%template(VectorvectorUS) std::vector< std::vector< unsigned short > >;
%template(VectorUL)       std::vector< unsigned long >;
%template(VectorvectorUL) std::vector< std::vector< unsigned long > >;
%template(VectorSC)       std::vector< signed char >;
%template(VectorvectorSC) std::vector< std::vector< signed char > >;
%template(VectorSS)       std::vector< signed short >;
%template(VectorvectorSS) std::vector< std::vector< signed short > >;
%template(VectorSL)       std::vector< signed long >;
%template(VectorvectorSL) std::vector< std::vector< signed long > >;
%template(VectorF)        std::vector< float >;
%template(VectorvectorF)  std::vector< std::vector< float > >;
%template(VectorD)        std::vector< double >;
%template(VectorvectorD)  std::vector< std::vector< double > >;

%template(ListB)          std::list< bool >;
%template(ListUC)         std::list< unsigned char >;
%template(ListUS)         std::list< unsigned short >;
%template(ListUL)         std::list< unsigned long >;
%template(ListSC)         std::list< signed char >;
%template(ListSS)         std::list< signed short >;
%template(ListSL)         std::list< signed long >;
%template(ListF)          std::list< float >;
%template(ListD)          std::list< double >;

%template(SetB)          std::set< bool >;
%template(SetUC)         std::set< unsigned char >;
%template(SetUS)         std::set< unsigned short >;
%template(SetUL)         std::set< unsigned long >;
%template(SetSC)         std::set< signed char >;
%template(SetSS)         std::set< signed short >;
%template(SetSL)         std::set< signed long >;
%template(SetF)          std::set< float >;
%template(SetD)          std::set< double >;
*/
