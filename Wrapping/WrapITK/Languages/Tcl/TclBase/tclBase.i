%module tclBaseTcl

%include <exception.i>
%include <typemaps.i>

%include <std_string.i>
/* %include <std_vector.i> */
%include <std_map.i>

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


// some code from stl

/*
Can't use that template because it make swig use the function SwigString_FromString which is not defined in the current
file when that template in only imported
TODO: make a bug report

%template(vectorstring)   std::vector< std::string >;
*/

/*
those template are useless because swig doesn't recognize that std::map< bool, bool > and std::map< bool, bool, std::less< bool > > are the same types.
TODO: make a bug report

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
*/


/* we can't wrap the vector< vector<X> > type because swig produce the folowing invalid code vector<vector<X>>
TODO: make a bug report
*/

/*
And also, can't use those templates because it make swig use the function std_vector_Sl_???_Sg__set/get
which are not defined in the current file when that template in only imported
TODO: make a bug report

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
*/
