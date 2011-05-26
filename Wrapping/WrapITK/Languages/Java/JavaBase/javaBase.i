%module javaBaseJava

%include javaBase_ext.i


%typemap(javacode) itkJavaLibraryLoader %{
  public static void Load(String name) {
    // System.loadLibrary() create the lib name by itself, but not GlobalLoad()
    // so one must get lib and the other name
    // Also, don't use GlobalLoad() for ITKJavaBase - it's not yet available
    System.loadLibrary(name);
    if(!name.equals("ITKJavaBaseJava")) {
      String lib = System.mapLibraryName(name);
      GlobalLoad(lib);
    }
    System.loadLibrary(name);
  }
%}


%inline %{

#if !defined(_WIN32)
#include <dlfcn.h>
#endif

class itkJavaLibraryLoader {
  public:
  static void GlobalLoad(const char* lib)
    {
#if !defined(_WIN32)
    dlopen(lib, RTLD_GLOBAL|RTLD_NOW);
#endif
    }

};


%}
