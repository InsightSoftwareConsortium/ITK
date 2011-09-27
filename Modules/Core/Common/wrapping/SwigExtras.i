// TODO: How much of this is useful anyway?

%module SwigExtras
%include "typemaps.i"
%include "carrays.i"
%array_functions(unsigned long, ULArray);
%array_functions(long, LArray);
%array_functions(int, IArray);
%array_functions(float, FArray);
%array_functions(double, DArray);
%array_class(unsigned long, ULArrayClass);
%array_class(long, LArrayClass);
%array_class(int, IArrayClass);
%array_class(float, FArrayClass);
%array_class(double, DArrayClass);

// Create swig version of std::list with
// a specialization for std::string.
// This is because list<std::string> is used
// in the wrapper interface of ITK and for java
// this creates SWIGTYPE_p_*.java files that are
// too big for windows file systems.   But if the
// class is wrapped, the shorter name StringList is used.

%{
#include <list>
%}
namespace std {
  template<class T> class list {
  public:
    list();
    unsigned int size() const;
    bool empty() const;
    void clear();
    void push_back(std::string x);
  };
  template<> class list<std::string> {
    // add specialized typemaps here
  public:
    list();
    unsigned int size() const;
    bool empty() const;
    void clear();
    void push_back(std::string x);
    %extend {
      std::string get(int i)
        {
        std::list<std::string>::iterator j = self->begin();
        while(i)
          {
          j++;
          i--;
          }
        return *j;
        }
    }
  };
}

/* See wrap_SwigExtras.cxx. */
%include stl.i
%template(StringVector) std::vector<std::string>;
%template(StringList) std::list<std::string>;
