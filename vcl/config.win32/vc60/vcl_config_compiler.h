#ifndef vcl_config_compiler_h_config_win32_vc60_
#define vcl_config_compiler_h_config_win32_vc60_
//:
// \file
// This file is *not* generated.

//----------------------------------------------------------------------
// syntax-like things.

//: VCL_HAS_BOOL
// Set to 1 if "bool" is accepted by the compiler as a built-in type.
#define VCL_HAS_BOOL 1


//: VCL_HAS_DYNAMIC_CAST
//
// True if the compiler supports dynamic cast.
//
#define VCL_HAS_DYNAMIC_CAST 1


//: VCL_HAS_RTTI
//
// True if the compiler supports RTTI, viz the 'typeid' function.
//
#define VCL_HAS_RTTI 0


//: VCL_HAS_TYPENAME
//
// True if the compiler supports the "typename" keyword
//
#define VCL_HAS_TYPENAME 1


//: VCL_HAS_EXPORT
//
// True if the compiler supports the "export" keyword. FIXME.
//
#define VCL_HAS_EXPORT 0


//: VCL_HAS_MUTABLE
//
// True if the compiler supports the "mutable" keyword
//
#define VCL_HAS_MUTABLE 1


//: VCL_HAS_EXPLICIT
//
// True if the compiler supports the "explicit" keyword
//
#define VCL_HAS_EXPLICIT 1


//: VCL_FOR_SCOPE_HACK:
//
// True if the compiler uses old-style 'for' loop scoping.
// Setting this nonzero causes the Henderson trick to be used.
#define VCL_FOR_SCOPE_HACK 1


//: VCL_COMPLEX_POW_WORKS
//
// It appears several programmers have (independently)
// not realised their lack of knowledge of complex numbers.
// pow(complex(-1,0),0.5) should return (0,1) not (Nan,0), etc.
#define VCL_COMPLEX_POW_WORKS 0


//: VCL_DEFAULT_VALUE(x)
//
// Used to provide default values for function args in definition
// Some compilers (GCC272) require defaults in template function definitions
// Other compilers (VC50) disallow defaults in both decls and defs

//#define VCL_DEFAULT_VALUE(x) /* no need */
//#define VCL_DEFAULT_VALUE(x) = x
#define VCL_DEFAULT_VALUE(x) /* no need */


//----------------------------------------------------------------------
// constant initializer issues.

//: VCL_STATIC_CONST_INIT_INT_DECL(x)
//
// ANSI allows
// \code
//     class A {
//       static const int x = 27;
//     };
// \endcode
// And there is a speed advantage, so we want to use it where supported.
// However, the standard also requires (9.4.2/4) that the constant be
// defined in namespace scope. (That is, space must be allocated.)
// To make matters worse, some compilers (at least VC 7) mistakenly
// allocate storage without the definition in namespace scope,
// which results in multiply defined symbols.
// To use the macro, use VCL_STATIC_CONST_INIT_INT_DECL in the class
// definition (header file). This declares the constant.
// \code
//     class A {
//       static const int x VCL_STATIC_CONST_INIT_INT_DECL(27);
//     };
// \endcode
// Use VCL_STATIC_CONST_INIT_INT_DEFN in some .cxx file to define
// the constant, but only if VCL_STATIC_CONST_INIT_INT_NO_DEFN
// evaluates to false.
// \code
//     #if !VCL_STATIC_CONST_INIT_INT_NO_DEFN
//     const int A::x VCL_STATIC_CONST_INIT_INT_DEFN(27);
//     #endif
// \endcode
//
// In order to be able to query the setting of this, one actually must
// define VCL_CAN_STATIC_CONST_INIT_INT to either 0 or 1.

//#define VCL_CAN_STATIC_CONST_INIT_INT 1 /* allowed */
//#define VCL_CAN_STATIC_CONST_INIT_INT 0 /* not allowed */
#ifndef VCL_CAN_STATIC_CONST_INIT_INT
# define VCL_CAN_STATIC_CONST_INIT_INT 0
#endif
#if VCL_CAN_STATIC_CONST_INIT_INT
# define VCL_STATIC_CONST_INIT_INT_DECL(x) = x
# define VCL_STATIC_CONST_INIT_INT_DEFN(x) /* initialized at declaration */
# define VCL_STATIC_CONST_INIT_INT_NO_DEFN 0
#else
# define VCL_STATIC_CONST_INIT_INT_DECL(x) /* not allowed */
# define VCL_STATIC_CONST_INIT_INT_DEFN(x) = x
# define VCL_STATIC_CONST_INIT_INT_NO_DEFN 0
#endif


//: VCL_STATIC_CONST_INIT_FLOAT(x)
//
// GCC allows the above, but with floating point types, ANSI doesn't.
// Again, we'll use it if we've got it.
//
// In order to be able to query the setting of this, one actually must
// define VCL_CAN_STATIC_CONST_INIT_FLOAT to either 0 or 1.

//#define VCL_CAN_STATIC_CONST_INIT_FLOAT 1 /* allowed */
//#define VCL_CAN_STATIC_CONST_INIT_FLOAT 0 /* not allowed */
#ifndef VCL_CAN_STATIC_CONST_INIT_FLOAT
# define VCL_CAN_STATIC_CONST_INIT_FLOAT 0
#endif
#if VCL_CAN_STATIC_CONST_INIT_FLOAT
# define VCL_STATIC_CONST_INIT_FLOAT_DECL(x) = x
# define VCL_STATIC_CONST_INIT_FLOAT_DEFN(x) /* initialized at declaration */
# define VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN 0
#else
# define VCL_STATIC_CONST_INIT_FLOAT_DECL(x) /* not allowed */
# define VCL_STATIC_CONST_INIT_FLOAT_DEFN(x) = x
# define VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN 0
#endif


//----------------------------------------------------------------------
// various template issues.

//: VCL_HAS_MEMBER_TEMPLATES
//
// True if the compiler supports template members of template classes.  e.g.
// \code
//     template <class U> class A {
//       template <class V> void f(V);
//     }
// \endcode
#define VCL_HAS_MEMBER_TEMPLATES 1


//: VCL_CAN_DO_PARTIAL_SPECIALIZATION
//
// True if the compiler supports partial specializations of templates. e.g.
// \code
// template <class T>
// class vector<T*> : public vector<void *> { .. inline methods .. };
// \endcode
//
#define VCL_CAN_DO_PARTIAL_SPECIALIZATION 0


//: VCL_DEFINE_SPECIALIZATION
//
// In order to *define* a template (function or class) specialization, the
// definition must be preceded by "template <>" on ISO-conforming compilers.
// Some compilers (eg gcc 2.7.2) make no distinction between an instance
// of a templated function and a function with the same name and signature,
// and so do not support the use of "template <>". Use VCL_DEFINE_SPECIALIZATION
// instead.
//
// Note that you DO NOT need to forward declare a specialization. E.g. if
// foo.h says "template <class T> void foo(T *);" and foo.cxx specializes
// void foo<int>(int *), the client doesn't need to know that the template
// symbol he links against is a specialization.

//#define VCL_DEFINE_SPECIALIZATION /* template <> */
//#define VCL_DEFINE_SPECIALIZATION template <>
#if defined(__ICL)
# define VCL_DEFINE_SPECIALIZATION /* template <> */
#else
# define VCL_DEFINE_SPECIALIZATION template <>
#endif


//: VCL_CANNOT_SPECIALIZE_CV
//
// Template specialization considers top-level cv-qualifiers of the
// argument type.  For example, A<int> and A<int const> are distinct
// types.  Some compilers (eg Borland 5.5) do not make this distinction.
// Specializations using top-level cv-qualifiers should not be defined
// in addition to the unqualified equivalents unless
// VCL_CANNOT_SPECIALIZE_CV is false.
//#define VCL_CANNOT_SPECIALIZE_CV 1 /* cannot specialize with cv-qualifiers */
//#define VCL_CANNOT_SPECIALIZE_CV 0 /* can specialize with cv-qualifiers */
#define VCL_CANNOT_SPECIALIZE_CV 0


//: VCL_TEMPLATE_MATCHES_TOO_OFTEN
//
// A function template is selected by overload resolution only if no
// non-template requires equal or better conversions.  Some compilers
// (eg MSVC 6.x and 7.0, Borland 5.5 and 5.6) select the template
// incorrectly in a case like this:
// \code
//    class A {};
//    template <class T> void f(T);
//    void f(const A&);
//    void g() { f(A()); } // should call non-template
// \endcode
//
// The work-around is to explicitly give the template a worse
// conversion than the non-templated overloads:
// \code
//    class A {};
//    template <class T> inline void f(T t) { f(t, 1); }
//    template <class T> void f(T t, long);
//    void f(const A&, int);
//    void g() { f(A()); } // will call non-template
// \endcode
// In this example, the inline one-argument template will always be
// called, which will call the real function with an "int" passed to
// the second argument.  The templated two-argument function has a
// "long" second argument while the others have "int".  Therefore, the
// template will be chosen only if no non-templates match.
//
// The VCL_TEMPLATE_MATCHES_TOO_OFTEN macro is set to 1
// if this work-around is required and 0 otherwise.
//#define VCL_TEMPLATE_MATCHES_TOO_OFTEN 1 /* need work-around */
//#define VCL_TEMPLATE_MATCHES_TOO_OFTEN 0 /* do not need it */
// !!! different from VC71
#define VCL_TEMPLATE_MATCHES_TOO_OFTEN 1


//: VCL_HAS_SLICED_DESTRUCTOR_BUG
//
// Consider this example code that creates a temporary in the call to f:
// \code
//  struct A { A(); A(const A&); ~A(); };
//  struct B: public A { B(); B(const B& b); ~B(); };
//  struct C { operator B(); };
//  void f(A);
//  void g(C c) { f(c); } // fails to call ~B() on 2nd temporary B
// \endcode
// Compilers will call c.operator B() to implement the conversion
// necessary to call f(c).  Some compilers will then create a
// temporary A by copy-constructing the temporary B to bind the
// argument of f.  Others will create a second temporary B by
// copy-constructing the first temporary B and bind the A-portion of
// the object to the argument of f.  Some compilers (at least Intel
// C++ 7.0 and 7.1) will create a second temporary B but forget to
// call ~B() when destroying it.  This can cause resource leaks.
//
// The VCL_HAS_SLICED_DESTRUCTOR_BUG is set to 1 if this bug exists in
// the compiler and 0 otherwise.
//#define VCL_HAS_SLICED_DESTRUCTOR_BUG 1 /* bug exists */
//#define VCL_HAS_SLICED_DESTRUCTOR_BUG 0 /* bug does not exist */
#define VCL_HAS_SLICED_DESTRUCTOR_BUG 0


//: VCL_NULL_TMPL_ARGS
//
// Define to <> for compilers that require them in friend template function
// declarations (i.e., EGCS, VC C++.NET 2003).

//#define VCL_NULL_TMPL_ARGS /* <> */
//#define VCL_NULL_TMPL_ARGS <>
// !!! different from VC71
#define VCL_NULL_TMPL_ARGS /* <> */


//----------------------------------------------------------------------
// template instantiation

//: VCL_ALLOWS_INLINE_INSTANTIATION
//
// True if the compiler allows explicit instantiation of inline
// function templates. The native SGI CC 7.2.1 does not.
#define VCL_ALLOWS_INLINE_INSTANTIATION 1


//: VCL_NEEDS_INLINE_INSTANTIATION
//
// True if the compiler needs explicit instantiation of inline
// function templates. gcc 2.7.2 (with -fno-implicit-templates) does.
#define VCL_NEEDS_INLINE_INSTANTIATION 0


//: VCL_DO_NOT_INSTANTIATE(text, ret)
//
// If a method is defined on some template, but makes no sense for some
// instances of that template, the compiler should not complain unless the
// method is actually used.  For example
// \code
//     template <class T>
//     class T {
//       int bad_method() {
//         return T::f();  // Requires T to have static method f
//       }
//     };
// \endcode
//
// The language allows you to use a T<int> even though int::f() is garbage,
// *providing* you never call T.bad_method().
//
// Most compilers don't implement that yet, so the solution is to provide a
// dummy specialization of T::bad_method that returns something mundane and
// stops the standard bad_method from being generated.  For this, use:
// \code
//     VCL_DO_NOT_INSTANTIATE(int T::bad_method(), some_return_value)
// \endcode
// if the function is void, use VCL_VOID_RETURN as the return value

//#define VCL_DO_NOT_INSTANTIATE(text, ret) text { return ret; }
//#define VCL_DO_NOT_INSTANTIATE(text, ret) template <> text { return ret; }
//#define VCL_DO_NOT_INSTANTIATE(text, ret) /* no need -- magic compiler */
//FIXME #define VCL_DO_NOT_INSTANTIATE(text, ret)
#define VCL_DO_NOT_INSTANTIATE(text, ret) \
VCL_DEFINE_SPECIALIZATION \
text { return ret; }


//: VCL_UNINSTANTIATE_SPECIALIZATION(symbol)
//
// OK, various compilers do various silly things about instantiation of
// functions/methods that have been specialized.  Use this macro to tell
// the compiler not to generate code for methods which have been specialized
// \code
//      VCL_UNINSTANTIATE_SPECIALIZATION(int T::specialized_method())
// \endcode
// It should be placed after the "template class A<T>;"

//#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol)  @pragma do_not_instantiate text@
//#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol) /* no need - sensible compiler */
//FIXME #define VCL_UNINSTANTIATE_SPECIALIZATION(symbol)
#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol) // which compiler needs this ?


//: VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol)
//
// gcc is sensible about specializations if it has seen the definition,
// but if it's in another file, need to use extern to tell it.
// \code
//      VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(int T::specialized_method())
// \endcode
// It should be placed before the "template class A<T>;"

//#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) extern symbol;
//#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) /* no need */
//FIXME #define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol)
#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) /* never used */


//: VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol)
//
// Some compilers (e.g. gcc 2.7.2) do not accept a templated definition
// of static members, as in
// \code
//   template <class T>
//   struct A {
//     A() { }
//     static char *fmt;
//   };
//
//   template <class T>
//   char *A<T>::fmt = 0;
//
//   template struct A<int>;
// \endcode
// The way round this is to supply an explicit definition for every
// instance of the class needed.
//
// Put the templated definition like this
// \code
//      #if VCL_CAN_DO_STATIC_TEMPLATE_MEMBER
//      template <class T>
//      char *A<T>::fmt = 0;
//      #endif
// \endcode
// and place
// \code
//      VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(int A<int>::fmt = 0)
// \endcode
// before the
// \code
//      template class A<int>;
// \endcode
// with
// \code
//      VCL_UNINSTANTIATE_STATIC_TEMPLATE_MEMBER(A<int>::var)
// \endcode
// afterwards.

//#define VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) /* no need */
//#define VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) symbol;
//
#define VCL_CAN_DO_STATIC_TEMPLATE_MEMBER 1
#if VCL_CAN_DO_STATIC_TEMPLATE_MEMBER
# define VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol)   /* */
# define VCL_UNINSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) /* */
#else
# define VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) symbol;
# define VCL_UNINSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) // which compiler needs this ?
#endif


//: VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER
//
// Some compilers (e.g. SunPro 5.0) do not accept non-type template
// parameters in function templates. E.g.
// \code
// template <class T, int n> struct vicky { T data[n]; } // can do
//
// template <class T, int n>
// void a_function_template(vicky<T, n> const &) { ... } // cannot
// \endcode
#define VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER 1


//----------------------------------------------------------------------
// overload resolution problems.

//: VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD
//
// On some compilers (in particular gcc 2.7.2.3), the compiler doesn't
// know how to cast a templated derived class to a templated base class
// (eg. vnl_matrix_fixed<3,3,double> -> vnl_matrix<double>) when doing overload
// resolution. Making the overloaded function a friend of the class makes
// the problem go away.
//
// True if the compiler needs this hack.

//#define VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD 0
//#define VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD 1
#define VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD 0


//: VCL_OVERLOAD_CAST
//
// Some compilers (gcc 2.7.2.3 and SGI native 6.0) often won't perform
// certain implicit casts. E.g. casting a templated derived class to a
// templated base class (see above), or even realizing that
// "template void foo(float const * const *, float * const *, int, int)"
// can be called with parameters of type "(float **, float **, int, int)".
//
//   To fix the code, it is tempting to add an explicit cast and get on
// with things, but that would throw away the checking performed by more
// helpful compilers. Use VCL_OVERLOAD_CAST instead.

//#define VCL_OVERLOAD_CAST(T, x) ((T)(x))
//#define VCL_OVERLOAD_CAST(T, x) (x)
#define VCL_OVERLOAD_CAST(T, x) (x)


//----------------------------------------------------------------------
// stuff


//: VCL_NO_STATIC_DATA_MEMBERS
//
// True if compiler does not support static data members in template classes.
//
#define VCL_NO_STATIC_DATA_MEMBERS 0


//: VCL_HAS_TEMPLATE_SYMBOLS
//
// True if the compiler mangles function template instances differently
// from non-templated functions with the same name and signature.
// This is correct behaviour.
//
#define VCL_HAS_TEMPLATE_SYMBOLS 0


//----------------------------------------------------------------------
// default template arguments

//: VCL_DEFAULT_TMPL_ARG(arg)
//
// It is wrong to provide a default for a template parameter in two
// declarations in the same scope (14.1.12), e.g.
// \code
//   template <class S, class T = int> class X;
//   template <class S, class T = int> class X { /* ... */ };
// \endcode
// is wrong.
// However, some older compilers insist on seeing the default argument
// again when defining a class body or instantiating.
// To satisfy them, use this macro as follows :
// \code
//   template <class S, class T VCL_DEFAULT_TMPL_ARG(= int)> X { /* ... */ };
//   template X<double VCL_DEFAULT_TMPL_ARG(, int)>;
// \endcode
//
// It's possible we need two macros, one for redeclaration and
// one for instantiation.

//#define VCL_DEFAULT_TMPL_ARG(arg) /* no need */
//#define VCL_DEFAULT_TMPL_ARG(arg) arg
#define VCL_DEFAULT_TMPL_ARG(arg) 

//
#define VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER 1
#define VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER 1

// VCL_DFL_TYPE_PARAM_STLDECL(A, a) and VCL_DFL_TMPL_PARAM_STLDECL(A, a)
// EGCS doesn't like definition of default types, viz:
// \code
//   template <class A = default> class vector;
//   template <class A = default> class vector { ... };
// \endcode
// This macro is used to say "define if not previously defined, like
// \code
//   template <VCL_DFL_TYPE_PARAM_STLDECL(A,a)> class vector { ... };
// \endcode

//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) A = a
//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) A /* = a */
//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) __DFL_TYPE_PARAM(A,a)
//FIXME #define VCL_DFL_TYPE_PARAM_STLDECL(A,a)
//
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) A = a
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) A /* = a */
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) __STL_DFL_TMPL_PARAM(A,a)
//FIXME #define VCL_DFL_TMPL_PARAM_STLDECL(A,a)


// VCL_DFL_TMPL_ARG(class)
// Similarly, when instantiating a templated class with a default
// template argument, some compilers don't like the redeclaration of
// that argument, while others insist on it.
// In such cases, specify the default argument as follows:
// \code
//   template class vector <int VCL_DFL_TMPL_ARG(default_iterator) >;
// \endcode
// (Note the missing comma after int: it is inside the macro.)

//#define VCL_DFL_TMPL_ARG(classname) , classname
//#define VCL_DFL_TMPL_ARG(classname) /* , classname */
//#define VCL_DFL_TMPL_ARG(classname) __DFL_TMPL_ARG(classname)
//FIXME #define VCL_DFL_TMPL_ARG(classname)


//: VCL_SUNPRO_CLASS_SCOPE_HACK(A)
//
// Nice one.  Can't use std::vector<T> in a class on SunPro 5, must use
// std::vector<T, std::allocator<T> >.  Of course, we cannot expect that other
// compilers call the default allocator std::allocator<T>, so we must use
// a macro.  I could call it something generic, like
// VCL_CLASS_SCOPE_HACK, but to be honest, it's a sunpro problem,
// they deserve the blame.
// Usage (the comma is inside the macro) :
// \code
//    vector<T VCL_SUNPRO_CLASS_SCOPE_HACK(std::allocator<T >)>
// \endcode

//#define VCL_SUNPRO_CLASS_SCOPE_HACK(A) /* , A */
//#define VCL_SUNPRO_CLASS_SCOPE_HACK(A) , A
#define VCL_SUNPRO_CLASS_SCOPE_HACK(A) /* , A */


//----------------------------------------------------------------------
// exception and namespace issues


//: VCL_HAS_EXCEPTIONS
// Set to true if the compiler supports the use of exceptions.
#define VCL_HAS_EXCEPTIONS 1


//: VCL_HAS_NAMESPACES
// Set to true if the compiler supports the use of namespaces.
#define VCL_HAS_NAMESPACES 1


//: VCL_ALLOWS_NAMESPACE_STD
// Set to true if the compiler allows namespace std:: for the standard library.
// !!! different from VC7
#define VCL_ALLOWS_NAMESPACE_STD 0


//: VCL_NEEDS_NAMESPACE_STD
// Set to true if the compiler needs namespace std:: for the standard library.
// !!! different from VC7
#define VCL_NEEDS_NAMESPACE_STD 0


//----------------------------------------------------------------------
// infinity issues

//: VCL_NUMERIC_LIMITS_HAS_INFINITY
// Set to true if there is a numeric_limits and it reports having an floating point infinity.
#define VCL_NUMERIC_LIMITS_HAS_INFINITY 1

//: VCL_PROCESSOR_HAS_INFINITY
// Set to true if the processor really does have an infinity.
// Although this is strictly not a C++ issue, some platforms' versions of
// numeric_limits<double> imply that there is no infinity, when there is.
#define VCL_PROCESSOR_HAS_INFINITY 1

//----------------------------------------------------------------------
// signedness of char

//: VCL_CHAR_IS_SIGNED
// Set to true if the type "char" is signed.
#define VCL_CHAR_IS_SIGNED 1

//----------------------------------------------------------------------

// architecture macros removed -- they're not in the C++ standard

#endif // vcl_config_compiler_h_config_win32_vc60_
