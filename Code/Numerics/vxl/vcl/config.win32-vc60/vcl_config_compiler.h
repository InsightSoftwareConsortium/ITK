#ifndef vcl_config_compiler_h_config_win32_vc60_
#define vcl_config_compiler_h_config_win32_vc60_

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


//: VCL_DEFAULT_VALUE(x)
//
// Used to provide default values for function args in definition
// Some compilers (GCC272) require defaults in template function definitions
// Other compilers (VC50) disallow defaults in both decls and defs

//#define VCL_DEFAULT_VALUE(x) /* no need */
//#define VCL_DEFAULT_VALUE(x) = x
#define VCL_DEFAULT_VALUE(x) /* no need */


//----------------------------------------------------------------------
// problems with static members.

//: VCL_STATIC_CONST_INIT_INT(x)
//
// ANSI allows
// \verbatim
//     class A {
//       static const int x = 27;
//     };
// \endverbatim
// And there is a speed advantage, so we want to use it where supported.
// The macro is used like this:
// \verbatim
//       static const int x VCL_STATIC_CONST_INIT_INT(27);
// \endverbatim

//#define VCL_STATIC_CONST_INIT_INT(x) /* not allowed */
//#define VCL_STATIC_CONST_INIT_INT(x) = x
#define VCL_STATIC_CONST_INIT_INT(x) /* = x */


//: VCL_STATIC_CONST_INIT_FLOAT(x)
//
// GCC allows the above, but with floating point types, ANSI doesn't.
// Again, we'll use it if we've got it.

//#define VCL_STATIC_CONST_INIT_FLOAT(x) /* not allowed */
//#define VCL_STATIC_CONST_INIT_FLOAT(x) = x
#define VCL_STATIC_CONST_INIT_FLOAT(x) /* = x */


// VCL_IMPLEMENT_STATIC_CONSTS
//
// True if static consts must be defined in some source file.  I don't know
// what ANSI has to say about this, but anyway, the above example needs this
// in some .C file:
// \verbatim
//     #if VCL_IMPLEMENT_STATIC_CONSTS
//     const int A::x = 27;
//     #endif
// \endverbatim
//#define VCL_IMPLEMENT_STATIC_CONSTS @VCL_IMPLEMENT_STATIC_CONSTS@


//----------------------------------------------------------------------
// various template issues.

//: VCL_HAS_MEMBER_TEMPLATES
//
// True if the compiler supports template members of template classes.  e.g.
// \verbatim
//     template <class U> class A {
//       template <class V> void f(V);
//     }
// \endverbatim
#define VCL_HAS_MEMBER_TEMPLATES 1


//: VCL_CAN_DO_PARTIAL_SPECIALIZATION
//
// True if the compiler supports partial specializations of templates. e.g.
// \verbatim
// \endverbatim
// template <class T>
// class vector<T*> : public vector<void *> { .. inline methods .. };
// \endverbatim
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
#define VCL_DEFINE_SPECIALIZATION /* template <> */
#else
#define VCL_DEFINE_SPECIALIZATION template <>
#endif


//: VCL_NULL_TMPL_ARGS
//
// Define to <> for compilers that require them in friend template function
// declarations (i.e., EGCS).

//#define VCL_NULL_TMPL_ARGS /* <> */
//#define VCL_NULL_TMPL_ARGS <>
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
// \verbatim
// \endverbatim
//     template <class T>
//     class T {
//       int bad_method() {
//         return T::f();  // Requires T to have static method f
//       }
//     };
// \endverbatim
//
// The language allows you to use a T<int> even though int::f() is garbage,
// *providing* you never call T.bad_method().
//
// Most compilers don't implement that yet, so the solution is to provide a
// dummy specialization of T::bad_method that returns something mundane and
// stops the standard bad_method from being generated.  For this, use:
//     VCL_DO_NOT_INSTANTIATE(int T::bad_method(), some_return_value)
// if the function is void, use VCL_VOID_RETURN as the return value

//#define VCL_DO_NOT_INSTANTIATE(text, ret) text { return ret; }
//#define VCL_DO_NOT_INSTANTIATE(text, ret) template <> text { return ret; }
//#define VCL_DO_NOT_INSTANTIATE(text, ret) /* no need -- magic compiler */
//FIXME #define VCL_DO_NOT_INSTANTIATE(text, ret) @VCL_DO_NOT_INSTANTIATE@
#define VCL_DO_NOT_INSTANTIATE(text, ret) \
VCL_DEFINE_SPECIALIZATION \
text { return ret; }


//: VCL_UNINSTANTIATE_SPECIALIZATION(symbol)
//
// OK, various compilers do various silly things about instantiation of
// functions/methods that have been specialized.  Use this macro to tell
// the compiler not to generate code for methods which have been specialized
// \verbatim
// \endverbatim
//      VCL_UNINSTANTIATE_SPECIALIZATION(int T::specialized_method())
// \endverbatim
// It should be placed after the "template class A<T>;"

//#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol)  @pragma do_not_instantiate text@
//#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol) /* no need - sensible compiler */
//FIXME #define VCL_UNINSTANTIATE_SPECIALIZATION(symbol) @VCL_UNINSTANTIATE_SPECIALIZATION@
#define VCL_UNINSTANTIATE_SPECIALIZATION(symbol) /* which compiler needs this ? */


//: VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol)
//
// gcc is sensible about specializations if it has seen the definition,
// but if it's in another file, need to use extern to tell it.
// \verbatim
//      VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(int T::specialized_method())
// \endverbatim
// It should be placed before the "template class A<T>;"

//#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) extern symbol;
//#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) /* no need */
//FIXME #define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) @VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION@
#define VCL_UNINSTANTIATE_UNSEEN_SPECIALIZATION(symbol) /* never used */


//: VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol)
//
// Some compilers (e.g. gcc 2.7.2) do not accept a templated definition
// of static members, as in
// \verbatim
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
// \endverbatim
//
// The way round this is to supply an explicit definition for every
// instance of the class needed.
//
// \verbatim
// Put the templated definition like this
//      #if VCL_CAN_DO_STATIC_TEMPLATE_MEMBER
//      template <class T>
//      char *A<T>::fmt = 0;
//      #endif
// \endverbatim
// and place
// \verbatim
//      VCL_INSTANTIATE_STATIC_TEMPLATE_MEMBER(int A<int>::fmt = 0)
// \endverbatim
// before the
// \verbatim
//      template class A<int>;
// \endverbatim
// with
// \verbatim
//      VCL_UNINSTANTIATE_STATIC_TEMPLATE_MEMBER(A<int>::var)
// \endverbatim
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
# define VCL_UNINSTANTIATE_STATIC_TEMPLATE_MEMBER(symbol) /* which compiler needs this ? */
#endif


//: VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER
//
// Some compilers (e.g. SunPro 5.0) do not accept non-type template
// parameters in function templates. E.g.
//
// \verbatim
// template <class T, int n> struct vicky { T data[n]; } // can do
//
// template <class T, int n>
// void a_function_template(vicky<T, n> const &) { ... } // cannot
// \endverbatim
//
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
// Set if compiler does not support static data members in template classes.
// Uses value determined for STL

//#if (__STL_STATIC_TEMPLATE_DATA < 1)
//#define VCL_NO_STATIC_DATA_MEMBERS 1
//#endif
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
// \verbatim
//   template <class S, class T = int> class X;
//   template <class S, class T = int> class X { /* ... */ };
// \endverbatim
// is wrong.
// However, some older compilers insist on seeing the default argument
// again when defining a class body or instantiating.
// To satisfy them, use this macro as follows :
// \verbatim
//   template <class S, class T VCL_DEFAULT_TMPL_ARG(= int)> X { /* ... */ };
//   template X<double VCL_DEFAULT_TMPL_ARG(, int)>;
// \endverbatim
//
// It's possible we need two macros, one for redeclaration and
// one for instantiation.

//#define VCL_DEFAULT_TMPL_ARG(arg) /* no need */
//#define VCL_DEFAULT_TMPL_ARG(arg) arg
#define VCL_DEFAULT_TMPL_ARG(arg) @VCL_DEFAULT_TMPL_ARG@

//
#define VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER 1
#define VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER 1

// VCL_DFL_TYPE_PARAM_STLDECL(A, a) and VCL_DFL_TMPL_PARAM_STLDECL(A, a)
// EGCS doesn't like definition of default types, viz:
//   template <class A = default> class vector;
//   template <class A = default> class vector { ... };
// This macro is used to say "define if not previously defined, like
//   template <VCL_DFL_TYPE_PARAM_STLDECL(A,a)> class vector { ... };

//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) A = a
//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) A /* = a */
//#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) __DFL_TYPE_PARAM(A,a)
//FIXME #define VCL_DFL_TYPE_PARAM_STLDECL(A,a) @VCL_DFL_TYPE_PARAM_STLDECL@
//
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) A = a
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) A /* = a */
//#define VCL_DFL_TMPL_PARAM_STLDECL(A,a) __STL_DFL_TMPL_PARAM(A,a)
//FIXME #define VCL_DFL_TMPL_PARAM_STLDECL(A,a) @VCL_DFL_TMPL_PARAM_STLDECL@


// VCL_DFL_TMPL_ARG(class)
// Similarly, when instantiating a templated class with a default
// template argument, some compilers don't like the redeclaration of
// that argument, while others insist on it.
// In such cases, specify the default argument as follows:
//   template class vector <int VCL_DFL_TMPL_ARG(default_iterator) >;
// (Note the missing comma after int: it is inside the macro.)

//#define VCL_DFL_TMPL_ARG(classname) , classname
//#define VCL_DFL_TMPL_ARG(classname) /* , classname */
//#define VCL_DFL_TMPL_ARG(classname) __DFL_TMPL_ARG(classname)
//FIXME #define VCL_DFL_TMPL_ARG(classname) @VCL_DFL_TMPL_ARG@


//: VCL_SUNPRO_CLASS_SCOPE_HACK(A)
//
// Nice one.  Can't use std::vector<T> in a class on SunPro 5, must use
// std::vector<T, std::allocator<T> >.  Of course, we cannot expect that other
// compilers call the default allocator std::allocator<T>, so we must use
// a macro.  I could call it something generic, like
// VCL_CLASS_SCOPE_HACK, but to be honest, it's a sunpro problem,
// they deserve the blame.
// Usage (the comma is inside the macro) :
// \verbatim
//    vector<T VCL_SUNPRO_CLASS_SCOPE_HACK(std::allocator<T >)>
// \endverbatim

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
#define VCL_ALLOWS_NAMESPACE_STD 0


//: VCL_NEEDS_NAMESPACE_STD
// Set to true if the compiler needs namespace std:: for the standard library.
#define VCL_NEEDS_NAMESPACE_STD 0

//----------------------------------------------------------------------
// architecture issues, like endianness.

//: VCL_LITTLE_ENDIAN
// Set to 0,1 or 1,0 respectively, dependending on the architecture.
#define VCL_LITTLE_ENDIAN 1
//: VCL_BIG_ENDIAN
// Set to 0,1 or 1,0 respectively, dependending on the architecture.
#define VCL_BIG_ENDIAN    0


//: VCL_SIZEOF_type
// These are useful to have as macros since you can't use the
// preprocessor to conditionalize on the size of a data type.
//
//                                                      typical value
//#define VCL_SIZEOF_bool     @VCL_SIZEOF_bool@         /* 4 */
//#define VCL_SIZEOF_char     @VCL_SIZEOF_char@         /* 1 */
//#define VCL_SIZEOF_short    @VCL_SIZEOF_short@        /* 2 */
//#define VCL_SIZEOF_int      @VCL_SIZEOF_int@          /* 4 */
//#define VCL_SIZEOF_long     @VCL_SIZEOF_long@         /* 4 */
//#define VCL_SIZEOF_float    @VCL_SIZEOF_float@        /* 4 */
//#define VCL_SIZEOF_double   @VCL_SIZEOF_double@       /* 8 */
//#define VCL_SIZEOF_void_ptr @VCL_SIZEOF_void_ptr@     /* 4 */

//: VCL_ALIGNMENT_type
// Alignment requirements of various types.             typical value
//#define VCL_ALIGNMENT_bool     @VCL_ALIGNMENT_bool@       /* 4 */
//#define VCL_ALIGNMENT_char     @VCL_ALIGNMENT_char@       /* 1 */
//#define VCL_ALIGNMENT_short    @VCL_ALIGNMENT_short@      /* 4 */
//#define VCL_ALIGNMENT_int      @VCL_ALIGNMENT_int@        /* 4 */
//#define VCL_ALIGNMENT_long     @VCL_ALIGNMENT_long@       /* 4 */
//#define VCL_ALIGNMENT_float    @VCL_ALIGNMENT_float@      /* 4 */
//#define VCL_ALIGNMENT_double   @VCL_ALIGNMENT_double@     /* 8 */
//#define VCL_ALIGNMENT_void_ptr @VCL_ALIGNMENT_void_ptr@   /* 4 */

//--------------------------------------------------------------------------------

#endif // vcl_config_compiler_h_config_win32_vc60_
