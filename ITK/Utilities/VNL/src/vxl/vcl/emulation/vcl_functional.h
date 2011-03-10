#ifndef vcl_emulation_functional_h
#define vcl_emulation_functional_h
#define FUNCTION_H // why?

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Moscow Center for SPARC Technology
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Moscow Center for SPARC Technology makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#include <vcl_cstddef.h>
#include "vcl_bool.h"

#if 0
// fsm: these function templates are non-standard, or rather, the
// standard ones live in namespace std::rel_ops.
template <class T>
inline bool operator!=(const T& x, const T& y) { return !(x == y); }

template <class T>
inline bool operator>(const T& x, const T& y) { return y < x; }

template <class T>
inline bool operator<=(const T& x, const T& y) { return !(y < x); }

template <class T>
inline bool operator>=(const T& x, const T& y) { return !(x < y); }
#endif

template <class Arg, class Result>
struct vcl_unary_function
{
  typedef Arg argument_type;
  typedef Result result_type;
};

template <class Arg1, class Arg2, class Result>
struct vcl_binary_function
{
  typedef Arg1 first_argument_type;
  typedef Arg2 second_argument_type;
  typedef Result result_type;
};

template <class T>
struct vcl_plus : public vcl_binary_function<T, T, T>
{
  T operator()(const T& x, const T& y) const { return x + y; }
};

template <class T>
struct vcl_minus : public vcl_binary_function<T, T, T>
{
  T operator()(const T& x, const T& y) const { return x - y; }
};

template <class T>
struct vcl_multiplies : public vcl_binary_function<T, T, T>
{
  T operator()(const T& x, const T& y) const { return x * y; }
};

template <class T>
struct vcl_divides : public vcl_binary_function<T, T, T>
{
  T operator()(const T& x, const T& y) const { return x / y; }
};

template <class T>
struct vcl_modulus : public vcl_binary_function<T, T, T>
{
  T operator()(const T& x, const T& y) const { return x % y; }
};

template <class T>
struct vcl_negate : public vcl_unary_function<T, T>
{
  T operator()(const T& x) const { return -x; }
};

template <class T>
struct vcl_equal_to : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x == y; }
};

template <class T>
struct vcl_not_equal_to : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x != y; }
};

template <class T>
struct vcl_greater : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x > y; }
};

template <class T>
struct vcl_less : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x < y; }
};

template <class T>
struct vcl_greater_equal : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x >= y; }
};

template <class T>
struct vcl_less_equal : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x <= y; }
};

template <class T>
struct vcl_logical_and : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x && y; }
};

template <class T>
struct vcl_logical_or : public vcl_binary_function<T, T, bool>
{
  bool operator()(const T& x, const T& y) const { return x || y; }
};

template <class T>
struct vcl_logical_not : public vcl_unary_function<T, bool>
{
  bool operator()(const T& x) const { return !x; }
};

#  if defined (__STL_BASE_TYPEDEF_BUG)
// this workaround is needed for SunPro 4.0.1
// suggested by "Martin Abernethy" <gma@paston.co.uk>:

// We have to introduce the XXary_predicate_aux structures in order to
// access the argument and return types of predicate functions supplied
// as type parameters. SUN C++ 4.0.1 compiler gives errors for template type parameters
// of the form 'name1::name2', where name1 is itself a type parameter.

template <class Operation>
struct vcl__unary_fun_aux : private Operation
{
        typedef typename Operation::argument_type argument_type;
        typedef typename Operation::result_type result_type;
};

template <class Operation>
struct vcl__binary_fun_aux : private Operation
{
        typedef typename Operation::first_argument_type first_argument_type;
        typedef typename Operation::second_argument_type second_argument_type;
        typedef typename Operation::result_type result_type;
};

#  define __UNARY_ARG(Operation,type)  vcl__unary_fun_aux<Operation>::type
#  define __BINARY_ARG(Operation,type)  vcl__binary_fun_aux<Operation>::type
# else
#  define __UNARY_ARG(Operation,type)  Operation::type
#  define __BINARY_ARG(Operation,type) Operation::type
# endif

template <class Predicate>
class vcl_unary_negate : public vcl_unary_function<typename __UNARY_ARG(Predicate,argument_type), bool>
{
 protected:
  Predicate pred;
 public:
  explicit vcl_unary_negate(const Predicate& x) : pred(x) {}
  bool operator()(const argument_type& x) const { return !pred(x); }
};

template <class Predicate>
inline vcl_unary_negate<Predicate> not1(const Predicate& pred)
{
  return vcl_unary_negate<Predicate>(pred);
}

template <class Predicate>
class vcl_binary_negate
    : public vcl_binary_function<typename __BINARY_ARG(Predicate,first_argument_type),
                                 typename __BINARY_ARG(Predicate,second_argument_type),
                                 bool>
{
 protected:
  Predicate pred;
 public:
  explicit vcl_binary_negate(const Predicate& x) : pred(x) {}
  bool operator()(const first_argument_type& x,
                  const second_argument_type& y) const { return !pred(x, y); }
};

template <class Predicate>
inline vcl_binary_negate<Predicate> not2(const Predicate& pred)
{
  return vcl_binary_negate<Predicate>(pred);
}

template <class Operation>
class vcl_binder1st :
    public vcl_unary_function<typename __BINARY_ARG(Operation,second_argument_type),
                          typename __BINARY_ARG(Operation,result_type) >
{
 protected:
  Operation op;
  typename __BINARY_ARG(Operation,first_argument_type) value;
 public:
  vcl_binder1st(const Operation& x,
                const typename __BINARY_ARG(Operation,first_argument_type)& y)
      : op(x), value(y) {}
  typename result_type operator()(const argument_type& x) const { return op(value, x); }
};

template <class Operation, class T>
vcl_binder1st<Operation> bind1st(const Operation& op, const T& x)
{
  typedef typename __BINARY_ARG(Operation,first_argument_type) arg_type;
  return vcl_binder1st<Operation>(op, arg_type(x));
}

template <class Operation>
class vcl_binder2nd :
    public vcl_unary_function<typename __BINARY_ARG(Operation,first_argument_type),
                              typename __BINARY_ARG(Operation,result_type)>
{
 protected:
  Operation op;
  typename __BINARY_ARG(Operation,second_argument_type) value;
 public:
  vcl_binder2nd(const Operation& x,
                const typename __BINARY_ARG(Operation,second_argument_type)& y)
      : op(x), value(y) {}
  typename result_type operator()(const argument_type& x) const { return op(x, value); }
};

template <class Operation, class T>
vcl_binder2nd<Operation> bind2nd(const Operation& op, const T& x)
{
  typedef typename __BINARY_ARG(Operation,second_argument_type) arg_type;
  return vcl_binder2nd<Operation>(op, arg_type(x));
}

template <class Operation1, class Operation2>
class vcl_unary_compose :
    public vcl_unary_function<typename __UNARY_ARG(Operation2,argument_type),
                              typename __UNARY_ARG(Operation1,result_type)>
{
 protected:
  Operation1 op1;
  Operation2 op2;
 public:
  vcl_unary_compose(const Operation1& x, const Operation2& y) : op1(x), op2(y) {}
  typename __UNARY_ARG(Operation1,result_type)
  operator()(const typename __UNARY_ARG(Operation2,argument_type)& x) const { return op1(op2(x)); }
};

template <class Operation1, class Operation2>
inline vcl_unary_compose<Operation1, Operation2> compose1(const Operation1& op1,
                                                          const Operation2& op2)
{
  return vcl_unary_compose<Operation1, Operation2>(op1, op2);
}

template <class Operation1, class Operation2, class Operation3>
class vcl_binary_compose :
    public vcl_unary_function<typename __UNARY_ARG(Operation2,argument_type),
                              typename __BINARY_ARG(Operation1,result_type)>
{
 protected:
  Operation1 op1;
  Operation2 op2;
  Operation3 op3;
 public:
  vcl_binary_compose(const Operation1& x, const Operation2& y,
                     const Operation3& z) : op1(x), op2(y), op3(z) { }
  typename __BINARY_ARG(Operation1,result_type)
  operator()(const typename __UNARY_ARG(Operation2,argument_type)& x) const { return op1(op2(x), op3(x)); }
};

template <class Operation1, class Operation2, class Operation3>
inline vcl_binary_compose<Operation1, Operation2, Operation3>
compose2(const Operation1& op1, const Operation2& op2, const Operation3& op3)
{
  return vcl_binary_compose<Operation1, Operation2, Operation3>(op1, op2, op3);
}

template <class Arg, class Result>
class vcl_pointer_to_unary_function : public vcl_unary_function<Arg, Result>
{
 protected:
  Result (*ptr)(Arg);
 public:
  vcl_pointer_to_unary_function() {}
  explicit vcl_pointer_to_unary_function(Result (*x)(Arg)) : ptr(x) {}
  Result operator()(Arg x) const { return ptr(x); }
};

template <class Arg, class Result>
inline vcl_pointer_to_unary_function<Arg, Result> ptr_fun(Result (*x)(Arg))
{
  return vcl_pointer_to_unary_function<Arg, Result>(x);
}

template <class Arg1, class Arg2, class Result>
class vcl_pointer_to_binary_function : public vcl_binary_function<Arg1, Arg2, Result>
{
 protected:
  Result (*ptr)(Arg1, Arg2);
 public:
  vcl_pointer_to_binary_function() {}
  explicit vcl_pointer_to_binary_function(Result (*x)(Arg1, Arg2)) : ptr(x) {}
  Result operator()(Arg1 x, Arg2 y) const { return ptr(x, y); }
};

template <class Arg1, class Arg2, class Result>
inline vcl_pointer_to_binary_function<Arg1, Arg2, Result>
ptr_fun(Result (*x)(Arg1, Arg2))
{
    return vcl_pointer_to_binary_function<Arg1, Arg2, Result>(x);
}

template <class T>
struct vcl_identity : public vcl_unary_function<T, T>
{
 public:
  const T& operator()(const T& x) const { return x; }
};

template <class Pair>
struct vcl_select1st : public vcl_unary_function<Pair, typename Pair::first_type>
{
  const typename Pair::first_type& operator()(const Pair& x) const { return x.first; }
};

template <class Pair>
struct vcl_select2nd : public vcl_unary_function<Pair, typename Pair::second_type>
{
  const typename Pair::second_type& operator()(const Pair& x) const { return x.second; }
};

template <class Arg1, class Arg2>
struct vcl_project1st : public vcl_binary_function<Arg1, Arg2, Arg1>
{
  Arg1 operator()(const Arg1& x, const Arg2&) const { return x; }
};

template <class Arg1, class Arg2>
struct vcl_project2nd : public vcl_binary_function<Arg1, Arg2, Arg2>
{
  Arg2 operator()(const Arg1&, const Arg2& y) const { return y; }
};

//  SGI extension (constant functions)

template <class Result>
struct vcl_constant_void_fun
{
  typedef Result result_type;
  result_type val;
  vcl_constant_void_fun(const result_type& v) : val(v) {}
  const result_type& operator()() const { return val; }
};

template <class Result, VCL_DFL_TMPL_PARAM_STLDECL(Argument, Result) >
struct vcl_constant_unary_fun : public vcl_unary_function<Argument, Result>
{
# if defined (__STL_BASE_TYPEDEF_BUG)
  typedef vcl_unary_function<Argument, Result> super;
  typedef typename super::result_type result_type;
  typedef typename super::argument_type argument_type;
#  endif
  result_type val;
  vcl_constant_unary_fun(const result_type& v) : val(v) {}
  const result_type& operator()(const argument_type&) const { return val; }
};

template <class Result, VCL_DFL_TMPL_PARAM_STLDECL(Arg1,Result), VCL_DFL_TMPL_PARAM_STLDECL(Arg2,Arg1) >
struct vcl_constant_binary_fun : public vcl_binary_function<Arg1, Arg2, Result>
{
# if defined (__STL_BASE_TYPEDEF_BUG)
  typedef vcl_binary_function<Arg1, Arg2, Result> super;
  typedef typename super::result_type result_type;
  typedef typename super::first_argument_type first_argument_type;
  typedef typename super::second_argument_type second_argument_type;
#  endif
  result_type val;
  vcl_constant_binary_fun(const result_type& v) : val(v) {}
  const result_type& operator()(const first_argument_type&,
                                const second_argument_type&) const { return val; }
};

template <class Result>
inline vcl_constant_void_fun<Result> constant0(const Result& val)
{
  return vcl_constant_void_fun<Result>(val);
}

template <class Result>
inline vcl_constant_unary_fun<Result VCL_DFL_TMPL_ARG(Result) > constant1(const Result& val)
{
  return vcl_constant_unary_fun<Result, Result>(val);
}

template <class Result>
inline vcl_constant_binary_fun<Result
  VCL_DFL_TMPL_ARG(Result) VCL_DFL_TMPL_ARG(Result) > constant2(const Result& val)
{
  return vcl_constant_binary_fun<Result VCL_DFL_TMPL_ARG(Result) VCL_DFL_TMPL_ARG(Result)  >(val);
}

//  SGI extension (subtractive range)

// Note: this code assumes that T is 32-bit unsigned integer.
template < class T >
class vcl__subtractive_rng_t : public vcl_unary_function<T, T>
{
 private:
  T table[55];
  vcl_size_t index1;
  vcl_size_t index2;
 public:
  vcl__subtractive_rng_t(T seed) { initialize(seed); }
  vcl__subtractive_rng_t() { initialize(161803398u); }

  T operator()(T limit)
  {
    index1 = (index1 + 1) % 55;
    index2 = (index2 + 1) % 55;
    table[index1] = table[index1] - table[index2];
    return table[index1] % limit;
  }
  inline void initialize(T seed);
};

template <class T>
void vcl__subtractive_rng_t<T>::initialize(T seed)
{
  T k = 1;
  table[54] = seed;
  vcl_size_t i;
  for (i = 0; i < 54; i++)
  {
    vcl_size_t ii = (21 * (i + 1) % 55) - 1;
    table[ii] = k;
    k = seed - k;
    seed = table[ii];
  }
  for (int loop = 0; loop < 4; loop++)
    for (i = 0; i < 55; i++)
      table[i] = table[i] - table[(1 + i + 30) % 55];
  index1 = 0;
  index2 = 31;
}

typedef vcl__subtractive_rng_t<__STL_UINT32_T> vcl_subtractive_rng;


// 20.3.8  Adaptors for pointers to members [lib.member.pointer.adaptors]

// vcl_mem_fun_t calls the member vcl_function it is  initialized  with  given  a
// pointer argument.
template <class Class, class Result>
class vcl_mem_fun_t : public vcl_unary_function<Class*, Result>
{
 protected:
  typedef Result (Class::*fun_type)(void);
  fun_type ptr;
 public:
  vcl_mem_fun_t() {}
  explicit vcl_mem_fun_t(fun_type p) : ptr(p) {}
  Result operator()(Class* x) const { return (x->*ptr)();}
};

//   vcl_mem_fun1_t  calls  the  member vcl_function it is initialized with given a
//   pointer argument and an additional argument of the appropriate type.
template <class Class, class Arg, class Result>
class vcl_mem_fun1_t: public vcl_binary_function<Class*, Arg, Result>
{
 protected:
  typedef Result (Class::*fun_type)(Arg);
  fun_type ptr;
 public:
  vcl_mem_fun1_t() {}
  explicit vcl_mem_fun1_t(fun_type p) : ptr(p) {}
  Result operator()(Class* x, Arg a) const { return (x->*ptr)(a);}
};

// vcl_mem_fun_ref_t calls the member vcl_function it is initialized with given a
// reference argument.
template <class Class, class Result>
class vcl_mem_fun_ref_t : public vcl_unary_function<Class, Result>
{
 protected:
  typedef Result (Class::*fun_type)(void);
  fun_type ptr;
 public:
  vcl_mem_fun_ref_t() {}
  explicit vcl_mem_fun_ref_t(fun_type p) : ptr(p) {}
  Result operator()(Class& x) const { return (x.*ptr)();}
};

// vcl_mem_fun1_ref_t  calls the member vcl_function it is initialized with given
// a reference argument and an additional  argument  of  the  appropriate
// type.
template <class Class, class Arg, class Result>
class vcl_mem_fun1_ref_t: public vcl_binary_function<Class, Arg, Result>
{
 protected:
  typedef Result (Class::*fun_type)(Arg);
  fun_type ptr;
 public:
  vcl_mem_fun1_ref_t() {}
  explicit vcl_mem_fun1_ref_t(fun_type p) : ptr(p) {}
  Result operator()(Class& x, Arg a) const { return (x.*ptr)(a);}
};

# if !defined (__STL_MEMBER_POINTER_PARAM_BUG)
//  mem_fun(&X::f) returns an object through  which  X::f  can  be  called
//  given  a  pointer  to an X followed by the argument required for f (if
//  any).
template <class Class, class Result>
inline vcl_mem_fun_t <Class, Result>
mem_fun(Result (Class::*ptr)(void))
{
  return vcl_mem_fun_t<Class, Result>(ptr);
}

template <class Class, class Arg, class Result>
inline vcl_mem_fun1_t <Class, Arg, Result>
mem_fun1(Result (Class::*ptr)(Arg))
{
  return vcl_mem_fun1_t<Class, Arg, Result>(ptr);
}

//  mem_fun_ref(&X::f)  returns an object through which X::f can be called
//  given a reference to an X followed by the argument required for f  (if
//  any).
template <class Class, class Result>
inline vcl_mem_fun_ref_t<Class, Result>
mem_fun_ref(Result (Class::*ptr)(void))
{
  return vcl_mem_fun_ref_t<Class, Result>(ptr);
}

template <class Class, class Arg, class Result>
inline vcl_mem_fun1_ref_t<Class, Arg, Result>
mem_fun1_ref(Result (Class::*ptr)(Arg))
{
  return vcl_mem_fun1_ref_t<Class, Arg, Result>(ptr);
}

# endif

#endif // vcl_emulation_functional_h
