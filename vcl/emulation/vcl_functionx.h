/*
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

#ifndef vcl_emulation_functionx_h
#define vcl_emulation_functionx_h

#include "vcl_functional.h"

// This header provides various non-standard functional extensions
// Some of them have to be specializations.


// Extension : void function

// used as adaptor's return/argument type,
// to allow binders/composers usage
# ifndef __VOID_TAG_DEFINED
# define __VOID_TAG_DEFINED
  struct __void_tag {};
# endif

template <class Result>
struct void_function {
    typedef Result   result_type;
    typedef __void_tag argument_type;
};

template <class Result>
class pointer_to_void_function : public vcl_unary_function<__void_tag,Result> {
protected:
    Result (*ptr)();
public:
    explicit pointer_to_void_function(Result (*x)()) : ptr(x) {}
    Result operator()(__void_tag = __void_tag()) const { return ptr(); }
};

// to feed composers
template <class Arg1>
struct projectvoid : public vcl_unary_function<Arg1,__void_tag> {
  result_type operator()(const Arg1& x) const { return result_type(); }
};

template <class Result>
pointer_to_void_function<Result> ptr_fun(Result (*x)()) {
    return pointer_to_void_function<Result>(x);
}

// generators binding

template <class Operation, class Generator>
class binder1st_gen :
public vcl_unary_function<typename __BINARY_ARG(Operation,second_argument_type),
                      typename __BINARY_ARG(Operation,result_type)> {
protected:
    Operation op;
    Generator gen;
public:
    binder1st_gen(const Operation& x, const Generator& y) : op(x), gen(y) {}
    result_type operator()(const argument_type& x) const {
        return op(gen(),x);
    }
};

template <class Operation,class Generator>
inline binder1st_gen<Operation, Generator>
bind1st_gen(const Operation& op, const Generator& gen)
{
    return binder1st_gen<Operation,Generator>(op,gen);
}

template <class Operation, class Generator>
class binder2nd_gen :
public vcl_unary_function<typename __BINARY_ARG(Operation,first_argument_type),
                      typename __BINARY_ARG(Operation,result_type)> {
protected:
    Operation op;
    Generator gen;
public:
    binder2nd_gen(const Operation& x, const Generator& y) : op(x), gen(y) {}
    result_type operator()(const argument_type& x) const {
        return op(x, gen());
    }
};

template <class Operation,class Generator>
inline binder2nd_gen<Operation, Generator>
bind2nd_gen(const Operation& op, const Generator& gen)
{
    return binder2nd_gen<Operation,Generator>(op,gen);
}

// 20.3.8  Adaptors for pointers to members [lib.member.pointer.adaptors]
// const versions for some compilers
// normally, you won't need them. Names are non-standard, so beware.

template <class Class, class Result>
class mem_fun_const_ref_t : public vcl_unary_function<const Class, Result> {
protected:
    typedef Result (Class::*fun_type)(void) const;
    fun_type ptr;
public:
    explicit mem_fun_const_ref_t(fun_type p) : ptr(p) {}
    Result operator()(const Class& x) const { return (x.*ptr)();}
};

template <class Class, class Arg, class Result>
class mem_fun1_const_ref_t: public vcl_binary_function<const Class, Arg, Result> {
public:
protected:
    typedef Result (Class::*fun_type)(Arg) const;
    fun_type ptr;
public:
    explicit mem_fun1_const_ref_t(fun_type p) : ptr(p) {}
    Result operator()(const Class& x, Arg a) const { return (x.*ptr)(a);}
};

template <class Class, class Result>
inline mem_fun_const_ref_t<Class, Result>
mem_fun_const_ref(Result (Class::*ptr)(void) const) {
    return mem_fun_const_ref_t<Class, Result>(ptr);
}

template <class Class, class Arg, class Result>
inline mem_fun1_const_ref_t<Class, Arg, Result>
mem_fun1_const_ref(Result (Class::*ptr)(Arg) const) {
    return mem_fun1_const_ref_t<Class, Arg, Result>(ptr);
}

// macros to declare functional objects for pointers to members
#define mem_fun_macro(Result,Class,Func) \
struct : public vcl_unary_function<Class*,Result> \
{ Result operator()(Class* obj) const { return obj->Func(); }}

#define mem_fun1_macro(Result,Class,Func,Param) \
struct : public vcl_binary_function<Class*, Param,Result>  \
{ Result operator()(Class* obj, Param p) const { return obj->Func(p); }}

#define mem_fun_ref_macro(Result,Class,Func) \
struct : public vcl_unary_function<Class,Result> \
{ Result operator()(Class& obj) const { return obj.Func(); }}

#define mem_fun1_ref_macro(Result,Class,Func,Param) \
struct : public vcl_binary_function<Class, Param,Result>  \
{ Result operator()(Class& obj, Param p) const { return obj.Func(p); }}

#endif // vcl_emulation_functionx_h
