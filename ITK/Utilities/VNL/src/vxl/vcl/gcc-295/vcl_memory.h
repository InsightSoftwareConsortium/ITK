#ifndef vcl_gcc_295_memory_h_
#define vcl_gcc_295_memory_h_

#include "../iso/vcl_memory.h"

#undef vcl_auto_ptr

// gcc 2.95 does not provide a correct implementation of
// auto_ptr.  This implementation is copied from Brad King's
// version in vcl/win32-vc60 -- Ian Scott

// C++98 Standard Section 20.4.5 - Template class auto_ptr.
template <class X>
class vcl_auto_ptr
{
  template <class Y> struct auto_ptr_ref
  {
    vcl_auto_ptr<Y>& p_;
    auto_ptr_ref(vcl_auto_ptr<Y>& p): p_(p) {}
  };
  X* x_;
public:
  typedef X element_type;

  template <class Y>
  vcl_auto_ptr(vcl_auto_ptr<Y>& a) throw(): x_(a.release()) {}
  template <class Y>
  vcl_auto_ptr& operator=(vcl_auto_ptr<Y>& a) throw()
    { reset(a.release()); return *this; }

  explicit vcl_auto_ptr(X* p=0) throw(): x_(p) {}
  vcl_auto_ptr(vcl_auto_ptr& a) throw(): x_(a.release()) {}
  vcl_auto_ptr& operator=(vcl_auto_ptr& a) throw() { reset(a.release()); return *this; }
  ~vcl_auto_ptr() throw() { delete get(); }

  X& operator*() const throw() { return *get(); }
  X* operator->() const throw() { return get(); }
  X* get() const throw() { return x_; }
  X* release() throw() { X* x = x_; x_ = 0; return x; }
  void reset(X* p=0) throw() { if(get() != p) { delete get(); x_ = p; } }

  vcl_auto_ptr(auto_ptr_ref<X> r) throw(): x_(r.p_.release()) {}
  template <class Y> operator auto_ptr_ref<Y>() throw() { return *this; }
  template <class Y> operator vcl_auto_ptr<Y>() throw() { return release(); }
  vcl_auto_ptr& operator=(auto_ptr_ref<X> r) throw() { x_ = r.p_.release(); return *this; }
};

#endif
