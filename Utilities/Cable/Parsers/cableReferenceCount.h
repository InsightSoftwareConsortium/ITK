#ifndef _referenceCount_h
#define _referenceCount_h

#include "win32Header.h"

class Object;

/**
 * Define a smart pointer used for the reference counted Object class below.
 */
template <class T>
class SmartPointer
{
public:
  SmartPointer(): m_Pointer(0) {}
  SmartPointer(const SmartPointer& p): m_Pointer(p.m_Pointer)
    {
      this->Register();
    }

  SmartPointer(T* p): m_Pointer(p)
    {
      this->Register();
    }

  ~SmartPointer()
    {
      this->Unregister();
    }

  /**
   * Allow equality comparison with another pointer.
   */
  bool operator == (SmartPointer r) const { return (m_Pointer==r.m_Pointer); }

  /**
   * Allow equality comparison with another pointer.
   */
  bool operator == (T* r) const { return (m_Pointer==r); }
  
  /**
   * Allow inequality comparison with another pointer.
   */
  bool operator != (SmartPointer r) const { return (m_Pointer!=r.m_Pointer); }

  /**
   * Allow inequality comparison with another pointer.
   */
  bool operator != (T* r) const { return (m_Pointer!=r); }
  
  /**
   * Allow assignment.  Conversion operator below allows this to take
   * a normal pointer, or another smart pointer to T or a descendent of T.
   */
  SmartPointer& operator = (T* r)
    {
      if(m_Pointer != r)
        {
        this->Unregister();
        m_Pointer = r;
        this->Register();
        }
      return *this;
    }  

  /**
   * Allow access to members of T as if this were a normal pointer.
   */
  T* operator -> () const
    { 
    return m_Pointer; 
    }

  /**
   * Let smart pointer look like normal pointer to T.
   */
  operator T* () const
    {
      return m_Pointer;
    }

  /**
   * Allow read access to real pointer to make casting easier.
   */
  T* RealPointer(void) const
    {
      return m_Pointer;
    }

private:

  /**
   * Increment reference count of object.
   */
  void Register(void)
    {
      if(m_Pointer)
        {
        ((Object*)m_Pointer)->Register();
        }
    }
  
  /**
   * Decrement reference count of object.
   */
  void Unregister(void)
    {
      if(m_Pointer)
        {
        ((Object*)m_Pointer)->Unregister();
        }
    }
  
  T* m_Pointer;
};


/**
 * Define a reference-counted object type.
 * All classes derived from this must have protected constructors,
 * destructors, and assignment operators.  They must be created with
 * a static New() method.
 */
class Object
{
public:
  typedef Object                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Create a new reference counted simple object.
   */
  static Pointer New()
    {
      return new Self;
    }
  
  /**
   * Increment object's reference count.
   */
  void Register(void)
    {
      ++m_ReferenceCount;
      if(m_ReferenceCount < 0)
        {
        delete this;
        }
    }
  
  /**
   * Decrement object's reference count.
   */
  void Unregister(void)
    {
      --m_ReferenceCount;
      if(m_ReferenceCount < 0)
        {
        delete this;
        }
    }

protected:
  Object(): m_ReferenceCount(0) {}
  Object(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Object() {}
  
private:
  int m_ReferenceCount;
};

#endif
