#ifndef ITK_SMART_POINTER_H
#define ITK_SMART_POINTER_H

// Insert block comment here Copyright, doxegen stuff

// To compile / test this class
// Windows: cl itkSmartPointerTest.cxx; .\itkSmartPointerTest.exe
// linux:   c++ itkSmartPointerTest.cxx ./a.out
// other:   CCcompiler itkSmartPointerTest.cxx  ./a.out

template <class T>
class itkSmartPointer 
{
public:

  itkSmartPointer () 
  {
    m_Pointer = 0;
  }

  itkSmartPointer (const itkSmartPointer<T> &p)
  { 
    m_Pointer = p.m_Pointer; 
    this->Register(); 
  }
  
  itkSmartPointer (T *p)
  { 
    m_Pointer = p; 
    this->Register(); 
  }				

  ~itkSmartPointer ()
  {
    this->UnRegister();
  }

  T *operator -> () const
  { 
    return m_Pointer; 
  }

  T *ptr () const 
  { 
    // This returns the pointer.  
    return m_Pointer; 
  }
  
    // Comparison of pointers
  bool operator < (const itkSmartPointer &r)
  { return (void*)m_Pointer < (void*) r.m_Pointer; }

  bool operator > (const itkSmartPointer &r)
  { return (void*)m_Pointer > (void*) r.m_Pointer; }

  bool operator <= (const itkSmartPointer &r)
  { return (void*)m_Pointer <= (void*) r.m_Pointer; }

  bool operator >= (const itkSmartPointer &r)
  { return (void*)m_Pointer >= (void*) r.m_Pointer; }

  itkSmartPointer &operator = (const itkSmartPointer &r)
  { 
    return this->operator = (r.ptr()); 
  }
  
  itkSmartPointer &operator = (T *r)
  {								 
    if (m_Pointer != r)
      {
	this->UnRegister();
	m_Pointer = r;
	this->Register();
      }
    return *this;
  }

  // DeRegistererencing the pointer
  T &operator * () const { return *m_Pointer; }

  // Cast to T * 
  operator T * () const { return m_Pointer; }

private:

  void Register()
    { 
      if(m_Pointer)
	{
	m_Pointer->Register();
	}
    }
  
  void UnRegister()
    {
      if(m_Pointer)
	{
	m_Pointer->UnRegister();
	}
    }
  
private:
  T* m_Pointer;
};

#endif
