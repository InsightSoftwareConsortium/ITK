#include "iostream.h"
#include "itkSmartPointer.h"


class itkTestObject
{
public:
  typedef itkSmartPointer<itkTestObject> Pointer;
  static itkTestObject::Pointer New();
  void Register()
    {
      cout << "Register " << *this << endl;
      
      m_ReferenceCount++;
    }
  void UnRegister()
    {
      cout << "UnRegister " << *this << endl;
      
      m_ReferenceCount--;
      if(m_ReferenceCount == 0)
	{
	delete this;
	}
    }
  inline friend ostream &operator << (ostream &os, itkTestObject const& o) 
    {
      os << "itkTestObject " << (void*)&o << " " << o.m_ReferenceCount; 
      return os;
    }
  
protected:
  itkTestObject() 
    { 
      cout << "construct itkTestObject " << *this << endl; 
    }
  ~itkTestObject() 
    {
      cout << "destruct itkTestObject " << *this << endl; 
    }

private:
  unsigned int m_ReferenceCount;
};

itkTestObject::Pointer itkTestObject::New()
{
  return itkTestObject::Pointer(new itkTestObject);
}



int main()
{
  itkTestObject::Pointer o1 = itkTestObject::New();
  cout << "o1 " << o1 << endl;
  itkTestObject::Pointer o2 = itkTestObject::New();
  cout << "o2 " << o2 << endl;
  itkTestObject::Pointer o3 = itkTestObject::New();
  cout << "o3 " << o3 << endl;
  itkTestObject::Pointer o4 = itkTestObject::New();
  cout << "o4 " << o4 << endl;
  
  o1 = o2;
  o2 = o3;
  o4 = o1;
  if(o1 < o2)
    {
    cout << "o1 is < o2 " << o1 << " " << o2 << endl;
    }
  else
    {
    cout << "o1 is not < o2 " << o1 << " " << o2 << endl;
    }
  return 0;
}
