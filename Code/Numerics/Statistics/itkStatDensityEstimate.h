#ifndef __itkStatDensityEstimate_h
#define __itkStatDensityEstimate_h

#include <limits.h>
#include <map>

#include "itkLightObject.h"
#include "itkObjectFactory.h"

namespace itk{

/** \class DensityEstimate 
 *  \brief This class is a container of density estimate of a sample.
 */

template <class TDensity = double>
class ITK_EXPORT DensityEstimate 
: public LightObject
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef DensityEstimate  Self;

 /**
  * Standard "Superclass" typedef.
  */
  typedef LightObject Superclass;

 /**
  * Density Type support
  */
  typedef TDensity DensityType;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Map typedef support
  */
  typedef std::map<unsigned long, DensityType> MapType;

 /**
  * Map Iterator typedef support
  */
  typedef typename MapType::iterator MapIterator;

 /** 
  * Run-time type information (and related methods).
  */
  itkTypeMacro(DensityEstimate, LightObject);

 /**
  * Method for creation through the object factory.
  */
  itkNewMacro(Self);

 /**
  * Method to get density
  */
  DensityType GetDensity(unsigned long id)
  { return m_Density[id]; };

 /**
  * Method to set density
  */
  void  SetDensity(unsigned long id, DensityType prob)
  { m_Density[id] = prob;}; 

  class Iterator;
  friend class Iterator;

  Iterator Begin()
  {
    Iterator iter(m_Density.begin(), this);
    return iter; 
  }
       
  Iterator  End()        
  { 
    Iterator iter(m_Density.end(), this); 
    return iter; 
  }
  
 /**
  * The non-const iterator type for the map.
  */
  class Iterator
  {
  public:
    Iterator(){};

    Iterator(Pointer density) 
    { 
      m_Iter  = density->Begin();
      m_Den = density; 
    } 

    Iterator(MapIterator i, Pointer l)
    :m_Iter(i), m_Den(l){}
     
    DensityType GetDensity() 
    { return  (*m_Iter).second; }

    void SetDensity(DensityType density) 
    { return  (*m_Iter).second = density; }
    
    Iterator& operator++() 
    { 
      ++m_Iter; 
      return *this;
    }
 
    bool operator!=(const Iterator& it) 
      { return (m_Iter != it.m_Iter); }
    
    bool operator==(const Iterator& it) 
      { return (m_Iter == it.m_Iter); }

    bool      IsAtBegin()  
    { 
      Iterator it = m_Den->Begin();
      return ( m_Iter == it.m_Iter ); 
    } 
    
    bool      IsAtEnd()    
    { 
      Iterator it = m_Den->End();
      return ( m_Iter == it.m_Iter ); 
    }
    
    Iterator& operator=(const Iterator& iter)
    { 
      m_Iter  = iter.m_Iter;
      m_Den = iter.m_Den; 
    }
     
  private:
    MapIterator m_Iter;
    Pointer m_Den;
  
  };

protected:
 
  DensityEstimate() {};
  virtual ~DensityEstimate() {};
  DensityEstimate(const Self&) {};
  void operator=(const Self&) {};

private:
  MapType m_Density;

};

} // end of namespace

#endif
