/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorComponentDataAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVectorComponentDataAccessor_h
#define __itkVectorComponentDataAccessor_h

namespace itk {

template<class TInternalType, class TExternalType>
class VectorComponentDataAccessor
{
public:
  typedef TExternalType ExternalType;
  typedef TInternalType InternalType;
  
  inline void Set( InternalType & output, const ExternalType & input ) const
  { output[m_VisibleComponent] = input; }
  
  inline ExternalType Get( const InternalType & input) const 
  { return input[m_VisibleComponent]; }

  inline void SetVisibleComponent( const unsigned int i )
  { m_VisibleComponent = i; }

  inline unsigned int GetVisibleComponent() const
  { return m_VisibleComponent; }

private:
  unsigned int m_VisibleComponent;
};


} // end namespace itk
  
#endif
