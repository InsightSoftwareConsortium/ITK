/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultVectorPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDefaultVectorPixelAccessor_h
#define __itkDefaultVectorPixelAccessor_h

#include "itkMacro.h"
#include "itkArray.h"

namespace itk
{

/** \class DefaultVectorPixelAccessor
 * \brief Give access to partial aspects of a type
 *
 * DefaultVectorPixelAccessor is specifically meant to provide VectorImage
 * with the same \c DefaultPixelAccessor interface that 
 * DefaultPixelAccessor provides to Image. 
 *
 * The template paramters is the type that is contained in by the elements of
 * a vector.
 * 
 * The class also contains a m_VectorLength paramter, set with the SetVectorLength
 * method to set the length of the vectors. This must be set before the accessor
 * can be used. This is the length of each of the vector containers.
 *
 * \thanks
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 */
template <class TType > 
class ITK_EXPORT DefaultVectorPixelAccessor  
{
public:

  typedef unsigned long VectorLengthType;
  
  /** External typedef. It defines the external aspect
   * that this class will exhibit. Here it is an Array. The container does not 
   * manage the memory. In other words it is an array reference with the contents
   * pointing to the actual data in the image. */
  typedef Array< TType >  ExternalType;
  
  /** Internal typedef. It defines the internal real representation of data. */
  typedef TType          InternalType;

  /** Set output using the value in input */
  inline void Set(InternalType & output, const ExternalType & input, 
                                          const unsigned long offset ) const
    { 
    InternalType *truePixel = (&output) + offset*m_OffsetMultiplier;
    for( VectorLengthType i=0; i< m_VectorLength; i++ ) 
      { 
      truePixel[i] = input[i];
      }
    }

  /** Get the value from input */
  inline ExternalType Get( const InternalType & input, const unsigned long offset ) const
    {
    ExternalType output( (&input)+(offset*m_OffsetMultiplier) , m_VectorLength );
    return output;
    }

  /** Set the length of each vector in the VectorImage */
  void SetVectorLength( VectorLengthType l) 
    { 
    m_VectorLength = l; 
    m_OffsetMultiplier = (l-1); 
    }

  /** Get Vector lengths */
  VectorLengthType GetVectorLength() const { return m_VectorLength; }

  DefaultVectorPixelAccessor() {}

  /** Constructor to initialize VectorLength at construction time */
  DefaultVectorPixelAccessor( VectorLengthType l ) 
    { 
    m_VectorLength = l; 
    m_OffsetMultiplier = l-1;
    }
  
  virtual ~DefaultVectorPixelAccessor() {};
  
private:
  VectorLengthType m_VectorLength;
  VectorLengthType m_OffsetMultiplier;
};
  
} // end namespace itk
  

#endif

