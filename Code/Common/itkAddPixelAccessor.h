/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAddPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAddPixelAccessor_h
#define __itkAddPixelAccessor_h



namespace itk
{
namespace Accessor
{

/** \class AddPixelAccessor
 * \brief Simulates the effect of adding a constant value to all pixels
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an image appear as having 
 * pixels with intensity values increased by a constant amount.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 */

template <class TPixel>
class ITK_EXPORT AddPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   AddPixelAccessor        Self;

 /** External typedef. It defines the external aspect
   * that this class will exhibit */
  typedef     TPixel      ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef     TPixel      InternalType;

  /** Write access to the pixel */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { output = static_cast<InternalType>( input - m_Value ); }

  /** Read access to the pixel */
  inline ExternalType Get( const InternalType & input ) const
    { return static_cast<ExternalType>( input + m_Value ); }

  /** Set the value to be added to pixels */
  void SetValue( TPixel newvalue )
    { m_Value = newvalue; }

  /** Get the value to be added to pixels */
  TPixel GetValue() 
    { return m_Value; }

  /** Assignment Operator */
  Self & operator=( const Self & apa )
    { this->m_Value = apa.m_Value;
      return *this; }

  /** Constructors */
  AddPixelAccessor():m_Value( NumericTraits<TPixel>::Zero ) {}
  AddPixelAccessor( const Self & apa ):m_Value(apa.m_Value) {}
  
private:

  TPixel m_Value;

};

  
  
}  // end namespace Accessor
}  // end namespace itk


#endif

