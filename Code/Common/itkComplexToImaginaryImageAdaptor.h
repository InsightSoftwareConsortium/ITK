/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComplexToImaginaryImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkComplexToImaginaryImageAdaptor_h
#define __itkComplexToImaginaryImageAdaptor_h

#include <itkImageAdaptor.h>
#include <complex>

namespace itk
{
 
namespace Accessor {
/** \class ComplexToImaginaryPixelAccessor
 * \brief Give access to the Imaginary part of a std::complex<> value 
 *
 * ComplexToImaginaryPixelAccessor is templated over an internal type and an
 * external type representation. The internal type is an std::complex<T> and
 * the external part is a type T. This class cast the input applies the funtion
 * to it and cast the result according to the types defined as template
 * parameters
 *
 * \ingroup ImageAdaptors
 */
template <class TInternalType, class TExternalType >
class ITK_EXPORT ComplexToImaginaryPixelAccessor  
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal imaginary
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType)(input);}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)(input.imag());}
};
  
} // end namespace Accessor
 
/** \class ComplexToImaginaryImageAdaptor
 * \brief Presents a complex image as being composed of imag() part of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT ComplexToImaginaryImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::ComplexToImaginaryPixelAccessor<
                                      typename TImage::PixelType,
                                      TOutputPixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ComplexToImaginaryImageAdaptor  Self;
  typedef ImageAdaptor<TImage, Accessor::ComplexToImaginaryPixelAccessor<
                               typename TImage::PixelType,
                               TOutputPixelType> >  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( ComplexToImaginaryImageAdaptor, ImageAdaptor );

 protected:
  ComplexToImaginaryImageAdaptor() {}
  virtual ~ComplexToImaginaryImageAdaptor() {}
  
 private:
  ComplexToImaginaryImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
