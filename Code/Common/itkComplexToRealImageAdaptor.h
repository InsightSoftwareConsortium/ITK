/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComplexToRealImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkComplexToRealImageAdaptor_h
#define __itkComplexToRealImageAdaptor_h

#include <itkImageAdaptor.h>
#include <complex>

namespace itk
{
 
namespace Accessor {
/** \class ComplexToRealPixelAccessor
 * \brief Give access to the Real part of a std::complex<> value 
 *
 * ComplexToRealPixelAccessor is templated over an internal type and an
 * external type representation. The internal type is an std::complex<T> and
 * the external part is a type T. This class cast the input applies the funtion
 * to it and cast the result according to the types defined as template
 * parameters
 *
 * \ingroup ImageAdaptors
 */
template <class TInternalType, class TExternalType >
class ITK_EXPORT ComplexToRealPixelAccessor  
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType)(input);}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)(input.real());}
};
  
} // end namespace Accessor
 
/** \class ComplexToRealImageAdaptor
 * \brief Presents a complex image as being composed of real() part of 
 *        its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT ComplexToRealImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::ComplexToRealPixelAccessor<
                                      typename TImage::PixelType,
                                      TOutputPixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ComplexToRealImageAdaptor  Self;
  typedef ImageAdaptor<TImage, Accessor::ComplexToRealPixelAccessor<
                               typename TImage::PixelType,
                               TOutputPixelType> >  Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( ComplexToRealImageAdaptor, ImageAdaptor );

protected:
  ComplexToRealImageAdaptor() {}
  virtual ~ComplexToRealImageAdaptor() {}
  
private:
  ComplexToRealImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
