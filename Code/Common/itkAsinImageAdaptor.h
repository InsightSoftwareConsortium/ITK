/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAsinImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAsinImageAdaptor_h
#define __itkAsinImageAdaptor_h

#include <itkImageAdaptor.h>

namespace itk
{
 
namespace Accessor {
/**
 * \class AsinDataAccessor
 * \brief Give access to the asin() function of a value
 *
 * AsinDataAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the funtion to it and cast the result according 
 * to the types defined as template parameters
 *
 */

template <class TInternalType, class TExternalType >
class ITK_EXPORT AsinDataAccessor  
{
public:

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit.
   */
  typedef TExternalType ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data.
   */
  typedef TInternalType InternalType;


  static inline void Set(TInternalType & output, const TExternalType & input) 
    {output = (TInternalType)asin(input);}

  static inline TExternalType Get( const TInternalType & input ) 
    {return (TExternalType)asin(input);}

};

  
} // end namespace Accessor


 
/**
 * \class AsinImageAdaptor
 * \brief Presents an image as being composed of the asin() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 */
template <class TImage, class TOutputPixelType>
class ITK_EXPORT AsinImageAdaptor : public
      ImageAdaptor<TImage,
                   Accessor::AsinDataAccessor<
                                      typename TImage::PixelType,
                                      TOutputPixelType>   >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AsinImageAdaptor  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageAdaptor<TImage,
                       Accessor::AsinDataAccessor<
                                       typename TImage::PixelType,
                                       TOutputPixelType> >
                                                            Superclass;
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( AsinImageAdaptor, ImageAdaptor );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  


};

} // end namespace itk

#endif
