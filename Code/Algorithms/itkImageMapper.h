/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkImageMapper_h
#define __itkImageMapper_h

#include "itkImage.h"
#include "itkPoint.h"
#include "itkRegistrationMapper.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{

/** \class ImageMapper
 * \brief Maps values from the image using a transformation
 *
 * This Class is templated over the image type and the 
 * transformation type. Using a point in space, it transforms
 * the point and gets the value of the image at that new point 
 * using some interpolation strategy.
 *
 * \ingroup ImageFunctions
 *
 */

template <class TImage, class TTransformation>
class ITK_EXPORT ImageMapper : 
  public RegistrationMapper< TImage, TTransformation > 

{
public:
  /** Standard class typedefs. */
  typedef ImageMapper<TImage,TTransformation>  Self;
  typedef RegistrationMapper< TImage, TTransformation >  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageMapper, RegistrationMapper);

  /** Typedef of the Point used to represent coordinates. */
   typedef typename TTransformation::InputPointType   InputPointType;

  /** Typedef of the Point used to represent coordinates. */
   typedef typename TTransformation::OutputPointType   OutputPointType;

   /** Typedef of transformation parameters. */
   typedef typename TTransformation::ParametersType   ParametersType;
 
  /** Typedef of the transformation coordinate representation. */
  typedef typename TTransformation::ScalarType CoordRepType;

  /**  Pointer type for the reference.  */
  typedef typename Superclass::DomainPointer DomainPointer;

  /**  Type for the reference.  */
  typedef typename Superclass::DomainType     DomainType;

  /** Typedef of the Image type. */
  typedef  TImage   ImageType;

  /** Typedef of the Pixel type. */
  typedef  typename   TImage::PixelType   PixelType;

  /** Typedef of the image index. */
  typedef  typename   TImage::IndexType   IndexType;

  /** Typedef of the image region. */
  typedef  typename   TImage::RegionType   RegionType;

  /** Typedef of the image size.  */
  typedef  typename   TImage::SizeType   SizeType;
  
  /** Type of the interpolation function. */
  typedef LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;

  /** Type of the interpolation function */
  typedef typename  InterpolatorType::Pointer  InterpolatorPointer;

  /** Set domain. */
  void SetDomain( const DomainType * domain );

  /** The dimension of the space. */
  enum { SpaceDimension = TTransformation::ParametersDimension };

public: 
  /** Evaluate the pixel value for the point previously 
   * specified in the IsInside() method.
   * \warning This method uses the point cached by IsInside() and
   * cannot  be safely used in more than one thread at a time.
   * \sa IsInside() */
   double Evaluate( void ) const; 

   /** Test whether the specified point is inside
    * the Image Domain and caches that point for later use by the Evaluate() 
    * method. 
    * \warning This method cannot be safely used in more than one thread at
    * a time.
    * \sa Evaluate(); */
   bool IsInside( const InputPointType & point );

protected:
  InterpolatorPointer           m_Interpolator;
  mutable OutputPointType       m_CurrentPoint;

  ImageMapper();
  ~ImageMapper(){};
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  ImageMapper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  

};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include <itkImageMapper.txx>
#endif

#endif







