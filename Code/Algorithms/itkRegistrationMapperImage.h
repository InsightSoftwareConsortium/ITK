/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegistrationMapperImage_h
#define __itkRegistrationMapperImage_h

#include "itkImage.h"
#include "itkPoint.h"
#include "itkRegistrationMapper.h"

namespace itk
{
  
/** \class RegistrationMapperImage
 * \brief Maps values from the image using a transformation
 *
 * This Class is templated over the image type and the 
 * transformation type. Using a point in space, it transforms
 * the point and gets the value of the image at that new point 
 * using some interpolation strategy.
 *
 * \ingroup ImageFunctions
 */
template <class TImage, class TTransformation>
class ITK_EXPORT RegistrationMapperImage : 
  public RegistrationMapper< TImage, TTransformation > 
{
public:
  /** Standard class typedefs. */
  typedef RegistrationMapperImage<TImage,TTransformation>  Self;
  typedef RegistrationMapper<TImage, TTransformation>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Typedef of the Point used to represent coordinates */
  typedef typename TTransformation::InputPointType   PointType;

  /** Typedef of the pixel type. */
  typedef  typename   TImage::PixelType   PixelType;

  /** Typedef of the image index. */
  typedef  typename   TImage::IndexType   IndexType;

  /** Typedef of the image region. */
  typedef  typename   TImage::RegionType   RegionType;

  /** Typedef of the image size. */
  typedef  typename   TImage::SizeType   SizeType;

public: 
  /** Evaluate the pixel at the current position. */
   PixelType Evaluate( void ) const; 

  /** Test whether the point is inside the image or not. */
   bool IsInside( const PointType & point ) const;

protected:
  RegistrationMapperImage();
  ~RegistrationMapperImage() {}

private:
  RegistrationMapperImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  mutable IndexType         m_CurrentIndex;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include <itkRegistrationMapperImage.txx>
#endif

#endif
