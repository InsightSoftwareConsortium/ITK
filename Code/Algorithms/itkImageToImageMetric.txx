/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageMetric_txx
#define _itkImageToImageMetric_txx



namespace itk
{

/**
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
ImageToImageMetric<TFixedImage,TMovingImage>
::ImageToImageMetric()
{
  m_FixedImage   = FixedImageType::New();
  m_MovingImage  = MovingImageType::New();
  m_Transform    = TransformType::New();
  m_Interpolator = InterpolatorType::New();
}




/**
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;

  os << indent << "MatchMeasure: " << m_MatchMeasure << std::endl;
  os << indent << "MatchMeasureDerivatives: ";
  os << m_MatchMeasureDerivatives << std::endl;

}


} // end namespace itk

#endif
