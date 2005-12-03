/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGetAverageSliceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGetAverageSliceImageFilter_h
#define __itkGetAverageSliceImageFilter_h

#include "itkAccumulateImageFilter.h"
  
namespace itk
{
  
/** \class GetAverageSliceImageFilter
 * \brief Averages a single dimension of an image.
 *
 *    This class averages an image along a dimension and reduces the size 
 * of this dimension to 1. The dimension being averaged is set by 
 * AveragedOutDimension.
 *
 *   Each pixel is the average of the pixels along the collapsed
 * dimension and reduce the size of the averaged dimension to 1 (only 
 * on the averaged dimension). 
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 * 
 * \ingroup   IntensityImageFilters     Singlethreaded
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT GetAverageSliceImageFilter : public AccumulateImageFilter<TInputImage,TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef GetAverageSliceImageFilter  Self;
  typedef AccumulateImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GetAverageSliceImageFilter, AccumulateImageFilter);

  /** Set the direction in which to reflect the data. */
  itkGetMacro( AveragedOutDimension, unsigned int );
  itkSetMacro( AveragedOutDimension, unsigned int );
  
protected:
  GetAverageSliceImageFilter();
  virtual ~GetAverageSliceImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  GetAverageSliceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_AveragedOutDimension;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGetAverageSliceImageFilter.txx"
#endif

#endif


