/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdImageFilter_h
#define __itkThresholdImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkConceptChecking.h"

namespace itk
{

/** \class ThresholdImageFilter
 * \brief Set image values to a user-specified value if they are below, 
 * above, or between simple threshold values.
 *
 * ThresholdImageFilter sets image values to a user-specified "outside"
 * value (by default, "black") if the image values are below, above, or
 * between simple threshold values. 
 *
 * The pixels must support the operators >= and <=.
 * 
 * \ingroup IntensityImageFilters
 */
template <class TImage>
class ITK_EXPORT ThresholdImageFilter:public ImageToImageFilter<TImage,TImage>
{
public:
  /** Standard class typedefs. */
  typedef ThresholdImageFilter         Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdImageFilter, ImageToImageFilter);

  /** Typedef to describe the type of pixel. */
  typedef typename TImage::PixelType PixelType;
  
  /** The pixel type must support comparison operators. */
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<PixelType>));
  
  /** Set the "outside" pixel value. The default value 
   * NumericTraits<PixelType>::Zero. */
  itkSetMacro(OutsideValue,PixelType);
  
  /** Get the "outside" pixel value. */
  itkGetMacro(OutsideValue,PixelType);
                 
  /** The values greater than or equal to the value are set to OutsideValue. */
  void ThresholdAbove(PixelType &thresh);
  
  /** The values less than or equal to the value are set to OutsideValue. */
  void ThresholdBelow(PixelType &thresh);

  /** The values outside the range are set to OutsideValue. */
  void ThresholdOutside(PixelType &lower, PixelType &upper);

  /** Some additional typedefs.  */
  typedef TImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;

protected:
  ThresholdImageFilter();
  ~ThresholdImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ThresholdImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  ThresholdImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PixelType m_OutsideValue;
  PixelType m_Lower;
  PixelType m_Upper;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdImageFilter.txx"
#endif
  
#endif
