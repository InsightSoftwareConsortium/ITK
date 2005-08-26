/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAccumulateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAccumulateImageFilter_h
#define __itkAccumulateImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
  
/** \class AccumulateImageFilter
 * \brief Implements an accumulation of an image along a selected direction.
 *
 *    This class accumulates an image along a dimension and reduce the size 
 * of this dimension to 1. The dimension being accumulated is set by 
 * AccumulateDimension. 
 *
 *   Each pixel is the cumulative sum of the pixels along the collapsed
 * dimension and reduce the size of the accumulated dimension to 1 (only 
 * on the accumulated). 
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 *
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 *
 * \author Emiliano Beronich
 *
 * This filter was contributed by Emiliano Beronich
 *
 * \ingroup   IntensityImageFilters     Singlethreaded
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AccumulateImageFilter : public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef AccumulateImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AccumulateImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType;
  typedef typename    InputImageType::PixelType  InputImagePixelType;
  typedef TOutputImage OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;


  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set the direction in which to accumulate the data.  It must be
   * set before the update of the filter. Defaults to the last
   * dimension. */
  itkGetMacro( AccumulateDimension, unsigned int );
  itkSetMacro( AccumulateDimension, unsigned int );

  /** Perform a division by the size of the accumulated dimension
   * after the accumulation is done. If true, the output image is the
   * average of the accumulated dimension, if false the output is the
   * sum of the pixels along the selected direction.  The default
   * value is false.**/
  itkSetMacro( Average, bool );
  itkGetMacro( Average, bool );
  itkBooleanMacro(Average);

  

protected:
  AccumulateImageFilter();
  virtual ~AccumulateImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation();

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion();

  /** This method implements the actual accumulation of the image.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void GenerateData(void);

private:
  AccumulateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_AccumulateDimension;
  bool m_Average;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAccumulateImageFilter.txx"
#endif

#endif


