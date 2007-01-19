/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProjectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProjectionImageFilter_h
#define __itkProjectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
  
/** \class ProjectionImageFilter
 * \brief Implements an accumulation of an image along a selected direction.
 *
 *    This class accumulates an image along a dimension and reduce the size 
 * of this dimension to 1. The dimension being accumulated is set by 
 * ProjectionDimension. 
 *
 *   Each pixel is the cumulative sum of the pixels along the collapsed
 * dimension and reduce the size of the accumulated dimension to 1 (only 
 * on the accumulated). 
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * \author Emiliano Beronich
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \ingroup   IntensityImageFilters     Singlethreaded
 */
template <class TInputImage, class TOutputImage, class TAccumulator>
class ITK_EXPORT ProjectionImageFilter : 
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ProjectionImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ProjectionImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType;
  typedef typename    InputImageType::PixelType  InputImagePixelType;
  typedef TOutputImage OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  typedef TAccumulator AccumulatorType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Input and output images must be the same dimension, or the output's
      dimension must be one less than that of the input. */
#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(ImageDimensionCheck,
    (Concept::SameDimensionOrMinusOne<
       itkGetStaticConstMacro(InputImageDimension),
       itkGetStaticConstMacro(OutputImageDimension)>));
  /** End concept checking */
#endif


  /** Set the direction in which to accumulate the data.  It must be
   * set before the update of the filter. Defaults to the last
   * dimension. */
  itkGetMacro( ProjectionDimension, unsigned int );
  itkSetMacro( ProjectionDimension, unsigned int );


protected:
  ProjectionImageFilter();
  virtual ~ProjectionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation();

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion();

  virtual void ThreadedGenerateData( 
   const OutputImageRegionType& outputRegionForThread, int threadId );

  virtual AccumulatorType NewAccumulator( unsigned long );

private:
  ProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_ProjectionDimension;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProjectionImageFilter.txx"
#endif

#endif


