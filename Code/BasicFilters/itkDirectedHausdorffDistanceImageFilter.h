/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectedHausdorffDistanceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDirectedHausdorffDistanceImageFilter_h
#define __itkDirectedHausdorffDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkImage.h"

namespace itk {

/** \class DirectedHausdorffDistanceImageFilter 
 * \brief Computes the directed Hausdorff distance between the set of 
 * non-zero pixels of two images.
 *
 * DirectedHausdorffDistanceImageFilter computes the distance between the set
 * non-zero pixels of two images using the following formula:
 * \f[ h(A,B) = \max_{a \in A} \min_{b \in B} \| a - b\| \f]
 * where \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images. It identifies the point \f$ a \in A \f$
 * that is farthest from any point of \f$B\f$ and measures the distance from \f$a\f$
 * to the nearest neighbor in \f$B\f$. Note that this function is not
 * is not symmetric and hence is not a true distance.
 *
 * In particular, this filter uses the DanielssonDistanceMapImageFilter inside to 
 * compute distance map from all non-zero pixels in the second image. It then
 * find the largest distance (in pixels) within the set of all non-zero pixels in the first
 * image.
 *
 * Use HausdorffDistanceImageFilter to compute the full Hausdorff distance.
 *
 * This filter requires the largest possible region of the first image
 * and the same corresponding region in the second image. 
 * It behaves as filter with
 * two input and one output. Thus it can be inserted in a pipeline with 
 * other filters. The filter passes the first input through unmodified.
 *
 * This filter is templated over the two input image type. It assume
 * both image have the same number of dimensions.
 *
 * \sa DanielssonDistanceMapImageFilter
 * \sa HausdorffDistanceImageFilter
 *
 * \ingroup MultiThreaded
 */
template<class TInputImage1, class TInputImage2>
class ITK_EXPORT DirectedHausdorffDistanceImageFilter : 
    public ImageToImageFilter<TInputImage1, TInputImage1>
{
public:
  /** Standard Self typedef */
  typedef DirectedHausdorffDistanceImageFilter Self;
  typedef ImageToImageFilter<TInputImage1,TInputImage1>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(DirectedHausdorffDistanceImageFilter, ImageToImageFilter);
  
  /** Image related typedefs. */
  typedef TInputImage1 InputImage1Type;
  typedef TInputImage2 InputImage2Type;
  typedef typename TInputImage1::Pointer InputImage1Pointer;
  typedef typename TInputImage2::Pointer InputImage2Pointer;
  typedef typename TInputImage1::ConstPointer InputImage1ConstPointer;
  typedef typename TInputImage2::ConstPointer InputImage2ConstPointer;

  typedef typename TInputImage1::RegionType RegionType ;
  typedef typename TInputImage1::SizeType SizeType ;
  typedef typename TInputImage1::IndexType IndexType ;

  typedef typename TInputImage1::PixelType InputImage1PixelType;
  typedef typename TInputImage2::PixelType InputImage2PixelType;
  
  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage1::ImageDimension);

  /** Type to use form computations. */
  typedef typename NumericTraits<InputImage1PixelType>::RealType RealType;

  /** Set the first input. */
  void SetInput1( const InputImage1Type * image )
  { this->SetInput( image ); }

  /** Set the second input. */
  void SetInput2( const InputImage2Type * image );

  /** Get the first input. */
  const InputImage1Type * GetInput1(void)
  { return this->GetInput(); }
  
  /** Get the second input. */
  const InputImage2Type * GetInput2(void);
  
  /** Return the computed directed Hausdorff distance. */
  itkGetMacro(DirectedHausdorffDistance,RealType);

protected:
  DirectedHausdorffDistanceImageFilter();
  ~DirectedHausdorffDistanceImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Pass the input through unmodified. Do this by Grafting in the AllocateOutputs method. */
  void AllocateOutputs();      

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData ();
  
  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData ();
  
  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const RegionType& 
                              outputRegionForThread,
                              int threadId) ;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data);

private:
  DirectedHausdorffDistanceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typedef Image<RealType,itkGetStaticConstMacro(ImageDimension)> DistanceMapType;
  
  typename DistanceMapType::Pointer   m_DistanceMap;
  Array<RealType>                     m_MaxDistance;
  RealType                            m_DirectedHausdorffDistance;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDirectedHausdorffDistanceImageFilter.txx"
#endif

#endif
