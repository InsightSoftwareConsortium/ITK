/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityIndexImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimilarityIndexImageFilter_h
#define __itkSimilarityIndexImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"

namespace itk {

/** \class SimilarityIndexImageFilter 
 * \brief Measures the similarity between the set of non-zero pixels of 
 * two images.
 *
 * SimilarityIndexImageFilter measures the similarity between the set
 * non-zero pixels of two images using the following formula:
 * \f[ S = \frac{2 | A \cap B |}{|A| + |B|} \f]
 * where \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images. Operator \f$|\cdot|\f$ represents
 * the size of a set and \f$\cap\f$ represents the intersection of two sets.
 *
 * The measure is derived from a reliability measure known as the kappa
 * statistic. \f$S\f$ is sensitive to both differences in size and in
 * location and have been in the literature for comparing two segmentation masks. 
 * For more information see:
 * "Morphometric Analysis of White Matter Lesions in MR Images: Method and
 * Validation", A. P. Zijdenbos, B. M. Dawant, R. A. Margolin and
 * A. C. Palmer, IEEE Trans. on Medical Imaging, 13(4) pp 716-724,1994
 *
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
 * \ingroup MultiThreaded
 */
template<class TInputImage1, class TInputImage2>
class ITK_EXPORT SimilarityIndexImageFilter : 
    public ImageToImageFilter<TInputImage1, TInputImage1>
{
public:
  /** Standard Self typedef */
  typedef SimilarityIndexImageFilter Self;
  typedef ImageToImageFilter<TInputImage1,TInputImage1>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(SimilarityIndexImageFilter, ImageToImageFilter);
  
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
                      TInputImage1::ImageDimension );

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
  
  /** Get the secong input. */
  const InputImage2Type * GetInput2(void);
  
  /** Return the computed similarity index. */
  itkGetMacro(SimilarityIndex,RealType);

protected:
  SimilarityIndexImageFilter();
  ~SimilarityIndexImageFilter(){};
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
  SimilarityIndexImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RealType m_SimilarityIndex;

  Array<unsigned long> m_CountOfImage1;
  Array<unsigned long> m_CountOfImage2;
  Array<unsigned long> m_CountOfIntersection;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarityIndexImageFilter.txx"
#endif

#endif
