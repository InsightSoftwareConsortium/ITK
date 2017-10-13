/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkContourMeanDistanceImageFilter_h
#define itkContourMeanDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class ContourMeanDistanceImageFilter
 * \brief Computes the Mean distance between the boundaries of
 * non-zero regions of two images.
 *
 * ContourMeanDistanceImageFilter computes the distance between the
 * set non-zero pixels of two images using the following formula:
 * \f[ H(A,B) = \max(h(A,B),h(B,A)) \f]
 * where
 * \f[ h(A,B) = \mathrm{mean}_{a \in A} \min_{b \in B} \| a - b\| \f] is the dir
ected
 * Mean distance
 * and \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images.
 *
 * In particular, this filter uses the ContourDirectedMeanImageFilter
 * inside to  compute the two directed distances and then select the
 * largest of the two.
 *
 * The Mean distance measures the degree of mismatch between two sets
 * and behaves like a metric over the set of all closed bounded sets -
 * with properties of identity, symmetry and triangle inequality.
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
 * \sa ContourDirectedMeanDistanceImageFilter
 *
 * \author Teo Popa, ISIS Center, Georgetown University
 *
 * \ingroup MultiThreaded
 * \ingroup ITKDistanceMap
 *
 * \wiki
 * \wikiexample{Curves/ContourMeanDistanceImageFilter,Compute the mean distance between all points of two curves}
 * \endwiki
 */
template< typename TInputImage1, typename TInputImage2 >
class ITK_TEMPLATE_EXPORT ContourMeanDistanceImageFilter:
  public ImageToImageFilter< TInputImage1, TInputImage1 >
{
public:
  /** Standard Self typedef */
  typedef ContourMeanDistanceImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage1, TInputImage1 > Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ContourMeanDistanceImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage1                        InputImage1Type;
  typedef TInputImage2                        InputImage2Type;
  typedef typename TInputImage1::Pointer      InputImage1Pointer;
  typedef typename TInputImage2::Pointer      InputImage2Pointer;
  typedef typename TInputImage1::ConstPointer InputImage1ConstPointer;
  typedef typename TInputImage2::ConstPointer InputImage2ConstPointer;

  typedef typename TInputImage1::RegionType RegionType;
  typedef typename TInputImage1::SizeType   SizeType;
  typedef typename TInputImage1::IndexType  IndexType;

  typedef typename TInputImage1::PixelType InputImage1PixelType;
  typedef typename TInputImage2::PixelType InputImage2PixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage1::ImageDimension);

  /** Type to use form computations. */
  typedef typename NumericTraits< InputImage1PixelType >::RealType RealType;

  /** Set the first input. */
  void SetInput1(const InputImage1Type *image);

  /** Set the second input. */
  void SetInput2(const InputImage2Type *image);

  /** Get the first input. */
  const InputImage1Type * GetInput1();

  /** Get the second input. */
  const InputImage2Type * GetInput2();

  /** Return the computed Mean distance. */
  itkGetConstMacro(MeanDistance, RealType);

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro( UseImageSpacing, bool );
  itkGetConstMacro( UseImageSpacing, bool );

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImage1PixelType > ) );
  // End concept checking
#endif

protected:
  ContourMeanDistanceImageFilter();
  ~ContourMeanDistanceImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** GenerateData. */
  void  GenerateData() ITK_OVERRIDE;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ContourMeanDistanceImageFilter);

  RealType  m_MeanDistance;
  bool      m_UseImageSpacing;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContourMeanDistanceImageFilter.hxx"
#endif

#endif
