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
#ifndef itkVotingBinaryIterativeHoleFillingImageFilter_h
#define itkVotingBinaryIterativeHoleFillingImageFilter_h

#include "itkVotingBinaryHoleFillingImageFilter.h"

namespace itk
{
/** \class VotingBinaryIterativeHoleFillingImageFilter
 * \brief Fills in holes and cavities by iteratively applying a voting operation.
 *
 * This filter uses internally the VotingBinaryHoleFillingImageFilter, and runs
 * it iteratively until no pixels are being changed or until it reaches the
 * maximum number of iterations. The purpose of the filter is to fill in holes
 * of medium size (tens of pixels in radius). In principle the number of
 * iterations is related to the size of the holes to be filled in. The larger
 * the holes, the more iteration must be run with this filter in order to fill
 * in the full hole. The size of the neighborhood is also related to the
 * curvature of the hole borders and therefore the hole size. Note that as a
 * collateral effect this filter may also fill in cavities in the external side
 * of structures.
 *
 * This filter is templated over a single image type because the output image
 * type must be the same as the input image type. This is required in order to
 * make the iterations possible, since the output image of one iteration is
 * taken as the input image for the next iteration.
 *
 * \sa Image
 * \sa VotingBinaryImageFilter
 * \sa VotingBinaryHoleFillingImageFilter
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKLabelVoting
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT VotingBinaryIterativeHoleFillingImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:

  /** Convenient typedefs for simplifying declarations. */
  typedef TImage InputImageType;
  typedef TImage OutputImageType;

  /** Standard class typedefs. */
  typedef VotingBinaryIterativeHoleFillingImageFilter           Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VotingBinaryIterativeHoleFillingImageFilter, ImageToImageFilter);

  /** Type of the internal Voting filter that is going to be executed
    iteratively */
  typedef VotingBinaryHoleFillingImageFilter<
    InputImageType,
    OutputImageType
    > VotingFilterType;

  /** Image typedef support. */
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename InputImageType::SizeType InputSizeType;

  /** Maximum number of iterations. This filter is executed iteratively as
   * long as at least one pixel has changed in a previous iteration, or until
   * the maximum number of iterations has been reached. */
  itkGetConstReferenceMacro(MaximumNumberOfIterations, unsigned int);
  itkSetMacro(MaximumNumberOfIterations, unsigned int);

  /** Number of iterations executed at any given time. This is useful at the
   * end of the execution in order to verify how many iterations were
   * performed.  */
  itkGetConstReferenceMacro(CurrentNumberOfIterations, unsigned int);
  itkSetMacro(CurrentNumberOfIterations, unsigned int);

  /** Set the radius of the neighborhood used to compute the median. */
  itkSetMacro(Radius, InputSizeType);

  /** Get the radius of the neighborhood used to compute the median */
  itkGetConstReferenceMacro(Radius, InputSizeType);

  /** Set the value associated with the Foreground (or the object) on
      the binary input image and the Background . */
  itkSetMacro(BackgroundValue, InputPixelType);
  itkSetMacro(ForegroundValue, InputPixelType);

  /** Get the value associated with the Foreground (or the object) on the
      binary input image and the Background . */
  itkGetConstReferenceMacro(BackgroundValue, InputPixelType);
  itkGetConstReferenceMacro(ForegroundValue, InputPixelType);

  /** Majority threshold. It is the number of pixels over 50% that will decide
   * whether an OFF pixel will become ON or not. For example, if the
   * neighborhood of a pixel has 124 pixels (excluding itself), the 50% will be
   * 62, and if you set upd a Majority threshold of 5, that means that the
   * filter will require 67 or more neighbor pixels to be ON in order to switch
   * the current OFF pixel to ON. The default value is 1. */
  itkGetConstReferenceMacro(MajorityThreshold, unsigned int);
  itkSetMacro(MajorityThreshold, unsigned int);

  /** Returns the number of pixels that changed when the filter was executed. */
  itkGetConstReferenceMacro(NumberOfPixelsChanged, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputPixelType > ) );
  itkConceptMacro( InputOStreamWritableeCheck,
                   ( Concept::OStreamWritable< InputPixelType > ) );
  // End concept checking
#endif

protected:
  VotingBinaryIterativeHoleFillingImageFilter();
  virtual ~VotingBinaryIterativeHoleFillingImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VotingBinaryIterativeHoleFillingImageFilter);

  InputSizeType m_Radius;

  InputPixelType m_ForegroundValue;
  InputPixelType m_BackgroundValue;

  unsigned int m_MaximumNumberOfIterations;
  unsigned int m_CurrentNumberOfIterations;
  unsigned int m_MajorityThreshold;
  unsigned int m_NumberOfPixelsChanged;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVotingBinaryIterativeHoleFillingImageFilter.hxx"
#endif

#endif
