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
#ifndef itkVotingBinaryImageFilter_h
#define itkVotingBinaryImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class VotingBinaryImageFilter
 * \brief Applies a voting operation in a neighborhood of each pixel.
 *
 * \note Pixels which are not Foreground or Background will remain unchanged.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKLabelVoting
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT VotingBinaryImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Standard class typedefs. */
  typedef VotingBinaryImageFilter                               Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VotingBinaryImageFilter, ImageToImageFilter);

  /** Image typedef support. */
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename InputImageType::SizeType InputSizeType;

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

  /** Birth threshold. Pixels that are OFF will turn ON when the number of
   * neighbors ON is larger than the value defined in this threshold. */
  itkGetConstReferenceMacro(BirthThreshold, unsigned int);
  itkSetMacro(BirthThreshold, unsigned int);

  /** Survival threshold. Pixels that are ON will turn OFF when the number of
   * neighbors ON is smaller than the value defined in this survival threshold. */
  itkGetConstReferenceMacro(SurvivalThreshold, unsigned int);
  itkSetMacro(SurvivalThreshold, unsigned int);

  /** VotingBinaryImageFilter needs a larger input requested region than
   * the output requested region.  As such, VotingBinaryImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputPixelType > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputPixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputPixelType, OutputPixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputPixelType > ) );
  // End concept checking
#endif

protected:
  VotingBinaryImageFilter();
  virtual ~VotingBinaryImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** VotingBinaryImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VotingBinaryImageFilter);

  InputSizeType m_Radius;

  InputPixelType m_ForegroundValue;
  InputPixelType m_BackgroundValue;

  unsigned int m_BirthThreshold;
  unsigned int m_SurvivalThreshold;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVotingBinaryImageFilter.hxx"
#endif

#endif
