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
#ifndef itkProjectionImageFilter_h
#define itkProjectionImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ProjectionImageFilter
 * \brief Implements an accumulation of an image along a selected direction.
 *
 * This class accumulates an image along a dimension and reduces the
 * size of this dimension to 1. The dimension being accumulated is set
 * by ProjectionDimension.
 *
 * Each pixel is the cumulative sum of the pixels along the collapsed
 * dimension and reduces the size of the accumulated dimension to 1 (only
 * on the accumulated).
 *
 * The dimensions of the InputImage and the OutputImage are either the same
 * or dimension of OutputImage is dimension of InputImage minus one. In the
 * latter case, the direction cosine of the output image is set to the
 * identity.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * This class was contributed to the Insight Journal by Emilian Beronich and
 * Gaetan Lehmann. The original paper can be found at
 *      https://hdl.handle.net/1926/164
 *
 * \author Emiliano Beronich
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa AccumulateImageFilter
 * \ingroup IntensityImageFilters SingleThreaded
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage, typename TOutputImage, typename TAccumulator >
class ITK_TEMPLATE_EXPORT ProjectionImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ProjectionImageFilter                           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ProjectionImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef typename    InputImageType::Pointer      InputImagePointer;
  typedef typename    InputImageType::RegionType   InputImageRegionType;
  typedef typename    InputImageType::PixelType    InputImagePixelType;
  typedef TOutputImage                             OutputImageType;
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
  // Begin concept checking
  itkConceptMacro( ImageDimensionCheck,
                   ( Concept::SameDimensionOrMinusOne<
                       itkGetStaticConstMacro(InputImageDimension),
                       itkGetStaticConstMacro(OutputImageDimension) > ) );
  // End concept checking
#endif

  /** Set/Get the direction in which to accumulate the data.  It must be set
   * before the update of the filter. Defaults to the last dimension. */
  itkSetMacro(ProjectionDimension, unsigned int);
  itkGetConstReferenceMacro(ProjectionDimension, unsigned int);

protected:
  ProjectionImageFilter();
  virtual ~ProjectionImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void ThreadedGenerateData(
    const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  virtual AccumulatorType NewAccumulator( SizeValueType ) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ProjectionImageFilter);

  unsigned int m_ProjectionDimension;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProjectionImageFilter.hxx"
#endif

#endif
