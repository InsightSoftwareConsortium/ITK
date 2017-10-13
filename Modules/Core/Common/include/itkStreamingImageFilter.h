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
#ifndef itkStreamingImageFilter_h
#define itkStreamingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionSplitterBase.h"

namespace itk
{
/** \class StreamingImageFilter
 * \brief Pipeline object to control data streaming for large data processing.
 *
 * StreamingImageFilter is a pipeline object that allows the user to control
 * how data is pulled through the pipeline.  To generate its
 * OutputRequestedRegion, this filter will divide the output into several
 * pieces (controlled by SetNumberOfStreamDivisions), and call the upstream
 * pipeline for each piece, tiling the individual outputs into one large
 * output. This reduces the memory footprint for the application since
 * each filter does not have to process the entire dataset at once.
 * This filter will produce the entire output as one image, but the upstream
 * filters will do their processing in pieces.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT StreamingImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef StreamingImageFilter                            Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StreamingImageFilter, ImageToImageFilter);

  /** Some typedefs for the input and output. */
  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::Pointer    InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename InputImageType::PixelType  InputImagePixelType;

  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename Superclass::DataObjectPointer DataObjectPointer;

  /** Dimension of input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      OutputImageType::ImageDimension);

  /** SmartPointer to a region splitting object */
  typedef ImageRegionSplitterBase        SplitterType;
  typedef typename SplitterType::Pointer RegionSplitterPointer;

  /** Set the number of pieces to divide the input.  The upstream pipeline
   * will be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions, unsigned int);

  /** Get the number of pieces to divide the input. The upstream pipeline
   * will be executed this many times. */
  itkGetConstReferenceMacro(NumberOfStreamDivisions, unsigned int);

  /** Get/Set the helper class for dividing the input into chunks. */
  itkSetObjectMacro(RegionSplitter, SplitterType);
  itkGetModifiableObjectMacro(RegionSplitter, SplitterType);

  /** Override UpdateOutputData() from ProcessObject to divide upstream
   * updates into pieces. This filter does not have a GenerateData()
   * or ThreadedGenerateData() method.  Instead, all the work is done
   * in UpdateOutputData() since it must update a little, execute a little,
   * update some more, execute some more, etc. */
  virtual void UpdateOutputData(DataObject *output) ITK_OVERRIDE;

  /** Override PropagateRequestedRegion from ProcessObject
   *  Since inside UpdateOutputData we iterate over streaming pieces
   *  we don't need to proapage up the pipeline
   */
  virtual void PropagateRequestedRegion(DataObject *output) ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputImagePixelType, OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  StreamingImageFilter();
  ~StreamingImageFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StreamingImageFilter);

  unsigned int          m_NumberOfStreamDivisions;
  RegionSplitterPointer m_RegionSplitter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStreamingImageFilter.hxx"
#endif

#endif
