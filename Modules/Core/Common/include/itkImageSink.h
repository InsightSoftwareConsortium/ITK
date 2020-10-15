/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkImageSink_h
#define itkImageSink_h

#include "itkStreamingProcessObject.h"
#include "itkImage.h"
#include "itkImageRegionSplitterBase.h"
#include "itkImageRegionSplitterSlowDimension.h"
#include "itkImageToImageFilterCommon.h"

namespace itk
{

/** \class ImageSink
 *
 * ImageSink is the base class for process objects which consume image
 * data. This class defaults to having at least one input of the
 * templated image type. The framework enables derived algorithms to
 * stream the input image as it's being consumed by the algorithm.
 *
 * The framework provides multi-threading of the streamed image regions.
 * The input image's pipeline is updated multiple times with the
 * streaming requested regions, then the  fulfilled requested region
 * are split again for multi-threading.
 *
 * By default, the NumberOfStreamDivisions is 1 (no streaming).
 * Derived implementations must change the access specification for
 * this method to be public to expose the streaming feature.
 *
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 *
 * \ingroup ITKCommon
 **/
template <typename TInputImage>
class ImageSink
  : public StreamingProcessObject
  , private ImageToImageFilterCommon
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageSink);

  /** Standard class type aliases. */
  using Self = ImageSink;
  using Superclass = StreamingProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSink, StreamingProcessObject);

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = DataObject::Pointer;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** SmartPointer to a region splitting object */
  using SplitterType = ImageRegionSplitterBase;
  using RegionSplitterPointer = typename SplitterType::Pointer;

  using DataObjectIdentifierType = typename Superclass::DataObjectIdentifierType;

  /** Dimension of input images. */
  itkStaticConstMacro(InputImageDimension, unsigned int, InputImageType::ImageDimension);


  using Superclass::SetInput;
  /** Set/Get the image input of this process object.  */
  virtual void
  SetInput(const InputImageType * input);

  virtual const InputImageType *
  GetInput() const;

  virtual const InputImageType *
  GetInput(unsigned int idx) const;

  virtual const InputImageType *
  GetInput(const DataObjectIdentifierType & key) const;

  void
  Update() override;

  void
  UpdateLargestPossibleRegion() override;

  /** get/set the Coordinate tolerance
   *  This tolerance is used when comparing the space defined
   *  by the input images.  ITK has a requirement that multiple input
   *  images be congruent in space by default.
   */
  itkSetMacro(CoordinateTolerance, double);
  itkGetConstMacro(CoordinateTolerance, double);

  /** get/set the direction tolerance
   *  This tolerance is used to make sure that all input
   *  images are oriented the same before performing the filter's
   *  transformations.
   */
  itkSetMacro(DirectionTolerance, double);
  itkGetConstMacro(DirectionTolerance, double);

  /** get/set the global default direction tolerance
   *
   * This value is used to initialize the DirectionTolerance upon
   * class construction of \b any Image filters. This has no
   * effect on currently constructed classes.
   */
  using ImageToImageFilterCommon::SetGlobalDefaultDirectionTolerance;
  using ImageToImageFilterCommon::GetGlobalDefaultDirectionTolerance;


  /** get/set the global default coordinate tolerance
   *
   * This value is used to initialize the CoordinateTolerance upon
   * class construction of \b any ImageToImage filter. This has no
   * effect on currently constructed  classes.
   */
  using ImageToImageFilterCommon::SetGlobalDefaultCoordinateTolerance;
  using ImageToImageFilterCommon::GetGlobalDefaultCoordinateTolerance;

protected:
  ImageSink();
  ~ImageSink() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  unsigned int
  GetNumberOfInputRequestedRegions() override;

  void
  GenerateNthInputRequestedRegion(unsigned int inputRequestedRegionNumber) override;

  virtual void
  AllocateOutputs()
  {}

  void
  VerifyInputInformation() ITKv5_CONST override;

  void
  BeforeStreamedGenerateData() override
  {
    this->AllocateOutputs();
  }

  void
  StreamedGenerateData(unsigned int inputRequestedRegionNumber) override;

  virtual void
  ThreadedStreamedGenerateData(const InputImageRegionType & inputRegionForChunk) = 0;


  /** Set the number of pieces to divide the input.  The upstream pipeline
   * will be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions, unsigned int);

  /** Get the number of pieces to divide the input. The upstream pipeline
   * will be executed this many times. */
  itkGetConstMacro(NumberOfStreamDivisions, unsigned int);

  /** Set/Get Helper class for dividing the input into regions for
   * streaming */
  itkSetObjectMacro(RegionSplitter, SplitterType);
  itkGetModifiableObjectMacro(RegionSplitter, SplitterType);


private:
  unsigned int          m_NumberOfStreamDivisions{ 1 };
  RegionSplitterPointer m_RegionSplitter;
  InputImageRegionType  m_CurrentInputRegion;

  /**
   *  Tolerances for checking whether input images are defined to
   *  occupy the same physical space.
   */
  double m_CoordinateTolerance{ Self::GetGlobalDefaultCoordinateTolerance() };
  double m_DirectionTolerance{ Self::GetGlobalDefaultDirectionTolerance() };
};

} // namespace itk

#include "itkImageSink.hxx"

#endif // itkImageSink_h
