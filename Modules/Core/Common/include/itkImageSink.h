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
#ifndef itkImageSink_h
#define itkImageSink_h

#include "itkStreamingProcessObject.h"
#include "itkImage.h"
#include "itkImageRegionSplitterBase.h"
#include "itkImageRegionSplitterSlowDimension.h"

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
template <class TInputImage >
class ImageSink
  : public StreamingProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageSink);

  /** Standard class type aliases. */
  using Self = ImageSink;
  using Superclass = StreamingProcessObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Run-time type information (and related methods). */
  itkTypeMacro( ImageSink, StreamingProcessObject );

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
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);


  using Superclass::SetInput;
  /** Set/Get the image input of this process object.  */
  virtual void SetInput(const InputImageType *input);

  virtual const InputImageType * GetInput(void) const;

  virtual const InputImageType *GetInput(unsigned int idx) const;

  virtual const InputImageType *GetInput(const DataObjectIdentifierType & key) const;

  virtual void Update( ) override;

protected:
  ImageSink();
  ~ImageSink() = default;

  virtual void PrintSelf(std::ostream & os, Indent indent) const override;

  virtual unsigned int GetNumberOfInputRequestedRegions () override;

  virtual void  GenerateNthInputRequestedRegion (unsigned int inputRequestedRegionNumber) override;

  virtual void AllocateOutputs( ) {}

  void BeforeStreamedGenerateData( ) override {this->AllocateOutputs();}

  virtual void StreamedGenerateData( unsigned int  inputRequestedRegionNumber) override;

  virtual void ThreadedStreamedGenerateData( const InputImageRegionType &inputRegionForChunk ) = 0;


  /** Set the number of pieces to divide the input.  The upstream pipeline
   * will be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions, unsigned int);

  /** Get the number of pieces to divide the input. The upstream pipeline
    * will be executed this many times. */
  itkGetConstMacro(NumberOfStreamDivisions, unsigned int);

  /** Set/Get Helper class for dividing the input into regions for
   * streaming */
  itkSetObjectMacro(RegionSplitter, SplitterType);
  itkGetObjectMacro(RegionSplitter, SplitterType);


private:

  unsigned int          m_NumberOfStreamDivisions;
  RegionSplitterPointer m_RegionSplitter;
  InputImageRegionType  m_CurrentInputRegion;
};

}

#include "itkImageSink.hxx"

#endif // itkImageSink_h
