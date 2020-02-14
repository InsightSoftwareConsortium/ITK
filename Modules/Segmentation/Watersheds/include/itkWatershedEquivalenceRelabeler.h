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
#ifndef itkWatershedEquivalenceRelabeler_h
#define itkWatershedEquivalenceRelabeler_h

#include "itkWatershedSegmenter.h"

namespace itk
{
namespace watershed
{
/**
 *\class EquivalenceRelabeler
 *
 * This class is part of the set of watershed segmentation component objects.
 * It is an image-to-image filter that relabels its input according to a set of
 * equivalencies defined in a table.  The filter is used in
 * itk::WatershedImageFilter, for example, to relabel a segmented image
 * at different hierarchies in the merge tree (see itk::WatershedImageFilter
 * for documentation on terminology).  It simply takes its input and changes
 * any values found in the equivalency table.
 *
 * \par Inputs
 * There are two inputs to this filter, an IdentifierType itk::Image of
 * arbitrary dimension, and an itk::EquivalencyTable.  The input
 * image is the image to be relabeled and copied to the output, and the
 * EquivalencyTable identifies  how to relabel the values.
 *
 * \par Output
 * The output of this filter is the relabeled IdentifierType itk::Image of same
 * dimension and size as the input.
 *
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter
 * \sa EquivalencyTable
 * \ingroup ITKWatersheds
 */
template <typename TScalar, unsigned int TImageDimension>
class ITK_TEMPLATE_EXPORT EquivalenceRelabeler : public ProcessObject
{
public:
  /** Expose templated image dimension parameter at run time */
  static constexpr unsigned int ImageDimension = TImageDimension;

  /**  Some convenient type alias.   */
  using ImageType = Image<IdentifierType, TImageDimension>;
  using Self = EquivalenceRelabeler;
  using Superclass = ProcessObject;
  using ScalarType = TScalar;
  using EquivalencyTableType = EquivalencyTable;
  using SegmenterType = Segmenter<Image<ScalarType, TImageDimension>>;
  using DataObjectPointer = DataObject::Pointer;

  /**  Define smart pointers for this object.   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(WatershedEquivalenceRelabeler, ProcessObject);

  /** Set/Get the image to relabel.   */
  void
  SetInputImage(ImageType * img)
  {
    this->ProcessObject::SetNthInput(0, img);
  }
  const ImageType *
  GetInputImage()
  {
    return static_cast<ImageType *>(this->ProcessObject::GetInput(0));
  }

  /** Set/Get the output image */
  void
  SetOutputImage(ImageType * img)
  {
    this->ProcessObject::SetNthOutput(0, img);
  }

  typename ImageType::Pointer
  GetOutputImage()
  {
    return static_cast<ImageType *>(this->ProcessObject::GetOutput(0));
  }

  /** Set/Get the table to use in relabeling the input image.   */
  void
  SetEquivalencyTable(EquivalencyTableType * et)
  {
    this->ProcessObject::SetNthInput(1, et);
  }

  EquivalencyTableType::Pointer
  GetEquivalencyTable()
  {
    return static_cast<EquivalencyTableType *>(this->ProcessObject::GetInput(1));
  }

  /** Standard non-threaded pipeline method */
  void
  GenerateData() override;

  /** Standard itk::ProcessObject subclass method. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

protected:
  EquivalenceRelabeler()
  {
    typename ImageType::Pointer img = static_cast<ImageType *>(this->MakeOutput(0).GetPointer());
    this->SetNumberOfRequiredOutputs(1);
    this->ProcessObject::SetNthOutput(0, img.GetPointer());
  }

  ~EquivalenceRelabeler() override = default;
  EquivalenceRelabeler(const Self &) {}
  void
  operator=(const Self &)
  {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputRequestedRegion(DataObject * output) override;

  void
  GenerateInputRequestedRegion() override;
};
} // end namespace watershed
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWatershedEquivalenceRelabeler.hxx"
#endif

#endif
