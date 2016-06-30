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
#ifndef itkSplitComponentsImageFilter_h
#define itkSplitComponentsImageFilter_h

#include "itkFixedArray.h"
#include "itkImageToImageFilter.h"

namespace itk
{

/** \class SplitComponentsImageFilter
 *
 * \brief Extract components of an Image with multi-component pixels.
 *
 * This class extracts components of itk::Image of itk::Vector's
 * itk::CovariantVector, itk::SymmetricSecondRankTensor, or other classes that
 * have the same interface.  The interface must implement ValueType operator[] (
 * unsigned int ).
 *
 * It puts an image on every output corresponding to each component.
 *
 * \ingroup SplitComponents
 *
 * \sa VectorImageToImageAdaptor
 * \sa Vector
 * \sa CovariantVector
 * \sa SymmetricSecondRankTensor
 * \sa DiffusionTensor3D
 * \sa NthElementImageAdaptor
 */
template <class TInputImage, class TOutputImage, unsigned int TComponents = TInputImage::ImageDimension>
class SplitComponentsImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);
  /** Components enumeration. */
  itkStaticConstMacro(Components, unsigned int, TComponents);

  /** Image types. */
  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  /** Standard class typedefs. */
  typedef SplitComponentsImageFilter                          Self;
  typedef ImageToImageFilter<InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  typedef FixedArray<bool, TComponents> ComponentsMaskType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SplitComponentsImageFilter, ImageToImageFilter);

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the components mask.  The mask is as long as the number of
   * components, and only values in the mask that evaluate are true will be
   * populated in the output.  The default is all true. */
  itkSetMacro(ComponentsMask, ComponentsMaskType);
  itkGetConstReferenceMacro(ComponentsMask, ComponentsMaskType);

protected:
  SplitComponentsImageFilter();
  virtual ~SplitComponentsImageFilter() {}

  /** Do not allocate outputs that we will not populate. */
  virtual void
  AllocateOutputs();

  virtual void
  ThreadedGenerateData(const OutputRegionType & outputRegion, ThreadIdType threadId);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SplitComponentsImageFilter);

  ComponentsMaskType m_ComponentsMask;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSplitComponentsImageFilter.hxx"
#endif

#endif
