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

#ifndef itkFastMarchingImageToNodePairContainerAdaptor_h
#define itkFastMarchingImageToNodePairContainerAdaptor_h

#include "itkObject.h"
#include "itkFastMarchingTraits.h"

namespace itk
{
/**
 * \class FastMarchingImageToNodePairContainerAdaptor
 * \brief Convenient adaptor class which converts Image into
 * FastMarching::NodePairContainerType used for initializing the FastMarching.
 *
 * One provides images by means of SetAliveImage, SetTrialImage, SetForbiddenImage
 * and get corresponding containers by means of GetAlivePoints, GetTrialPoints,
 * GetForbiddenPoints.
 *
 * One can globally set the associated value for all FastMarchingTraitsBase::Alive
 * points by means of SetAliveValue; and for all FastMarchingTraitsBase::Trial
 * points by means of SetTrialValue.
 *
 * To restrict the evolution of the front in certain area, one can use
 * SetForbiddenImage and SetIsForbiddenImageBinaryMask depending on the input
 * image.
 *
 * \sa FastMarchingTraitsBase
 * \sa FastMarchingBase
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput, typename TImage>
class ITK_TEMPLATE_EXPORT FastMarchingImageToNodePairContainerAdaptor : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingImageToNodePairContainerAdaptor);

  using Self = FastMarchingImageToNodePairContainerAdaptor;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingImageToNodePairContainerAdaptor, LightObject);

  using Traits = FastMarchingTraits<TInput, TOutput>;
  using NodePairType = typename Traits::NodePairType;
  using NodePairContainerType = typename Traits::NodePairContainerType;
  using NodePairContainerPointer = typename Traits::NodePairContainerPointer;
  using LabelType = typename Traits::LabelType;
  using OutputPixelType = typename Traits::OutputPixelType;

  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using ImagePixelType = typename ImageType::PixelType;

  static constexpr unsigned int ImageDimension = Traits::ImageDimension;

  /** \brief Set one Alive Image.
    \note Only pixels with non null values are considered as
    FastMarchingTraitsBase::Alive points.*/
  void
  SetAliveImage(const ImageType * iImage);

  /** \brief Set one Trial Image.
    \note Only pixels with non null values are considered as
    FastMarchingTraitsBase::Trialpoints.*/
  void
  SetTrialImage(const ImageType * iImage);

  /** \brief Set one Forbidden Image.
    There are two possible behaviors here depending on
    m_IsForbiddenImageBinaryMask:

    \li if m_IsForbiddenImageBinaryMask is \c true, then the input image
    is a binary mask; thus null values are considered as
    FastMarchingTraitsBase::Forbidden points

    \li else (m_IsForbiddenImageBinaryMask is \c fasle) non null values
    represents FastMarchingTraitsBase::Forbidden points*/
  void
  SetForbiddenImage(const ImageType * iImage);

  itkSetMacro(IsForbiddenImageBinaryMask, bool);
  itkBooleanMacro(IsForbiddenImageBinaryMask);

  /** \brief Get resulting Alive Points container*/
  NodePairContainerType *
  GetAlivePoints();

  /** \brief Get resulting Trial Points container*/
  NodePairContainerType *
  GetTrialPoints();

  /** \brief Get resulting Forbidden Points container*/
  NodePairContainerType *
  GetForbiddenPoints();

  itkSetMacro(AliveValue, OutputPixelType);
  itkSetMacro(TrialValue, OutputPixelType);

  /** \brief Perform the conversion. */
  void
  Update();

protected:
  /** \brief Constructor */
  FastMarchingImageToNodePairContainerAdaptor();

  /** \brief Destructor */
  ~FastMarchingImageToNodePairContainerAdaptor() override = default;

  ImageConstPointer m_AliveImage;
  ImageConstPointer m_TrialImage;
  ImageConstPointer m_ForbiddenImage;

  NodePairContainerPointer m_AlivePoints;
  NodePairContainerPointer m_TrialPoints;
  NodePairContainerPointer m_ForbiddenPoints;

  OutputPixelType m_AliveValue;
  OutputPixelType m_TrialValue;

  bool m_IsForbiddenImageBinaryMask{ false };

  virtual void
  GenerateData();

  /** */
  void
  SetPointsFromImage(const ImageType * image, const LabelType & iLabel, const OutputPixelType & iValue);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingImageToNodePairContainerAdaptor.hxx"
#endif

#endif // itkFastMarchingImageToNodePairContainerAdaptor_h
