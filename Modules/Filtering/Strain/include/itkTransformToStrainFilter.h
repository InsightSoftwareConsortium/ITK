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
#ifndef itkTransformToStrainFilter_h
#define itkTransformToStrainFilter_h

#include "itkDataObjectDecorator.h"
#include "itkCovariantVector.h"
#include "itkGenerateImageSource.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{

/** \class TransformToStrainFilter
 *
 * \brief Generate a strain field image from a transform.
 *
 * \tparam TTransform The first template parameter is the input transform type.
 *
 * \tparam TOperatorValueType The second template parameter defines the value type used in the derivative
 * operator (defaults to float).
 *
 * \tparam TOutputValueType The third template parameter defines the value
 * type used for output image (defaults to float).  The output image is defined
 * as a symmetric second rank tensor image whose value type is specified as this
 * third template parameter.
 *
 * Three different types of strains can be calculated, infinitesimal (default), aka
 * engineering strain, which is appropriate for small strains, Green-Lagrangian,
 * which uses a material reference system, and Eulerian-Almansi, which uses a
 * spatial reference system.  This is set with SetStrainForm().
 *
 * \sa StrainImageFilter
 *
 * \ingroup Strain
 *
 */
template <typename TTransform, typename TOperatorValueType = float, typename TOutputValueType = float>
class TransformToStrainFilter
  : public GenerateImageSource<Image<SymmetricSecondRankTensor<TOutputValueType, TTransform::InputSpaceDimension>,
                                     TTransform::InputSpaceDimension>>
{
public:
  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TTransform::InputSpaceDimension);

  typedef TTransform                                                  TransformType;
  typedef DataObjectDecorator<TransformType>                          TransformInputType;
  typedef SymmetricSecondRankTensor<TOutputValueType, ImageDimension> OutputPixelType;
  typedef Image<OutputPixelType, ImageDimension>                      OutputImageType;

  /** Standard class typedefs. */
  typedef TransformToStrainFilter              Self;
  typedef GenerateImageSource<OutputImageType> Superclass;

  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformToStrainFilter, GenerateImageSource);

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this must
   * be in physical coordinates. */
  itkSetGetDecoratedObjectInputMacro(Transform, TransformType);

  /**
   * Three different types of strains can be calculated, infinitesimal (default), aka
   * engineering strain, which is appropriate for small strains, Green-Lagrangian,
   * which uses a material reference system, and Eulerian-Almansi, which uses a
   * spatial reference system.  This is set with SetStrainForm(). */
  enum StrainFormType
  {
    INFINITESIMAL = 0,
    GREENLAGRANGIAN = 1,
    EULERIANALMANSI = 2
  };

  itkSetMacro(StrainForm, StrainFormType);
  itkGetConstMacro(StrainForm, StrainFormType);

protected:
  typedef typename OutputImageType::RegionType OutputRegionType;

  TransformToStrainFilter();

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputRegionType & outputRegion, ThreadIdType threadId) ITK_OVERRIDE;

  StrainFormType m_StrainForm;

private:
  TransformToStrainFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTransformToStrainFilter.hxx"
#endif

#endif
