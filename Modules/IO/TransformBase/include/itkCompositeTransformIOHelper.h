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
#ifndef itkCompositeTransformIOHelper_h
#define itkCompositeTransformIOHelper_h

#include "itkTransformIOBase.h"
#include "itkCompositeTransform.h"

namespace itk
{

/** \class CompositeTransformIOHelperTemplate
 *  \brief An adapter that adapts CompositeTransform into the
 *  TransformReader/Writer
 *
 *  The stumbling block with fitting CompositeTransforms into
 *  the Transform hierarchy is that it encapsulates a list of
 *  itk::Transform, templated to match the dimension and scalar
 *  type of the CompositeTransform itself.  But the Transform
 *  Reader/Writer uses instances of the superclass itk::TransformBase
 *  which is not templated.
 *
 *  This class handles this by hiding the conversion between a list
 *  of TransformBase and the queue of Transform.  It handles the
 *  conversion in a cascade of template function invocations to
 *  match up the transform list with the particular instantiation
 *  of CompositeTransform.
 * \ingroup ITKIOTransformBase
 */
template <typename TScalar>
class CompositeTransformIOHelperTemplate
{
public:
  typedef typename TransformIOBaseTemplate<TScalar>::TransformType          TransformType;
  typedef typename TransformIOBaseTemplate<TScalar>::TransformPointer       TransformPointer;
  typedef typename TransformIOBaseTemplate<TScalar>::TransformListType      TransformListType;
  typedef typename TransformIOBaseTemplate<TScalar>::ConstTransformPointer  ConstTransformPointer;
  typedef typename TransformIOBaseTemplate<TScalar>::ConstTransformListType ConstTransformListType;

  /** from a composite transform, recover a
   * TransformIOBase::ConstTransformList.
   * This will re-build the list each time it is called, so it is best
   * to call it once per CompositeTransform and access it through the
   * ConstTransformListType reference, as any subsequent calls will
   * rebuild the list and possibly invalidate any iterators on the
   * list.
   */
  ConstTransformListType &GetTransformList(const TransformType *transform);
  /** set a compositeTransform's transform list from a
   ** TransformIOABase::TransformList.  If there is any mismatch
   ** between a transform being added to composite and the composite,
   ** this will throw an exception
   */
  void SetTransformList(TransformType *transform,TransformListType &transformList);

private:
  ConstTransformListType m_TransformList;

  /** Builds a list of TransformBase from the CompositeTransform's
   ** queue. A cascade of calls with different template parameters
   ** selects the correct concrete type for CompositeTransform.
   */
  template <unsigned TDim>
  int BuildTransformList(const TransformType *transform);

  /** Sets a CompositeTransform's TransformQueue from the
   **  TransformIO's list of TransformBase. Will throw an exception
   **  if the scalar type or dimension of the transform being added
   **  doesn't match that of the concrete CompositeTransform's type.
   */
  template <unsigned TDim>
  int InternalSetTransformList(TransformType *transform,TransformListType &transformList);

};

/** This helps to meet backward compatibility */
typedef CompositeTransformIOHelperTemplate<double> CompositeTransformIOHelper;

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompositeTransformIOHelper.hxx"
#endif

#endif //  itkCompositeTransformIOHelper_h
