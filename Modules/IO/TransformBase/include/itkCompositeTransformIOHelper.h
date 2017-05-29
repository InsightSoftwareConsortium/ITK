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

#include "ITKIOTransformBaseExport.h"

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
template<typename TParametersValueType>
class ITK_TEMPLATE_EXPORT CompositeTransformIOHelperTemplate
{
public:
  typedef typename TransformIOBaseTemplate<TParametersValueType>::TransformType          TransformType;
  typedef typename TransformIOBaseTemplate<TParametersValueType>::TransformPointer       TransformPointer;
  typedef typename TransformIOBaseTemplate<TParametersValueType>::TransformListType      TransformListType;
  typedef typename TransformIOBaseTemplate<TParametersValueType>::ConstTransformPointer  ConstTransformPointer;
  typedef typename TransformIOBaseTemplate<TParametersValueType>::ConstTransformListType ConstTransformListType;

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
  template <unsigned int VDimension>
  int BuildTransformList(const TransformType *transform);

  /** Sets a CompositeTransform's TransformQueue from the
   **  TransformIO's list of TransformBase. Will throw an exception
   **  if the scalar type or dimension of the transform being added
   **  doesn't match that of the concrete CompositeTransform's type.
   */
  template <unsigned int VDimension>
  int InternalSetTransformList(TransformType *transform,TransformListType &transformList);
};

/** This helps to meet backward compatibility */
typedef CompositeTransformIOHelperTemplate<double> CompositeTransformIOHelper;

} // namespace itk

// Note: Explicit instantiation is done in itkCompositeTransformIOHelper.cxx

#endif //  itkCompositeTransformIOHelper_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_CompositeTransformIOHelper
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#  if defined( ITKIOTransformBase_EXPORTS )
//   We are building this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#  else
//   We are using this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_EXPORT
#  endif
namespace itk
{

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

  extern template class ITKIOTransformBase_EXPORT_EXPLICIT CompositeTransformIOHelperTemplate< double >;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT CompositeTransformIOHelperTemplate< float >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

} // end namespace itk
#  undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif
