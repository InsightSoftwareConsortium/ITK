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
#ifndef itkLabelSelectionPixelAccessor_h
#define itkLabelSelectionPixelAccessor_h

#include "itkImageAdaptor.h"

namespace itk
{
namespace Accessor
{
/** \class LabelSelectionPixelAccessor
 * \brief Return a binary mask of the selected label
 *
 * LabelSelectionPixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the function to it and cast the result according
 * to the types defined as template parameters
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 * \ingroup GenericLabelInterpolator
 */
template< class TInternalType, class TExternalType >
class ITK_EXPORT LabelSelectionPixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  void SetAcceptedValue(TInternalType value) { m_AcceptedValue = value; }

  inline TExternalType Get(const TInternalType & input) const
  {
    return (TExternalType)(
             ( input == m_AcceptedValue ) ? 1 : 0 );
  }
protected:
  TInternalType m_AcceptedValue;
};
} // end namespace Accessor
} // end namespace itk
#endif
