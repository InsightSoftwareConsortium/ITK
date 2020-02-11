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
#ifndef itkBlobSpatialObject_hxx
#define itkBlobSpatialObject_hxx


#include "itkBlobSpatialObject.h"

#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
BlobSpatialObject<TDimension>::BlobSpatialObject()
{
  this->SetTypeName("BlobSpatialObject");

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
BlobSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  return loPtr;
}

/** Print the blob spatial object */
template <unsigned int TDimension>
void
BlobSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "BlobSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
