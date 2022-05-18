/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFEMObjectSpatialObject_hxx
#define itkFEMObjectSpatialObject_hxx


namespace itk
{

template <unsigned int TDimension>
FEMObjectSpatialObject<TDimension>::FEMObjectSpatialObject()
{
  this->SetTypeName("FEMObjectSpatialObject");
  m_FEMObject = FEMObjectType::New();
}

template <unsigned int TDimension>
FEMObjectSpatialObject<TDimension>::~FEMObjectSpatialObject() = default;

template <unsigned int TDimension>
void
FEMObjectSpatialObject<TDimension>::SetFEMObject(FEMObjectType * femobject)
{
  if (!femobject)
  {
    return;
  }

  m_FEMObject = femobject;
}

template <unsigned int TDimension>
void
FEMObjectSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(FEMObject);
}

template <unsigned int TDimension>
ModifiedTimeType
FEMObjectSpatialObject<TDimension>::GetMTime() const
{
  ModifiedTimeType       latestMTime = Superclass::GetMTime();
  const ModifiedTimeType femobjectMTime = m_FEMObject->GetMTime();

  if (femobjectMTime > latestMTime)
  {
    latestMTime = femobjectMTime;
  }

  return latestMTime;
}

} // end namespace itk

#endif
