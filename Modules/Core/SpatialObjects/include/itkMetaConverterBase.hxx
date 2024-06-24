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
#ifndef itkMetaConverterBase_hxx
#define itkMetaConverterBase_hxx

#include "metaObject.h"

namespace itk
{

template <unsigned int VDimension>
void
MetaConverterBase<VDimension>::MetaObjectToSpatialObjectBase(const MetaObjectType * mo, SpatialObjectPointer rval)
{
  rval->SetId(mo->ID());
  rval->SetParentId(mo->ParentID());
  typename SpatialObject<VDimension>::TransformType::Pointer    tfm = SpatialObject<VDimension>::TransformType::New();
  typename SpatialObject<VDimension>::TransformType::OffsetType off;
  typename SpatialObject<VDimension>::TransformType::MatrixType mat;
  typename SpatialObject<VDimension>::TransformType::CenterType cen;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    off[i] = mo->Offset()[i];
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      mat[i][j] = mo->TransformMatrix()[i * VDimension + j];
    }
    cen[i] = mo->CenterOfRotation()[i];
  }
  tfm->SetCenter(cen);
  tfm->SetMatrix(mat);
  tfm->SetOffset(off);
  rval->SetObjectToParentTransform(tfm);
  rval->GetProperty().SetName(mo->Name());
  rval->GetProperty().SetRed(mo->Color()[0]);
  rval->GetProperty().SetGreen(mo->Color()[1]);
  rval->GetProperty().SetBlue(mo->Color()[2]);
  rval->GetProperty().SetAlpha(mo->Color()[3]);
}

template <unsigned int VDimension>
void
MetaConverterBase<VDimension>::SpatialObjectToMetaObjectBase(SpatialObjectConstPointer spatialObject,
                                                             MetaObjectType *          mo)
{
  mo->APIVersion(m_MetaIOVersion);

  mo->ID(spatialObject->GetId());
  if (spatialObject->GetParent())
  {
    mo->ParentID(spatialObject->GetParent()->GetId());
    typename SpatialObject<VDimension>::TransformType::ConstPointer tfm = spatialObject->GetObjectToParentTransform();
    double                                                          mo_off[10];
    double                                                          mo_mat[100];
    double                                                          mo_cen[10];
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      mo_off[i] = tfm->GetOffset()[i];
      for (unsigned int j = 0; j < VDimension; ++j)
      {
        mo_mat[i * VDimension + j] = tfm->GetMatrix()[i][j];
      }
      mo_cen[i] = tfm->GetCenter()[i];
    }
    mo->CenterOfRotation(mo_cen);
    mo->TransformMatrix(mo_mat);
    mo->Offset(mo_off);
  }
  mo->Name(spatialObject->GetProperty().GetName().c_str());
  mo->Color(spatialObject->GetProperty().GetRed(),
            spatialObject->GetProperty().GetGreen(),
            spatialObject->GetProperty().GetBlue(),
            spatialObject->GetProperty().GetAlpha());
}

template <unsigned int VDimension>
auto
MetaConverterBase<VDimension>::ReadMeta(const char * name) -> SpatialObjectPointer
{
  SpatialObjectPointer rval;
  MetaObjectType *     mo = this->CreateMetaObject();

  mo->APIVersion(m_MetaIOVersion);
  mo->Read(name);

  rval = this->MetaObjectToSpatialObject(mo);

  delete mo;
  return rval;
}

template <unsigned int VDimension>
bool
MetaConverterBase<VDimension>::WriteMeta(const SpatialObjectType * spatialObject, const char * name)
{
  MetaObject * mo = this->SpatialObjectToMetaObject(spatialObject);

  mo->APIVersion(m_MetaIOVersion);
  mo->FileFormatVersion(m_MetaIOVersion);
  mo->Write(name);

  delete mo;
  return true;
}

} // namespace itk

#endif // itkMetaConverterBase_hxx
