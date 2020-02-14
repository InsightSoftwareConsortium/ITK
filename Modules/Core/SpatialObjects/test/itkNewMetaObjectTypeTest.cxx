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
#include "itkMetaSceneConverter.h"

/**
 *\class MetaDummy
 *  dummy MetaObject to add to MetaScene
 */
class MetaDummy : public MetaObject
{
public:
  MetaDummy(unsigned int dims = 3)
  {
    strcpy(m_ObjectTypeName, "Dummy");
    m_NDims = dims;
  }
  float
  GetValue() const
  {
    return m_Value;
  }
  void
  SetValue(float val)
  {
    m_Value = val;
  }

protected:
  void
  M_SetupReadFields() override
  {
    MetaObject::M_SetupReadFields();
    auto * mf = new MET_FieldRecordType;
    MET_InitReadField(mf, "Value", MET_FLOAT, true);
    mf->terminateRead = false;
    m_Fields.push_back(mf);
  }
  void
  M_SetupWriteFields() override
  {
    strcpy(m_ObjectTypeName, "Dummy");
    MetaObject::M_SetupWriteFields();

    auto * mf = new MET_FieldRecordType;
    MET_InitWriteField(mf, "Value", MET_FLOAT, m_Value);
    m_Fields.push_back(mf);
  }
  bool
  M_Read() override
  {
    if (!MetaObject::M_Read())
    {
      return false;
    }
    MET_FieldRecordType * mf_Value = MET_GetFieldRecord("Value", &m_Fields);
    if (mf_Value->defined)
    {
      m_Value = (float)mf_Value->value[0];
    }
    return true;
  }

private:
  float m_Value{ 0.0 };
};

namespace itk
{
/**
 *\class DummySpatialObject
 */
template <unsigned int TDimension = 3>
class DummySpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DummySpatialObject);

  using Self = DummySpatialObject;
  using Superclass = SpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** method for creation through the object factory */
  itkNewMacro(Self);

  itkTypeMacro(DummySpatialObject, SpatialObject);
  void
  SetValue(float val)
  {
    this->m_Value = val;
  }
  float
  GetValue() const
  {
    return this->m_Value;
  }

protected:
  DummySpatialObject()
  {
    this->SetTypeName("DummySpatialObject");
    this->GetProperty().SetRed(1);
    this->GetProperty().SetGreen(0);
    this->GetProperty().SetBlue(0);
    this->GetProperty().SetAlpha(1);
  }
  ~DummySpatialObject() override = default;

private:
  float m_Value{ 0.0 };
};

/**
 *\class MetaConverterBase
 *  Dummy converter class
 */
template <unsigned int NDimensions = 3>
class MetaDummyConverter : public MetaConverterBase<NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaDummyConverter);

  /** Standard class type aliases */
  using Self = MetaDummyConverter;
  using Superclass = MetaConverterBase<NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDummyConverter, MetaConverterBase);

  using SpatialObjectType = typename Superclass::SpatialObjectType;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using MetaObjectType = typename Superclass::MetaObjectType;

  /** Specific class types for conversion */
  using DummySpatialObjectType = DummySpatialObject<NDimensions>;
  using DummySpatialObjectPointer = typename DummySpatialObjectType::Pointer;
  using DummySpatialObjectConstPointer = typename DummySpatialObjectType::ConstPointer;
  using DummyMetaObjectType = MetaDummy;

  /** Convert the MetaObject to Spatial Object */
  SpatialObjectPointer
  MetaObjectToSpatialObject(const MetaObjectType * mo) override
  {
    const auto * dummyMO = dynamic_cast<const MetaDummy *>(mo);
    if (dummyMO == nullptr)
    {
      itkExceptionMacro(<< "Can't convert MetaObject to MetaDummy");
    }
    DummySpatialObjectPointer dummySO = DummySpatialObjectType::New();
    dummySO->SetValue(dummyMO->GetValue());

    dummySO->GetProperty().SetName(dummyMO->Name());
    dummySO->SetId(dummyMO->ID());
    dummySO->SetParentId(dummyMO->ParentID());
    dummySO->GetProperty().SetRed(dummyMO->Color()[0]);
    dummySO->GetProperty().SetGreen(dummyMO->Color()[1]);
    dummySO->GetProperty().SetBlue(dummyMO->Color()[2]);
    dummySO->GetProperty().SetAlpha(dummyMO->Color()[3]);

    return dummySO.GetPointer();
  }

  /** Convert the SpatialObject to MetaObject */
  MetaObjectType *
  SpatialObjectToMetaObject(const SpatialObjectType * spatialObject) override
  {
    DummySpatialObjectConstPointer dummySO = dynamic_cast<const DummySpatialObjectType *>(spatialObject);
    if (dummySO.IsNull())
    {
      itkExceptionMacro(<< "Can't downcast SpatialObject to DummySpatialObject");
    }
    auto * dummyMO = new MetaDummy;
    dummyMO->SetValue(dummySO->GetValue());

    dummyMO->ID(dummySO->GetId());
    dummyMO->Color(dummySO->GetProperty().GetRed(),
                   dummySO->GetProperty().GetGreen(),
                   dummySO->GetProperty().GetBlue(),
                   dummySO->GetProperty().GetAlpha());

    return dummyMO;
  }

protected:
  /** Create the specific MetaObject for this class */
  MetaObjectType *
  CreateMetaObject() override
  {
    return dynamic_cast<MetaObjectType *>(new DummyMetaObjectType);
  }

  MetaDummyConverter() = default;
  ~MetaDummyConverter() override = default;
};

} // namespace itk

int
itkNewMetaObjectTypeTest(int, char *[])
{
  const float Pi(3.1415926);

  using GroupType = itk::GroupSpatialObject<3>;
  using DummyType = itk::DummySpatialObject<3>;
  using MetaSceneConverterType = itk::MetaSceneConverter<3, unsigned short>;
  using SpatialObjectType = itk::SpatialObject<3>;

  using DummyConverterType = itk::MetaDummyConverter<3>;

  GroupType::Pointer group(GroupType::New());

  DummyType::Pointer dummy(DummyType::New());
  dummy->GetProperty().SetName("Dummy");
  dummy->SetId(1);
  dummy->SetValue(Pi);

  group->AddChild(dummy);

  DummyConverterType::Pointer dummyConverter(DummyConverterType::New());

  MetaSceneConverterType::Pointer converter = MetaSceneConverterType::New();
  converter->RegisterMetaConverter("Dummy", "DummySpatialObject", dummyConverter);

  MetaScene * metaScene = converter->CreateMetaScene(group);

  SpatialObjectType::Pointer myScene = converter->CreateSpatialObjectScene(metaScene);


  if (!myScene)
  {
    std::cout << "No Scene : [FAILED]" << std::endl;
    delete metaScene;
    return EXIT_FAILURE;
  }

  if (myScene->GetNumberOfChildren(1) != 1)
  {
    std::cout << "found " << myScene->GetNumberOfChildren(1) << " instead of 1 [FAILED]" << std::endl;
    delete metaScene;
    return EXIT_FAILURE;
  }

  SpatialObjectType::ObjectListType * mySceneChildren = myScene->GetChildren();

  SpatialObjectType::ObjectListType::const_iterator obj;

  for (obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
  {
    std::string childType((*obj)->GetTypeName());
    if (childType != "DummySpatialObject")
    {
      std::cout << "Expected child type Dummy but found " << childType << " [FAILED]" << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
    }
    DummyType::Pointer p = dynamic_cast<DummyType *>((*obj).GetPointer());
    if (p.IsNull())
    {
      std::cout << "Unable to downcast child SpatialObject to DummySpatialObject"
                << "[FAILED]" << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
    }
    float value = p->GetValue();
    if (std::fabs(value - Pi) > 0.00001)
    {
      std::cout << "Expected value " << Pi << "but found " << value << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
    }
  }
  delete mySceneChildren;
  delete metaScene;
  return EXIT_SUCCESS;
}
