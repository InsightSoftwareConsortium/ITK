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
#include "itkMetaSceneConverter.h"

/** \class MetaDummy
 *  dummy MetaObject to add to MetaScene
 */
class MetaDummy : public MetaObject
{
public:
  MetaDummy(unsigned int dims = 3) : m_Value(0.0)
    {
      strcpy(m_ObjectTypeName,"Dummy");
      m_NDims = dims;
    }
  float GetValue() const { return m_Value; }
  void SetValue(float val) { m_Value = val; }

protected:
  virtual void M_SetupReadFields(void) ITK_OVERRIDE
    {
      MetaObject::M_SetupReadFields();
      MET_FieldRecordType *mf = new MET_FieldRecordType;
      MET_InitReadField(mf,"Value", MET_FLOAT, true);
      mf->terminateRead = false;
      m_Fields.push_back(mf);
    }
  virtual void M_SetupWriteFields(void) ITK_OVERRIDE
    {
      strcpy(m_ObjectTypeName,"Dummy");
      MetaObject::M_SetupWriteFields();

      MET_FieldRecordType *mf = new MET_FieldRecordType;
      MET_InitWriteField(mf, "Value", MET_FLOAT, m_Value);
      m_Fields.push_back(mf);
    }
  virtual bool M_Read(void) ITK_OVERRIDE
    {
      if(!MetaObject::M_Read())
        {
        return false;
        }
      MET_FieldRecordType *mf_Value =
        MET_GetFieldRecord("Value",&m_Fields);
      if(mf_Value->defined)
        {
        m_Value = (float)mf_Value->value[0];
        }
      return true;
    }

private:
  float m_Value;
};

namespace itk
{
/** \class DummySpatialObject
 */
template <unsigned int TDimension = 3>
class DummySpatialObject :
  public SpatialObject<TDimension>
{
public:
  typedef DummySpatialObject        Self;
  typedef SpatialObject<TDimension> Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** method for creation through the object factory */
  itkNewMacro(Self);

  itkTypeMacro(DummySpatialObject, SpatialObject);
  void SetValue(float val)
    {
      this->m_Value = val;
    }
  float GetValue() const
    {
      return this->m_Value;
    }

protected:
  DummySpatialObject() : m_Value(0.0)
    {
      this->SetDimension(TDimension);
      this->SetTypeName ("DummySpatialObject");
      this->GetProperty()->SetRed(1);
      this->GetProperty()->SetGreen(0);
      this->GetProperty()->SetBlue(0);
      this->GetProperty()->SetAlpha(1);
    }
  ~DummySpatialObject() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DummySpatialObject);
  float m_Value;
};

/** \class MetaConverterBase
 *  Dummy converter class
 */
template< unsigned int NDimensions = 3 >
class MetaDummyConverter :
    public MetaConverterBase< NDimensions >
{
public:
  /** Standard class typedefs */
  typedef MetaDummyConverter               Self;
  typedef MetaConverterBase< NDimensions > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDummyConverter, MetaConverterBase);

  typedef typename Superclass::SpatialObjectType SpatialObjectType;
  typedef typename SpatialObjectType::Pointer    SpatialObjectPointer;
  typedef typename Superclass::MetaObjectType    MetaObjectType;

  /** Specific class types for conversion */
  typedef DummySpatialObject<NDimensions>               DummySpatialObjectType;
  typedef typename DummySpatialObjectType::Pointer      DummySpatialObjectPointer;
  typedef typename DummySpatialObjectType::ConstPointer DummySpatialObjectConstPointer;
  typedef MetaDummy                                     DummyMetaObjectType;

  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) ITK_OVERRIDE
  {
    const DummyMetaObjectType *dummyMO = dynamic_cast<const MetaDummy *>(mo);
    if(dummyMO == ITK_NULLPTR)
      {
      itkExceptionMacro(<< "Can't convert MetaObject to MetaDummy");
      }
    DummySpatialObjectPointer dummySO = DummySpatialObjectType::New();
    dummySO->SetValue(dummyMO->GetValue());

    dummySO->GetProperty()->SetName( dummyMO->Name() );
    dummySO->SetId( dummyMO->ID() );
    dummySO->SetParentId( dummyMO->ParentID() );
    dummySO->GetProperty()->SetRed(dummyMO->Color()[0]);
    dummySO->GetProperty()->SetGreen(dummyMO->Color()[1]);
    dummySO->GetProperty()->SetBlue(dummyMO->Color()[2]);
    dummySO->GetProperty()->SetAlpha(dummyMO->Color()[3]);

    return dummySO.GetPointer();
  }

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) ITK_OVERRIDE
  {
    DummySpatialObjectConstPointer dummySO =
      dynamic_cast<const DummySpatialObjectType *>(spatialObject);
    if(dummySO.IsNull())
      {
      itkExceptionMacro(<< "Can't downcast SpatialObject to DummySpatialObject");
      }
    DummyMetaObjectType *dummyMO = new MetaDummy;
    dummyMO->SetValue(dummySO->GetValue());

    dummyMO->ID( dummySO->GetId() );
    dummyMO->Color( dummySO->GetProperty()->GetRed(),
                    dummySO->GetProperty()->GetGreen(),
                    dummySO->GetProperty()->GetBlue(),
                    dummySO->GetProperty()->GetAlpha() );

    return dummyMO;
  }

protected:
  /** Create the specific MetaObject for this class */
  virtual MetaObjectType *CreateMetaObject() ITK_OVERRIDE
  {
    return dynamic_cast<MetaObjectType *>(new DummyMetaObjectType);
  }

  MetaDummyConverter() {}
  ~MetaDummyConverter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaDummyConverter);

};

}

int itkNewMetaObjectTypeTest(int, char* [])
{
  const float Pi(3.1415926);

  typedef itk::SceneSpatialObject<3>                 SceneType;
  typedef itk::DummySpatialObject<3>                 DummyType;
  typedef itk::MetaSceneConverter<3,unsigned short>  MetaSceneConverterType;

  typedef itk::MetaDummyConverter<3>                 DummyConverterType;

  SceneType::Pointer scene(SceneType::New());

  DummyType::Pointer dummy(DummyType::New());
  dummy->GetProperty()->SetName("Dummy");
  dummy->SetId(1);
  dummy->SetValue(Pi);

  scene->AddSpatialObject(dummy);

  DummyConverterType::Pointer dummyConverter(DummyConverterType::New());

  MetaSceneConverterType converter;
  converter.RegisterMetaConverter("Dummy","DummySpatialObject",dummyConverter);

  MetaScene *metaScene = converter.CreateMetaScene(scene);
  SceneType::Pointer myScene = converter.CreateSpatialObjectScene(metaScene);

  if(!myScene)
    {
    std::cout << "No Scene : [FAILED]" << std::endl;
    delete metaScene;
    return EXIT_FAILURE;
    }
  if(myScene->GetNumberOfObjects(1) != 1)
    {
    std::cout << "found " << myScene->GetNumberOfObjects(1) << " instead of 1 [FAILED]"
              << std::endl;
    delete metaScene;
    return EXIT_FAILURE;
    }
  SceneType::ObjectListType *mySceneChildren =
    myScene->GetObjects();
  SceneType::ObjectListType::const_iterator obj;

  for(obj = mySceneChildren->begin(); obj != mySceneChildren->end(); ++obj)
    {
    std::string childType((*obj)->GetTypeName());
    if(childType != "DummySpatialObject")
      {
      std::cout << "Expected child type Dummy but found "
                << childType << " [FAILED]" << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
      }
    DummyType::Pointer p = dynamic_cast<DummyType *>((*obj).GetPointer());
    if(p.IsNull())
      {
      std::cout << "Unable to downcast child SpatialObject to DummySpatialObject"
                << "[FAILED]" << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
      }
    float value = p->GetValue();
    if(std::fabs(value - Pi) > 0.00001)
      {
      std::cout << "Expected value " << Pi
                << "but found " << value << std::endl;
      delete metaScene;
      delete mySceneChildren;
      return EXIT_FAILURE;
      }
    }
  delete mySceneChildren;
  delete metaScene;
  return EXIT_SUCCESS;
}
