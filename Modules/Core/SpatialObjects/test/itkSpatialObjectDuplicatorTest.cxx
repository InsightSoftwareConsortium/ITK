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

#include "itkSpatialObjectDuplicator.h"
#include "itkEllipseSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkDTITubeSpatialObject.h"
#include "itkMath.h"

int itkSpatialObjectDuplicatorTest(int, char* [])
{
  typedef itk::EllipseSpatialObject<3> EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(3);
  ellipse->GetProperty()->SetColor(0,1,1);

  typedef itk::SpatialObjectDuplicator<EllipseType> DuplicatorType;
  DuplicatorType::Pointer duplicator = DuplicatorType::New();


  duplicator->SetInput(ellipse);
  duplicator->Update();
  duplicator->Print(std::cout);

  EllipseType::Pointer ellipse_copy = duplicator->GetModifiableOutput();

  std::cout << ellipse_copy->GetRadius() << std::endl;
  std::cout << ellipse_copy->GetProperty()->GetColor() << std::endl;

  // Test with a group
  typedef itk::GroupSpatialObject<3> GroupType;
  GroupType::Pointer group = GroupType::New();
  group->AddSpatialObject(ellipse);

  typedef itk::SpatialObjectDuplicator<GroupType> DuplicatorGroupType;
  DuplicatorGroupType::Pointer duplicatorGroup = DuplicatorGroupType::New();
  duplicatorGroup->SetInput(group);
  duplicatorGroup->Update();
  GroupType::Pointer group_copy = duplicatorGroup->GetModifiableOutput();

  GroupType::ChildrenListType* children = group_copy->GetChildren();

  EllipseType::Pointer ellipse_copy2 = static_cast<EllipseType*>((*(children->begin())).GetPointer());
  std::cout << ellipse_copy2->GetRadius() << std::endl;

  delete children;


  // Test copying a DTITube
  typedef itk::DTITubeSpatialObject<3>       DTITubeType;
  typedef itk::DTITubeSpatialObjectPoint<3>  DTITubePointType;

  // Tubes
  DTITubeType::PointListType    list3;

  for( unsigned int i=0; i<7; i++)
    {
    DTITubePointType p;
    p.SetPosition(i*3,i*3,i*3);
    p.SetRadius(i);
    p.SetRed(i);
    p.SetGreen(i+1);
    p.SetBlue(i+2);
    p.SetAlpha(i+3);
    p.AddField(DTITubePointType::FA,i);
    p.AddField(DTITubePointType::ADC,2*i);
    p.AddField(DTITubePointType::GA,3*i);
    p.AddField("Lambda1",4*i);
    p.AddField("Lambda2",5*i);
    p.AddField("Lambda3",6*i);
    float* v = new float[6];
    // this is only for testing
    // the tensor matrix should be definite positive
    // in the real case
    for(unsigned int k=0;k<6;k++)
      {
      v[k] = k;
      }
    p.SetTensorMatrix(v);
    delete[] v;
    list3.push_back(p);
    }

  DTITubeType::Pointer dtiTube = DTITubeType::New();
  dtiTube->GetProperty()->SetName("Tube 3");
  dtiTube->SetId(3);
  dtiTube->SetPoints(list3);

  typedef itk::SpatialObjectDuplicator<DTITubeType> DuplicatorDTIType;
  DuplicatorDTIType::Pointer duplicatorDti = DuplicatorDTIType::New();
  duplicatorDti->SetInput(dtiTube);
  duplicatorDti->Update();
  DTITubeType::Pointer dtiTube_copy = duplicatorDti->GetModifiableOutput();

  // Testing DTITubeSO
  std::cout << "Testing DTITubeSpatialObject: ";
  DTITubeType::PointListType::const_iterator jdti;

  bool found = false;
  if(!strcmp(dtiTube_copy->GetTypeName(),"DTITubeSpatialObject"))
    {
    found = true;
    unsigned int value=0;

    if(dtiTube_copy->GetPoints().size() == 0)
      {
      std::cout<<" [FAILED] : Size of the point list is zero" <<std::endl;
      return EXIT_FAILURE;
      }

    for(jdti = dtiTube_copy->GetPoints().begin();
        jdti != dtiTube_copy->GetPoints().end();
        jdti++)
      {
      for(unsigned int d=0;d<3;d++)
        {
        if( itk::Math::NotAlmostEquals( (*jdti).GetPosition()[d], value * dtiTube_copy->GetId() ) )
          {
          std::cout<<" [FAILED] (Position is: " << (*jdti).GetPosition()[d] << " expected : "<< value * dtiTube_copy->GetId()<< " ) " <<std::endl;
          return EXIT_FAILURE;
          }
        }
          // Testing the color of the tube points
        if( itk::Math::NotExactlyEquals((*jdti).GetRed(), value))
          {
          std::cout<<" [FAILED] : Red : found " << ( *jdti).GetRed() << " instead of " << value <<std::endl;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetGreen(), value+1))
          {
          std::cout<<" [FAILED] : Green : found " << ( *jdti).GetGreen() << " instead of " << value+1 <<std::endl;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetBlue(), value+2))
          {
          std::cout<<"[FAILED] : Blue : found " << ( *jdti).GetBlue() << " instead of " << value+2 <<std::endl;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetAlpha(), value+3))
          {
          std::cout<<" [FAILED] : Alpha : found " << ( *jdti).GetAlpha() << " instead of " << value+3 <<std::endl;
          return EXIT_FAILURE;
          }

        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::FA), value))
          {
          std::cout<<" [FAILED] : FA : found " << ( *jdti).GetField("FA") << " instead of " << value <<std::endl;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::ADC), value*2))
          {
          std::cout<<" [FAILED] : ADC : found " << ( *jdti).GetField("ADC") << " instead of " << value*2 <<std::endl;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField(DTITubePointType::GA), value*3))
          {
          std::cout<<" [FAILED] : GA : found " << ( *jdti).GetField("FA") << " instead of " << value*3 <<std::endl;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda1"), value*4))
          {
          std::cout<<" [FAILED] : GetLambda1 : found " << ( *jdti).GetField("Lambda1") << " instead of " << value*4 <<std::endl;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda2"), value*5))
          {
          std::cout<<" [FAILED] : GetLambda2 : found " << ( *jdti).GetField("Lambda2") << " instead of " << value*5 <<std::endl;
          return EXIT_FAILURE;
          }
        if(itk::Math::NotExactlyEquals((*jdti).GetField("Lambda3"), value*6))
          {
          std::cout<<" [FAILED] : GetLambda3 : found " << ( *jdti).GetField("Lambda3") << " instead of " << value*6 <<std::endl;
          return EXIT_FAILURE;
          }
        int ind;
        for(ind=0;ind<6;ind++)
          {
          if(itk::Math::NotExactlyEquals((*jdti).GetTensorMatrix()[ind], ind))
            {
            std::cout<<" [FAILED] : GetTensorMatrix : found " << ( *jdti).GetTensorMatrix()[ind] << " instead of " << ind <<std::endl;
            return EXIT_FAILURE;
            }
          }
        value++;
        }
      }

  if(found)
    {
    std::cout<<" [PASSED]"<<std::endl;
    }
  else
    {
    std::cout << " [FAILED] : Cannot found VesselSpatialObject" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "TEST: [DONE]" << std::endl;


  return EXIT_SUCCESS;
}
