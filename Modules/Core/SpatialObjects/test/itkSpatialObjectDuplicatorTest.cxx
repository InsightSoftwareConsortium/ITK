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

#include "itkSpatialObjectDuplicator.h"
#include "itkEllipseSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkDTITubeSpatialObject.h"
#include "itkMath.h"

int
itkSpatialObjectDuplicatorTest(int, char *[])
{
  using EllipseType = itk::EllipseSpatialObject<3>;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadiusInObjectSpace(3);
  ellipse->GetProperty().SetColor(0, 1, 1);

  using DuplicatorType = itk::SpatialObjectDuplicator<EllipseType>;
  DuplicatorType::Pointer duplicator = DuplicatorType::New();


  duplicator->SetInput(ellipse);
  duplicator->Update();
  duplicator->Print(std::cout);

  EllipseType::Pointer ellipse_copy = duplicator->GetOutput();

  std::cout << ellipse_copy->GetRadiusInObjectSpace() << std::endl;
  std::cout << ellipse_copy->GetProperty().GetColor() << std::endl;

  // Test with a group
  using GroupType = itk::GroupSpatialObject<3>;
  GroupType::Pointer group = GroupType::New();
  group->AddChild(ellipse);

  using DuplicatorGroupType = itk::SpatialObjectDuplicator<GroupType>;
  DuplicatorGroupType::Pointer duplicatorGroup = DuplicatorGroupType::New();
  duplicatorGroup->SetInput(group);
  duplicatorGroup->Update();
  GroupType::Pointer group_copy = duplicatorGroup->GetOutput();

  GroupType::ChildrenListType * children = group_copy->GetChildren();

  EllipseType::Pointer ellipse_copy2 = static_cast<EllipseType *>((*(children->begin())).GetPointer());
  std::cout << ellipse_copy2->GetRadiusInObjectSpace() << std::endl;

  delete children;


  // Test copying a DTITube
  using DTITubeType = itk::DTITubeSpatialObject<3>;
  using DTITubePointType = itk::DTITubeSpatialObjectPoint<3>;

  // Tubes
  DTITubeType::DTITubePointListType list3;

  for (unsigned int i = 0; i < 7; i++)
  {
    DTITubePointType p;
    p.SetPositionInObjectSpace(i * 3, i * 3, i * 3);
    p.SetRadiusInObjectSpace(i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA, i);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC, 2 * i);
    p.AddField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA, 3 * i);
    p.AddField("Lambda1", 4 * i);
    p.AddField("Lambda2", 5 * i);
    p.AddField("Lambda3", 6 * i);
    auto * v = new float[6];
    // this is only for testing
    // the tensor matrix should be definite positive
    // in the real case
    for (unsigned int k = 0; k < 6; k++)
    {
      v[k] = k;
    }
    p.SetTensorMatrix(v);
    delete[] v;
    list3.push_back(p);
  }

  DTITubeType::Pointer dtiTube = DTITubeType::New();
  dtiTube->GetProperty().SetName("Tube 3");
  dtiTube->SetId(3);
  dtiTube->SetPoints(list3);

  using DuplicatorDTIType = itk::SpatialObjectDuplicator<DTITubeType>;
  DuplicatorDTIType::Pointer duplicatorDti = DuplicatorDTIType::New();
  duplicatorDti->SetInput(dtiTube);
  duplicatorDti->Update();
  DTITubeType::Pointer dtiTube_copy = duplicatorDti->GetOutput();

  // Testing DTITubeSO
  std::cout << "Testing DTITubeSpatialObject: ";
  DTITubeType::DTITubePointListType::const_iterator jdti;

  bool found = false;
  if (!strcmp(dtiTube_copy->GetTypeName().c_str(), "DTITubeSpatialObject"))
  {
    found = true;
    unsigned int value = 0;

    if (dtiTube_copy->GetPoints().empty())
    {
      std::cout << " [FAILED] : Size of the point list is zero" << std::endl;
      return EXIT_FAILURE;
    }

    for (jdti = dtiTube_copy->GetPoints().begin(); jdti != dtiTube_copy->GetPoints().end(); jdti++)
    {
      for (unsigned int d = 0; d < 3; d++)
      {
        if (itk::Math::NotAlmostEquals((*jdti).GetPositionInWorldSpace()[d], value * dtiTube_copy->GetId()))
        {
          std::cout << " [FAILED] (Position is: " << (*jdti).GetPositionInWorldSpace()[d]
                    << " expected : " << value * dtiTube_copy->GetId() << " ) " << std::endl;
          return EXIT_FAILURE;
        }
      }
      // Testing the color of the tube points
      if (itk::Math::NotExactlyEquals((*jdti).GetRed(), value))
      {
        std::cout << " [FAILED] : RGBColormapFilterEnum::Red : found " << (*jdti).GetRed() << " instead of " << value
                  << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals((*jdti).GetGreen(), value + 1))
      {
        std::cout << " [FAILED] : RGBColormapFilterEnum::Green : found " << (*jdti).GetGreen() << " instead of "
                  << value + 1 << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals((*jdti).GetBlue(), value + 2))
      {
        std::cout << "[FAILED] : RGBColormapFilterEnum::Blue : found " << (*jdti).GetBlue() << " instead of "
                  << value + 2 << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals((*jdti).GetAlpha(), value + 3))
      {
        std::cout << " [FAILED] : Alpha : found " << (*jdti).GetAlpha() << " instead of " << value + 3 << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals(
            (*jdti).GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA), value))
      {
        std::cout << " [FAILED] : FA : found " << (*jdti).GetField("FA") << " instead of " << value << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals(
            (*jdti).GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC), value * 2))
      {
        std::cout << " [FAILED] : ADC : found " << (*jdti).GetField("ADC") << " instead of " << value * 2 << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals(
            (*jdti).GetField(itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA), value * 3))
      {
        std::cout << " [FAILED] : GA : found " << (*jdti).GetField("FA") << " instead of " << value * 3 << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals((*jdti).GetField("Lambda1"), value * 4))
      {
        std::cout << " [FAILED] : GetLambda1 : found " << (*jdti).GetField("Lambda1") << " instead of " << value * 4
                  << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals((*jdti).GetField("Lambda2"), value * 5))
      {
        std::cout << " [FAILED] : GetLambda2 : found " << (*jdti).GetField("Lambda2") << " instead of " << value * 5
                  << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals((*jdti).GetField("Lambda3"), value * 6))
      {
        std::cout << " [FAILED] : GetLambda3 : found " << (*jdti).GetField("Lambda3") << " instead of " << value * 6
                  << std::endl;
        return EXIT_FAILURE;
      }
      int ind;
      for (ind = 0; ind < 6; ind++)
      {
        if (itk::Math::NotExactlyEquals((*jdti).GetTensorMatrix()[ind], ind))
        {
          std::cout << " [FAILED] : GetTensorMatrix : found " << (*jdti).GetTensorMatrix()[ind] << " instead of " << ind
                    << std::endl;
          return EXIT_FAILURE;
        }
      }
      value++;
    }
  }

  // Test streaming enumeration for DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField elements
  const std::set<itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField> allDTITubeSpatialObjectPointField{
    itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA,
    itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC,
    itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA
  };
  for (const auto & ee : allDTITubeSpatialObjectPointField)
  {
    std::cout << "STREAMED ENUM VALUE DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField: " << ee
              << std::endl;
  }

  if (found)
  {
    std::cout << " [PASSED]" << std::endl;
  }
  else
  {
    std::cout << " [FAILED] : Cannot found VesselSpatialObject" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "TEST: [DONE]" << std::endl;


  return EXIT_SUCCESS;
}
