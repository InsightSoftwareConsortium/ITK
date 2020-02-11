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

#include "itkCastSpatialObjectFilter.h"
#include "itkGroupSpatialObject.h"
#include "itkTubeSpatialObject.h"
#include "itkMath.h"

int
itkCastSpatialObjectFilterTest(int, char *[])
{
  // Ellipse
  using EllipseType = itk::EllipseSpatialObject<3>;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadiusInObjectSpace(3);
  ellipse->GetProperty().SetColor(0, 1, 1);

  // Tube
  using TubeType = itk::TubeSpatialObject<3>;
  using TubePointType = itk::TubeSpatialObjectPoint<3>;
  TubeType::TubePointListType list3;
  for (unsigned int i = 0; i < 7; i++)
  {
    TubePointType p;
    p.SetPositionInObjectSpace(i * 3, i * 3, i * 3);
    p.SetRadiusInObjectSpace(i);
    p.SetRed(i);
    p.SetGreen(i + 1);
    p.SetBlue(i + 2);
    p.SetAlpha(i + 3);
    list3.push_back(p);
  }
  TubeType::Pointer tube = TubeType::New();
  tube->GetProperty().SetName("Tube 3");
  tube->SetId(3);
  tube->SetPoints(list3);

  // Group
  using GroupType = itk::GroupSpatialObject<3>;
  GroupType::Pointer group = GroupType::New();
  group->AddChild(ellipse);
  ellipse->AddChild(tube);

  using CastType = itk::CastSpatialObjectFilter<3>;
  using TubeListType = std::list<TubeType::Pointer>;
  CastType::Pointer caster = CastType::New();
  caster->SetInput(group);
  TubeListType * tList = caster->GetTubes();

  TubeType::Pointer tListTube = (*tList).begin()->GetPointer();

  bool found = false;
  if (!strcmp(tListTube->GetTypeName().c_str(), "TubeSpatialObject"))
  {
    found = true;
    unsigned int value = 0;

    if (tListTube->GetPoints().empty())
    {
      std::cout << " [FAILED] : Size of the point list is zero" << std::endl;
      return EXIT_FAILURE;
    }

    TubeType::TubePointListType::const_iterator pnt;
    for (pnt = tListTube->GetPoints().begin(); pnt != tListTube->GetPoints().end(); pnt++)
    {
      for (unsigned int d = 0; d < 3; d++)
      {
        if (itk::Math::NotAlmostEquals((*pnt).GetPositionInWorldSpace()[d], value * tListTube->GetId()))
        {
          std::cout << " [FAILED] (Position is: " << (*pnt).GetPositionInWorldSpace()[d]
                    << " expected : " << value * tListTube->GetId() << " ) " << std::endl;
          return EXIT_FAILURE;
        }
      }
      value++;
    }
  }

  if (found)
  {
    std::cout << " [PASSED]" << std::endl;
  }
  else
  {
    std::cout << " [FAILED] : Cannot found TubeSpatialObject" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "TEST: [DONE]" << std::endl;


  return EXIT_SUCCESS;
}
