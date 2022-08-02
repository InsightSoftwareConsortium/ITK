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
/*
 * itkDTITubeSpatialObject test file.
 * This test file test also the basic functions of the CompositeSpatialObject class,
 * like Add/RemoveChild(...), Get/SetChildren(...), etc...
 */

#include "itkTestingMacros.h"
#include "itkDTITubeSpatialObject.h"
#include "itkGroupSpatialObject.h"


int
itkDTITubeSpatialObjectTest(int, char *[])
{
  using ScalarType = double;
  using Vector = itk::Vector<ScalarType, 3>;
  using Point = itk::Point<ScalarType, 3>;
  using TubeType = itk::DTITubeSpatialObject<3>;
  using TubePointer = itk::SmartPointer<TubeType>;
  using GroupType = itk::GroupSpatialObject<3>;
  using GroupPointer = itk::SmartPointer<GroupType>;
  using TubePointType = TubeType::TubePointType;
  using TubePointListType = TubeType::TubePointListType;
  using ChildrenListType = std::list<itk::SpatialObject<3>::Pointer>;
  using ChildrenListPointer = ChildrenListType *;

  Vector axis, translation;
  Point  in, out;
  double angle;
  bool   passed = true;

  //======================================
  // testing of a single SpatialObject...
  //======================================

  std::cout << "==================================" << std::endl;
  std::cout << "Testing SpatialObject:" << std::endl << std::endl;

  TubePointer tube1 = TubeType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(tube1, DTITubeSpatialObject, TubeSpatialObject);


  tube1->GetProperty().SetName("Tube 1");
  tube1->SetId(1);

  TubePointListType list;

  TubeType::TransformType::OffsetType offset;
  offset.Fill(10);
  tube1->GetModifiableObjectToParentTransform()->SetOffset(offset);

  for (unsigned int i = 0; i < 10; ++i)
  {
    TubePointType p;
    p.SetPositionInObjectSpace(i, i, i);
    p.SetRadiusInObjectSpace(1);
    list.push_back(p);
  }

  // For coverage
  TubePointType p;
  p.SetPositionInObjectSpace(1, 2, 3);
  p.SetRadiusInObjectSpace(1);
  p.Print(std::cout);

  tube1->SetPoints(list);
  tube1->Update();

  in.Fill(15);
  out.Fill(5);

  std::cout << "IsInside()...";
  if (!tube1->IsInsideInWorldSpace(in) || tube1->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  TubeType::CovariantVectorType derivative;

  std::cout << "DerivativeAt()...";
  try
  {
    tube1->DerivativeAtInWorldSpace(in, 1, derivative);
  }
  catch (...)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  TubeType::CovariantVectorType expectedDerivative;
  expectedDerivative.Fill(0);

  if (expectedDerivative != derivative)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  std::cout << "itkTubeSpatialObjectTest ";
  if (passed)
  {
    std::cout << "[PASSED]" << std::endl;
  }
  else
  {
    std::cout << "[FAILED]" << std::endl;
  }

  //==============================================
  // testing of a single CompositeSpatialObject...
  //==============================================

  std::cout << "==================================" << std::endl;
  std::cout << "Testing GroupSpatialObject:" << std::endl << std::endl;

  ChildrenListType    childrenList;
  ChildrenListPointer returnedList;
  unsigned int        nbChildren;

  TubePointer tube2 = TubeType::New();
  tube2->GetProperty().SetName("Tube 2");
  tube2->SetId(2);
  tube2->SetPoints(list);
  tube2->Update();

  TubePointer tube3 = TubeType::New();
  tube3->GetProperty().SetName("Tube 3");
  tube3->SetId(3);
  tube3->SetPoints(list);
  tube3->Update();

  GroupPointer tubeNet1 = GroupType::New();
  tubeNet1->GetProperty().SetName("tube network 1");


  tubeNet1->AddChild(tube1);
  tubeNet1->AddChild(tube2);
  tubeNet1->AddChild(tube3);
  tubeNet1->Update();

  // testing the AddChild() function...
  nbChildren = tubeNet1->GetNumberOfChildren();

  std::cout << "AddChild()...";
  if (nbChildren != 3)
  {
    std::cout << "[FAILED] [" << nbChildren << "!= 3]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  // testing the RemoveChild() function...
  std::cout << "Removing 1" << std::endl;
  tubeNet1->RemoveChild(tube1);
  std::cout << "Removing 2" << std::endl;
  tubeNet1->RemoveChild(tube2);
  std::cout << "Removing 3" << std::endl;
  tubeNet1->RemoveChild(tube3);

  nbChildren = tubeNet1->GetNumberOfChildren();

  std::cout << "RemoveChild()...";
  if (nbChildren != 0)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  tubeNet1->AddChild(tube1);
  tubeNet1->AddChild(tube2);
  tubeNet1->AddChild(tube3);

  // testing the GetChildren() function...
  childrenList.push_back(tube1);
  childrenList.push_back(tube2);
  childrenList.push_back(tube3);

  returnedList = tubeNet1->GetChildren();

  if (childrenList.size() == returnedList->size())
  {
    auto itTest = returnedList->begin();
    auto it = childrenList.begin();
    auto end = childrenList.end();

    for (unsigned int i = 0; it != end; ++itTest, ++it, i++)
    {
      if ((*itTest) != (*it))
      {
        passed = false;
        break;
      }
    }
  }
  else
  {
    passed = false;
  }

  std::cout << "GetChildren()...";
  if (!passed)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  tubeNet1->RemoveChild(tube1);
  tubeNet1->RemoveChild(tube2);
  tubeNet1->RemoveChild(tube3);

  delete returnedList;

  // testing the SetChildren() function...
  std::cout << "Set children ..." << std::endl;
  tubeNet1->SetChildren(childrenList);
  returnedList = tubeNet1->GetChildren();

  if (childrenList.size() == returnedList->size())
  {
    auto itTest = returnedList->begin();
    auto it = childrenList.begin();
    auto end = childrenList.end();

    passed = true;

    for (unsigned int i = 0; it != end; ++itTest, ++it, i++)
    {
      if ((*itTest) != (*it))
      {
        passed = false;
        break;
      }
    }
  }
  else
  {
    passed = false;
  }

  delete returnedList;
  std::cout << "SetChildren()...";
  if (!passed)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  tubeNet1->Update();

  std::cout << "HasParent()...";
  if (!tube2->HasParent())
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  translation.Fill(10);
  tubeNet1->GetModifiableObjectToParentTransform()->Translate(translation, false);
  tubeNet1->Update();

  axis.Fill(0);
  axis[1] = 1;
  angle = itk::Math::pi_over_2;
  tube2->GetModifiableObjectToParentTransform()->Rotate3D(axis, angle);
  tube2->Update();

  angle = -itk::Math::pi_over_2;
  tube3->GetModifiableObjectToParentTransform()->Rotate3D(axis, angle);
  tube3->Update();

  in.Fill(25);
  out.Fill(15);

  Point p1, p2;
  p1.Fill(15);
  p1[2] = 5;
  p2.Fill(15);
  p2[0] = 5;

  std::cout << "IsInside()...";
  if (!tubeNet1->IsInsideInWorldSpace(in, 3) || tubeNet1->IsInsideInWorldSpace(out, 3))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  std::cout << "DerivativeAt()...";
  try
  {
    tubeNet1->DerivativeAtInWorldSpace(in, static_cast<unsigned short>(1), derivative, true);
  }
  catch (...)
  {
    std::cout << "[FAILED]" << std::endl;
  }

  if (derivative == expectedDerivative)
  {
    std::cout << "[PASSED]" << std::endl;
  }
  else
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }


  //====================================================
  // testing of references behavior for SpatialObject...
  //====================================================

  std::cout << "==============================================" << std::endl;
  std::cout << "Testing references behavior for SpatialObject:" << std::endl << std::endl;

  TubePointer  tube = TubeType::New();
  GroupPointer net = GroupType::New();

  unsigned int tubeCount, netCount;
  tubeCount = tube->GetReferenceCount();
  netCount = net->GetReferenceCount();

  std::cout << "References test...";
  if (tubeCount != 1)
  {
    std::cout << "[FAILED]: Problem in Tube initialization of references count" << tubeCount << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    TubePointer localTube = tube;
    tubeCount = tube->GetReferenceCount();
    if (tubeCount != 2)
    {
      std::cout << "[FAILED]: Problem in Tube with incrementation of references count" << std::endl;
      return EXIT_FAILURE;
    }
  }

  if (netCount != 1)
  {
    std::cout << "[FAILED]: Problem in TubeNetwork initialization of references count" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    GroupPointer localNet = net;
    netCount = net->GetReferenceCount();
    if (netCount != 2)
    {
      std::cout << "[FAILED]: Problem in TubeNetwork with incrementation of references count" << std::endl;
      return EXIT_FAILURE;
    }
  }

  tubeCount = tube->GetReferenceCount();
  netCount = net->GetReferenceCount();

  if (tubeCount != 1)
  {
    std::cout << "[FAILED]: Problem in Tube with decrementation of references count" << std::endl;
    return EXIT_FAILURE;
  }

  if (netCount != 1)
  {
    std::cout << "[FAILED]: Problem in TubeNetwork with decrementation of references count" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing Set/GetParentPoint
  std::cout << "Set/GetParentPoint: ";

  tube->SetParentPoint(1);
  if (tube->GetParentPoint() != 1)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  // Testing ComputeTangentAndNormals();
  std::cout << "ComputeTangentAndNormals: ";
  tube1->ComputeTangentsAndNormals();

  TubePointType::VectorType t = static_cast<const TubePointType *>(tube1->GetPoint(1))->GetTangentInWorldSpace();
  TubePointType::CovariantVectorType n1 =
    static_cast<const TubePointType *>(tube1->GetPoint(1))->GetNormal1InWorldSpace();
  TubePointType::CovariantVectorType n2 =
    static_cast<const TubePointType *>(tube1->GetPoint(1))->GetNormal2InWorldSpace();


  const Point  t_known(itk::MakePoint(0.57735, 0.57735, 0.57735));
  const Point  n1_known(itk::MakePoint(0.707107, 0.707107, 0.0));
  const Point  n2_known(itk::MakePoint(0.408248, 0.408248, 0.816497));
  const double tol = 0.0001;

  if ((itk::Math::abs(t[0] - t_known[0]) > tol) || (itk::Math::abs(t[1] - t_known[1]) > tol) ||
      (itk::Math::abs(t[2] - t_known[2]) > tol))
  {
    std::cout << "[FAILED]" << std::endl;
    std::cout << " t = " << t << " != " << t_known << " within " << tol << std::endl;
    return EXIT_FAILURE;
  }
  if ((itk::Math::abs(n1[0] - n1_known[0]) > tol) || (itk::Math::abs(n1[1] + n1_known[1]) > tol) ||
      (itk::Math::abs(n1[2] - n1_known[2]) > tol))
  {
    std::cout << "[FAILED]" << std::endl;
    std::cout << " n1 = " << n1 << " != " << n1_known << " within " << tol << std::endl;
    return EXIT_FAILURE;
  }
  if ((itk::Math::abs(n2[0] - n2_known[0]) > tol) || (itk::Math::abs(n2[1] - n2_known[1]) > tol) ||
      (itk::Math::abs(n2[2] + n2_known[2]) > tol))
  {
    std::cout << "[FAILED]" << std::endl;
    std::cout << " n2 = " << n2 << " != " << n2_known << " within " << tol << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing IsInside() with m_EndType set to rounded end-type;
  std::cout << "IsInside() with m_RoundedEnd=True: ";
  p1.Fill(19.5);
  tube1->SetEndRounded(false);

  if (tube1->IsInsideInWorldSpace(p1))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  tube1->SetEndRounded(true);

  if (!tube1->IsInsideInWorldSpace(p1))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  // For coverage only
  std::cout << "Testing PointBasedSO: ";
  using PointBasedType = itk::PointBasedSpatialObject<3>;
  auto                                       pBSO = PointBasedType::New();
  PointBasedType::SpatialObjectPointType     pnt;
  PointBasedType::SpatialObjectPointListType ll;
  ll.push_back(pnt);
  pBSO->SetPoints(ll);
  pBSO->GetPoint(0);
  pBSO->Update();
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing PointBasedSO AddPoint: ";
  pnt.SetPositionInObjectSpace(1, 1, 1);
  pBSO->AddPoint(pnt);
  if (pBSO->GetPoint(1)->GetPositionInObjectSpace()[0] != 1)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing PointBasedSO RemovePoint: ";
  pBSO->RemovePoint(0);
  if (pBSO->GetPoints().size() != 1 || pBSO->GetPoint(0)->GetPositionInObjectSpace()[0] != 1)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Test Copy and Assignment for TubePointType
  {
    TubePointType pOriginal;

    // itk::SpatialObjectPoint
    pOriginal.SetId(250);
    pOriginal.SetColor(0.5, 0.4, 0.3, 0.2);
    pOriginal.SetPositionInObjectSpace(42, 41, 43);

    // itk::TubeSpatialObjectPoint
    TubePointType::VectorType tangent;
    tangent.Fill(1);
    pOriginal.SetTangentInObjectSpace(tangent);
    TubePointType::CovariantVectorType normal1;
    normal1.Fill(2);
    pOriginal.SetNormal1InObjectSpace(normal1);
    TubePointType::CovariantVectorType normal2;
    normal2.Fill(3);
    pOriginal.SetNormal2InObjectSpace(normal2);
    pOriginal.SetRadiusInObjectSpace(1.0);
    pOriginal.SetMedialness(2.0);
    pOriginal.SetRidgeness(3.0);
    pOriginal.SetBranchness(4.0);
    pOriginal.SetCurvature(5.0);
    pOriginal.SetLevelness(6.0);
    pOriginal.SetRoundness(7.0);
    pOriginal.SetIntensity(8.0);
    pOriginal.SetAlpha1(9.0);
    pOriginal.SetAlpha2(10.0);
    pOriginal.SetAlpha3(11.0);

    // itk::DTITubeSpatialObjectTest.cxx
    itk::DiffusionTensor3D<float> tensor;
    tensor.Fill(0);
    pOriginal.SetTensorMatrix(tensor);

    // Copy
    TubePointType pCopy(pOriginal);
    // Assign
    TubePointType pAssign = pOriginal;

    std::vector<TubePointType> pointVector;
    pointVector.push_back(pCopy);
    pointVector.push_back(pAssign);

    for (const auto & pv : pointVector)
    {
      // itk::SpatialObjectPoint
      ITK_TEST_EXPECT_EQUAL(pOriginal.GetId(), pv.GetId());
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetRed(), pv.GetRed()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetGreen(), pv.GetGreen()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetBlue(), pv.GetBlue()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetAlpha(), pv.GetAlpha()));
      for (size_t j = 0; j < 3; ++j)
      {
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetPositionInObjectSpace()[j], pv.GetPositionInObjectSpace()[j]));
      }
      // itk::TubeSpatialObjectPoint
      for (size_t j = 0; j < 3; ++j)
      {
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetTangentInObjectSpace()[j], pv.GetTangentInObjectSpace()[j]));
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetNormal1InObjectSpace()[j], pv.GetNormal1InObjectSpace()[j]));
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetNormal2InObjectSpace()[j], pv.GetNormal2InObjectSpace()[j]));
      }
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetRadiusInObjectSpace(), pv.GetRadiusInObjectSpace()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetMedialness(), pv.GetMedialness()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetRidgeness(), pv.GetRidgeness()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetBranchness(), pv.GetBranchness()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetCurvature(), pv.GetCurvature()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetLevelness(), pv.GetLevelness()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetRoundness(), pv.GetRoundness()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetIntensity(), pv.GetIntensity()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetAlpha1(), pv.GetAlpha1()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetAlpha2(), pv.GetAlpha2()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetAlpha3(), pv.GetAlpha3()));

      // itk::DTITubeSpatialObjectPoint
      const auto pOriginal_tensor = pOriginal.GetTensorMatrix();
      const auto pv_tensor = pv.GetTensorMatrix();
      for (size_t j = 0; j < 6; ++j)
      {
        ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal_tensor[j], pv_tensor[j]));
      }
    }

    std::cout << "[DONE]" << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
