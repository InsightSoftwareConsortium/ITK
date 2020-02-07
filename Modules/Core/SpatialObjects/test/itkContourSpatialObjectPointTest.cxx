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

#include "itkContourSpatialObjectPoint.h"
#include "itkTestingMacros.h"


int
itkContourSpatialObjectPointTest(int, char *[])
{
  // Test for 2D
  //
  using ContourSpatialObjectPoint2DType = itk::ContourSpatialObjectPoint<2>;

  ContourSpatialObjectPoint2DType contourSpatialObjectPoint2D;

  constexpr double                           pickedPointX = 4.35;
  constexpr double                           pickedPointY = 7.56;
  ContourSpatialObjectPoint2DType::PointType pickedPoint2D;
  pickedPoint2D[0] = pickedPointX;
  pickedPoint2D[1] = pickedPointY;

  contourSpatialObjectPoint2D.SetPickedPointInObjectSpace(pickedPoint2D);
  ITK_TEST_SET_GET_VALUE(pickedPoint2D, contourSpatialObjectPoint2D.GetPickedPointInObjectSpace());

  constexpr double                                     normalX = 1.0;
  constexpr double                                     normalY = 1.0;
  ContourSpatialObjectPoint2DType::CovariantVectorType normal2D;
  normal2D[0] = normalX;
  normal2D[1] = normalY;

  contourSpatialObjectPoint2D.SetNormalInObjectSpace(normal2D);
  ITK_TEST_SET_GET_VALUE(normal2D, contourSpatialObjectPoint2D.GetNormalInObjectSpace());

  // Create another ContourSpatialObjectPoint

  ContourSpatialObjectPoint2DType contourSpatialObjectPoint2DAlt;

  constexpr double                           pickedPointXAlt = 25.89;
  constexpr double                           pickedPointYAlt = 57.26;
  ContourSpatialObjectPoint2DType::PointType pickedPoint2DAlt;
  pickedPoint2DAlt[0] = pickedPointXAlt;
  pickedPoint2DAlt[1] = pickedPointYAlt;

  contourSpatialObjectPoint2DAlt.SetPickedPointInObjectSpace(pickedPoint2DAlt);
  ITK_TEST_SET_GET_VALUE(pickedPoint2DAlt, contourSpatialObjectPoint2DAlt.GetPickedPointInObjectSpace());

  constexpr double                                     normalXAlt = 7.28;
  constexpr double                                     normalYAlt = 14.03;
  ContourSpatialObjectPoint2DType::CovariantVectorType normal2DAlt;
  normal2DAlt[0] = normalXAlt;
  normal2DAlt[1] = normalYAlt;

  contourSpatialObjectPoint2DAlt.SetNormalInObjectSpace(normal2DAlt);
  ITK_TEST_SET_GET_VALUE(normal2DAlt, contourSpatialObjectPoint2DAlt.GetNormalInObjectSpace());

  // Assign the old ContourSpatialObjectPoint object to the alternative one
  contourSpatialObjectPoint2DAlt = contourSpatialObjectPoint2D;

  ITK_TEST_SET_GET_VALUE(pickedPoint2D, contourSpatialObjectPoint2DAlt.GetPickedPointInObjectSpace());
  ITK_TEST_SET_GET_VALUE(normal2D, contourSpatialObjectPoint2DAlt.GetNormalInObjectSpace());


  // Test for 3D
  //
  using ContourSpatialObjectPoint3DType = itk::ContourSpatialObjectPoint<3>;

  ContourSpatialObjectPoint3DType contourSpatialObjectPoint3D;

  constexpr double                           pickedPointZ = 23.78;
  ContourSpatialObjectPoint3DType::PointType pickedPoint3D;
  pickedPoint3D[0] = pickedPointX;
  pickedPoint3D[1] = pickedPointY;
  pickedPoint3D[2] = pickedPointZ;

  contourSpatialObjectPoint3D.SetPickedPointInObjectSpace(pickedPoint3D);
  ITK_TEST_SET_GET_VALUE(pickedPoint3D, contourSpatialObjectPoint3D.GetPickedPointInObjectSpace());

  constexpr double                                     normalZ = 1.0;
  ContourSpatialObjectPoint3DType::CovariantVectorType normal3D;
  normal3D[0] = normalX;
  normal3D[1] = normalY;
  normal3D[2] = normalZ;

  contourSpatialObjectPoint3D.SetNormalInObjectSpace(normal3D);
  ITK_TEST_SET_GET_VALUE(normal3D, contourSpatialObjectPoint3D.GetNormalInObjectSpace());

  // Create another ContourSpatialObjectPoint

  ContourSpatialObjectPoint3DType contourSpatialObjectPoint3DAlt;

  constexpr double                           pickedPointZAlt = 19.6;
  ContourSpatialObjectPoint3DType::PointType pickedPoint3DAlt;
  pickedPoint3DAlt[0] = pickedPointXAlt;
  pickedPoint3DAlt[1] = pickedPointYAlt;
  pickedPoint3DAlt[2] = pickedPointZAlt;

  contourSpatialObjectPoint3DAlt.SetPickedPointInObjectSpace(pickedPoint3DAlt);
  ITK_TEST_SET_GET_VALUE(pickedPoint3DAlt, contourSpatialObjectPoint3DAlt.GetPickedPointInObjectSpace());

  constexpr double                                     normalZAlt = 1.4;
  ContourSpatialObjectPoint3DType::CovariantVectorType normal3DAlt;
  normal3DAlt[0] = normalXAlt;
  normal3DAlt[1] = normalYAlt;
  normal3DAlt[2] = normalZAlt;

  contourSpatialObjectPoint3DAlt.SetNormalInObjectSpace(normal3DAlt);
  ITK_TEST_SET_GET_VALUE(normal3DAlt, contourSpatialObjectPoint3DAlt.GetNormalInObjectSpace());

  // Assign the old ContourSpatialObjectPoint object to the alternative one
  contourSpatialObjectPoint3DAlt = contourSpatialObjectPoint3D;

  ITK_TEST_SET_GET_VALUE(pickedPoint3D, contourSpatialObjectPoint3DAlt.GetPickedPointInObjectSpace());
  ITK_TEST_SET_GET_VALUE(normal3D, contourSpatialObjectPoint3DAlt.GetNormalInObjectSpace());

  // Test for 4D
  //
  using ContourSpatialObjectPoint4DType = itk::ContourSpatialObjectPoint<4>;

  ContourSpatialObjectPoint4DType contourSpatialObjectPoint4D;

  constexpr double                           pickedPointW = 4.63;
  ContourSpatialObjectPoint4DType::PointType pickedPoint4D;
  pickedPoint4D[0] = pickedPointX;
  pickedPoint4D[1] = pickedPointY;
  pickedPoint4D[2] = pickedPointZ;
  pickedPoint4D[3] = pickedPointW;

  contourSpatialObjectPoint4D.SetPickedPointInObjectSpace(pickedPoint4D);
  ITK_TEST_SET_GET_VALUE(pickedPoint4D, contourSpatialObjectPoint4D.GetPickedPointInObjectSpace());

  constexpr double                                     normalW = 3.1;
  ContourSpatialObjectPoint4DType::CovariantVectorType normal4D;
  normal4D[0] = normalX;
  normal4D[1] = normalY;
  normal4D[2] = normalZ;
  normal4D[3] = normalW;

  contourSpatialObjectPoint4D.SetNormalInObjectSpace(normal4D);
  ITK_TEST_SET_GET_VALUE(normal4D, contourSpatialObjectPoint4D.GetNormalInObjectSpace());

  // Create another ContourSpatialObjectPoint

  ContourSpatialObjectPoint4DType contourSpatialObjectPoint4DAlt;

  constexpr double                           pickedPointWAlt = 8.71;
  ContourSpatialObjectPoint4DType::PointType pickedPoint4DAlt;
  pickedPoint4DAlt[0] = pickedPointXAlt;
  pickedPoint4DAlt[1] = pickedPointYAlt;
  pickedPoint4DAlt[2] = pickedPointZAlt;
  pickedPoint4DAlt[3] = pickedPointWAlt;

  contourSpatialObjectPoint4DAlt.SetPickedPointInObjectSpace(pickedPoint4DAlt);
  ITK_TEST_SET_GET_VALUE(pickedPoint4DAlt, contourSpatialObjectPoint4DAlt.GetPickedPointInObjectSpace());

  constexpr double                                     normalWAlt = 4.23;
  ContourSpatialObjectPoint4DType::CovariantVectorType normal4DAlt;
  normal4DAlt[0] = normalXAlt;
  normal4DAlt[1] = normalYAlt;
  normal4DAlt[2] = normalZAlt;
  normal4DAlt[3] = normalWAlt;

  contourSpatialObjectPoint4DAlt.SetNormalInObjectSpace(normal4DAlt);
  ITK_TEST_SET_GET_VALUE(normal4DAlt, contourSpatialObjectPoint4DAlt.GetNormalInObjectSpace());

  // Assign the old ContourSpatialObjectPoint object to the alternative one
  contourSpatialObjectPoint4DAlt = contourSpatialObjectPoint4D;

  ITK_TEST_SET_GET_VALUE(pickedPoint4D, contourSpatialObjectPoint4DAlt.GetPickedPointInObjectSpace());
  ITK_TEST_SET_GET_VALUE(normal4D, contourSpatialObjectPoint4DAlt.GetNormalInObjectSpace());

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
