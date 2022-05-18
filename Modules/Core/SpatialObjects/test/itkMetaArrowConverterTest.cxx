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
#include "itkMetaArrowConverter.h"
#include "itkGroupSpatialObject.h"
#include <iostream>
#include "itkMath.h"
#include "itkTestingMacros.h"

/**
 * This is a test file for the itkMetaArrowConverter class.
 *
 *
 * This test creates a sample ArrowSpatialObject and a sample MetaArrow and
 * Converts between the two, testing for conversion completeness.  At the
 * moment, this means testing to make sure length and color are properly
 * converted, but it may also include testing direction and position.  The
 * test also runs the read and write methods to test them.
 *
 * Notes: Parent cannot be converted from MetaObject to SpatialObject since
 *        MetaObject only holds a parent id rather than a parent object.
 *        Only the ParentID can be properly converted.
 */
int
itkMetaArrowConverterTest(int argc, char * argv[])
{
  // Check parameters
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " OutputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  // type alias
  constexpr unsigned int Dimensions = 3;
  using SpatialObjectType = itk::ArrowSpatialObject<Dimensions>;
  using SpatialObjectParentType = itk::GroupSpatialObject<Dimensions>;
  using ConverterType = itk::MetaArrowConverter<Dimensions>;

  // instantiate new converter and object
  auto converter = ConverterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(converter, MetaArrowConverter, MetaConverterBase);

  //
  // create the test data
  //

  // direction
  SpatialObjectType::VectorType direction;
  direction[0] = 0;
  direction[1] = 1;
  direction[2] = 2;
  double norm = direction[0] * direction[0] + direction[1] * direction[1] + direction[2] * direction[2];
  norm = std::sqrt(norm);
  direction[0] /= norm;
  direction[1] /= norm;
  direction[2] /= norm;

  double mDirection[3];
  mDirection[0] = 0;
  mDirection[1] = 1;
  mDirection[2] = 2;
  double mNorm = mDirection[0] * mDirection[0] + mDirection[1] * mDirection[1] + mDirection[2] * mDirection[2];
  mNorm = std::sqrt(mNorm);
  mDirection[0] /= mNorm;
  mDirection[1] /= mNorm;
  mDirection[2] /= mNorm;

  // position
  SpatialObjectType::PointType position;
  position[0] = -1;
  position[1] = -2;
  position[2] = -3;
  double mPosition[3];
  mPosition[0] = -1;
  mPosition[1] = -2;
  mPosition[2] = -3;

  // length
  double length = 2.3;

  // color
  float color[4];
  color[0] = 1;
  color[1] = .5;
  color[2] = .25;
  color[3] = 1;

  // set up itkArrow
  auto itkArrow = SpatialObjectType::New();
  itkArrow->SetDirectionInObjectSpace(direction);
  itkArrow->SetPositionInObjectSpace(position);
  itkArrow->SetLengthInObjectSpace(length);
  itkArrow->GetProperty().SetRed(color[0]);
  itkArrow->GetProperty().SetGreen(color[1]);
  itkArrow->GetProperty().SetBlue(color[2]);
  itkArrow->GetProperty().SetAlpha(color[3]);

  auto itkParent = SpatialObjectParentType::New();
  itkParent->SetId(1);
  itkParent->AddChild(itkArrow);
  itkParent->Update();

  // set up metaArrow
  auto * metaArrow = new MetaArrow(Dimensions);
  metaArrow->Length(static_cast<float>(length));
  metaArrow->Position((const double *)mPosition);
  metaArrow->Direction((const double *)mDirection);
  metaArrow->Color((const float *)color);
  metaArrow->ParentID(itkParent->GetId());

  // precision limit for comparing floats and doubles
  double precisionLimit = .000001;

  //
  // test itk to metaArrow
  //
  auto * newMetaArrow = dynamic_cast<MetaArrow *>(converter->SpatialObjectToMetaObject(itkArrow));
  if (newMetaArrow == nullptr)
  {
    itkGenericExceptionMacro(<< "Failed to downcast from MetaObject to MetaArrow");
  }

  // check length
  double metaLength = newMetaArrow->Length();

  // if (metaLength != static_cast<float>(length))
  if (itk::Math::abs(metaLength - length) > precisionLimit)
  {
    std::cout << "Conversion to MetaArrow failed to convert length [FAILED]" << std::endl;
    std::cout << "  Meta Length = " << metaLength << std::endl;
    std::cout << "  SO Length = " << length << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: length" << std::endl;

  // check color
  const float * newMetaColor = newMetaArrow->Color();
  if (itk::Math::NotExactlyEquals(newMetaColor[0], color[0]) ||
      itk::Math::NotExactlyEquals(newMetaColor[1], color[1]) ||
      itk::Math::NotExactlyEquals(newMetaColor[2], color[2]) || itk::Math::NotExactlyEquals(newMetaColor[3], color[3]))
  {
    std::cout << "Conversion to MetaArrow failed to convert color [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: color" << std::endl;

  // check parent id
  if (newMetaArrow->ParentID() != itkArrow->GetParent()->GetId())
  {
    std::cout << "Conversion to MetaArrow failed to convert parent [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: parent id" << std::endl;

  // check position
  const double * metaPosition = newMetaArrow->Position();
  if (itk::Math::abs(metaPosition[0] - position[0]) > precisionLimit ||
      itk::Math::abs(metaPosition[1] - position[1]) > precisionLimit ||
      itk::Math::abs(metaPosition[2] - position[2]) > precisionLimit)
  {
    std::cout << "Conversion to MetaArrow failed to convert position [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: position" << std::endl;

  // check direction (note: need to normalize before comparing)
  SpatialObjectType::VectorType directionNorm = direction;
  directionNorm.Normalize();
  const double *                newMetaDirection = newMetaArrow->Direction();
  SpatialObjectType::VectorType newMetaDirectionNorm;
  newMetaDirectionNorm[0] = newMetaDirection[0];
  newMetaDirectionNorm[1] = newMetaDirection[1];
  newMetaDirectionNorm[2] = newMetaDirection[2];

  // normalize if the vector isn't all zeros
  if (newMetaDirection[0] != 0.0 || newMetaDirection[1] != 0.0 || newMetaDirection[2] != 0.0)
  {
    newMetaDirectionNorm.Normalize();
  }

  if (itk::Math::abs(newMetaDirectionNorm[0] - directionNorm[0]) > precisionLimit ||
      itk::Math::abs(newMetaDirectionNorm[1] - directionNorm[1]) > precisionLimit ||
      itk::Math::abs(newMetaDirectionNorm[2] - directionNorm[2]) > precisionLimit)
  {
    std::cout << "Conversion to SpatialObject failed to convert direction [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: direction" << std::endl;

  // newMetaArrow had served its purpose,
  // must now return to the emptiness of the universe
  delete newMetaArrow;


  //
  // test metaArrow to itk
  //
  SpatialObjectType::Pointer newItkArrow =
    dynamic_cast<SpatialObjectType *>(converter->MetaObjectToSpatialObject(metaArrow).GetPointer());
  newItkArrow->Update();

  // check length
  if (itk::Math::abs(newItkArrow->GetLengthInWorldSpace() - metaArrow->Length()) > precisionLimit)
  {
    std::cout << "Conversion to SpatialObject failed to convert length [FAILED]" << std::endl;
    std::cout << "  Meta Length = " << metaArrow->Length() << std::endl;
    std::cout << "  SO Length In World = " << newItkArrow->GetLengthInWorldSpace() << std::endl;
    std::cout << "  SO Length In Object = " << newItkArrow->GetLengthInObjectSpace() << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: length" << std::endl;

  // metaArrow had served its purpose,
  // must now return to the emptiness of the universe
  delete metaArrow;

  // check color
  if (itk::Math::NotExactlyEquals(newItkArrow->GetProperty().GetRed(), color[0]) ||
      itk::Math::NotExactlyEquals(newItkArrow->GetProperty().GetGreen(), color[1]) ||
      itk::Math::NotExactlyEquals(newItkArrow->GetProperty().GetBlue(), color[2]) ||
      itk::Math::NotExactlyEquals(newItkArrow->GetProperty().GetAlpha(), color[3]))
  {
    std::cout << "Conversion to SpatialObject failed to convert color [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: color" << std::endl;

  // check parent id
  if (newItkArrow->GetParentId() != itkParent->GetId())
  {
    std::cout << "Conversion to SpatialObject failed to convert parent id [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: parent id" << std::endl;

  // check position
  SpatialObjectType::PointType itkPosition = newItkArrow->GetPositionInWorldSpace();
  if (itk::Math::abs(itkPosition[0] - mPosition[0]) > precisionLimit ||
      itk::Math::abs(itkPosition[1] - mPosition[1]) > precisionLimit ||
      itk::Math::abs(itkPosition[2] - mPosition[2]) > precisionLimit)
  {
    std::cout << "Conversion to SpatialObject failed to convert position [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: position" << std::endl;

  // check direction (note: need to normalize before comparing)
  SpatialObjectType::VectorType itkDirectionNorm = newItkArrow->GetDirectionInWorldSpace();
  itkDirectionNorm.Normalize();
  SpatialObjectType::VectorType mDirectionNorm = newItkArrow->GetDirectionInWorldSpace();
  mDirectionNorm[0] = mDirection[0];
  mDirectionNorm[1] = mDirection[1];
  mDirectionNorm[2] = mDirection[2];
  if (itk::Math::NotExactlyEquals(mDirection[0],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()) ||
      itk::Math::NotExactlyEquals(mDirection[1],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()) ||
      itk::Math::NotExactlyEquals(mDirection[2],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()))
  {
    mDirectionNorm.Normalize();
  }

  if (itk::Math::abs(itkDirectionNorm[0] - mDirectionNorm[0]) > precisionLimit ||
      itk::Math::abs(itkDirectionNorm[1] - mDirectionNorm[1]) > precisionLimit ||
      itk::Math::abs(itkDirectionNorm[2] - mDirectionNorm[2]) > precisionLimit)
  {
    std::cout << "Conversion to SpatialObject failed to convert direction [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: direction" << std::endl;


  //
  // test writing
  //
  if (!converter->WriteMeta(itkArrow, argv[1]))
  {
    std::cout << "Didn't write properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject write as MetaObject" << std::endl;

  //
  // test reading
  //
  SpatialObjectType::Pointer reLoad = dynamic_cast<SpatialObjectType *>(converter->ReadMeta(argv[1]).GetPointer());

  // check length
  if (itk::Math::abs(reLoad->GetLengthInWorldSpace() - length) > precisionLimit)
  {
    std::cout << "Didn't read length properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: length" << std::endl;

  // check color
  if (itk::Math::NotExactlyEquals(reLoad->GetProperty().GetRed(), color[0]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetGreen(), color[1]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetBlue(), color[2]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetAlpha(), color[3]))
  {
    std::cout << "Didn't read color properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: color" << std::endl;

  // check parent id
  if (reLoad->GetParentId() != itkParent->GetId())
  {
    std::cout << "Didn't read parent id properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: parent id" << std::endl;

  // check position
  itkPosition = reLoad->GetPositionInWorldSpace();
  if (itk::Math::abs(itkPosition[0] - mPosition[0]) > precisionLimit ||
      itk::Math::abs(itkPosition[1] - mPosition[1]) > precisionLimit ||
      itk::Math::abs(itkPosition[2] - mPosition[2]) > precisionLimit)
  {
    std::cout << "Didn't read position properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: position" << std::endl;

  // check direction (note: need to normalize before comparing)
  SpatialObjectType::VectorType reLoadDirectionNorm = reLoad->GetDirectionInWorldSpace();
  if (itk::Math::NotExactlyEquals(reLoadDirectionNorm[0],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()) ||
      itk::Math::NotExactlyEquals(reLoadDirectionNorm[1],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()) ||
      itk::Math::NotExactlyEquals(reLoadDirectionNorm[2],
                                  itk::NumericTraits<SpatialObjectType::VectorType::ValueType>::ZeroValue()))
  {
    reLoadDirectionNorm.Normalize();
  }

  if (itk::Math::abs(reLoadDirectionNorm[0] - directionNorm[0]) > precisionLimit ||
      itk::Math::abs(reLoadDirectionNorm[1] - directionNorm[1]) > precisionLimit ||
      itk::Math::abs(reLoadDirectionNorm[2] - directionNorm[2]) > precisionLimit)
  {
    std::cout << "Didn't read direction properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]  Reading: direction" << std::endl;


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
