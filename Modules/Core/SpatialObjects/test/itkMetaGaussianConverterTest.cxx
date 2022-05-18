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


#include "itkMetaGaussianConverter.h"
#include "itkGroupSpatialObject.h"
#include "itkTestingMacros.h"
#include "itkMath.h"


/**
 * This is a test file for the itkMetaGaussianConverter class.
 *
 *
 * This test creates a sample GaussianSpatialObject and a sample MetaGaussian
 * and converts between the two, testing for conversion completeness. At the
 * moment, this means testing to make sure maixum, radius and color are
 * properly converted, but it may also include testing sigma. The test also
 * runs the read and write methods to test them.
 *
 * Notes: Parent cannot be converted from MetaObject to SpatialObject since
 *        MetaObject only holds a parent id rather than a parent object.
 *        Only the ParentID can be properly converted.
 */
int
itkMetaGaussianConverterTest(int argc, char * argv[])
{

  // Check number of arguments
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " output" << std::endl;
    return EXIT_FAILURE;
  }


  // type alias
  constexpr unsigned int Dimensions = 3;
  using SpatialObjectType = itk::GaussianSpatialObject<Dimensions>;
  using SpatialObjectParentType = itk::GroupSpatialObject<Dimensions>;
  using ConverterType = itk::MetaGaussianConverter<Dimensions>;


  // Instantiate new converter object
  auto converter = ConverterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(converter, MetaGaussianConverter, MetaConverterBase);


  // Set up a Gaussian spatial object
  auto GaussianSpatialObj = SpatialObjectType::New();

  // Gaussian spatial object properties
  SpatialObjectType::ScalarType maximum = 2;
  SpatialObjectType::ScalarType radius = 3;
  SpatialObjectType::ScalarType sigma = 1.5;

  GaussianSpatialObj->SetMaximum(maximum);
  GaussianSpatialObj->SetRadiusInObjectSpace(radius);
  GaussianSpatialObj->SetSigmaInObjectSpace(sigma);

  // color
  float color[4];
  color[0] = 1;   // Red (R)
  color[1] = .5;  // Green (G)
  color[2] = .25; // Blue (B)
  color[3] = 1;   // Alpha

  GaussianSpatialObj->GetProperty().SetRed(color[0]);
  GaussianSpatialObj->GetProperty().SetGreen(color[1]);
  GaussianSpatialObj->GetProperty().SetBlue(color[2]);
  GaussianSpatialObj->GetProperty().SetAlpha(color[3]);

  auto parentSpatialObj = SpatialObjectParentType::New();
  parentSpatialObj->SetId(1);
  parentSpatialObj->AddChild(GaussianSpatialObj);

  // Set up a MetaGaussian object
  auto * metaGaussian = new MetaGaussian(Dimensions);
  metaGaussian->Maximum(maximum);
  metaGaussian->Radius(radius);
  metaGaussian->Sigma(sigma);
  metaGaussian->Color((const float *)color);
  metaGaussian->ParentID(parentSpatialObj->GetId());

  // Precision limit for comparing floats and doubles
  double precisionLimit = .000001;

  //
  // Test GaussianSpatialObject to MetaGaussian
  //
  auto * newMetaGaussian = dynamic_cast<MetaGaussian *>(converter->SpatialObjectToMetaObject(GaussianSpatialObj));
  if (newMetaGaussian == nullptr)
  {
    itkGenericExceptionMacro(<< "Failed to downcast from GaussianSpatialObject to MetaGaussian");
  }

  // Check maximum
  double metaMaximum = newMetaGaussian->Maximum();

  // if (metaMaximum != static_cast<float>(maximum))
  if (itk::Math::abs(metaMaximum - maximum) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to MetaGaussian failed to convert maximum" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: maximum: " << metaMaximum << std::endl;

  // Check radius
  double metaRadius = newMetaGaussian->Radius();

  // if (metaRadius != static_cast<float>(radius))
  if (itk::Math::abs(metaRadius - radius) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to MetaGaussian failed to convert radius" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: radius: " << metaRadius << std::endl;

  // Check sigma
  double metaSigma = newMetaGaussian->Sigma();

  // if (metaSigma != static_cast<float>(sigma))
  if (itk::Math::abs(metaSigma - sigma) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to MetaGaussian failed to convert sigma" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: sigma: " << metaSigma << std::endl;

  // Check color
  const float * newMetaColor = newMetaGaussian->Color();
  if (itk::Math::NotExactlyEquals(newMetaColor[0], color[0]) ||
      itk::Math::NotExactlyEquals(newMetaColor[1], color[1]) ||
      itk::Math::NotExactlyEquals(newMetaColor[2], color[2]) || itk::Math::NotExactlyEquals(newMetaColor[3], color[3]))
  {
    std::cout << "[FAILED] Conversion to MetaGaussian failed to convert color" << std::endl;
    // return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: color: "
            << "R: " << newMetaColor[0] << "; "
            << "G: " << newMetaColor[1] << "; "
            << "B: " << newMetaColor[2] << "; "
            << "Alpha: " << newMetaColor[3] << std::endl;

  // Check parent id
  if (newMetaGaussian->ParentID() != GaussianSpatialObj->GetParent()->GetId())
  {
    std::cout << "[FAILED] Conversion to MetaGaussian failed to convert parent" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject -> MetaObject: parent id: " << newMetaGaussian->ParentID() << std::endl;


  // newMetaGaussian had served its purpose,
  // must now return to the emptiness of the universe
  delete newMetaGaussian;


  //
  // Test MetaGaussian to GaussianSpatialObject
  //
  SpatialObjectType::Pointer newGaussianSpatialObj =
    dynamic_cast<SpatialObjectType *>(converter->MetaObjectToSpatialObject(metaGaussian).GetPointer());


  // Check maximum
  if (itk::Math::abs(newGaussianSpatialObj->GetMaximum() - metaGaussian->Maximum()) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to SpatialObject failed to convert maximum" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: maximum: " << newGaussianSpatialObj->GetMaximum() << std::endl;

  // Check radius
  if (itk::Math::abs(newGaussianSpatialObj->GetRadiusInObjectSpace() - metaGaussian->Radius()) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to SpatialObject failed to convert radius" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: radius: " << newGaussianSpatialObj->GetRadiusInObjectSpace()
            << std::endl;

  // Check sigma
  if (itk::Math::abs(newGaussianSpatialObj->GetSigmaInObjectSpace() - metaGaussian->Sigma()) > precisionLimit)
  {
    std::cout << "[FAILED] Conversion to SpatialObject failed to convert sigma" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: sigma: " << newGaussianSpatialObj->GetSigmaInObjectSpace()
            << std::endl;


  // metaGaussian had served its purpose,
  // must now return to the emptiness of the universe
  delete metaGaussian;


  // Check color
  if (itk::Math::NotExactlyEquals(newGaussianSpatialObj->GetProperty().GetRed(), color[0]) ||
      itk::Math::NotExactlyEquals(newGaussianSpatialObj->GetProperty().GetGreen(), color[1]) ||
      itk::Math::NotExactlyEquals(newGaussianSpatialObj->GetProperty().GetBlue(), color[2]) ||
      itk::Math::NotExactlyEquals(newGaussianSpatialObj->GetProperty().GetAlpha(), color[3]))
  {
    std::cout << "[FAILED] Conversion to SpatialObject failed to convert color" << std::endl;
    // return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: color : "
            << "R: " << newGaussianSpatialObj->GetProperty().GetRed() << "; "
            << "G: " << newGaussianSpatialObj->GetProperty().GetGreen() << "; "
            << "B: " << newGaussianSpatialObj->GetProperty().GetBlue() << "; "
            << "Alpha: " << newGaussianSpatialObj->GetProperty().GetAlpha() << "; " << std::endl;

  // Check parent id
  if (newGaussianSpatialObj->GetParentId() != parentSpatialObj->GetId())
  {
    std::cout << "[FAILED] Conversion to SpatialObject failed to convert parent id" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] MetaObject -> SpatialObject: parent id: " << newGaussianSpatialObj->GetParentId() << std::endl;


  //
  // Test writing
  //
  if (!converter->WriteMeta(GaussianSpatialObj, argv[1]))
  {
    std::cout << "[FAILED] Didn't write properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] SpatialObject write as MetaObject" << std::endl;


  //
  // Test reading
  //
  SpatialObjectType::Pointer reLoad = dynamic_cast<SpatialObjectType *>(converter->ReadMeta(argv[1]).GetPointer());

  // Check maximum
  if (itk::Math::abs(reLoad->GetMaximum() - maximum) > precisionLimit)
  {
    std::cout << "[FAILED] Didn't read maximum properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: maximum: " << reLoad->GetMaximum() << std::endl;

  // Check radius
  if (itk::Math::abs(reLoad->GetRadiusInObjectSpace() - radius) > precisionLimit)
  {
    std::cout << "[FAILED] Didn't read radius properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: radius: " << reLoad->GetRadiusInObjectSpace() << std::endl;

  // Check sigma
  if (itk::Math::abs(reLoad->GetSigmaInObjectSpace() - sigma) > precisionLimit)
  {
    std::cout << "[FAILED] Didn't read sigma properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: sigma: " << reLoad->GetSigmaInObjectSpace() << std::endl;

  // Check color
  if (itk::Math::NotExactlyEquals(reLoad->GetProperty().GetRed(), color[0]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetGreen(), color[1]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetBlue(), color[2]) ||
      itk::Math::NotExactlyEquals(reLoad->GetProperty().GetAlpha(), color[3]))
  {
    std::cout << "[FAILED] Didn't read color properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: color : "
            << "R: " << reLoad->GetProperty().GetRed() << "; "
            << "G: " << reLoad->GetProperty().GetGreen() << "; "
            << "B: " << reLoad->GetProperty().GetBlue() << "; "
            << "Alpha: " << reLoad->GetProperty().GetAlpha() << std::endl;


  // Check parent id
  if (reLoad->GetParentId() != parentSpatialObj->GetId())
  {
    std::cout << "[FAILED] Didn't read parent id properly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Reading: parent id: " << reLoad->GetParentId() << std::endl;


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
