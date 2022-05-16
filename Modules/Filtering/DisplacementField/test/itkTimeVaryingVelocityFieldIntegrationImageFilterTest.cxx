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

#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkTestingMacros.h"

int
itkTimeVaryingVelocityFieldIntegrationImageFilterTest(int argc, char * argv[])
{
  if (argc != 10)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " homoConstLowerTimeBound"
              << " homoConstUpperTimeBound"
              << " homoConstNumberOfIntegrationSteps"
              << " heterogVarConstLowerTimeBound"
              << " heterogVarConstUpperTimeBound"
              << " heterogVarNumberOfIntegrationSteps"
              << " invLowerTimeBound"
              << " invUpperTimeBound"
              << " invNumberOfIntegrationSteps" << std::endl;
    return EXIT_FAILURE;
  }

  /* First test with homogeneous and constant velocity field
   */
  using VectorType = itk::Vector<double, 3>;
  using DisplacementFieldType = itk::Image<VectorType, 3>;
  using TimeVaryingVelocityFieldType = itk::Image<VectorType, 4>;

  TimeVaryingVelocityFieldType::PointType origin;
  origin.Fill(0.0);

  TimeVaryingVelocityFieldType::SpacingType spacing;
  spacing.Fill(2.0);

  TimeVaryingVelocityFieldType::SizeType size;
  size.Fill(25);

  VectorType constantVelocity;
  constantVelocity.Fill(0.1);

  auto constantVelocityField = TimeVaryingVelocityFieldType::New();

  constantVelocityField->SetOrigin(origin);
  constantVelocityField->SetSpacing(spacing);
  constantVelocityField->SetRegions(size);
  constantVelocityField->Allocate();
  constantVelocityField->FillBuffer(constantVelocity);


  using IntegratorType =
    itk::TimeVaryingVelocityFieldIntegrationImageFilter<TimeVaryingVelocityFieldType, DisplacementFieldType>;
  auto integrator = IntegratorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(integrator, TimeVaryingVelocityFieldIntegrationImageFilter, ImageToImageFilter);


  auto lowerTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[1]));
  integrator->SetLowerTimeBound(lowerTimeBound);
  ITK_TEST_SET_GET_VALUE(lowerTimeBound, integrator->GetLowerTimeBound());

  auto upperTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[2]));
  integrator->SetUpperTimeBound(upperTimeBound);
  ITK_TEST_SET_GET_VALUE(upperTimeBound, integrator->GetUpperTimeBound());

  auto numberOfIntegrationSteps = static_cast<unsigned int>(std::stoi(argv[3]));
  integrator->SetNumberOfIntegrationSteps(numberOfIntegrationSteps);
  ITK_TEST_SET_GET_VALUE(numberOfIntegrationSteps, integrator->GetNumberOfIntegrationSteps());

  auto timeBoundsAsRates = true;
  ITK_TEST_SET_GET_BOOLEAN(integrator, TimeBoundsAsRates, timeBoundsAsRates);

  integrator->SetInput(constantVelocityField);

  integrator->Update();

  DisplacementFieldType::IndexType index;
  index.Fill(0);
  VectorType displacement;

  auto inverseIntegrator = IntegratorType::New();

  auto invLowerTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[7]));
  inverseIntegrator->SetLowerTimeBound(invLowerTimeBound);
  ITK_TEST_SET_GET_VALUE(invLowerTimeBound, inverseIntegrator->GetLowerTimeBound());

  auto invUpperTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[8]));
  inverseIntegrator->SetUpperTimeBound(invUpperTimeBound);
  ITK_TEST_SET_GET_VALUE(invUpperTimeBound, inverseIntegrator->GetUpperTimeBound());

  auto invNumberOfIntegrationSteps = static_cast<unsigned int>(std::stoi(argv[9]));
  inverseIntegrator->SetNumberOfIntegrationSteps(invNumberOfIntegrationSteps);
  ITK_TEST_SET_GET_VALUE(invNumberOfIntegrationSteps, inverseIntegrator->GetNumberOfIntegrationSteps());

  inverseIntegrator->SetInput(constantVelocityField);

  inverseIntegrator->Update();

  // This integration should result in a constant image of value
  // -( 0.1 * 1.0 - ( 0.1 * 0.0 ) ) = -0.1 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * inverseField = inverseIntegrator->GetOutput();
  displacement = inverseField->GetPixel(index);

  std::cout << "Estimated inverse displacement vector: " << displacement << std::endl;
  if (itk::Math::abs(displacement[0] + 0.101852) > 0.01)
  {
    std::cerr << "Failed to produce the correct inverse integration." << std::endl;
    return EXIT_FAILURE;
  }

  // This integration should result in a constant image of value
  // 0.75 * 0.1 - 0.3 * 0.1 = 0.045 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * displacementField = integrator->GetOutput();

  displacement = displacementField->GetPixel(index);

  std::cout << "Estimated forward displacement vector: " << displacement << std::endl;
  if (itk::Math::abs(displacement[0] - 0.045) > 0.0001)
  {
    std::cerr << "Failed to produce the correct forward integration." << std::endl;
    return EXIT_FAILURE;
  }


  /* Second test with inhomogeneous and time-varying velocity field
   */

  /* Generation of the velocity field defined by
   *    v(x, y, z, t) = z/(1+t) * ( -sin(z), cos(z), 1 )
   * which can be analytically integrated giving the motion
   * corresponding to the displacement field:
   *    Displacement(x0, y0, z0, t) =
   *      ( cos(z0*(1+t)) - 1, sin(z0*(1+t)), z0*(1+t) )
   */
  using ImportFilterType = itk::ImportImageFilter<VectorType, 4>;

  auto importFilter = ImportFilterType::New();

  /* Size is made denser on the z and t coordinates, for which
   * the velocity field is rapidly changing.
   * The velocity field is homogeneous in the x-y plane.*/
  size[0] = 3;
  size[1] = 3;
  size[2] = 401;
  size[3] = 61;
  ImportFilterType::IndexType start;
  start.Fill(0);

  ImportFilterType::RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  importFilter->SetRegion(region);

  origin.Fill(0.);
  importFilter->SetOrigin(origin);

  double spaceTimeSpan[4] = { 20., 20., 20., 1.5 };
  for (unsigned int i = 0; i < 4; i++)
  {
    spacing[i] = spaceTimeSpan[i] / (size[i] - 1);
  }
  importFilter->SetSpacing(spacing);

  const unsigned int numberOfPixels = size[0] * size[1] * size[2] * size[3];
  auto *             localBuffer = new VectorType[numberOfPixels];

  VectorType * it = localBuffer;
  for (unsigned int t = 0; t < size[3]; ++t)
  {
    for (unsigned int z = 0; z < size[2]; ++z)
    {
      const double zz = spacing[2] * z;
      const double kappa = zz / (1 + spacing[3] * t);
      for (unsigned int y = 0; y < size[1]; ++y)
      {
        for (unsigned int x = 0; x < size[0]; ++x, ++it)
        {
          (*it)[0] = -kappa * std::sin(zz);
          (*it)[1] = kappa * std::cos(zz);
          (*it)[2] = kappa;
        }
      }
    }
  }

  const bool importImageFilterWillOwnTheBuffer = true;
  importFilter->SetImportPointer(localBuffer, numberOfPixels, importImageFilterWillOwnTheBuffer);

  TimeVaryingVelocityFieldType::Pointer timeVaryingVelocityField = importFilter->GetOutput();

  lowerTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[4]));
  integrator->SetLowerTimeBound(lowerTimeBound);

  upperTimeBound = static_cast<typename IntegratorType::RealType>(std::stod(argv[5]));
  integrator->SetUpperTimeBound(upperTimeBound);

  integrator->SetInput(timeVaryingVelocityField);
  /* This time bounds are meant to be absolute
   * for the velocity field original time span [0, 1.5].
   * Thus, we need to switch off the default rescaling
   * to the normalized time span [0, 1] */
  integrator->TimeBoundsAsRatesOff();

  numberOfIntegrationSteps = static_cast<unsigned int>(std::stod(argv[6]));
  integrator->SetNumberOfIntegrationSteps(numberOfIntegrationSteps);

  integrator->Update();

  index[0] = (size[0] - 1) / 2;  // Corresponding to x = 10.
  index[1] = (size[1] - 1) / 2;  // Corresponding to y = 10.
  index[2] = (size[2] - 1) / 10; // Corresponding to z = 2.

  displacementField = integrator->GetOutput();
  displacement = displacementField->GetPixel(index);
  // The analytic result is displacement = ( cos(3) - cos(2), sin(3) - sin(2), 3 - 2 )
  std::cout << "Estimated forward displacement vector: " << displacement << std::endl;
  if (itk::Math::abs(displacement[0] + 0.5738) > 0.0002 || itk::Math::abs(displacement[1] + 0.7682) > 0.0001 ||
      itk::Math::abs(displacement[2] - 1.0000) > 0.0001)
  {
    std::cerr << "Failed to produce the correct forward integration." << std::endl;
    return EXIT_FAILURE;
  }

  /* The same integrator is used backwards in time. */
  integrator->SetLowerTimeBound(invLowerTimeBound);
  integrator->SetUpperTimeBound(invUpperTimeBound);
  integrator->Update();

  inverseField = integrator->GetOutput();
  displacement = inverseField->GetPixel(index);
  // The analytic result is displacement = ( cos(1) - cos(2), sin(1) - sin(2), 1 - 2 )
  std::cout << "Estimated inverse displacement vector: " << displacement << std::endl;
  if (itk::Math::abs(displacement[0] - 0.9564) > 0.0001 || itk::Math::abs(displacement[1] + 0.0678) > 0.0003 ||
      itk::Math::abs(displacement[2] + 1.0000) > 0.0001)
  {
    std::cerr << "Failed to produce the correct inverse integration." << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
