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

#include "itkHistogramToTextureFeaturesFilter.h"

int
itkHistogramToTextureFeaturesFilterTest(int, char *[])
{
  // Data definitions
  constexpr unsigned int HISTOGRAM_AXIS_LEN = 25;


  //------------------------------------------------------
  // Create a simple test histogram. The histogram must be
  // symmetric and normalized.
  //------------------------------------------------------
  using MeasurementType = float;
  using HistogramType = itk::Statistics::Histogram<MeasurementType>;
  auto histogram = HistogramType::New();

  constexpr unsigned int measurementVectorSize = 2;

  histogram->SetMeasurementVectorSize(measurementVectorSize);

  HistogramType::SizeType size(measurementVectorSize);

  size.Fill(HISTOGRAM_AXIS_LEN);

  HistogramType::MeasurementVectorType lowerBound(measurementVectorSize);
  HistogramType::MeasurementVectorType upperBound(measurementVectorSize);

  lowerBound[0] = 0;
  lowerBound[1] = 0;

  upperBound[0] = HISTOGRAM_AXIS_LEN + 1;
  upperBound[1] = HISTOGRAM_AXIS_LEN + 1;

  histogram->Initialize(size, lowerBound, upperBound);

  HistogramType::IndexType index(measurementVectorSize);

  index[0] = 0;
  index[1] = 0;
  HistogramType::AbsoluteFrequencyType frequency = 10;
  HistogramType::InstanceIdentifier    identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 3;
  index[1] = 3;
  frequency = 50;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 2;
  index[1] = 1;
  frequency = 5;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 1;
  index[1] = 2;
  frequency = 5;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 7;
  index[1] = 6;
  frequency = 10;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 6;
  index[1] = 7;
  frequency = 10;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  index[0] = 10;
  index[1] = 10;
  frequency = 10;
  identifier = histogram->GetInstanceIdentifier(index);
  histogram->SetFrequency(identifier, frequency);

  using HistogramToTextureFeaturesFilterType = itk::Statistics::HistogramToTextureFeaturesFilter<HistogramType>;

  auto filter = HistogramToTextureFeaturesFilterType::New();

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  bool passed = true;
  // Invoke update before adding an input. An exception should be
  // thrown.
  try
  {
    filter->Update();
    passed = false;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  if (filter->GetInput() != nullptr)
  {
    passed = false;
  }

  filter->ResetPipeline();

  filter->SetInput(histogram);
  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
    return EXIT_FAILURE;
  }

  constexpr double trueEnergy = 0.295;
  constexpr double trueEntropy = 2.26096;
  constexpr double trueCorrelation = 0.12819;
  constexpr double trueInverseDifferenceMoment = 0.85;
  constexpr double trueInertia = 0.3;
  constexpr double trueClusterShade = 139.1879;
  constexpr double trueClusterProminence = 2732.557;
  constexpr double trueHaralickCorrelation = 2264.549;

  const double energy = filter->GetEnergy();
  const double entropy = filter->GetEntropy();
  const double correlation = filter->GetCorrelation();
  const double inverseDifferenceMoment = filter->GetInverseDifferenceMoment();
  const double inertia = filter->GetInertia();
  const double clusterShade = filter->GetClusterShade();
  const double clusterProminence = filter->GetClusterProminence();
  const double haralickCorrelation = filter->GetHaralickCorrelation();


  if (itk::Math::abs(energy - trueEnergy) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Energy calculated wrong. Expected: " << trueEnergy << ", got: " << energy << std::endl;
    passed = false;
  }

  if (itk::Math::abs(entropy - trueEntropy) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Entropy calculated wrong. Expected: " << trueEntropy << ", got: " << entropy << std::endl;
    passed = false;
  }

  if (itk::Math::abs(correlation - trueCorrelation) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Correlation calculated wrong. Expected: " << trueCorrelation << ", got: " << correlation << std::endl;
    passed = false;
  }

  if (itk::Math::abs(inverseDifferenceMoment - trueInverseDifferenceMoment) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "InverseDifferenceMoment calculated wrong. Expected: " << trueInverseDifferenceMoment
              << ", got: " << inverseDifferenceMoment << std::endl;
    passed = false;
  }

  if (itk::Math::abs(inertia - trueInertia) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Inertia calculated wrong. Expected: " << trueInertia << ", got: " << inertia << std::endl;
    passed = false;
  }

  if (itk::Math::abs(clusterShade - trueClusterShade) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterShade calculated wrong. Expected: " << trueClusterShade << ", got: " << clusterShade
              << std::endl;
    passed = false;
  }

  if (itk::Math::abs(clusterProminence - trueClusterProminence) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterProminence calculated wrong. Expected: " << trueClusterProminence
              << ", got: " << clusterProminence << std::endl;
    passed = false;
  }

  if (itk::Math::abs(haralickCorrelation - trueHaralickCorrelation) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Haralick's Correlation calculated wrong. Expected: " << trueHaralickCorrelation
              << ", got: " << haralickCorrelation << std::endl;
    passed = false;
  }

  // Get the texture features using GetFeature() method
  const double energy2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Energy);

  const double entropy2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Entropy);

  const double correlation2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Correlation);

  const double inverseDifferenceMoment2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InverseDifferenceMoment);

  const double inertia2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Inertia);

  const double clusterShade2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterShade);

  const double clusterProminence2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterProminence);

  const double haralickCorrelation2 =
    filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::HaralickCorrelation);

  if (itk::Math::abs(energy2 - trueEnergy) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Energy calculated wrong. Expected: " << trueEnergy << ", got: " << energy2 << std::endl;
    passed = false;
  }

  if (itk::Math::abs(entropy2 - trueEntropy) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Entropy calculated wrong. Expected: " << trueEntropy << ", got: " << entropy2 << std::endl;
    passed = false;
  }

  if (itk::Math::abs(correlation2 - trueCorrelation) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Correlation calculated wrong. Expected: " << trueCorrelation << ", got: " << correlation2
              << std::endl;
    passed = false;
  }

  if (itk::Math::abs(inverseDifferenceMoment2 - trueInverseDifferenceMoment) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "InverseDifferenceMoment calculated wrong. Expected: " << trueInverseDifferenceMoment
              << ", got: " << inverseDifferenceMoment2 << std::endl;
    passed = false;
  }

  if (itk::Math::abs(inertia2 - trueInertia) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Inertia calculated wrong. Expected: " << trueInertia << ", got: " << inertia2 << std::endl;
    passed = false;
  }

  if (itk::Math::abs(clusterShade2 - trueClusterShade) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterShade calculated wrong. Expected: " << trueClusterShade << ", got: " << clusterShade2
              << std::endl;
    passed = false;
  }

  if (itk::Math::abs(clusterProminence2 - trueClusterProminence) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterProminence calculated wrong. Expected: " << trueClusterProminence
              << ", got: " << clusterProminence2 << std::endl;
    passed = false;
  }

  if (itk::Math::abs(haralickCorrelation2 - trueHaralickCorrelation) > 0.001)
  {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Haralick's Correlation calculated wrong. Expected: " << trueHaralickCorrelation
              << ", got: " << haralickCorrelation2 << std::endl;
    passed = false;
  }

  // Test inquiry for invalid feature

  if (filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InvalidFeatureName))
  {
    std::cerr << "Error: " << std::endl;
    std::cerr << "GetFeature() is returning non-zero feature value: "
              << "for invalid feature request" << std::endl;
    passed = false;
  }

  // Test streaming enumeration for HistogramToTextureFeaturesFilterEnums::TextureFeature elements
  const std::set<itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature> allTextureFeature{
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Energy,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Entropy,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Correlation,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InverseDifferenceMoment,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Inertia,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterShade,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterProminence,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::HaralickCorrelation,
    itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InvalidFeatureName
  };
  for (const auto & ee : allTextureFeature)
  {
    std::cout << "STREAMED ENUM VALUE HistogramToTextureFeaturesFilterEnums::TextureFeature: " << ee << std::endl;
  }

  if (!passed)
  {
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cerr << "Test succeeded" << std::endl;
  return EXIT_SUCCESS;
}
