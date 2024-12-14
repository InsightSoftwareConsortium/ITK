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

  HistogramType::AbsoluteFrequencyType frequency;
  HistogramType::InstanceIdentifier    identifier;

  index[0] = 0;
  index[1] = 0;
  frequency = 10;
  identifier = histogram->GetInstanceIdentifier(index);
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

  std::cout << filter->GetNameOfClass() << '\n';
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
    std::cerr << "Exception caught: " << excp << '\n';
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
    std::cerr << "Exception caught: " << excp << '\n';
    return EXIT_FAILURE;
  }

  const double trueEnergy = 0.295;
  const double trueEntropy = 2.26096;
  const double trueCorrelation = 0.12819;
  const double trueInverseDifferenceMoment = 0.85;
  const double trueInertia = 0.3;
  const double trueClusterShade = 139.1879;
  const double trueClusterProminence = 2732.557;
  const double trueHaralickCorrelation = 2264.549;

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
    std::cerr << "Error:" << '\n';
    std::cerr << "Energy calculated wrong. Expected: " << trueEnergy << ", got: " << energy << '\n';
    passed = false;
  }

  if (itk::Math::abs(entropy - trueEntropy) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Entropy calculated wrong. Expected: " << trueEntropy << ", got: " << entropy << '\n';
    passed = false;
  }

  if (itk::Math::abs(correlation - trueCorrelation) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Correlation calculated wrong. Expected: " << trueCorrelation << ", got: " << correlation << '\n';
    passed = false;
  }

  if (itk::Math::abs(inverseDifferenceMoment - trueInverseDifferenceMoment) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "InverseDifferenceMoment calculated wrong. Expected: " << trueInverseDifferenceMoment
              << ", got: " << inverseDifferenceMoment << '\n';
    passed = false;
  }

  if (itk::Math::abs(inertia - trueInertia) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Inertia calculated wrong. Expected: " << trueInertia << ", got: " << inertia << '\n';
    passed = false;
  }

  if (itk::Math::abs(clusterShade - trueClusterShade) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "ClusterShade calculated wrong. Expected: " << trueClusterShade << ", got: " << clusterShade << '\n';
    passed = false;
  }

  if (itk::Math::abs(clusterProminence - trueClusterProminence) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "ClusterProminence calculated wrong. Expected: " << trueClusterProminence
              << ", got: " << clusterProminence << '\n';
    passed = false;
  }

  if (itk::Math::abs(haralickCorrelation - trueHaralickCorrelation) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Haralick's Correlation calculated wrong. Expected: " << trueHaralickCorrelation
              << ", got: " << haralickCorrelation << '\n';
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
    std::cerr << "Error:" << '\n';
    std::cerr << "Energy calculated wrong. Expected: " << trueEnergy << ", got: " << energy2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(entropy2 - trueEntropy) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Entropy calculated wrong. Expected: " << trueEntropy << ", got: " << entropy2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(correlation2 - trueCorrelation) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Correlation calculated wrong. Expected: " << trueCorrelation << ", got: " << correlation2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(inverseDifferenceMoment2 - trueInverseDifferenceMoment) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "InverseDifferenceMoment calculated wrong. Expected: " << trueInverseDifferenceMoment
              << ", got: " << inverseDifferenceMoment2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(inertia2 - trueInertia) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Inertia calculated wrong. Expected: " << trueInertia << ", got: " << inertia2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(clusterShade2 - trueClusterShade) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "ClusterShade calculated wrong. Expected: " << trueClusterShade << ", got: " << clusterShade2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(clusterProminence2 - trueClusterProminence) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "ClusterProminence calculated wrong. Expected: " << trueClusterProminence
              << ", got: " << clusterProminence2 << '\n';
    passed = false;
  }

  if (itk::Math::abs(haralickCorrelation2 - trueHaralickCorrelation) > 0.001)
  {
    std::cerr << "Error:" << '\n';
    std::cerr << "Haralick's Correlation calculated wrong. Expected: " << trueHaralickCorrelation
              << ", got: " << haralickCorrelation2 << '\n';
    passed = false;
  }

  // Test inquiry for invalid feature

  if (filter->GetFeature(itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InvalidFeatureName))
  {
    std::cerr << "Error: " << '\n';
    std::cerr << "GetFeature() is returning non-zero feature value: "
              << "for invalid feature request" << '\n';
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
    std::cout << "STREAMED ENUM VALUE HistogramToTextureFeaturesFilterEnums::TextureFeature: " << ee << '\n';
  }

  if (!passed)
  {
    std::cerr << "Test failed" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cerr << "Test succeeded" << '\n';
    return EXIT_SUCCESS;
  }
}
