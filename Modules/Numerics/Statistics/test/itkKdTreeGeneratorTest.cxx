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

#include "itkListSample.h"
#include "itkKdTreeGenerator.h"
#include "itkTestingMacros.h"

int
itkKdTreeGeneratorTest(int, char *[])
{
  // Testing KdTreeGenerator with Arrays as the measurement vectors
  {
    using MeasurementVectorType = itk::Array<float>;

    using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

    constexpr SampleType::MeasurementVectorSizeType measurementVectorSize = 2;

    auto sample = SampleType::New();
    sample->SetMeasurementVectorSize(measurementVectorSize);

    MeasurementVectorType mv(measurementVectorSize);
    for (unsigned int i = 0; i < 1000; ++i)
    {
      mv[0] = static_cast<float>(i);
      mv[1] = static_cast<float>((1000 - i) / 2);
      sample->PushBack(mv);
    }

    using TreeGeneratorType = itk::Statistics::KdTreeGenerator<SampleType>;
    auto treeGenerator = TreeGeneratorType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(treeGenerator, KdTreeGenerator, Object);


    treeGenerator->SetSample(sample);
    ITK_TEST_SET_GET_VALUE(sample, treeGenerator->GetSourceSample());

    constexpr unsigned int bucketSize = 16;
    treeGenerator->SetBucketSize(bucketSize);
    ITK_TEST_SET_GET_VALUE(bucketSize, treeGenerator->GetBucketSize());

    treeGenerator->Update();

    using TreeType = TreeGeneratorType::KdTreeType;
    using NodeType = TreeType::KdTreeNodeType;

    const TreeType::Pointer tree = treeGenerator->GetOutput();

    NodeType * root = tree->GetRoot();

    if (root->IsTerminal())
    {
      std::cout << "Root node is a terminal node." << std::endl;
    }
    else
    {
      std::cout << "Root node is not a terminal node." << std::endl;
    }

    unsigned int partitionDimension = 0;
    float        partitionValue = NAN;
    root->GetParameters(partitionDimension, partitionValue);
    std::cout << "Dimension chosen to split the space = " << partitionDimension << std::endl;
    std::cout << "Split point on the partition dimension = " << partitionValue << std::endl;

    std::cout << "Address of the left chile of the root node = " << root->Left() << std::endl;

    std::cout << "Address of the right chile of the root node = " << root->Right() << std::endl;

    MeasurementVectorType queryPoint(measurementVectorSize);
    queryPoint[0] = 10.0;
    queryPoint[1] = 7.0;

    using DistanceMetricType = itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType>;
    auto                           distanceMetric = DistanceMetricType::New();
    DistanceMetricType::OriginType origin(measurementVectorSize);
    for (unsigned int i = 0; i < measurementVectorSize; ++i)
    {
      origin[i] = queryPoint[i];
    }
    distanceMetric->SetOrigin(origin);

    constexpr unsigned int                 numberOfNeighbors = 3;
    TreeType::InstanceIdentifierVectorType neighbors;
    tree->Search(queryPoint, numberOfNeighbors, neighbors);

    std::cout << "kd-tree knn search result:" << std::endl
              << "query point = [" << queryPoint << ']' << std::endl
              << "k = " << numberOfNeighbors << std::endl;
    std::cout << "measurement vector : distance" << std::endl;
    for (unsigned int i = 0; i < numberOfNeighbors; ++i)
    {
      std::cout << '[' << tree->GetMeasurementVector(neighbors[i])
                << "] : " << distanceMetric->Evaluate(tree->GetMeasurementVector(neighbors[i])) << std::endl;
    }

    constexpr double radius = 437.0;

    tree->Search(queryPoint, radius, neighbors);

    std::cout << "kd-tree radius search result:" << std::endl
              << "query point = [" << queryPoint << ']' << std::endl
              << "search radius = " << radius << std::endl;
    std::cout << "measurement vector : distance" << std::endl;
    for (const auto neighbor : neighbors)
    {
      std::cout << '[' << tree->GetMeasurementVector(neighbor)
                << "] : " << distanceMetric->Evaluate(tree->GetMeasurementVector(neighbor)) << std::endl;
    }
  }

  // Testing KdTreeGenerator with Fixed length vectors as the measurement vectors.
  {
    using MeasurementVectorType = itk::Vector<float, 2>;

    using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;
    auto sample = SampleType::New();
    sample->SetMeasurementVectorSize(2);

    MeasurementVectorType mv;
    for (unsigned int i = 0; i < 1000; ++i)
    {
      mv[0] = static_cast<float>(i);
      mv[1] = static_cast<float>((1000 - i) / 2);
      sample->PushBack(mv);
    }

    using TreeGeneratorType = itk::Statistics::KdTreeGenerator<SampleType>;
    auto treeGenerator = TreeGeneratorType::New();

    treeGenerator->SetSample(sample);
    treeGenerator->SetBucketSize(16);
    treeGenerator->Update();

    using TreeType = TreeGeneratorType::KdTreeType;
    using NodeType = TreeType::KdTreeNodeType;

    const TreeType::Pointer tree = treeGenerator->GetOutput();

    NodeType * root = tree->GetRoot();

    if (root->IsTerminal())
    {
      std::cout << "Root node is a terminal node." << std::endl;
    }
    else
    {
      std::cout << "Root node is not a terminal node." << std::endl;
    }

    unsigned int partitionDimension = 0;
    float        partitionValue = NAN;
    root->GetParameters(partitionDimension, partitionValue);
    std::cout << "Dimension chosen to split the space = " << partitionDimension << std::endl;
    std::cout << "Split point on the partition dimension = " << partitionValue << std::endl;

    std::cout << "Address of the left chile of the root node = " << root->Left() << std::endl;

    std::cout << "Address of the right chile of the root node = " << root->Right() << std::endl;

    MeasurementVectorType queryPoint;
    queryPoint[0] = 10.0;
    queryPoint[1] = 7.0;

    using DistanceMetricType = itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType>;
    auto                           distanceMetric = DistanceMetricType::New();
    DistanceMetricType::OriginType origin(2);
    for (unsigned int i = 0; i < MeasurementVectorType::Length; ++i)
    {
      origin[i] = queryPoint[i];
    }
    distanceMetric->SetOrigin(origin);

    constexpr unsigned int                 numberOfNeighbors = 3;
    TreeType::InstanceIdentifierVectorType neighbors;
    tree->Search(queryPoint, numberOfNeighbors, neighbors);

    std::cout << "kd-tree knn search result:" << std::endl
              << "query point = [" << queryPoint << ']' << std::endl
              << "k = " << numberOfNeighbors << std::endl;
    std::cout << "measurement vector : distance" << std::endl;
    for (unsigned int i = 0; i < numberOfNeighbors; ++i)
    {
      std::cout << '[' << tree->GetMeasurementVector(neighbors[i])
                << "] : " << distanceMetric->Evaluate(tree->GetMeasurementVector(neighbors[i])) << std::endl;
    }

    constexpr double radius = 437.0;

    tree->Search(queryPoint, radius, neighbors);

    std::cout << "kd-tree radius search result:" << std::endl
              << "query point = [" << queryPoint << ']' << std::endl
              << "search radius = " << radius << std::endl;
    std::cout << "measurement vector : distance" << std::endl;
    for (const auto neighbor : neighbors)
    {
      std::cout << '[' << tree->GetMeasurementVector(neighbor)
                << "] : " << distanceMetric->Evaluate(tree->GetMeasurementVector(neighbor)) << std::endl;
    }
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
