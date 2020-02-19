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
=========================================================================*/

#include "itkMesh.h"
#include "itkMeshFileReader.h"
#include "itkVectorFieldPCA.h"
#include "itkTestingMacros.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector.h"


template <typename TPixel, typename TMesh, typename TVectorContainer>
int
ParseVectorFields(std::vector<std::string> vectorFieldFilenames, typename TVectorContainer::Pointer vectorFieldSet)
{
  int testStatus = EXIT_SUCCESS;

  using ReaderType = itk::MeshFileReader<TMesh>;
  typename ReaderType::Pointer meshReader = ReaderType::New();

  unsigned int fieldSetCount = vectorFieldFilenames.size();
  vectorFieldSet->Reserve(fieldSetCount);


  typename TVectorContainer::Element vectorField;

  unsigned int vectorFieldDim = 0;
  unsigned int vectorFieldCount = 0;

  unsigned int setIx = 0;
  for (unsigned int i = 0; i < fieldSetCount; i++)
  {
    std::string vectorFieldName = vectorFieldFilenames[i];

    meshReader->SetFileName(vectorFieldName);

    ITK_TRY_EXPECT_NO_EXCEPTION(meshReader->Update());

    // Get the objects
    typename TMesh::Pointer meshWithField = meshReader->GetOutput();

    typename TMesh::PointDataContainerPointer pointData = meshWithField->GetPointData();
    if (setIx == 0)
    {
      vectorFieldCount = pointData->Size();
      if (vectorFieldCount)
      {
        TPixel oneDataSetVal = pointData->GetElement(0);
        vectorFieldDim = oneDataSetVal.size();
      }
      if (vectorFieldCount != meshWithField->GetNumberOfPoints())
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Vector field count (" << vectorFieldCount << ") doesn't match mesh vertext count ("
                  << meshWithField->GetNumberOfPoints() << ")." << std::endl;
        testStatus = EXIT_FAILURE;
      }
      vectorField.set_size(vectorFieldCount, vectorFieldDim);
    }
    else
    {
      if (vectorFieldDim != vectorField.cols() || vectorFieldCount != meshWithField->GetNumberOfPoints())
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Unexpected dimensions in vector field file " << vectorFieldName << std::endl;
        std::cerr << "Expected: " << vectorFieldCount << " x " << vectorFieldDim << ", but got "
                  << meshWithField->GetNumberOfPoints() << " x " << vectorField.cols() << std::endl;
        testStatus = EXIT_FAILURE;
      }
    }

    for (unsigned int k = 0; k < pointData->Size(); k++)
    {
      TPixel oneDataSetVal = pointData->GetElement(k);
      vectorField.set_row(k, oneDataSetVal);
    }

    vectorFieldSet->SetElement(setIx++, vectorField);
  }

  return testStatus;
}


int
itkVectorKernelPCATest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << "<vtkMeshFile> <vectorField1> ... <vectorFieldN> " << std::endl;
    std::cerr << "where N must be greater than 3" << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  const unsigned int Dimension = 3;

  using PointDataType = double;
  using PointDataVectorType = itk::Array<PointDataType>;
  using PixelType = PointDataVectorType;
  using CoordRep = double;

  using PCAResultsType = double;

  // Declare the type of the input mesh
  using MeshType = itk::Mesh<PixelType, Dimension>;

  // Declare the type of the kernel function class
  using KernelType = itk::GaussianDistanceKernel<CoordRep>;

  // Declare the type of the PCA calculator
  using PCACalculatorType =
    itk::VectorFieldPCA<PointDataType, PCAResultsType, PixelType, CoordRep, KernelType, MeshType>;

  // Instantiate the reader
  using ReaderType = itk::MeshFileReader<MeshType>;
  ReaderType::Pointer meshReader = ReaderType::New();

  meshReader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(meshReader->Update());


  // Get the input mesh
  MeshType::Pointer mesh = meshReader->GetOutput();


  PCACalculatorType::Pointer pcaCalc = PCACalculatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(pcaCalc, VectorFieldPCA, Object);

  // Test exception when trying to compute before setting much of anything
  ITK_TRY_EXPECT_EXCEPTION(pcaCalc->Compute());


  // Set user variables
  unsigned int pcaCount = 3;
  pcaCalc->SetComponentCount(pcaCount);

  // Connect the input
  pcaCalc->SetPointSet(mesh);
  ITK_TEST_SET_GET_VALUE(mesh, pcaCalc->GetPointSet());

  // Set vector fields

  PCACalculatorType::VectorFieldType vectorField;

  // Should know vector field dimensions now
  unsigned int vectorFieldDim = 0;

  // how many vector field sets?
  std::vector<std::string> vectorFieldFilenames;
  unsigned int             firstVectorFieldIdx = 2;
  unsigned int             fieldSetCount = argc - firstVectorFieldIdx;
  for (unsigned int i = firstVectorFieldIdx; i < firstVectorFieldIdx + fieldSetCount; i++)
  {
    vectorFieldFilenames.emplace_back(argv[i]);
  }

  PCACalculatorType::VectorFieldSetTypePointer vectorFieldSet = PCACalculatorType::VectorFieldSetType::New();

  testStatus =
    ParseVectorFields<PixelType, MeshType, PCACalculatorType::VectorFieldSetType>(vectorFieldFilenames, vectorFieldSet);

  pcaCalc->SetVectorFieldSet(vectorFieldSet);
  ITK_TEST_SET_GET_VALUE(vectorFieldSet, pcaCalc->GetVectorFieldSet());

  // Execute the PCA calculator
  ITK_TRY_EXPECT_NO_EXCEPTION(pcaCalc->Compute());

  double              kernelSigma = 6.25;
  KernelType::Pointer distKernel = KernelType::New();
  distKernel->SetKernelSigma(kernelSigma);
  pcaCalc->SetKernelFunction(distKernel);


  std::ofstream debugOut;
  debugOut.precision(15);

  // Get the output and perform basic checks
  unsigned int computedNumberOfAverageVectorFieldCols = pcaCalc->GetAveVectorField().cols();

  unsigned int computedNumberOfAverageVectorFieldRows = pcaCalc->GetAveVectorField().rows();

  for (unsigned int j = 0; j < pcaCalc->GetComponentCount(); j++)
  {
    unsigned int expectedBasisVectorCols = pcaCalc->GetVectorFieldSet()->GetElement(j).cols();
    unsigned int computedBasisVectorCols = pcaCalc->GetBasisVectors()->GetElement(j).cols();
    if (computedBasisVectorCols != expectedBasisVectorCols)
    {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetBasisVectors() dimension check at index [" << j << "]" << std::endl;
      std::cout << "Expected: " << expectedBasisVectorCols << " columns, but got: " << computedBasisVectorCols
                << std::endl;
      testStatus = EXIT_FAILURE;
    }

    unsigned int expectedBasisVectorRows = pcaCalc->GetVectorFieldSet()->GetElement(j).rows();
    unsigned int computedBasisVectorRows = pcaCalc->GetBasisVectors()->GetElement(j).rows();
    if (computedBasisVectorRows != expectedBasisVectorRows)
    {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetBasisVectors() dimension check at index [" << j << "]" << std::endl;
      std::cout << "Expected: " << expectedBasisVectorRows << " row, but got: " << computedBasisVectorRows << std::endl;
      testStatus = EXIT_FAILURE;
    }

    if (computedNumberOfAverageVectorFieldCols != expectedBasisVectorCols)
    {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetAveVectorField() dimension check at index [" << j << "]" << std::endl;
      std::cout << "Expected: " << vectorFieldDim << " columns, but got: " << computedNumberOfAverageVectorFieldCols
                << std::endl;
      testStatus = EXIT_FAILURE;
    }

    if (computedNumberOfAverageVectorFieldRows != expectedBasisVectorRows)
    {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetAveVectorField() dimension check at index [" << j << "]" << std::endl;
      std::cout << "Expected: " << vectorFieldDim << " rows, but got: " << computedNumberOfAverageVectorFieldRows
                << std::endl;
      testStatus = EXIT_FAILURE;
    }
  }

  unsigned int computedNumberOfEigenValueVectors = pcaCalc->GetPCAEigenValues().size();
  if (computedNumberOfEigenValueVectors != pcaCount)
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Error in GetPCAEigenValues() dimension check." << std::endl;
    std::cout << "Expected: " << pcaCount << ", but got: " << computedNumberOfEigenValueVectors << std::endl;
    testStatus = EXIT_FAILURE;
  }

  // Test exception when trying to compute with a requested input count greater
  // than the number of vector field sets
  pcaCalc->SetComponentCount(fieldSetCount + 1);

  ITK_TRY_EXPECT_EXCEPTION(pcaCalc->Compute());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
