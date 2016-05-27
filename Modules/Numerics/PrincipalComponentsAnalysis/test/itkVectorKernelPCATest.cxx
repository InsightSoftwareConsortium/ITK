/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector.h"

int
showUsage(const char * programName)
{
  std::cerr << "USAGE:  " << programName << std::endl;
  std::cerr << "<vtk_mesh_file>  <vectorField1>  ... <vectorFieldN> " << std::endl;
  std::cerr << "\t\tN must be greater than 3" << std::endl;
  return EXIT_FAILURE;
}

int
itkVectorKernelPCATest(int argc, char * argv[])
{
#define MIN_ARG_COUNT 6
#define FIRST_VECTOR_FIELD_ARG 2
  if (argc < MIN_ARG_COUNT)
    return (showUsage(argv[0]));

  unsigned int pcaCount = 3;
  double       kernelSigma = 6.25;

  typedef double                    PointDataType;
  typedef itk::Array<PointDataType> PointDataVectorType;
  typedef PointDataVectorType       PixelType;
  typedef double                    CoordRep;
  const unsigned int                Dimension = 3;

  //    typedef float              PCAResultsType;
  typedef double PCAResultsType;

  // Declare the type of the input mesh
  typedef itk::Mesh<PixelType, Dimension> InMeshType;

  // Declare the type of the kernel function class
  // typedef itk::GaussianDistanceKernel KernelType;
  typedef itk::GaussianDistanceKernel<CoordRep> KernelType;

  // Declare the type of the PCA calculator
  typedef itk::VectorFieldPCA<PointDataType, PCAResultsType, PixelType, CoordRep, KernelType, InMeshType>
    PCACalculatorType;

  // Here we recover the file names from the command line arguments
  const char * inMeshFile = argv[1];

  //  We can now instantiate the types of the reader/writer.
  typedef itk::MeshFileReader<InMeshType> ReaderType;

  // create readers/writers
  ReaderType::Pointer meshReader = ReaderType::New();

  //  The name of the file to be read or written is passed with the
  //  SetFileName() method.
  meshReader->SetFileName(inMeshFile);

  try
  {
    meshReader->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << "Error reading mesh file " << inMeshFile << std::endl;
    std::cerr << excp << std::endl;
  }

  // get the objects
  InMeshType::Pointer mesh = meshReader->GetOutput();

  std::cout << "Vertex Count:  " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "Cell Count:  " << mesh->GetNumberOfCells() << std::endl;

  const char *                       vectorFieldName;
  PCACalculatorType::VectorFieldType vectorField;

  // should know vector field dimensions now
  unsigned int vectorFieldDim = 0;
  unsigned int vectorFieldCount = 0;

  // how many vector field sets?
  unsigned int fieldSetCount = argc - FIRST_VECTOR_FIELD_ARG;

  PCACalculatorType::VectorFieldSetTypePointer vectorFieldSet = PCACalculatorType::VectorFieldSetType::New();

  vectorFieldSet->Reserve(fieldSetCount);

  unsigned int setIx = 0;
  for (int i = FIRST_VECTOR_FIELD_ARG; i < argc; i++)
  {
    vectorFieldName = argv[i];
    //  The name of the file to be read or written is passed with the
    //  SetFileName() method.
    meshReader->SetFileName(vectorFieldName);

    try
    {
      meshReader->Update();
    }
    catch (itk::ExceptionObject & excp)
    {
      std::cerr << "Error reading mesh field file " << vectorFieldName << std::endl;
      std::cerr << excp << std::endl;
    }

    // get the objects
    InMeshType::Pointer meshWithField = meshReader->GetOutput();

    InMeshType::PointDataContainerPointer pointData = meshWithField->GetPointData();
    if (setIx == 0)
    {
      vectorFieldCount = pointData->Size();
      if (vectorFieldCount)
      {
        PixelType oneDataSetVal = pointData->GetElement(0);
        vectorFieldDim = oneDataSetVal.size();
      }
      if (vectorFieldCount != meshWithField->GetNumberOfPoints())
      {
        std::cerr << "Vector field count (" << vectorFieldCount << ") doesn't match mesh vertext count ("
                  << meshWithField->GetNumberOfPoints() << ")." << std::endl;
        exit(EXIT_FAILURE);
      }
      vectorField.set_size(vectorFieldCount, vectorFieldDim);
    }
    else
    {
      if (vectorFieldDim != vectorField.cols() || vectorFieldCount != meshWithField->GetNumberOfPoints())
      {
        std::cerr << "Unexpected dimensions in vector field file " << vectorFieldName << std::endl;
        std::cerr << "\tExpected " << vectorFieldCount << " x " << vectorFieldDim;
        std::cerr << "\t, got " << meshWithField->GetNumberOfPoints() << " x " << vectorField.cols() << std::endl;
        exit(1);
      }
    }

    for (unsigned int k = 0; k < pointData->Size(); k++)
    {
      PixelType oneDataSetVal = pointData->GetElement(k);
      vectorField.set_row(k, oneDataSetVal);
    }

    vectorFieldSet->SetElement(setIx++, vectorField);
  }

  PCACalculatorType::Pointer pcaCalc = PCACalculatorType::New();

  std::cout << "Name of Class = " << pcaCalc->GetNameOfClass() << std::endl;

  std::cout << "Test Print() = " << std::endl;
  pcaCalc->Print(std::cout);

  // try to Compute before setting much of anything - expect failure
  try
  {
    pcaCalc->Compute();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cout << "SUCCESSFULLY caught expected exception" << std::endl;
    std::cout << excp << std::endl;
  }

  // set user variables
  pcaCalc->SetComponentCount(pcaCount);

  //
  //  Now connect the input.
  //
  pcaCalc->SetPointSet(mesh);
  // set vector fields
  pcaCalc->SetVectorFieldSet(vectorFieldSet);
  //
  //  Now verify that it runs fine.
  //
  try
  {
    pcaCalc->Compute();
    std::cout << "SUCCESSFULLY ran non-Kernel version" << std::endl;
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cout << "Failed to run non-Kernel version" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  KernelType::Pointer distKernel = KernelType::New();
  distKernel->SetKernelSigma(kernelSigma);
  pcaCalc->SetKernelFunction(distKernel);

  std::cout << "PCA Calculator All Set Up:  Print() = " << std::endl;
  pcaCalc->Print(std::cout);

  std::ofstream debugOut;
  debugOut.precision(15);

  // get the output and perform basic checks
  for (unsigned int j = 0; j < pcaCalc->GetComponentCount(); j++)
  {
    if ((pcaCalc->GetBasisVectors()->GetElement(j)).cols() != vectorFieldDim)
    {
      std::cout << "Basis Vector Results Failed Dimension check:" << std::endl;
      std::cout << "Expected:  " << vectorFieldDim << std::endl
                << " columns.  Got:  " << (pcaCalc->GetBasisVectors()->GetElement(j)).cols() << std::endl;
      return EXIT_FAILURE;
    }
    if ((pcaCalc->GetBasisVectors()->GetElement(j)).rows() != vectorFieldCount)
    {
      std::cout << "Basis Vector Results Failed Dimension check:" << std::endl;
      std::cout << "Expected:  " << vectorFieldDim << std::endl
                << " rows. Got:  " << pcaCalc->GetBasisVectors()->GetElement(j).rows() << std::endl;
      return EXIT_FAILURE;
    }
  }

  if (pcaCalc->GetPCAEigenValues().size() != pcaCount)
  {
    std::cout << "Eigenvalue Vector Results Failed Dimension check:" << std::endl;
    std::cout << "Expected:  " << pcaCount << std::endl
              << ".  Got:  " << pcaCalc->GetPCAEigenValues().size() << std::endl;
    return EXIT_FAILURE;
  }

  if (pcaCalc->GetAveVectorField().cols() != vectorFieldDim)
  {
    std::cout << "Average Vector Field Results Failed Dimension check:" << std::endl;
    std::cout << "Expected:  " << vectorFieldDim << std::endl
              << " columns.  Got:  " << pcaCalc->GetAveVectorField().cols() << std::endl;
    return EXIT_FAILURE;
  }
  if (pcaCalc->GetAveVectorField().rows() != vectorFieldCount)
  {
    std::cout << "Average Vector Field Failed Dimension check:" << std::endl;
    std::cout << "Expected:  " << vectorFieldDim << std::endl
              << " rows. Got:  " << pcaCalc->GetAveVectorField().rows() << std::endl;
    return EXIT_FAILURE;
  }

  // set the requested input count greater than the number of vector field sets
  pcaCalc->SetComponentCount(fieldSetCount + 1);
  // try to Compute - expect failure
  try
  {
    pcaCalc->Compute();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cout << "SUCCESSFULLY caught expected exception" << std::endl;
    std::cout << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
