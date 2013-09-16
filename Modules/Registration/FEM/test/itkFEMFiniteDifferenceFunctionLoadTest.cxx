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
 *=========================================================================*/


#include <fstream>
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMFiniteDifferenceFunctionLoad.h"
#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkFEMSolver.h"

#include "itkImageFileWriter.h"

// tyepdefs used for registration
const unsigned int ImageDimension = 3;
const unsigned int ImageWidth = 16;
typedef unsigned char                                 PixelType;
typedef itk::Image<PixelType, ImageDimension>         testImageType;
typedef itk::Vector<float, ImageDimension>            VectorType;
typedef itk::Image<VectorType, ImageDimension>        FieldType;

const unsigned int PixelsPerElement = 1;
typedef itk::fem::Element2DC0LinearQuadrilateralMembrane  Element2DType;
typedef itk::fem::Element3DC0LinearHexahedronMembrane     Element3DType;
typedef itk::fem::FEMObject<ImageDimension>               FEMObjectType;
typedef itk::fem::Solver<ImageDimension>                  SolverType;
typedef itk::fem::FiniteDifferenceFunctionLoad<testImageType, testImageType> ImageMetricLoadType;

typedef itk::MeanSquareRegistrationFunction<testImageType, testImageType, FieldType> MetricType0;
typedef itk::NCCRegistrationFunction<testImageType, testImageType, FieldType>        MetricType1;
typedef itk::MIRegistrationFunction<testImageType, testImageType, FieldType>         MetricType2;
typedef itk::DemonsRegistrationFunction<testImageType, testImageType, FieldType>     MetricType3;

// Template function to fill in an image with a value
template <typename TImage>
void
FillImage(
  TImage * image,
  typename TImage::PixelType value )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set( value );
    }

}

// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(
  TImage * image,
  double * center,
  double radius,
  typename TImage::PixelType foregnd,
  typename TImage::PixelType backgnd )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );

  typename TImage::IndexType index;
  double r2 = vnl_math_sqr( radius );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += vnl_math_sqr( (double) index[j] - center[j]);
      }
    if( distance <= r2 )
      {
      it.Set( foregnd );
      }
    else
      {
      it.Set( backgnd );
      }
    }

}

// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(
  TImage *input,
  TImage *output )
{
  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator inIt( input, output->GetBufferedRegion() );
  Iterator outIt( output, output->GetBufferedRegion() );
  for( inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++outIt )
    {
    outIt.Set( inIt.Get() );
    }

}

FEMObjectType::Pointer CreateMesh(testImageType* image, unsigned int elementWidth = 1)
{
  typedef itk::fem::MaterialLinearElasticity                          MaterialType;
  typedef itk::fem::ImageToRectilinearFEMObjectFilter<testImageType>  MeshFilterType;

  vnl_vector<unsigned int> pixelsPerElement;
  pixelsPerElement.set_size(ImageDimension);
  for (unsigned int i = 0; i < ImageDimension; i++)
    pixelsPerElement[i] = elementWidth;

  // Setup image to mesh filter
  MeshFilterType::Pointer meshFilter = MeshFilterType::New();
  meshFilter->SetInput(image);
  meshFilter->SetPixelsPerElement(pixelsPerElement);
  MaterialType::Pointer m = MaterialType::New();
  if (ImageDimension == 2)
  {
    Element2DType::Pointer e = Element2DType::New();
    e->SetMaterial(m.GetPointer());
    meshFilter->SetElement(e.GetPointer());
  }
  else
  {
    Element3DType::Pointer e = Element3DType::New();
    e->SetMaterial(m.GetPointer());
    meshFilter->SetElement(e.GetPointer());
  }
  meshFilter->Update();

  return meshFilter->GetOutput();
}

int RunTest(testImageType* fixed, testImageType* moving, FieldType* initField,
            FieldType* outField, SolverType* solver, unsigned int metricType, std::string filenamePrefix)
{
  testImageType::SpacingType spacing = fixed->GetSpacing();
  testImageType::PointType origin = fixed->GetOrigin();

  // --------------------------------------------------------
  std::cout << "Create a FiniteElementFunctionLoad\n" << std::endl;
  ImageMetricLoadType::Pointer load = ImageMetricLoadType::New();
  load->SetMovingImage(moving);
  load->SetFixedImage(fixed);
  load->SetDisplacementField(initField);

  // SetMetric() must to be called after SetDisplacementField()!!
  if (metricType == 1)
    {
    MetricType1::Pointer metric = MetricType1::New();
    load->SetMetric(metric.GetPointer());
    load->SetDescentDirectionMinimize();
    }
  else if (metricType == 2)
    {
    MetricType2::Pointer metric = MetricType2::New();
    load->SetMetric(metric.GetPointer());
    load->SetDescentDirectionMaximize();
    }
  else if (metricType == 3)
    {
    MetricType3::Pointer metric = MetricType3::New();
    load->SetMetric(metric.GetPointer());
    load->SetDescentDirectionMinimize();
    }
  else
    {
    MetricType0::Pointer metric = MetricType0::New();
    load->SetMetric(metric.GetPointer());
    load->SetDescentDirectionMinimize();
    }
  load->InitializeMetric();

  ImageMetricLoadType::RadiusType r;
  for (unsigned int i = 0; i < ImageDimension; i++)
      r[i] = 2;
  load->SetMetricRadius(r);
  load->SetGamma(1);
  load->SetNumberOfIntegrationPoints(1);
  load->PrintCurrentEnergy();


  // --------------------------------------------------------
  // Test force computations Fe()
  Element2DType::VectorType position, solution;
  position.set_size(ImageDimension);
  solution.set_size(ImageDimension);
  typedef itk::ImageRegionIteratorWithIndex<FieldType> Iterator;
  Iterator iter(outField, outField->GetLargestPossibleRegion());
  for( iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
    // Query the image-based force (function Fe())
    for (unsigned int d = 0; d < ImageDimension; d++)
      {
      position[d] = iter.GetIndex()[d] * spacing[d] + origin[d];
      solution[d] = 0;
      }
    solution = load->Fe(position);

    // Write to output displacement field
    FieldType::PixelType pixelVal;
    for (unsigned int d = 0; d < ImageDimension; d++)
      pixelVal[d] = solution[d];
    iter.Set(pixelVal);
    } // end of for (each pixel in displacement field)

  // Write to vector image
  typedef itk::ImageFileWriter<FieldType> FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();
  std::ostringstream outFilenameStream;
  outFilenameStream << filenamePrefix << "ForcesWithMetric" << metricType << ".vtk";
  std::string outFilename = outFilenameStream.str();
  fieldWriter->SetFileName(outFilename.c_str());
  fieldWriter->SetInput(outField);
  fieldWriter->Update();

  // --------------------------------------------------------
  // Test ApplyLoad() function
  FieldType::PixelType pixelVal;
  for (unsigned int d = 0; d < ImageDimension; d++)
    pixelVal = 0.0;
  outField->FillBuffer(pixelVal);
  const FieldType::RegionType& region = outField->GetLargestPossibleRegion();

  FEMObjectType::Pointer femObject = solver->GetInput();
  load->SetSolution(solver->GetLinearSystemWrapper());
  Element2DType::VectorType force;
  force.set_size(ImageDimension);
  for (unsigned int i = 0; i < femObject->GetNumberOfElements(); i++)
    {
    itk::fem::Element* element = femObject->GetElement(i);
    load->ApplyLoad(element, force);
    for (unsigned int n = 0; n < element->GetNumberOfNodes(); n++)
      {
      // Accumulate to corresponding pixel in displacement field
      FieldType::PointType coords;
      FieldType::IndexType index;
      for (unsigned int d = 0; d < ImageDimension; d++)
        {
        coords[d] = element->GetNodeCoordinates(n)[d];
        }
      fixed->TransformPhysicalPointToIndex(coords, index);
      if (!region.IsInside(index))
        continue;

      pixelVal = outField->GetPixel(index);
      for (unsigned int d = 0; d < ImageDimension; d++)
        pixelVal[d] += force(n * ImageDimension + d);
      outField->SetPixel(index, pixelVal);

      } // end of for (each node in an element)
    } // end of for(each element)

  // Write to vector image
  std::ostringstream vectorOutFilenameStream;
  vectorOutFilenameStream << filenamePrefix << "NodalForcesWithMetric" << metricType << ".vtk";

  std::string vectorOutFilename  = vectorOutFilenameStream.str();

  FieldWriterType::Pointer forceFieldWriter = FieldWriterType::New();
  forceFieldWriter->SetFileName(vectorOutFilename.c_str());
  forceFieldWriter->SetInput(outField);
  forceFieldWriter->Update();

  return EXIT_SUCCESS;
}


int itkFEMFiniteDifferenceFunctionLoadTest(int argc, char* argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing output file name prefix" << std::endl;
    return EXIT_FAILURE;
  }
  std::string filenamePrefix = argv[1];

  typedef testImageType::IndexType                          IndexType;
  typedef testImageType::SizeType                           SizeType;
  typedef testImageType::RegionType                         RegionType;
  typedef testImageType::SpacingType                        SpacingType;

  // --------------------------------------------------------
  std::cout << "Generate input images and initial deformation field" << std::endl;

  SpacingType spacing;
  float origin[ImageDimension];
  testImageType::SizeValueType sizeArray[ImageDimension];

  for (unsigned int i = 0; i < ImageDimension; i++)
    {
    spacing[i] = 0.84;
    origin[i] = i * 25.0 + 50.0;  // add some randomness to test robustness
    sizeArray[i] = ImageWidth;
    }

  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  testImageType::Pointer moving = testImageType::New();
  testImageType::Pointer fixed = testImageType::New();
  FieldType::Pointer     initField = FieldType::New();
  FieldType::Pointer     outField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();
  moving->SetSpacing(spacing);
  moving->SetOrigin(origin);

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();
  fixed->SetSpacing(spacing);
  fixed->SetOrigin(origin);

  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();
  initField->SetSpacing(spacing);
  initField->SetOrigin(origin);

  outField->SetLargestPossibleRegion( region );
  outField->SetBufferedRegion( region );
  outField->Allocate();
  outField->SetSpacing(spacing);
  outField->SetOrigin(origin);

  double    center[ImageDimension];
  double    radius;
  for (unsigned int i = 0; i < ImageDimension; i++)
    center[i] = ImageWidth / 2.0;

  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle
  radius = ImageWidth / 4;
  FillWithCircle<testImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  radius = ImageWidth / 2.5;
  FillWithCircle<testImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  typedef itk::ImageFileWriter<testImageType> ImageWriterType;
  ImageWriterType::Pointer writer = ImageWriterType::New();
  std::string filename(filenamePrefix);
  writer->SetInput(moving);
  writer->SetFileName((filename + "Moving.mha").c_str());
  writer->Update();
  writer->SetInput(fixed);
  writer->SetFileName((filename + "Fixed.mha").c_str());
  writer->Update();

  // --------------------------------------------------------
  // Create mesh from image
  FEMObjectType::Pointer femObject = CreateMesh(fixed, PixelsPerElement);
  SolverType::Pointer solver = SolverType::New();
  solver->SetInput(femObject);


  // --------------------------------------------------------
  // Test FinitDifferenceFunctionLoad with four metric types

  for (unsigned int i = 0; i < 4; i++)
    {
    if (RunTest(fixed, moving, initField, outField, solver, i, filename) != EXIT_SUCCESS)
      {
      std::cerr << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;
}
