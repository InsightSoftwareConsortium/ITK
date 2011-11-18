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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkLabelGeometryImageFilter.h"
#include "itkRelabelComponentImageFilter.h"


template < const unsigned int NDimension >
int LabelGeometryImageFilterTest(const char * labelImageName,const char * outputImageName,const char * intensityImageName, const char * outputOrientedImagePath)
{
  typedef unsigned short   LabelPixelType;
  typedef unsigned short   IntensityPixelType;
  typedef unsigned char    UCharPixelType;

  typedef itk::Image<LabelPixelType, NDimension>        LabelImageType;
  typedef itk::Image<IntensityPixelType, NDimension>    IntensityImageType;
  typedef itk::Image<UCharPixelType,NDimension>         UCharImageType;

  // Read the binary image.
  typedef itk::ImageFileReader< LabelImageType >  LabelReaderType;
  typename LabelReaderType::Pointer labelReader = LabelReaderType::New();
  labelReader->SetFileName( labelImageName );

  // Read the intensity image if it is defined.
  typedef itk::ImageFileReader< IntensityImageType > IntensityReaderType;
  typename IntensityReaderType::Pointer intensityReader = IntensityReaderType::New();
  intensityReader->SetFileName( intensityImageName );

  // Set up a connected components filter to label the binary objects.
  typedef itk::ConnectedComponentImageFilter< LabelImageType, LabelImageType > ConnectedComponentType;
  typename ConnectedComponentType::Pointer connectedComponentFilter = ConnectedComponentType::New();
  connectedComponentFilter->SetInput( labelReader->GetOutput() );

  // Relabel the components in order of size.
  typedef itk::RelabelComponentImageFilter< LabelImageType, LabelImageType > RelabelType;
  typename RelabelType::Pointer relabeler = RelabelType::New();
  relabeler->SetInput( connectedComponentFilter->GetOutput() );

  // First test the filter without any intensity image.
  typedef itk::LabelGeometryImageFilter< LabelImageType > LabelGeometryType;
  typename LabelGeometryType::Pointer labelGeometryFilter = LabelGeometryType::New();
  labelGeometryFilter->SetInput( relabeler->GetOutput() );

  // These generate optional outputs.
  labelGeometryFilter->CalculatePixelIndicesOn();
  labelGeometryFilter->CalculateOrientedBoundingBoxOn();
  labelGeometryFilter->CalculateOrientedLabelRegionsOn();

  try
    {
    labelGeometryFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }
  std::cout << "\n\nRUNNING THE FILTER WITHOUT AN INTENSITY IMAGE..." << std::endl;
  labelGeometryFilter->Print(std::cout);

  // Print out the features for the last label.
  // The integrated intensity and weighted centroid values will not be
  // valid sine an intensity image has not been defined.
  typename LabelGeometryType::LabelPixelType labelValue = labelGeometryFilter->GetNumberOfLabels()-1;
  std::cout << "Label value: " << labelValue << std::endl;
  std::cout << "\tVolume: " << labelGeometryFilter->GetVolume(labelValue) << std::endl;
  std::cout << "\tIntegrated Intensity: " << labelGeometryFilter->GetIntegratedIntensity(labelValue) << std::endl;
  std::cout << "\tCentroid: " << labelGeometryFilter->GetCentroid(labelValue) << std::endl;
  std::cout << "\tWeighted Centroid: " << labelGeometryFilter->GetWeightedCentroid(labelValue) << std::endl;
  std::cout << "\tAxes Length: " << labelGeometryFilter->GetAxesLength(labelValue) << std::endl;
  std::cout << "\tMajorAxisLength: " << labelGeometryFilter->GetMajorAxisLength(labelValue) << std::endl;
  std::cout << "\tMinorAxisLength: " << labelGeometryFilter->GetMinorAxisLength(labelValue) << std::endl;
  std::cout << "\tEccentricity: " << labelGeometryFilter->GetEccentricity(labelValue) << std::endl;
  std::cout << "\tElongation: " << labelGeometryFilter->GetElongation(labelValue) << std::endl;
  std::cout << "\tOrientation: " << labelGeometryFilter->GetOrientation(labelValue) << std::endl;
  std::cout << "\tBounding box: " << labelGeometryFilter->GetBoundingBox(labelValue) << std::endl;
  std::cout << "\n\n";

  typename LabelGeometryType::LabelsType allLabels = labelGeometryFilter->GetLabels();
  typename LabelGeometryType::LabelsType::iterator allLabelsIt;
  std::cout << "Number of labels: " << labelGeometryFilter->GetNumberOfLabels() << std::endl;
  std::cout << "All labels: " << std::endl;
  for( allLabelsIt = allLabels.begin(); allLabelsIt != allLabels.end(); allLabelsIt++ )
    {
    std::cout << "\tVolume of label(" << *allLabelsIt << "): " << labelGeometryFilter->GetVolume( *allLabelsIt ) << std::endl;
    }
  std::cout << "\n\n";


  // If there is no intensity image, stop here.  Otherwise, continue.
  if( !strcmp(intensityImageName,"") )
    {
    return EXIT_SUCCESS;
    }

  // ---------------------------------------------------------------------------------------------//
  // Now add an intensity image to measure the features based on shape
  // and intensity.
  // If the type of the two input images is the same, we only need to
  // template over the first since the second is templated by default
  // over the same as the first.  Alternatively, we can specify
  // different types for the two images as is done here.
  typedef itk::LabelGeometryImageFilter< UCharImageType, IntensityImageType > LabelGeometryType2;
  typename LabelGeometryType2::Pointer labelGeometryFilter2 = LabelGeometryType2::New();

  // Convert the labeled image to unsigned char.
  typedef itk::CastImageFilter< LabelImageType, UCharImageType > CastType;
  typename CastType::Pointer caster = CastType::New();
  caster->SetInput( relabeler->GetOutput() );

  labelGeometryFilter2->SetInput( caster->GetOutput() );

  // These generate optional outputs.
  labelGeometryFilter2->CalculatePixelIndicesOn();
  labelGeometryFilter2->CalculateOrientedBoundingBoxOn();
  labelGeometryFilter2->CalculateOrientedLabelRegionsOn();
  labelGeometryFilter2->CalculateOrientedIntensityRegionsOn();

  labelGeometryFilter2->SetIntensityInput( intensityReader->GetOutput() );

  try
    {
    labelGeometryFilter2->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  std::cout << "RUNNING THE FILTER WITH AN INTENSITY IMAGE..." << std::endl;
  labelGeometryFilter2->Print(std::cout);

  typedef itk::ImageFileWriter< UCharImageType >  LabelWriterType;

  typename LabelWriterType::Pointer labelWriter = LabelWriterType::New();
  labelWriter->SetFileName( outputImageName );
  labelWriter->SetInput( caster->GetOutput() );
  try
    {
    labelWriter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  // Print out the rotation matrix.
  typename LabelGeometryType::MatrixType matrix = labelGeometryFilter2->GetRotationMatrix(1);
  std::cout << "Rotation Matrix" << std::endl;
  for( unsigned int i = 0; i < NDimension; i++ )
    {
    for( unsigned int j = 0; j < NDimension; j++ )
      {
      std::cout << matrix.get(i,j) << "\t";
      }
    std::cout << std::endl;
    }

  // Write out images of the oriented objects.
  for( allLabelsIt = allLabels.begin(); allLabelsIt != allLabels.end(); ++allLabelsIt )
    {
    unsigned int label = *allLabelsIt;

    // Ignore the background label.
    if( label == 0 )
      {
      continue;
      }

    std::ostringstream filename;
    char buffer[50];
    sprintf(buffer,"%3.3u",label);
    filename << outputOrientedImagePath << "/OrientedLabelImage" << buffer << ".mhd";

    labelWriter->SetFileName( filename.str().c_str() );
    labelWriter->SetInput( labelGeometryFilter2->GetOrientedLabelImage(label) );

    try
      {
      labelWriter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }

    typedef itk::ImageFileWriter< IntensityImageType > IntensityWriterType;
    typename IntensityWriterType::Pointer intensityWriter = IntensityWriterType::New();
    std::ostringstream filename2;
    filename2 << outputOrientedImagePath << "/OrientedIntensityImage" << buffer << ".mhd";
    intensityWriter->SetFileName( filename2.str().c_str() );
    intensityWriter->SetInput( labelGeometryFilter2->GetOrientedIntensityImage(label) );

    try
      {
      intensityWriter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }
    }

  return EXIT_SUCCESS;
}

int itkLabelGeometryImageFilterTest( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " binaryImage outputLabeledImage dimension [intensityImage outputOrientedImagePath]" << std::endl;
    return EXIT_FAILURE;
    }

  const char * labelImageName  = argv[1];
  const char * outputImageName = argv[2];
  unsigned int dimension = atoi(argv[3]);
  const char * intensityImageName = "";
  const char * outputOrientedImagePath= "";

  if (argc == 6){
    outputOrientedImagePath = argv[5];
    intensityImageName = argv[4];
    }

  if( dimension == 2 )
    {
    LabelGeometryImageFilterTest<2>(labelImageName,outputImageName,intensityImageName,outputOrientedImagePath);
    }
  else if( dimension == 3 )
    {
    LabelGeometryImageFilterTest<3>(labelImageName,outputImageName,intensityImageName,outputOrientedImagePath);
    }
  return EXIT_SUCCESS;
}
