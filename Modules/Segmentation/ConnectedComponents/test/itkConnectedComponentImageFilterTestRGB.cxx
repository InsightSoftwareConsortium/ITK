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

#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "vnl/vnl_sample.h"

int itkConnectedComponentImageFilterTestRGB(int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage threshold_low threshold_hi [fully_connected] [minimum_object_size]" << std::endl;
    return EXIT_FAILURE;
    }

  typedef   unsigned short  InternalPixelType;
  const     unsigned int    Dimension = 2;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  typedef itk::Image<unsigned short,Dimension>       OutputImageType;

  typedef itk::RGBPixel<unsigned char>        RGBPixelType;
  typedef itk::Image<RGBPixelType, Dimension> RGBImageType;

  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter<  RGBImageType  >    WriterType;


  typedef itk::BinaryThresholdImageFilter< InternalImageType, RGBImageType > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< RGBImageType, OutputImageType > FilterType;
  typedef itk::RelabelComponentImageFilter< OutputImageType, OutputImageType > RelabelType;


  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  FilterType::Pointer filter = FilterType::New();
  RelabelType::Pointer relabel = RelabelType::New();

  itk::SimpleFilterWatcher watcher(filter);
  watcher.QuietOn();

  reader->SetFileName( argv[1] );

  InternalPixelType threshold_low, threshold_hi;
  threshold_low = atoi( argv[3]);
  threshold_hi = atoi( argv[4]);

  threshold->SetInput (reader->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::OneValue());
  threshold->SetOutsideValue(itk::NumericTraits<InternalPixelType>::ZeroValue());
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  filter->SetInput (threshold->GetOutput());
  if (argc > 5)
    {
    int fullyConnected = atoi( argv[5] );
    filter->SetFullyConnected( fullyConnected );
    }
  relabel->SetInput( filter->GetOutput() );
  if (argc > 6)
    {
    int minSize = atoi( argv[6] );
    relabel->SetMinimumObjectSize( minSize );
    std::cerr << "minSize: " << minSize << std::endl;
    }

  try
    {
    relabel->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Relabel: exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  // Remap the labels to viewable colors
  RGBImageType::Pointer colored = RGBImageType::New();
  colored->SetRegions( filter->GetOutput()->GetBufferedRegion() );
  colored->Allocate();

  unsigned short numObjects = relabel->GetNumberOfObjects();

  std::vector<RGBPixelType> colormap;
  RGBPixelType px;
  colormap.resize( numObjects+1 );
  vnl_sample_reseed( 1031571 );
  for (unsigned short i=0; i < colormap.size(); ++i)
    {
    px.SetRed(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));
    px.SetGreen(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));
    px.SetBlue(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));

    colormap[i] = px;
    }

  itk::ImageRegionIterator<OutputImageType>
    it(relabel->GetOutput(), relabel->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<RGBImageType> cit(colored,
                                             colored->GetBufferedRegion());

  while( !it.IsAtEnd() )
    {
    if (it.Get() == 0)
      {
      cit.Set(RGBPixelType(itk::NumericTraits< unsigned char >::ZeroValue() ));
      }
    else
      {
      cit.Set( colormap[it.Get()] );
      }
    ++it;
    ++cit;
    }

  try
    {
    writer->SetInput (colored);
    writer->SetFileName( argv[2] );
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }


  return EXIT_SUCCESS;
}
