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

#include "itkLiThresholdImageFilter.h"
#include "itkHuangThresholdImageFilter.h"
#include "itkIntermodesThresholdImageFilter.h"
#include "itkIsoDataThresholdImageFilter.h"
#include "itkKittlerIllingworthThresholdImageFilter.h"
#include "itkMaximumEntropyThresholdImageFilter.h"
#include "itkMomentsThresholdImageFilter.h"
#include "itkOtsuThresholdImageFilter.h"
#include "itkRenyiEntropyThresholdImageFilter.h"
#include "itkShanbhagThresholdImageFilter.h"
#include "itkTriangleThresholdImageFilter.h"
#include "itkYenThresholdImageFilter.h"

#include "itkConnectedComponentImageFilter.h"
#include "itkLabelToRGBImageFilter.h"
#include "itkImageFileReader.h"

#include "itksys/SystemTools.hxx"
#include <sstream>
#include <map>
#include "QuickView.h"

int itkVtkConnectedComponentImageFilterTest (int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cout << "Usage: " << argv[0];
    std::cout << " inputImageFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  using InputPixelType = short;
  using OutputPixelType = int;
  using RGBPixelType = itk::RGBPixel<unsigned char>;

  using InputImageType = itk::Image< InputPixelType,  2 >;
  using OutputImageType = itk::Image< OutputPixelType, 2 >;
  using RGBImageType = itk::Image<RGBPixelType, 2>;

  using LiFilterType =
      itk::LiThresholdImageFilter<InputImageType, OutputImageType >;
  using HuangFilterType =
      itk::HuangThresholdImageFilter<InputImageType, OutputImageType >;
  using IntermodesFilterType =
      itk::IntermodesThresholdImageFilter<InputImageType, OutputImageType >;
  using IsoDataFilterType =
      itk::IsoDataThresholdImageFilter<InputImageType, OutputImageType >;
  using KittlerIllingworthFilterType =
      itk::KittlerIllingworthThresholdImageFilter<InputImageType, OutputImageType >;
  using LiFilterType =
      itk::LiThresholdImageFilter<InputImageType, OutputImageType >;
  using MaximumEntropyFilterType =
      itk::MaximumEntropyThresholdImageFilter<InputImageType, OutputImageType >;
  using MomentsFilterType =
      itk::MomentsThresholdImageFilter<InputImageType, OutputImageType >;
  using OtsuFilterType =
      itk::OtsuThresholdImageFilter<InputImageType, OutputImageType >;
  using RenyiEntropyFilterType =
      itk::RenyiEntropyThresholdImageFilter<InputImageType, OutputImageType >;
  using ShanbhagFilterType =
      itk::ShanbhagThresholdImageFilter<InputImageType, OutputImageType >;
  using TriangleFilterType =
      itk::TriangleThresholdImageFilter<InputImageType, OutputImageType >;
  using YenFilterType =
      itk::YenThresholdImageFilter<InputImageType, OutputImageType >;

  using ConnectedComponentImageFilterType =
      itk::ConnectedComponentImageFilter <OutputImageType, OutputImageType >;
  using RGBFilterType = itk::LabelToRGBImageFilter<OutputImageType, RGBImageType>;

  using ReaderType = itk::ImageFileReader< InputImageType >;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  QuickView viewer;
  viewer.SetViewPortSize(200);
  viewer.SetNumberOfColumns(6);

  viewer.AddImage(
    reader->GetOutput(),true,
    itksys::SystemTools::GetFilenameName(argv[1]));

  using FilterContainerType = std::map<std::string, itk::HistogramThresholdImageFilter<InputImageType, OutputImageType>::Pointer>;
  FilterContainerType filterContainer;

  filterContainer["Huang"] = HuangFilterType::New();
  filterContainer["Intermodes"] = IntermodesFilterType::New();
  filterContainer["IsoData"] = IsoDataFilterType::New();
  filterContainer["KittlerIllingworth"] = KittlerIllingworthFilterType::New();
  filterContainer["Li"] = LiFilterType::New();
  filterContainer["MaximumEntropy"] = MaximumEntropyFilterType::New();
  filterContainer["Moments"] = MomentsFilterType::New();
  filterContainer["Otsu"] = OtsuFilterType::New();
  filterContainer["RenyiEntropy"] = RenyiEntropyFilterType::New();
  filterContainer["Shanbhag"] = ShanbhagFilterType::New();
  filterContainer["Triangle"] = TriangleFilterType::New();
  filterContainer["Yen"] = YenFilterType::New();

  auto it = filterContainer.begin();
  for (it = filterContainer.begin(); it != filterContainer.end(); ++it)
    {
    (*it).second->SetInsideValue( 255 );
    (*it).second->SetOutsideValue( 0 );
    (*it).second->SetNumberOfHistogramBins( 256 );
    (*it).second->SetInput( reader->GetOutput() );
    (*it).second->Update();

    ConnectedComponentImageFilterType::Pointer connected =
      ConnectedComponentImageFilterType::New ();
    connected->SetInput((*it).second->GetOutput());

    RGBFilterType::Pointer rgbFilter = RGBFilterType::New();
    rgbFilter->SetInput( connected->GetOutput() );
    std::stringstream desc;
    desc << (*it).first << " threshold = "
         << (*it).second->GetThreshold();
    viewer.AddRGBImage(
      rgbFilter->GetOutput(),
      true,
      desc.str());
    }


  // For testing, turn off interaction
  viewer.Visualize(false);

  return EXIT_SUCCESS;
}
