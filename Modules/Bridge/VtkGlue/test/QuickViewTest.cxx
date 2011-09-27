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

#include "QuickView.h"

#include "itkRandomImageSource.h"
#include "itkComposeImageFilter.h"
#include <string>

#include "itkRGBPixel.h"

template<class T> void View(const char *name, T,
                            bool sharedCamera=false,
                            const std::string snapshotPath = "",
                            const std::string ext = "png")
{
  typedef itk::Image<T, 2 >                 ImageType;
  typedef itk::RandomImageSource<ImageType> SourceType;

  typename ImageType::SizeType size;
  size.Fill(10);
  typename SourceType::Pointer source = SourceType::New();
  source->SetSize(size);

  QuickView viewer1;
  if (sharedCamera)
    {
    viewer1.ShareCameraOn();
    }
  else
    {
    viewer1.ShareCameraOff();
    }
  viewer1.AddImage(source->GetOutput(),
                  true,
                   std::string(name) + " flipped");
  viewer1.AddImage(source->GetOutput(),
                  false,
                   std::string(name) + " not flipped");

  std::string path(snapshotPath);
  if (path != "")
    {
    viewer1.SnapshotOn();
    viewer1.SetSnapshotPrefix("/QuickViewTest");
    viewer1.SetSnapshotPath(snapshotPath);
    viewer1.SetSnapshotExtension(ext);
    }
  viewer1.Visualize(false);
}

template<class T> void ViewRGB(const char *name,
                               T,
                               bool sharedCamera=false,
                               const std::string snapshotPath = "",
                               const std::string ext = "png")
{
  typedef itk::RGBPixel<T>                        ColorPixelType;
  typedef itk::Image<T, 2 >                       ScalarImageType;
  typedef itk::Image<ColorPixelType, 2 >          ColorImageType;
  typedef itk::RandomImageSource<ScalarImageType> SourceType;
  typedef itk::ComposeImageFilter<
    ScalarImageType, ColorImageType >             ComposeFilterType;

  typename ScalarImageType::SizeType size;
  size.Fill(10);
  typename SourceType::Pointer sourceR = SourceType::New();
  typename SourceType::Pointer sourceG = SourceType::New();
  typename SourceType::Pointer sourceB = SourceType::New();
  sourceR->SetSize(size);
  sourceG->SetSize(size);
  sourceB->SetSize(size);

  typename ComposeFilterType::Pointer compose =
    ComposeFilterType::New();
  compose->SetInput1(sourceR->GetOutput());
  compose->SetInput2(sourceG->GetOutput());
  compose->SetInput3(sourceB->GetOutput());

  QuickView viewer1;
  if (sharedCamera)
    {
    viewer1.ShareCameraOn();
    }
  else
    {
    viewer1.ShareCameraOff();
    }

  viewer1.AddRGBImage(compose->GetOutput(),
                  true,
                   std::string(name) + " flipped");
  viewer1.AddRGBImage(compose->GetOutput(),
                  false,
                   std::string(name) + " not flipped");

  std::string path(snapshotPath);
  if (path != "")
    {
    viewer1.SnapshotOn();
    viewer1.SetSnapshotPrefix("/QuickViewTest");
    viewer1.SetSnapshotPath(snapshotPath);
    viewer1.SetSnapshotExtension(ext);
    }
  viewer1.Visualize(false);
}

int QuickViewTest (int argc, char *argv[])
{
  View("unsigned char", static_cast<unsigned char>(0), true);
  View("unsigned char", static_cast<unsigned char>(0));
  View("char", char(0));
  View("unsigned short", static_cast<unsigned short>(0));
  View("short", short(0));
  View("unsigned int", static_cast<unsigned int>(0));
  View("int", int(0));
  View("unsigned long", static_cast<unsigned long>(0));
  View("long", long(0));
  View("float", float(0));
  View("double", double(0));

  ViewRGB("RGB-float", float(0), true);
  ViewRGB("RGB-float", float(0));

  if (argc > 1)
    {
    View("unsigned char", static_cast<unsigned char>(0), false, argv[1]);
    View("unsigned char", static_cast<unsigned char>(0), false, argv[1], std::string("tif"));
    View("unsigned char", static_cast<unsigned char>(0), false, argv[1], std::string("jpg"));
    View("unsigned char", static_cast<unsigned char>(0), false, argv[1], std::string("bmp"));
    }

  return EXIT_SUCCESS;
}
