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

#include "itkImageFileReader.h"

template<typename T> void View(const char *name, T,
                            std::string fileName,
                            bool sharedCamera=false,
                            bool interpolate=true,
                            const std::string & snapshotPath = "",
                            const std::string & ext = "png")
{
  typedef itk::Image<T, 2 >                ImageType;
  typedef itk::ImageFileReader<ImageType>  SourceType;

  typename SourceType::Pointer source = SourceType::New();
  source->SetFileName(fileName);

  QuickView viewer1;
  if (sharedCamera)
    {
    viewer1.ShareCameraOn();
    }
  else
    {
    viewer1.ShareCameraOff();
    }
  if (interpolate)
    {
    viewer1.InterpolateOn();
    }
  else
    {
    viewer1.InterpolateOff();
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

template<typename T> void ViewRGB(const char *name,
                               T,
                               std::string fileName,
                               bool sharedCamera=false,
                               bool interpolate=true,
                               const std::string & snapshotPath = "",
                               const std::string & ext = "png")
{
  typedef itk::RGBPixel<T>                      ColorPixelType;
  typedef itk::Image<ColorPixelType, 2 >        ColorImageType;
  typedef itk::ImageFileReader<ColorImageType>  SourceType;

  typename SourceType::Pointer source = SourceType::New();

  source->SetFileName(fileName);
  QuickView viewer1;

  if (sharedCamera)
    {
    viewer1.ShareCameraOn();
    }
  else
    {
    viewer1.ShareCameraOff();
    }
  if (interpolate)
    {
    viewer1.InterpolateOn();
    }
  else
    {
    viewer1.InterpolateOff();
    }


  viewer1.AddRGBImage(source->GetOutput(),
                  true,
                   std::string(name) + " flipped");
  viewer1.AddRGBImage(source->GetOutput(),
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
  View("unsigned char", static_cast<unsigned char>(0), argv[1], true, true);
  View("unsigned char", static_cast<unsigned char>(0), argv[1]);
  View("char", char(0), argv[1]);
  View("unsigned short", static_cast<unsigned short>(0), argv[1]);
  View("short", short(0), argv[1]);
  View("unsigned int", static_cast<unsigned int>(0), argv[1]);
  View("int", int(0), argv[1]);
  View("unsigned long", static_cast<unsigned long>(0), argv[1]);
  View("long", long(0), argv[1]);
  View("float", float(0), argv[1]);
  View("double", double(0), argv[1]);

  ViewRGB("RGB-float", float(0), argv[1], true, false);
  ViewRGB("RGB-float", float(0), argv[1]);

  if (argc > 2)
    {
    View("unsigned char", static_cast<unsigned char>(0),
         argv[1],
         false,
         true,
         argv[2]);
    View("unsigned char", static_cast<unsigned char>(0),
         argv[1],
         false,
         true,
         argv[2],
         std::string("tif"));
    View("unsigned char", static_cast<unsigned char>(0),
         argv[1],
         false,
         true,
         argv[2],
         std::string("jpg"));
    View("unsigned char", static_cast<unsigned char>(0),
         argv[1],
         false,
         true,
         argv[2],
         std::string("bmp"));
    }

  return EXIT_SUCCESS;
}
