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
#ifndef QuickView_h
#define QuickView_h

#include <vector>
#include <algorithm>
#include <string>

#include <itkImage.h>
#include <itkRGBPixel.h>
#include <itkIntTypes.h>

#include "ITKVtkGlueExport.h"

/** \class ImageInfo
 * \brief A container for an image and its descriptiom
 * \ingroup ITKVtkGlue
 */
class ITKVtkGlue_EXPORT ImageInfo
{
public:
  typedef itk::Image<unsigned char, 2> ImageType;

  ImageInfo(ImageType *image, std::string description="")
  {
    m_Image = image;
    m_Description = description;
  }

  ImageType::Pointer m_Image;
  std::string        m_Description;
};

/** \class RGBImageInfo
 * \brief A container for an rgb image and its descriptiom
 * \ingroup ITKVtkGlue
 */
class ITKVtkGlue_EXPORT RGBImageInfo
{
public:
  typedef itk::Image<itk::RGBPixel<unsigned char>, 2> ImageType;
  RGBImageInfo(ImageType *image, std::string description="")
  {
    m_Image = image;
    m_Description = description;
  }

  ImageType::Pointer m_Image;
  std::string        m_Description;
};

/** \class QuickView
 * \brief A convenient class to render itk images with vtk
 *
 * This class presents a convenient and efficient mechanism to display
 * ITK images in VTK render windows.
 *
 * The AddImage and AddRGBImage methods collect ITK images to be
 * rendered in a collection of VTK RenderWindow's. Each image can
 * be flipped about the vertical axis. An optional description will be
 * displayed at the bottom of each render window.
 *
 * If m_ShareCamera is true, a single <a href="http://www.vtk.org/doc/nightly/html/classvtkCamera.html">vtkCamera</a>
 * will be used for each render window (default is false).
 *
 * Each image is rescaled to have a range between 0 and 255. Currently, the size
 * of each render window is fixed at 300,300 and the text size for descriptions
 * is fixed at 10.
 *
 * The Visualize method displays the render windows and starts a
 * <a href="http://www.vtk.org/doc/nightly/html/classvtkInteractorStyleImage.html">vtkInteractorStyleImage</a>.
 * The layout and background color of each render window is fixed. The optional
 * boolean for the constructor, if false, bypasses the interactor. This is useful
 * for running tests.
 *
 * Typical usage:
 *
 * \code
 *  QuickView viewer;
 *  viewer.AddImage(someFilter->GetOutput().
 *                  true (to flip image) or false.
 *                  "text to display with the image");
 *
 *  viewer.AddRGBImage(someFilter->GetOutput().
 *                     true (to flip image) or false.
 *                     "text to display with the image");
 *  \endcode
 *
 * \ingroup ITKVtkGlue
 */
class ITKVtkGlue_EXPORT QuickView
{
public:
  QuickView()
  {
    m_ShareCamera = true;
    m_Interpolate = true;
    m_Counter = 0;
    m_Snapshot = false;
    m_SnapshotPath = "";
    m_SnapshotPrefix = "snapshot_";
    m_SnapshotExtension = "png";
    m_NumberOfColumns = 4;
    m_ViewPortSize = 300;
  }
  /** Add an image to be rendered. */
  template<typename TImage> void AddImage(
    TImage *,
    bool FlipVertical=true,
    std::string Description="");

  /** Add an RGB image to be rendered */
  template<typename TImage> void AddRGBImage(
    TImage *,
    bool FlipVertical=true,
    std::string Description="");

  /** Render the images. If interact is tru, start a vtk
   * Interactor. If false, return after one render.
   */
  void Visualize(bool interact=true);


  /** Each render window will have its own camera */
  void ShareCameraOff()
  {
    m_ShareCamera = false;
  }

  /** Each render window will use the same camera */
  void ShareCameraOn()
  {
    m_ShareCamera = true;
  }

  /** Use pixel replication in rendered image */
  void InterpolateOff()
  {
    m_Interpolate = false;
  }

  /** Use pixel interpolation in rendered image */
  void InterpolateOn()
  {
    m_Interpolate = true;
  }

  /** Each render window will take a snaphot */
  void SnapshotOn()
  {
    m_Snapshot = true;
  }

  /** Each render window will take a snaphot */
  void SnapshotOff()
  {
    m_Snapshot = false;
  }

  void SetSnapshotPath( const std::string& iPath )
  {
    m_SnapshotPath = iPath;
  }

  void SetSnapshotPrefix( const std::string& iPrefix )
  {
    m_SnapshotPrefix = iPrefix;
  }

  /** Provide the image format to be used when taking snapshot */
  void SetSnapshotExtension( const std::string& iExtension )
  {
    m_SnapshotExtension = iExtension;
    std::transform(
          m_SnapshotExtension.begin(),
          m_SnapshotExtension.end(),
          m_SnapshotExtension.begin(),
          ::tolower );
  }

  /** Set the number of columns, default 4.*/
  void SetNumberOfColumns (const unsigned int columns)
  {
    m_NumberOfColumns = columns;
  }

  /** Set the viewport size, default 300.*/
  void SetViewPortSize (const unsigned int size)
  {
    m_ViewPortSize = size;
  }

private:
  std::vector<ImageInfo>    Images;        // Container for images
  std::vector<RGBImageInfo> RGBImages;     // Container for rgb images
  itk::IdentifierType       m_Counter;
  std::string               m_SnapshotPath;
  std::string               m_SnapshotPrefix;
  std::string               m_SnapshotExtension;
  bool                      m_ShareCamera;
  bool                      m_Snapshot;
  bool                      m_Interpolate;
  unsigned int              m_NumberOfColumns;
  unsigned int              m_ViewPortSize;
};

#endif
