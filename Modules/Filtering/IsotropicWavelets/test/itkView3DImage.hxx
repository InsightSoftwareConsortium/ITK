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
#ifndef itkView3DImage_hxx
#define itkView3DImage_hxx
#include <vtkSmartPointer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkInteractorStyleRubberBand3D.h>
#include <vtkRenderer.h>
#include <vtkImageMapper.h>
#include <vtkImagePlaneWidget.h>
#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include <array>
namespace itk
{
template <typename T>
void
View3DImage(const T * img, size_t win_x, size_t win_y)
{
  const unsigned int                    kDimension = T::ImageDimension;
  typedef itk::ImageToVTKImageFilter<T> ConnectorType;
  typename ConnectorType::Pointer       connector = ConnectorType::New();
  connector->SetInput(img);
  connector->Update();
  connector->UpdateLargestPossibleRegion();

  // Setup renderers
  vtkSmartPointer<vtkRenderer> renderer = vtkSmartPointer<vtkRenderer>::New();

  // Setup render window
  vtkSmartPointer<vtkRenderWindow> renderWindow = vtkSmartPointer<vtkRenderWindow>::New();
  renderWindow->SetSize(win_x, win_y);
  renderWindow->AddRenderer(renderer);

  // Setup render window interactor
  vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor = vtkSmartPointer<vtkRenderWindowInteractor>::New();
  vtkSmartPointer<vtkInteractorStyleRubberBand3D> style = vtkSmartPointer<vtkInteractorStyleRubberBand3D>::New();
  renderWindowInteractor->SetInteractorStyle(style);

  // Render and start interaction
  renderWindowInteractor->SetRenderWindow(renderWindow);

  // Prepare for slices.
  typedef itk::StatisticsImageFilter<T> FilterType;
  typename FilterType::Pointer          filter = FilterType::New();
  filter->SetInput(img);
  filter->Update();
  filter->UpdateLargestPossibleRegion();
  double min_intensity = filter->GetMinimum();
  double max_intensity = filter->GetMaximum();
  double window = max_intensity - min_intensity;
  double level = min_intensity + window / 2;
  /** SLICES */
  std::array<vtkSmartPointer<vtkImagePlaneWidget>, kDimension> slice_planes;
  for (unsigned i = 0; i < kDimension; ++i)
  {
    slice_planes[i] = vtkSmartPointer<vtkImagePlaneWidget>::New();
    slice_planes[i]->SetResliceInterpolateToCubic();
    slice_planes[i]->DisplayTextOn();
    slice_planes[i]->SetInteractor(renderWindowInteractor);
    slice_planes[i]->PlaceWidget();
    slice_planes[i]->SetSliceIndex(0);
    slice_planes[i]->SetMarginSizeX(0);
    slice_planes[i]->SetMarginSizeY(0);
    slice_planes[i]->SetRightButtonAction(vtkImagePlaneWidget::VTK_SLICE_MOTION_ACTION);
    slice_planes[i]->SetMiddleButtonAction(vtkImagePlaneWidget::VTK_WINDOW_LEVEL_ACTION);
    slice_planes[i]->TextureInterpolateOff();

    slice_planes[i]->SetInputData(connector->GetOutput());
    slice_planes[i]->SetPlaneOrientation(i);
    slice_planes[i]->UpdatePlacement();
    slice_planes[i]->SetWindowLevel(window, level);
    slice_planes[i]->On();
  }
  renderer->ResetCamera();

  renderWindowInteractor->Initialize();
  renderWindowInteractor->Start();
}

template <typename TLeft, typename TRight>
void
View3DImages(const TLeft * leftImg, const TRight * rightImg, size_t win_x, size_t win_y)
{
  const unsigned int leftDimension = TLeft::ImageDimension;
  const unsigned int rightDimension = TRight::ImageDimension;

  typedef itk::ImageToVTKImageFilter<TLeft> LeftConnectorType;
  typename LeftConnectorType::Pointer       leftConnector = LeftConnectorType::New();
  leftConnector->SetInput(leftImg);
  leftConnector->Update();
  leftConnector->UpdateLargestPossibleRegion();

  typedef itk::ImageToVTKImageFilter<TRight> RightConnectorType;
  typename RightConnectorType::Pointer       rightConnector = RightConnectorType::New();
  rightConnector->SetInput(rightImg);
  rightConnector->Update();
  rightConnector->UpdateLargestPossibleRegion();

  // Setup renderer (UNIQUE)
  vtkSmartPointer<vtkRenderer> renderer = vtkSmartPointer<vtkRenderer>::New();

  // Setup render window (UNIQUE)
  vtkSmartPointer<vtkRenderWindow> renderWindow = vtkSmartPointer<vtkRenderWindow>::New();
  renderWindow->SetSize(win_x, win_y);
  renderWindow->AddRenderer(renderer);

  // Setup render window interactor (UNIQUE)
  vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor = vtkSmartPointer<vtkRenderWindowInteractor>::New();
  vtkSmartPointer<vtkInteractorStyleRubberBand3D> style = vtkSmartPointer<vtkInteractorStyleRubberBand3D>::New();
  renderWindowInteractor->SetInteractorStyle(style);

  // Render and start interaction
  renderWindowInteractor->SetRenderWindow(renderWindow);

  typedef itk::StatisticsImageFilter<TLeft> LeftFilterType;
  // Prepare for slices (BOTH)
  typename LeftFilterType::Pointer leftFilter = LeftFilterType::New();
  leftFilter->SetInput(leftImg);
  leftFilter->Update();
  leftFilter->UpdateLargestPossibleRegion();
  double leftMin_intensity = leftFilter->GetMinimum();
  double leftMax_intensity = leftFilter->GetMaximum();
  double leftWindow = leftMax_intensity - leftMin_intensity;
  double leftLevel = leftMin_intensity + leftWindow / 2;

  typedef itk::StatisticsImageFilter<TRight> RightFilterType;
  typename RightFilterType::Pointer          rightFilter = RightFilterType::New();
  rightFilter->SetInput(rightImg);
  rightFilter->Update();
  rightFilter->UpdateLargestPossibleRegion();
  double rightMin_intensity = rightFilter->GetMinimum();
  double rightMax_intensity = rightFilter->GetMaximum();
  double rightWindow = rightMax_intensity - rightMin_intensity;
  double rightLevel = rightMin_intensity + rightWindow / 2;
  /** SLICES (BOTH) */
  std::array<vtkSmartPointer<vtkImagePlaneWidget>, leftDimension> leftSlice_planes;
  for (unsigned i = 0; i < leftDimension; ++i)
  {
    leftSlice_planes[i] = vtkSmartPointer<vtkImagePlaneWidget>::New();
    leftSlice_planes[i]->SetResliceInterpolateToCubic();
    leftSlice_planes[i]->DisplayTextOn();
    leftSlice_planes[i]->SetInteractor(renderWindowInteractor);
    leftSlice_planes[i]->PlaceWidget();
    leftSlice_planes[i]->SetSliceIndex(0);
    leftSlice_planes[i]->SetMarginSizeX(0);
    leftSlice_planes[i]->SetMarginSizeY(0);
    leftSlice_planes[i]->SetRightButtonAction(vtkImagePlaneWidget::VTK_SLICE_MOTION_ACTION);
    leftSlice_planes[i]->SetMiddleButtonAction(vtkImagePlaneWidget::VTK_WINDOW_LEVEL_ACTION);
    leftSlice_planes[i]->TextureInterpolateOff();

    leftSlice_planes[i]->SetInputData(leftConnector->GetOutput());
    leftSlice_planes[i]->SetPlaneOrientation(i);
    leftSlice_planes[i]->UpdatePlacement();
    leftSlice_planes[i]->SetWindowLevel(leftWindow, leftLevel);
    leftSlice_planes[i]->On();
  }
  std::array<vtkSmartPointer<vtkImagePlaneWidget>, rightDimension> rightSlice_planes;
  for (unsigned i = 0; i < rightDimension; ++i)
  {
    rightSlice_planes[i] = vtkSmartPointer<vtkImagePlaneWidget>::New();
    rightSlice_planes[i]->SetResliceInterpolateToCubic();
    rightSlice_planes[i]->DisplayTextOn();
    rightSlice_planes[i]->SetInteractor(renderWindowInteractor);
    rightSlice_planes[i]->PlaceWidget();
    rightSlice_planes[i]->SetSliceIndex(0);
    rightSlice_planes[i]->SetMarginSizeX(0);
    rightSlice_planes[i]->SetMarginSizeY(0);
    rightSlice_planes[i]->SetRightButtonAction(vtkImagePlaneWidget::VTK_SLICE_MOTION_ACTION);
    rightSlice_planes[i]->SetMiddleButtonAction(vtkImagePlaneWidget::VTK_WINDOW_LEVEL_ACTION);
    rightSlice_planes[i]->TextureInterpolateOff();

    rightSlice_planes[i]->SetInputData(rightConnector->GetOutput());
    rightSlice_planes[i]->SetPlaneOrientation(i);
    rightSlice_planes[i]->UpdatePlacement();
    rightSlice_planes[i]->SetWindowLevel(rightWindow, rightLevel);
    rightSlice_planes[i]->On();
  }
  renderer->ResetCamera();

  renderWindowInteractor->Initialize();
  renderWindowInteractor->Start();
}
} // namespace itk
#endif
