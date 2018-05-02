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
#ifndef itkViewImage_hxx
#define itkViewImage_hxx
#include <vtkSmartPointer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkInteractorStyleRubberBand3D.h>
#include <vtkRenderer.h>
#include <vtkCamera.h>
#include <vtkImageMapper.h>
#include <vtkImagePlaneWidget.h>
#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include <itkFixedArray.h>
namespace itk
{
template< typename TImageType >
void ViewImage<TImageType>::View( const TImageType* img,
           const std::string& win_title,
           size_t win_x,
           size_t win_y )
{
  using ConnectorType = itk::ImageToVTKImageFilter< TImageType >;
  auto connector = ConnectorType::New();
  connector->SetInput(img);
  connector->Update();
  connector->UpdateLargestPossibleRegion();

  // Setup renderers
  vtkSmartPointer< vtkRenderer > renderer = vtkSmartPointer< vtkRenderer >::New();

  // Setup render window
  vtkSmartPointer< vtkRenderWindow > renderWindow = vtkSmartPointer< vtkRenderWindow >::New();
  renderWindow->SetWindowName(win_title.c_str());
  renderWindow->SetSize(win_x, win_y);
  renderWindow->AddRenderer(renderer);

  // Setup render window interactor
  vtkSmartPointer< vtkRenderWindowInteractor > renderWindowInteractor =
    vtkSmartPointer< vtkRenderWindowInteractor >::New();
  vtkSmartPointer< vtkInteractorStyleRubberBand3D > style =
    vtkSmartPointer< vtkInteractorStyleRubberBand3D >::New();
  renderWindowInteractor->SetInteractorStyle(style);

  // Render and start interaction
  renderWindowInteractor->SetRenderWindow(renderWindow);

  // Prepare for slices.
  using FilterType = itk::StatisticsImageFilter< TImageType >;
  auto filter = FilterType::New();
  filter->SetInput(img);
  filter->Update();
  filter->UpdateLargestPossibleRegion();
  double min_intensity = filter->GetMinimum();
  double max_intensity = filter->GetMaximum();
  double window = max_intensity - min_intensity;
  double level  = min_intensity + window / 2;
  /** SLICES */
  FixedArray< vtkSmartPointer< vtkImagePlaneWidget >, 3 > slice_planes;
  for ( unsigned i = 0; i < 3; ++i )
    {
    slice_planes[i] = vtkSmartPointer< vtkImagePlaneWidget >::New();
    slice_planes[i]->SetResliceInterpolateToCubic();
    slice_planes[i]->DisplayTextOn();
    slice_planes[i]->SetInteractor(renderWindowInteractor);
    slice_planes[i]->PlaceWidget();
    slice_planes[i]->SetSliceIndex(0);
    slice_planes[i]->SetMarginSizeX(0);
    slice_planes[i]->SetMarginSizeY(0);
    slice_planes[i]->SetRightButtonAction(
      vtkImagePlaneWidget::VTK_SLICE_MOTION_ACTION);
    slice_planes[i]->SetMiddleButtonAction(
      vtkImagePlaneWidget::VTK_WINDOW_LEVEL_ACTION);
    slice_planes[i]->TextureInterpolateOff();

    slice_planes[i]->SetInputData(connector->GetOutput());
    slice_planes[i]->SetPlaneOrientation(i);
    slice_planes[i]->UpdatePlacement();
    slice_planes[i]->SetWindowLevel(window, level);
    slice_planes[i]->On();
    }
  // Flip camera because VTK-ITK different corner for origin.
  double pos[3];
  double vup[3];
  vtkCamera *cam = renderer->GetActiveCamera();
  cam->GetPosition(pos);
  cam->GetViewUp(vup);
  for ( unsigned int i = 0; i < 3; ++i )
    {
    pos[i] = -pos[i];
    vup[i] = -vup[i];
    }
  cam->SetPosition(pos);
  cam->SetViewUp(vup);

  renderer->ResetCamera();
  renderWindowInteractor->Initialize();
  renderWindowInteractor->Start();
}
}// namespace itk
#endif
