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
#include "vtkSmartPointer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkInteractorStyleRubberBand3D.h"
#include "vtkRenderer.h"
#include "vtkCamera.h"
#include "vtkImageMapper.h"
#include "vtkImagePlaneWidget.h"

#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkFixedArray.h"
#include "itkViewImage.h"

namespace itk
{

template< typename TImage >
void
ViewImage< TImage >
::View( const ImageType* img,
        const std::string& winTitle,
        size_t winWidth,
        size_t winHeight )
{
  using ConnectorType = ImageToVTKImageFilter< ImageType >;
  auto connector = ConnectorType::New();
  connector->SetInput(img);
  connector->Update();
  connector->UpdateLargestPossibleRegion();

  // Setup renderers
  vtkSmartPointer< vtkRenderer > renderer = vtkSmartPointer< vtkRenderer >::New();

  // Setup render window
  vtkSmartPointer< vtkRenderWindow > renderWindow = vtkSmartPointer< vtkRenderWindow >::New();
  renderWindow->SetWindowName(winTitle.c_str());
  renderWindow->SetSize(winWidth, winHeight);
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
  using FilterType = StatisticsImageFilter< ImageType >;
  auto filter = FilterType::New();
  filter->SetInput(img);
  filter->Update();
  filter->UpdateLargestPossibleRegion();
  double minIntensity = filter->GetMinimum();
  double maxIntensity = filter->GetMaximum();
  double window = maxIntensity - minIntensity;
  double level  = minIntensity + window / 2;
  /** SLICES */
  FixedArray< vtkSmartPointer< vtkImagePlaneWidget >, 3 > slicePlanes;
  for ( unsigned i = 0; i < 3; ++i )
    {
    slicePlanes[i] = vtkSmartPointer< vtkImagePlaneWidget >::New();
    slicePlanes[i]->SetResliceInterpolateToCubic();
    slicePlanes[i]->DisplayTextOn();
    slicePlanes[i]->SetInteractor(renderWindowInteractor);
    slicePlanes[i]->PlaceWidget();
    slicePlanes[i]->SetSliceIndex(0);
    slicePlanes[i]->SetMarginSizeX(0);
    slicePlanes[i]->SetMarginSizeY(0);
    slicePlanes[i]->SetRightButtonAction(
      vtkImagePlaneWidget::VTK_SLICE_MOTION_ACTION);
    slicePlanes[i]->SetMiddleButtonAction(
      vtkImagePlaneWidget::VTK_WINDOW_LEVEL_ACTION);
    slicePlanes[i]->TextureInterpolateOff();

    slicePlanes[i]->SetInputData(connector->GetOutput());
    slicePlanes[i]->SetPlaneOrientation(i);
    slicePlanes[i]->UpdatePlacement();
    slicePlanes[i]->SetWindowLevel(window, level);
    slicePlanes[i]->On();
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
