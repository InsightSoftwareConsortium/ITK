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

#ifndef __itkVTKVisualize3DLevelSetImage_h
#define __itkVTKVisualize3DLevelSetImage_h

#include "itkVTKVisualizeImageLevelSet.h"

#include "itkImageToVTKImageFilter.h"
#include "itkLevelSetTovtkImageData.h"

#include "vtkPolyDataMapper.h"

namespace itk
{
/**
 * \class VTKVisualize3DLevelSetImage
 * \brief visualization for 3D Level Set
 *
 * \tparam TInputImage Input Image Type
 * \tparam TLevelSet Level Set Type
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< class TInputImage, class TLevelSet >
class VTKVisualize3DLevelSetImage :
    public VTKVisualizeImageLevelSet< TInputImage, ImageToVTKImageFilter< TInputImage > >
{
public:
  typedef ImageToVTKImageFilter< TInputImage >  ImageConverterType;
  typedef typename ImageConverterType::Pointer  ImageConverterPointer;


  typedef VTKVisualize3DLevelSetImage                                   Self;
  typedef VTKVisualizeImageLevelSet< TInputImage, ImageConverterType >  Superclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualize3DLevelSetImage, VTKVisualizeImageLevelSet );

  typedef TInputImage     InputImageType;
  typedef TLevelSet       LevelSetType;

  virtual void SetInputImage( const InputImageType * iImage );

  void SetLevelSet( LevelSetType *f );

protected:
  VTKVisualize3DLevelSetImage();

  virtual ~VTKVisualize3DLevelSetImage();

  virtual void PrepareVTKPipeline();

private:
  VTKVisualize3DLevelSetImage ( const Self& );
  void operator = ( const Self& );

  typedef LevelSetTovtkImageData< LevelSetType >    LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer   LevelSetConverterPointer;

  LevelSetConverterPointer              m_LevelSetConverter;
  vtkSmartPointer< vtkPolyDataMapper >  m_MeshMapper;
  vtkSmartPointer< vtkActor >           m_MeshActor;
  };
}


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualize3DLevelSetImage.hxx"
#endif
#endif
