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
#ifndef __itkVTKVisualizeDenseImageLevelSet_h
#define __itkVTKVisualizeDenseImageLevelSet_h

#include "itkVTKVisualizeImageLevelSet.h"

#include "itkImageToVTKImageFilter.h"
#include "itkLevelSetTovtkImageData.h"

#include "vtkCornerAnnotation.h"
#include "vtkImageData.h"
#include "vtkLookupTable.h"
#include "vtkMarchingSquares.h"
#include "vtkPolyDataMapper.h"
#include "vtkActor.h"
#include "vtkImageActor.h"
#include "vtkScalarBarActor.h"
#include "vtkProperty.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkImageShiftScale.h"

namespace itk
{

template< class TInputPixel, unsigned int VInputImageDimension, class TLevelSetImage >
class VTKVisualizeDenseImageLevelSet
{};

template< class TInputPixel, class TLevelSetImage >
class VTKVisualizeDenseImageLevelSet< TInputPixel, 2, TLevelSetImage >
  : public VTKVisualizeImageLevelSet< Image< TInputPixel, 2 >, ImageToVTKImageFilter< Image< TInputPixel, 2 > > >
{
public:
  typedef VTKVisualizeDenseImageLevelSet                                                 Self;
  typedef VTKVisualizeImageLevelSet< Image< TInputPixel, 2 >, ImageToVTKImageFilter< Image< TInputPixel, 2 > > >
                                                                                         Superclass;
  typedef SmartPointer< Self >                                                           Pointer;
  typedef SmartPointer< const Self >                                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualizeDenseImageLevelSet, VTKVisualizeImageLevelSet);

  typedef typename Superclass::InputImageType     InputImageType;

  typedef TLevelSetImage                          LevelSetImageType;

  typedef LevelSetDenseImageBase< LevelSetImageType > LevelSetType;

  typedef LevelSetTovtkImageData< LevelSetType >  LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer LevelSetConverterPointer;

  void SetLevelSet( LevelSetType * levelSet );

  /** Set/Get the number of level set contours to visualize. */
  void SetNumberOfLevels( const SizeValueType numLevels );
  SizeValueType GetNumberOfLevels() const;

  /** Set/Get/ the maximum magnitude the levels contours are from the zero level
   * set. */
  void SetLevelLimit( double iLimit );
  double GetLevelLimit() const;

protected:
  VTKVisualizeDenseImageLevelSet();
  virtual ~VTKVisualizeDenseImageLevelSet();

  /** Render the visualization. */
  virtual void PrepareVTKPipeline();

private:
  VTKVisualizeDenseImageLevelSet( const Self& ); // purposely not implemented
  void operator= ( const Self& ); // purposely not implemented

  LevelSetConverterPointer  m_LevelSetConverter;

  vtkSmartPointer< vtkCornerAnnotation >  m_Annotation;

  itk::IdentifierType m_Count;
  SizeValueType       m_NumberOfLevels;
  double              m_LevelLimit;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualizeDenseImageLevelSet.hxx"
#endif

#endif
