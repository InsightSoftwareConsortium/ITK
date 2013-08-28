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
#ifndef __itkVTKVisualizeImageLevelSetIsoValues_h
#define __itkVTKVisualizeImageLevelSetIsoValues_h

#include "itkVTKVisualizeImageLevelSet.h"

#include "itkImageToVTKImageFilter.h"
#include "itkLevelSetTovtkImageData.h"

#include "vtkImageData.h"
#include "vtkLookupTable.h"
#include "vtkMarchingSquares.h"
#include "vtkPolyDataMapper.h"
#include "vtkActor.h"
#include "vtkScalarBarActor.h"
#include "vtkProperty.h"
#include "vtkRenderWindowInteractor.h"

namespace itk
{

template< class TImage, class TLevelSet >
class VTKVisualizeImageLevelSetIsoValues
{};

template< typename TInputPixel, class TLevelSet >
class VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
  : public VTKVisualizeImageLevelSet<
      Image< TInputPixel, 2 >,
      ImageToVTKImageFilter< Image< TInputPixel, 2 > > >
{
public:
  typedef VTKVisualizeImageLevelSetIsoValues                                             Self;
  typedef VTKVisualizeImageLevelSet<  Image< TInputPixel, 2 >,
                                      ImageToVTKImageFilter< Image< TInputPixel, 2 > > > Superclass;
  typedef SmartPointer< Self >                                                           Pointer;
  typedef SmartPointer< const Self >                                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualizeImageLevelSetIsoValues, VTKVisualizeImageLevelSet);

  typedef typename Superclass::InputImageType     InputImageType;

  typedef TLevelSet LevelSetType;

  void SetLevelSet( LevelSetType * levelSet );

  /** Set/Get the number of level set contours to visualize. */
  void SetNumberOfLevels( const SizeValueType numLevels );
  SizeValueType GetNumberOfLevels() const;

  /** Set/Get/ the maximum magnitude the levels contours are from the zero level
   * set. */
  void SetLevelLimit( double iLimit );
  double GetLevelLimit() const;

protected:
  VTKVisualizeImageLevelSetIsoValues();
  virtual ~VTKVisualizeImageLevelSetIsoValues();

  /** Render the visualization. */
  virtual void PrepareVTKPipeline();

private:
  VTKVisualizeImageLevelSetIsoValues( const Self& ); // purposely not implemented
  void operator= ( const Self& ); // purposely not implemented

  typedef LevelSetTovtkImageData< LevelSetType >  LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer LevelSetConverterPointer;

  LevelSetConverterPointer  m_LevelSetConverter;

  vtkSmartPointer< vtkMarchingSquares > m_MarchingSquare;
  vtkSmartPointer< vtkPolyDataMapper >  m_ContourMapper;
  vtkSmartPointer< vtkActor >           m_ContourActor;
  vtkSmartPointer< vtkScalarBarActor >  m_ScalarBar;
  vtkSmartPointer< vtkLookupTable >     m_Lut;

  itk::IdentifierType m_Count;
  SizeValueType       m_NumberOfLevels;
  double              m_LevelLimit;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualizeImageLevelSetIsoValues.hxx"
#endif

#endif
