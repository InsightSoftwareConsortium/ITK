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
#ifndef itkVTKVisualizeImageLevelSetIsoValues_h
#define itkVTKVisualizeImageLevelSetIsoValues_h

#include "itkVTKVisualizeImageLevelSet.h"

#include "itkConceptChecking.h"
#include "itkImageToVTKImageFilter.h"
#include "itkLevelSetTovtkImageData.h"

#include "vtkImageData.h"
#include "vtkLookupTable.h"
#include "vtkMarchingSquares.h"
#include "vtkMarchingCubes.h"
#include "vtkPolyDataMapper.h"
#include "vtkActor.h"
#include "vtkScalarBarActor.h"
#include "vtkProperty.h"
#include "vtkRenderWindowInteractor.h"

namespace itk
{
/**
 * \class VTKVisualizeImageLevelSetIsoValues
 *
 * \tparam TImage Image Type
 * \tparam TLevelSet Level Set Type
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TImage, typename TLevelSet >
class ITK_TEMPLATE_EXPORT VTKVisualizeImageLevelSetIsoValues
{};

/**
 * \class VTKVisualizeImageLevelSetIsoValues
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputPixel, typename TLevelSet >
class ITK_TEMPLATE_EXPORT VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 2 >, TLevelSet >
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

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( Is2Dimensional,
                   ( Concept::SameDimension< LevelSetType::Dimension, 2 > ) );
#endif

protected:
  VTKVisualizeImageLevelSetIsoValues();
  virtual ~VTKVisualizeImageLevelSetIsoValues();

  /** Render the visualization. */
  virtual void PrepareVTKPipeline();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualizeImageLevelSetIsoValues);

  typedef LevelSetTovtkImageData< LevelSetType >  LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer LevelSetConverterPointer;

  LevelSetConverterPointer  m_LevelSetConverter;

  vtkSmartPointer< vtkMarchingSquares > m_MarchingSquare;
  vtkSmartPointer< vtkPolyDataMapper >  m_ContourMapper;
  vtkSmartPointer< vtkActor >           m_ContourActor;
  vtkSmartPointer< vtkScalarBarActor >  m_ScalarBar;
  vtkSmartPointer< vtkLookupTable >     m_Lut;

  IdentifierType  m_Count;
  SizeValueType   m_NumberOfLevels;
  double          m_LevelLimit;
};


/**
 * \class VTKVisualizeImageLevelSetIsoValues
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputPixel, typename TLevelSet >
class ITK_TEMPLATE_EXPORT VTKVisualizeImageLevelSetIsoValues< Image< TInputPixel, 3 >, TLevelSet >
  : public VTKVisualizeImageLevelSet<
      Image< TInputPixel, 3 >,
      ImageToVTKImageFilter< Image< TInputPixel, 3 > > >
{
public:
  typedef VTKVisualizeImageLevelSetIsoValues                                             Self;
  typedef VTKVisualizeImageLevelSet<  Image< TInputPixel, 3 >,
                                      ImageToVTKImageFilter< Image< TInputPixel, 3 > > > Superclass;
  typedef SmartPointer< Self >                                                           Pointer;
  typedef SmartPointer< const Self >                                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKVisualizeImageLevelSetIsoValues, VTKVisualizeImageLevelSet);

  typedef typename Superclass::InputImageType     InputImageType;

  typedef TLevelSet LevelSetType;

  virtual void SetInputImage( const InputImageType * iImage );
  void SetLevelSet( LevelSetType * levelSet );

  /** Set/Get the number of level set contours to visualize. */
  void SetNumberOfLevels( const SizeValueType numLevels );
  SizeValueType GetNumberOfLevels() const;

  /** Set/Get/ the maximum magnitude the levels contours are from the zero level
   * set. */
  void SetLevelLimit( double iLimit );
  double GetLevelLimit() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( Is3Dimensional,
                   ( Concept::SameDimension< LevelSetType::Dimension, 3 > ) );
#endif

protected:
  VTKVisualizeImageLevelSetIsoValues();
  virtual ~VTKVisualizeImageLevelSetIsoValues();

  /** Render the visualization. */
  virtual void PrepareVTKPipeline();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKVisualizeImageLevelSetIsoValues);

  typedef LevelSetTovtkImageData< LevelSetType >  LevelSetConverterType;
  typedef typename LevelSetConverterType::Pointer LevelSetConverterPointer;

  LevelSetConverterPointer  m_LevelSetConverter;

  vtkSmartPointer< vtkMarchingCubes >   m_MarchingCubes;
  vtkSmartPointer< vtkPolyDataMapper >  m_ContourMapper;
  vtkSmartPointer< vtkActor >           m_ContourActor;
  vtkSmartPointer< vtkScalarBarActor >  m_ScalarBar;
  vtkSmartPointer< vtkLookupTable >     m_Lut;

  IdentifierType  m_Count;
  SizeValueType   m_NumberOfLevels;
  double          m_LevelLimit;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKVisualizeImageLevelSetIsoValues.hxx"
#endif

#endif
