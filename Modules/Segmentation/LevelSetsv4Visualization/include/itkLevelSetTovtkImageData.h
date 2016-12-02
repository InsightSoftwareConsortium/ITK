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

#ifndef itkLevelSetTovtkImageData_h
#define itkLevelSetTovtkImageData_h

#include "itkLevelSetTovtkImageDataBase.h"

#include "itkLevelSetDenseImage.h"
#include "itkWhitakerSparseLevelSetImage.h"
#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

#include "itkLabelMapToLabelImageFilter.h"

namespace itk
{
template< typename TLevelSet >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData
  {};

/** \class LevelSetTovtkImageData
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< LevelSetDenseImage< TImage > > :
    public LevelSetTovtkImageDataBase< LevelSetDenseImage< TImage > >
{
public:
  typedef TImage                              ImageType;
  typedef LevelSetDenseImage< ImageType >     LevelSetType;

  typedef LevelSetTovtkImageData                      Self;
  typedef LevelSetTovtkImageDataBase< LevelSetType >  Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  typedef typename LevelSetType::Pointer  LevelSetPointer;

  vtkImageData* GetOutput() const;

protected:
  LevelSetTovtkImageData();
  virtual ~LevelSetTovtkImageData();

  void GenerateData();

private:
  LevelSetTovtkImageData( const Self& );
  void operator = ( const Self& );

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  ConverterPointer m_Converter;
};

// -----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > > :
    public LevelSetTovtkImageDataBase< WhitakerSparseLevelSetImage< TOutput, VDimension > >
{
public:
  typedef WhitakerSparseLevelSetImage< TOutput, VDimension > LevelSetType;

  typedef LevelSetTovtkImageData                      Self;
  typedef LevelSetTovtkImageDataBase< LevelSetType >  Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  typedef typename LevelSetType::Pointer  LevelSetPointer;

  vtkImageData* GetOutput() const;

protected:
  LevelSetTovtkImageData();
  virtual ~LevelSetTovtkImageData();

  void GenerateData();

private:
  LevelSetTovtkImageData( const Self& );
  void operator = ( const Self& );

  typedef Image< TOutput, VDimension >  ImageType;
  typedef typename ImageType::Pointer   ImagePointer;

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  ImagePointer     m_InternalImage;
  ConverterPointer m_Converter;
};


// -----------------------------------------------------------------------------
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > > :
    public LevelSetTovtkImageDataBase< ShiSparseLevelSetImage< VDimension > >
{
public:
  typedef ShiSparseLevelSetImage< VDimension > LevelSetType;

  typedef LevelSetTovtkImageData                      Self;
  typedef LevelSetTovtkImageDataBase< LevelSetType >  Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  typedef typename LevelSetType::Pointer  LevelSetPointer;

  vtkImageData* GetOutput() const;

protected:
  LevelSetTovtkImageData();
  virtual ~LevelSetTovtkImageData();

  void GenerateData();

private:
  LevelSetTovtkImageData( const Self& );
  void operator = ( const Self& );

  typedef typename LevelSetType::LabelMapType     LabelMapType;
  typedef typename LevelSetType::LabelMapPointer  LabelMapPointer;

  typedef Image< int8_t, VDimension >   ImageType;
  typedef typename ImageType::Pointer   ImagePointer;

  typedef LabelMapToLabelImageFilter< LabelMapType, ImageType >   LabelMapToLabelImageFilterType;
  typedef typename LabelMapToLabelImageFilterType::Pointer        LabelMapToLabelImageFilterPointer;

  LabelMapToLabelImageFilterPointer   m_LabelMapToLabelImageFilter;

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  ConverterPointer                    m_Converter;
};

// -----------------------------------------------------------------------------
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > > :
    public LevelSetTovtkImageDataBase< MalcolmSparseLevelSetImage< VDimension > >
{
public:
  typedef MalcolmSparseLevelSetImage< VDimension > LevelSetType;

  typedef LevelSetTovtkImageData                      Self;
  typedef LevelSetTovtkImageDataBase< LevelSetType >  Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  typedef typename LevelSetType::Pointer  LevelSetPointer;

  vtkImageData* GetOutput() const;

protected:
  LevelSetTovtkImageData();
  virtual ~LevelSetTovtkImageData();

  void GenerateData();

private:
  LevelSetTovtkImageData( const Self& );
  void operator = ( const Self& );

  typedef typename LevelSetType::LabelMapType     LabelMapType;
  typedef typename LevelSetType::LabelMapPointer  LabelMapPointer;

  typedef Image< int8_t, VDimension >   ImageType;
  typedef typename ImageType::Pointer   ImagePointer;

  typedef LabelMapToLabelImageFilter< LabelMapType, ImageType >   LabelMapToLabelImageFilterType;
  typedef typename LabelMapToLabelImageFilterType::Pointer        LabelMapToLabelImageFilterPointer;

  LabelMapToLabelImageFilterPointer   m_LabelMapToLabelImageFilter;

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  ConverterPointer                    m_Converter;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetTovtkImageData.hxx"
#endif
#endif // itkLevelSetTovtkImageData_h
