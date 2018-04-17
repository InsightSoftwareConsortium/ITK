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
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTovtkImageData);

  using ImageType = TImage;
  using LevelSetType = LevelSetDenseImage< ImageType >;

  using Self = LevelSetTovtkImageData;
  using Superclass = LevelSetTovtkImageDataBase< LevelSetType >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  using LevelSetPointer = typename LevelSetType::Pointer;

  vtkImageData* GetOutput() const override;

protected:
  LevelSetTovtkImageData();
  ~LevelSetTovtkImageData() override;

  void GenerateData() override;

private:
  using ConverterType = ImageToVTKImageFilter< ImageType >;
  using ConverterPointer = typename ConverterType::Pointer;

  ConverterPointer m_Converter;
};

// -----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > > :
    public LevelSetTovtkImageDataBase< WhitakerSparseLevelSetImage< TOutput, VDimension > >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTovtkImageData);

  using LevelSetType = WhitakerSparseLevelSetImage< TOutput, VDimension >;

  using Self = LevelSetTovtkImageData;
  using Superclass = LevelSetTovtkImageDataBase< LevelSetType >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  using LevelSetPointer = typename LevelSetType::Pointer;

  vtkImageData* GetOutput() const override;

protected:
  LevelSetTovtkImageData();
  ~LevelSetTovtkImageData() override;

  void GenerateData() override;

private:
  using ImageType = Image< TOutput, VDimension >;
  using ImagePointer = typename ImageType::Pointer;

  using ConverterType = ImageToVTKImageFilter< ImageType >;
  using ConverterPointer = typename ConverterType::Pointer;

  ImagePointer     m_InternalImage;
  ConverterPointer m_Converter;
};


// -----------------------------------------------------------------------------
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > > :
    public LevelSetTovtkImageDataBase< ShiSparseLevelSetImage< VDimension > >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTovtkImageData);

  using LevelSetType = ShiSparseLevelSetImage< VDimension >;

  using Self = LevelSetTovtkImageData;
  using Superclass = LevelSetTovtkImageDataBase< LevelSetType >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  using LevelSetPointer = typename LevelSetType::Pointer;

  vtkImageData* GetOutput() const override;

protected:
  LevelSetTovtkImageData();
  ~LevelSetTovtkImageData() override;

  void GenerateData() override;

private:
  using LabelMapType = typename LevelSetType::LabelMapType;
  using LabelMapPointer = typename LevelSetType::LabelMapPointer;
  using LabelMapConstPointer = typename LevelSetType::LabelMapConstPointer;

  using ImageType = Image< int8_t, VDimension >;
  using ImagePointer = typename ImageType::Pointer;

  using LabelMapToLabelImageFilterType = LabelMapToLabelImageFilter< LabelMapType, ImageType >;
  using LabelMapToLabelImageFilterPointer = typename LabelMapToLabelImageFilterType::Pointer;

  LabelMapToLabelImageFilterPointer   m_LabelMapToLabelImageFilter;

  using ConverterType = ImageToVTKImageFilter< ImageType >;
  using ConverterPointer = typename ConverterType::Pointer;

  ConverterPointer                    m_Converter;
};

// -----------------------------------------------------------------------------
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > > :
    public LevelSetTovtkImageDataBase< MalcolmSparseLevelSetImage< VDimension > >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTovtkImageData);

  using LevelSetType = MalcolmSparseLevelSetImage< VDimension >;

  using Self = LevelSetTovtkImageData;
  using Superclass = LevelSetTovtkImageDataBase< LevelSetType >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetTovtkImageData, LevelSetTovtkImageDataBase );

  using LevelSetPointer = typename LevelSetType::Pointer;

  vtkImageData* GetOutput() const override;

protected:
  LevelSetTovtkImageData();
  ~LevelSetTovtkImageData() override;

  void GenerateData() override;

private:
  using LabelMapType = typename LevelSetType::LabelMapType;
  using LabelMapPointer = typename LevelSetType::LabelMapPointer;

  using ImageType = Image< int8_t, VDimension >;
  using ImagePointer = typename ImageType::Pointer;

  using LabelMapToLabelImageFilterType = LabelMapToLabelImageFilter< LabelMapType, ImageType >;
  using LabelMapToLabelImageFilterPointer = typename LabelMapToLabelImageFilterType::Pointer;

  LabelMapToLabelImageFilterPointer   m_LabelMapToLabelImageFilter;

  using ConverterType = ImageToVTKImageFilter< ImageType >;
  using ConverterPointer = typename ConverterType::Pointer;

  ConverterPointer                    m_Converter;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetTovtkImageData.hxx"
#endif
#endif // itkLevelSetTovtkImageData_h
