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

#ifndef __itkWhitakerLevelSetTovtkImageData_h
#define __itkWhitakerLevelSetTovtkImageData_h

#include "itkProcessObject.h"
#include "itkWhitakerSparseLevelSetImage.h"
#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"

class vtkImageData;

namespace itk
{
template< typename TOutput, unsigned int VDimension >
class WhitakerLevelSetTovtkImageData : public ProcessObject
{
public:
  typedef WhitakerLevelSetTovtkImageData  Self;
  typedef ProcessObject                   Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToVTKImageFilter, ProcessObject);

  typedef WhitakerSparseLevelSetImage< TOutput, VDimension >  LevelSetType;
  typedef typename LevelSetType::Pointer                      LevelSetPointer;

  typedef Image< TOutput, VDimension >  ImageType;
  typedef typename ImageType::Pointer   ImagePointer;

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  void SetInput( LevelSetType* iLevelSet );

  vtkImageData* GetOutput() const;

  void Update();

protected:
  WhitakerLevelSetTovtkImageData();
  virtual ~WhitakerLevelSetTovtkImageData() {}

private:
  WhitakerLevelSetTovtkImageData( const Self& );
  void operator = ( const Self& );

  ImagePointer      m_InternalImage;
  LevelSetPointer   m_LevelSet;
  ConverterPointer  m_Converter;
};
}

#include "itkWhitakerLevelSetTovtkImageData.hxx"
#endif // __itkWhitakerLevelSetTovtkImageData_h
