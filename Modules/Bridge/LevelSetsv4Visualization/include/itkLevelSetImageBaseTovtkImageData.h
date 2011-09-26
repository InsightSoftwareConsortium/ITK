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

#ifndef __itkLevelSetImageBaseTovtkImageData_h
#define __itkLevelSetImageBaseTovtkImageData_h

#include "itkProcessObject.h"
#include "itkLevelSetDenseImageBase.h"
#include "itkImageToVTKImageFilter.h"

class vtkImageData;

namespace itk
{
/** \class LevelSetImageBaseTovtkImageData
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< class TImage >
class LevelSetImageBaseTovtkImageData : public ProcessObject
{
public:
  typedef LevelSetImageBaseTovtkImageData Self;
  typedef ProcessObject                   Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToVTKImageFilter, ProcessObject);

  typedef TImage                          ImageType;

  typedef LevelSetDenseImageBase< ImageType >  LevelSetType;

  typedef ImageToVTKImageFilter< ImageType >  ConverterType;
  typedef typename ConverterType::Pointer     ConverterPointer;

  using Superclass::SetInput;
  virtual void SetInput( LevelSetType* iLevelSet );

  vtkImageData* GetOutput() const;

  void Update();

protected:
  LevelSetImageBaseTovtkImageData();
  ~LevelSetImageBaseTovtkImageData();

private:
  LevelSetImageBaseTovtkImageData( const Self& );
  void operator = ( const Self& );

  ConverterPointer m_Converter;
};
}

#include "itkLevelSetImageBaseTovtkImageData.hxx"
#endif // __itkLevelSetImageBaseTovtkImageData_h
