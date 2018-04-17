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

#ifndef itkLevelSetTovtkImageDataBase_h
#define itkLevelSetTovtkImageDataBase_h

#include "itkProcessObject.h"

class vtkImageData;

namespace itk
{
/** \class LevelSetTovtkImageDataBase
 *  \ingroup ITKLevelSetsv4Visualization
 */
template< typename TLevelSet >
class ITK_TEMPLATE_EXPORT LevelSetTovtkImageDataBase : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTovtkImageDataBase);

  using Self = LevelSetTovtkImageDataBase;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetTovtkImageDataBase, ProcessObject);

  using LevelSetType = TLevelSet;
  using LevelSetPointer = typename LevelSetType::Pointer;

  using Superclass::SetInput;
  virtual void SetInput( LevelSetType* iLevelSet );

  virtual vtkImageData* GetOutput() const = 0;

  void Update() override;

protected:
  LevelSetTovtkImageDataBase();
  ~LevelSetTovtkImageDataBase() override;

  LevelSetPointer m_LevelSet;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetTovtkImageDataBase.hxx"
#endif
#endif // itkLevelSetTovtkImageDataBase_h
