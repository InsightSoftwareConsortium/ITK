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

#ifndef itkBinaryImageToLevelSetImageAdaptorBase_h
#define itkBinaryImageToLevelSetImageAdaptorBase_h

#include "itkImage.h"
#include "itkObject.h"

namespace itk
{
/** \class BinaryImageToLevelSetImageAdaptorBase
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInputImage, typename TLevelSet >
class BinaryImageToLevelSetImageAdaptorBase : public Object
{
public:
  typedef BinaryImageToLevelSetImageAdaptorBase Self;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;
  typedef Object                                Superclass;

  /** Run-time type information */
  itkTypeMacro( BinaryImageToLevelSetImageAdaptorBase, Object );

  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    InputImageIndexType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename NumericTraits< InputImagePixelType >::RealType
                                                InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

  typedef TLevelSet                       LevelSetType;
  typedef typename LevelSetType::Pointer  LevelSetPointer;

  /**
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetImagePointer  */
  virtual void Initialize() = 0;

  /** Get the sparse levet set function */
  itkGetModifiableObjectMacro(LevelSet, LevelSetType );

  /** Set/Get the input image*/
  itkSetObjectMacro( InputImage, InputImageType );
  itkGetModifiableObjectMacro(InputImage, InputImageType );

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptorBase()
    {
    this->m_LevelSet = LevelSetType::New();
    }

  /** Destructor */
  virtual ~BinaryImageToLevelSetImageAdaptorBase() ITK_OVERRIDE {}

  InputImagePointer       m_InputImage;
  LevelSetPointer         m_LevelSet;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptorBase);

};
}

#endif // itkBinaryImageToLevelSetImageAdaptorBase_h
