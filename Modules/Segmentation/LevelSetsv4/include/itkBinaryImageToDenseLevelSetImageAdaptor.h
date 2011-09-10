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

#ifndef __itkBinaryImageToDenseLevelSetImageAdaptor_h
#define __itkBinaryImageToDenseLevelSetImageAdaptor_h

#include "itkBinaryImageToLevelSetImageAdaptorBase.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/** \class BinaryImageToDenseLevelSetImageAdaptor
 *  \ingroup ITKLevelSetsv4
 */
template< class TInput, class TLevelSet >
class BinaryImageToDenseLevelSetImageAdaptor :
    public BinaryImageToLevelSetImageAdaptorBase< TInput, TLevelSet >
{
public:
  typedef BinaryImageToDenseLevelSetImageAdaptor                      Self;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;
  typedef BinaryImageToLevelSetImageAdaptorBase< TInput, TLevelSet >  Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToDenseLevelSetImageAdaptor,
                BinaryImageToLevelSetImageAdaptorBase );

  typedef TInput                                InputImageType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    InputImageIndexType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename NumericTraits< InputImagePixelType >::RealType
                                                InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

  typedef TLevelSet                         LevelSetType;
  typedef typename LevelSetType::Pointer    LevelSetPointer;
  typedef typename LevelSetType::ImageType  LevelSetImageType;

  typedef ImageToImageFilter< InputImageType, LevelSetType >  InternalFilterType;
  typedef typename InternalFilterType::Pointer                InternalFilterPointer;

  itkSetObjectMacro( SignedDistanceTransformFilter, InternalFilterType );
  itkGetObjectMacro( SignedDistanceTransformFilter, InternalFilterType );

  /**
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetImagePointer  */
  void Initialize();

protected:
  /** Constructor */
  BinaryImageToDenseLevelSetImageAdaptor();

  /** Destructor */
  virtual ~BinaryImageToDenseLevelSetImageAdaptor();

  InternalFilterPointer   m_SignedDistanceTransformFilter;

private:
  BinaryImageToDenseLevelSetImageAdaptor( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented

};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryImageToDenseLevelSetImageAdaptor.hxx"
#endif
#endif // __itkBinaryImageToDenseLevelSetImageAdaptor_h
