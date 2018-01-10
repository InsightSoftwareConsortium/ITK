/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#ifndef __itkMicroscopyTileStitcher_h
#define __itkMicroscopyTileStitcher_h

#include "itkProcessObject.h"
#include "itkImage.h"

namespace itk
{

template <class TPixelType, unsigned int VImageDimension>
class MicroscopyTileStitcher : public ProcessObject
{
public:
  typedef  MicroscopyTileStitcher       Self;
  typedef  ProcessObject                Superclass;
  typedef  SmartPointer<Self>           Pointer;

  itkNewMacro( Self );

  typedef itk::Image<TPixelType, VImageDimension>    ImageType;
  typedef typename ImageType::Pointer        ImagePointerType;
  typedef typename ImageType::RegionType     ImageRegionType;
  typedef typename ImageType::IndexType      ImageIndexType;
  typedef typename ImageType::SizeType       ImageSizeType;
  typedef typename ImageType::SpacingType    ImageSpacingType;
  typedef typename ImageType::PointType      ImagePointType;
  typedef typename ImageType::PixelType      ImagePixelType;
  typedef std::vector<double>                StitchingParameterType;

  itkSetMacro(FixedImage, ImagePointerType);
  itkGetMacro(FixedImage, ImagePointerType);

  itkSetMacro(MovingImage, ImagePointerType);
  itkGetMacro(MovingImage, ImagePointerType);

  itkGetMacro(PreprocessFlag, bool);
  itkSetMacro(PreprocessFlag, bool);

  const StitchingParameterType & GetStitchingParameters() const;

  virtual void Update();

protected:
  MicroscopyTileStitcher();
  ~MicroscopyTileStitcher();

  int ComputeOverlapImageRegion(ImageRegionType & fixedOverlapRegion,
                                ImageRegionType & movingOverlapRegion);

  int RegisterWithPhaseCorrelationFilter(ImagePointerType fixedImage,
                                         ImagePointerType movingImage);

  int RegisterThresholdedImages(ImagePointerType fixedImage,
                                ImagePointerType movingImage);

  bool                                   m_PreprocessFlag;

private:
  MicroscopyTileStitcher(const MicroscopyTileStitcher&);   // Not implemented.
  void operator=(const MicroscopyTileStitcher&);  // Not implemented.

  ImagePointerType                    m_FixedImage;
  ImagePointerType                    m_MovingImage;
  StitchingParameterType              m_StitchingParameters;
};

} // end namespace itk

#include "itkMicroscopyTileStitcher.txx"

#endif
