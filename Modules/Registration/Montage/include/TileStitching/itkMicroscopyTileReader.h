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

#ifndef __itkMicroscopyTileReader_h
#define __itkMicroscopyTileReader_h

#include <itkProcessObject.h>
#include <itkImage.h>

#include <vector>
#include <string>

namespace itk
{

template <class TImageType>
class MicroscopyTileReader : public itk::ProcessObject
{
public:
  typedef  MicroscopyTileReader           Self;
  typedef  itk::ProcessObject             Superclass;
  typedef  itk::SmartPointer<Self>        Pointer;

  itkNewMacro( Self );

  typedef TImageType                      ImageType;
  typedef typename ImageType::Pointer     ImagePointerType;
  typedef std::vector<std::string>        StringVectorType;

  void SetFileNames(const StringVectorType & filenames);

  virtual void Update();

  itkGetMacro(OutputImage, ImagePointerType);

protected:
  MicroscopyTileReader();
  ~MicroscopyTileReader();

  ImagePointerType ReadImage(const std::string & filename);
  ImagePointerType ReadImageSeries(const std::vector<std::string> & filenames);

private:
  MicroscopyTileReader(const MicroscopyTileReader&);   // Not implemented.
  void operator=(const MicroscopyTileReader&);  // Not implemented.

  StringVectorType          m_FileNames;
  ImagePointerType          m_OutputImage;
};

} // end namespace itk

#include "itkMicroscopyTileReader.txx"

#endif
