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
#ifndef itkTIFFReaderInternal_h
#define itkTIFFReaderInternal_h

#include "ITKIOTIFFExport.h"
#include "itkIntTypes.h"
#include "itk_tiff.h"


namespace itk
{

class ITKIOTIFF_HIDDEN TIFFReaderInternal
{
public:
  TIFFReaderInternal();
  int Initialize();

  void Clean();

  int CanRead();

  int Open(const char *filename);

  TIFF *         m_Image;
  bool           m_IsOpen;
  uint32_t       m_Width;
  uint32_t       m_Height;
  uint16_t       m_NumberOfPages;
  uint16_t       m_CurrentPage;
  uint16_t       m_SamplesPerPixel;
  uint16_t       m_Compression;
  uint16_t       m_BitsPerSample;
  uint16_t       m_Photometrics;
  bool           m_HasValidPhotometricInterpretation;
  uint16_t       m_PlanarConfig;
  uint16_t       m_Orientation;
  uint32_t       m_TileRows;
  uint32_t       m_TileColumns;
  uint32_t       m_TileWidth;
  uint32_t       m_TileHeight;
  uint32_t       m_NumberOfTiles;
  uint32_t       m_SubFiles;
  uint32_t       m_IgnoredSubFiles;
  uint16_t       m_ResolutionUnit;
  float          m_XResolution;
  float          m_YResolution;
  uint16_t       m_SampleFormat;
};

}

#endif // itkTIFFReaderInternal_h
