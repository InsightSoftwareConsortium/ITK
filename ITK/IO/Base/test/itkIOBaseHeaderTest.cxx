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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "BigEndian_hdr.h"
#include "BigEndian_img.h"
#include "itkArchetypeSeriesFileNames.h"
#include "itkConvertPixelBuffer.txx"
#include "itkDefaultConvertPixelTraits.h"
#include "itkImageFileReader.txx"
#include "itkImageFileWriter.txx"
#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"
#include "itkImageIORegion.h"
#include "itkImageSeriesReader.txx"
#include "itkImageSeriesWriter.txx"
#include "itkInternationalizationIOHelpers.h"
#include "itkIOCommon.h"
#include "itkNumericSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"
#include "itkStreamingImageIOBase.h"
#include "LittleEndian_hdr.h"
#include "LittleEndian_img.h"



int itkIOBaseHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
