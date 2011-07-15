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

#include "itkArchetypeSeriesFileNames.h"
#include "itkConvertPixelBuffer.hxx"
#include "itkDefaultConvertPixelTraits.h"
#include "itkImageFileReader.hxx"
#include "itkImageFileWriter.hxx"
#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"
#include "itkImageIORegion.h"
#include "itkImageSeriesReader.hxx"
#include "itkImageSeriesWriter.hxx"
#include "itkInternationalizationIOHelpers.h"
#include "itkIOCommon.h"
#include "itkNumericSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"
#include "itkStreamingImageIOBase.h"

int itkIOBaseHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
