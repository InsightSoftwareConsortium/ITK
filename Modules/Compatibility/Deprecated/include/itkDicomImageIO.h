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
#ifndef itkDicomImageIO_h
#define itkDicomImageIO_h
#if !defined( ITK_LEGACY_REMOVE )


#include "itkGDCMImageIO.h"

namespace itk
{
/** \class DicomImageIO
 *
 *  \brief Read DicomImage file format.
 *  \warning NOTE: This reader has been replaced with GDCMImageIO
 *
 *  \deprecated
 * \ingroup ITKDeprecated
 * \ingroup IOFilters
 */
class DicomImageIO:public GDCMImageIO
{
public:
  /** Standard class typedefs. */
  typedef DicomImageIO         Self;
  typedef GDCMImageIO          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DicomImageIO, Superclass);

protected:
  DicomImageIO()
  {
    itkWarningMacro (
      <<
      "DicomImageIO is now implemented as a subclass of GDCMImageIO. Please replace your DicomImageIO references with GDCMImageIO.");
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DicomImageIO);
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif // itkDicomImageIO_h
