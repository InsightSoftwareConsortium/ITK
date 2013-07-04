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
#ifndef __itkMINCTransformIO_h
#define __itkMINCTransformIO_h

#include "itkTransformIOBase.h"

#include <string>
#include <vector>
#include <itk_minc2.h>

namespace itk
{

/** \class MINCTransformIO
*  \brief Read&Write transforms in Minc  XFM Format
*
*
* \author Vladimir S. FONOV
*         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
*
* \ingroup ITKIOMINC
*/
class MINCTransformIO: public TransformIOBase
{
public:
  typedef MINCTransformIO               Self;
  typedef TransformIOBase               Superclass;
  typedef SmartPointer< Self >          Pointer;

  typedef TransformBase                 TransformType;
  typedef Superclass::TransformPointer  TransformPointer;
  typedef Superclass::TransformListType TransformListType;
  typedef TransformType::ParametersType ParametersType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MINCTransformIO, TransformIOBase);
  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
  * file specified. */
  virtual bool CanReadFile(const char *);

  /** Determine the file type. Returns true if this ImageIO can read the
  * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read();

  virtual void Write();

protected:
  MINCTransformIO();
  virtual ~MINCTransformIO();

  VIO_General_transform m_XFM;
  bool                  m_XFM_initialized;

private:
  void _cleanup(void);
  void WriteOneTransform(const int transformIndex,
                         const TransformType *transform,
                         std::vector<VIO_General_transform> &_xfm,
                         const char * xfm_file_base,int & serial);

  void ReadOneTransform(VIO_General_transform *xfm);
};

} // end namespace itk

#endif // __itkMINCTransformIO_h
