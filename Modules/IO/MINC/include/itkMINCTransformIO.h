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
#ifndef itkMINCTransformIO_h
#define itkMINCTransformIO_h

#include "itkTransformIOBase.h"

#include <string>
#include <vector>
#include <itk_minc2.h>
#include "itkMatrixOffsetTransformBase.h"

namespace itk
{

/** \class MINCTransformIOTemplate
 *
* \brief Read and write transforms in Minc XFM Format
*
* \author Vladimir S. FONOV
*         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
*
* \ingroup ITKIOMINC
*/
template< typename TInternalComputationValueType >
class MINCTransformIOTemplate: public TransformIOBaseTemplate< TInternalComputationValueType >
{
public:
  typedef MINCTransformIOTemplate                                  Self;
  typedef TransformIOBaseTemplate< TInternalComputationValueType > Superclass;
  typedef SmartPointer< Self >                                     Pointer;

  typedef typename Superclass::TransformType          TransformType;
  typedef typename Superclass::TransformPointer       TransformPointer;
  typedef typename Superclass::TransformListType      TransformListType;
  typedef typename Superclass::ConstTransformListType ConstTransformListType;
  typedef typename TransformType::ParametersType      ParametersType;

  typedef MatrixOffsetTransformBase<TInternalComputationValueType, 3, 3> MatrixOffsetTransformBaseType;

  typedef typename MatrixOffsetTransformBaseType::MatrixType    MatrixType;
  typedef typename MatrixOffsetTransformBaseType::OffsetType    OffsetType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( MINCTransformIOTemplate, TransformIOBaseTemplate );
  itkNewMacro( Self );

  /** Determine the file type. Returns true if this ImageIO can read the
  * file specified. */
  virtual bool CanReadFile( const char * fileName ) ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can write the
  * file specified. */
  virtual bool CanWriteFile( const char * fileName ) ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read() ITK_OVERRIDE;

  virtual void Write() ITK_OVERRIDE;

protected:
  MINCTransformIOTemplate();
  virtual ~MINCTransformIOTemplate();

  VIO_General_transform m_XFM;
  bool                  m_XFM_initialized;

private:
  void _cleanup();
  void WriteOneTransform(const int transformIndex,
                         const TransformType *transform,
                         std::vector<VIO_General_transform> &_xfm,
                         const char * xfm_file_base,int & serial);

  void ReadOneTransform(VIO_General_transform *xfm);
};

/** This helps to meet backward compatibility */
typedef MINCTransformIOTemplate< double > MINCTransformIO;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMINCTransformIO.hxx"
#endif

#endif // itkMINCTransformIO_h
