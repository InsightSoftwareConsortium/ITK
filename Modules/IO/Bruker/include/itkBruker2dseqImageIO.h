/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkBruker2dseqImageIO_h
#define itkBruker2dseqImageIO_h

#include "ITKIOBrukerExport.h"

#include "itkImageIOBase.h"
#include "itkVectorContainer.h"

namespace itk
{
/**
 *\class Bruker2dseqImageIO
 * \brief Class that defines how to read Bruker file format.
 *
 * The following is a brief description of the Bruker file format.
 *
 * Within the directory representing a 'session' on the scanner, data is laid
 * out thus:
 *
 * session/
 *     1/                          <- Series/Acquisition number
 *         method                  <- An important header file
 *         acqp                    <- Another important header
 *         fid                     <- Raw data
 *         **other unimportant files**
 *         pdata/
 *             1/                  <- Reconstruction number (may be multiple)
 *                 2dseq           <- Reconstructed data
 *                 visu_pars       <- Most important header
 *                 reco            <- Mostly duplicated in visu_pars
 *                 procs           <- Unimportant header
 *                 id              <- Unimportant header
 *             2/
 *                 ...
 *
 * The minimum required data to read the image is the '2dseq' and 'visu_pars'
 * file. To use this reader, specify the 2dseq file as the filename. It will
 * check for the existence of the visu_pars file. If both these exist, the file
 * is opened. If the other header files exist (method, acqp, etc.) in the
 * correct locations then they will be read and added to the meta-data
 * dictionary, but they are  not used to read the image data itself.
 *
 * This class supports reading only.
 *
 * This file reader has been updated for ParaVision 6 2dseq files. The original
 * code was written by Don C. Bigler at Penn State in 2004. It has been
 * significantly changed, as Bruker also changed the format for ParaVision 6.
 * In particular a new header file, 'visu_pars' was introduced that means that
 * multiple headers no longer need to be read in order to read the '2dseq'
 * file. However, if the other Bruker headers are still present they are read
 * and added to the meta-data in case users wish to extract data from them.
 *
 * The original implementation was contributed as a paper to the Insight Journal
 * https://www.insight-journal.org/browse/publication/237
 *
 * \ingroup ITKIOBruker
 *
 * \author Tobias C Wood, King's College London 2017
 */

class ITKIOBruker_EXPORT Bruker2dseqImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Bruker2dseqImageIO);

  /* Standard class type aliases. */
  using Self = Bruker2dseqImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a SmartPointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Bruker2dseqImageIO, ImageIOBase);

  /** Determine if the necessary files exist to read the specified 2dseq file.
   * Returns true if all required files exist. */
  bool
  CanReadFile(const char * FileNameToRead) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Writing files has not been implemented for Bruker 2dseq.
   * This function will always return false. */
  bool
  CanWriteFile(const char * itkNotUsed(FileNameToWrite)) override
  {
    return false;
  }

  /** Not implemented. */
  void
  WriteImageInformation() override
  {
    return;
  }

  /** Not implemented - does nothing */
  void
  Write(const void * itkNotUsed(buffer)) override
  {
    return;
  }

protected:
  Bruker2dseqImageIO();
  ~Bruker2dseqImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  SwapBytesIfNecessary(void * buff, SizeValueType components);

  IOComponentEnum m_OnDiskComponentType{ IOComponentEnum::UCHAR };
  IOByteOrderEnum m_MachineByteOrder;
};

} // end namespace itk

#endif // itkBruker2dseqImageIO_h
