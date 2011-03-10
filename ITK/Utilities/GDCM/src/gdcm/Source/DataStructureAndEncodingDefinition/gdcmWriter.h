/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#ifndef GDCMWRITER_H
#define GDCMWRITER_H

#include "gdcmFile.h"

namespace gdcm
{

class FileMetaInformation;
/**
 * \brief Writer ala DOM (Document Object Model)
 * This class is a non-validating writer, it will only performs well-
 * formedness check only.
 *
 * \details Detailled description here
 * To avoid GDCM being yet another broken DICOM lib we try to
 * be user level and avoid writing illegal stuff (odd length,
 * non-zero value for Item start/end length ...)
 * Therefore you cannot (well unless you are really smart) write
 * DICOM with even length tag.
 * All the checks are consider basics:
 * - Correct Meta Information Header (see gdcm::FileMetaInformation)
 * - Zero value for Item Length (0xfffe, 0xe00d/0xe0dd)
 * - Even length for any elements
 * - Alphabetical order for elements (garanteed by design of internals)
 * - 32bits VR will be rewritten with 00
 *
 * \warning
 * gdcm::Writer cannot write a DataSet if no SOP Instance UID (0008,0018) is found
 *
 * \see Reader DataSet File
 */
class GDCM_EXPORT Writer
{
public:
  Writer():F(new File),CheckFileMetaInformation(true) {
    Stream = NULL;
    Ofstream = NULL;
  }
  virtual ~Writer();

  /// Main function to tell the writer to write
  virtual bool Write(); // Execute()

  /// Set the filename of DICOM file to write:
  void SetFileName(const char *filename) {
    //std::cerr << "Stream: " << filename << std::endl;
    //std::cerr << "Ofstream: " << Ofstream << std::endl;
    if (Ofstream && Ofstream->is_open())
      {
      Ofstream->close();
      delete Ofstream;
      }
    Ofstream = new std::ofstream();
    Ofstream->open(filename, std::ios::out | std::ios::binary );
    assert( Ofstream->is_open() );
    assert( !Ofstream->fail() );
    //std::cerr << Stream.is_open() << std::endl;
    Stream = Ofstream;
  }
  /// Set user ostream buffer
  void SetStream(std::ostream &output_stream) {
    Stream = &output_stream;
  }

  /// Set/Get the DICOM file (DataSet + Header)
  void SetFile(const File& f) { F = f; }
  File &GetFile() { return *F; }

  /// Undocumented function, do not use (= leave default)
  void SetCheckFileMetaInformation(bool b) { CheckFileMetaInformation = b; }
  void CheckFileMetaInformationOff() { CheckFileMetaInformation = false; }
  void CheckFileMetaInformationOn() { CheckFileMetaInformation = true; }

protected:
  std::ostream *Stream;
  std::ofstream *Ofstream;

private:
  SmartPointer<File> F;
  bool CheckFileMetaInformation;
};

} // end namespace gdcm

#endif //GDCMWRITER_H
