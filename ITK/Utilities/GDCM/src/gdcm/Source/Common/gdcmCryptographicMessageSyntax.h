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
#ifndef GDCMCRYPTOGRAPHICMESSAGESYNTAX_H
#define GDCMCRYPTOGRAPHICMESSAGESYNTAX_H

#include "gdcmTypes.h"

namespace gdcm
{
class CryptographicMessageSyntaxInternals;
//-----------------------------------------------------------------------------

/**
 * \brief
 * Class for CryptographicMessageSyntax encryption. This is just a simple
 * wrapper around openssl PKCS7_encrypt functionalities
 *
 * See online documentation
 * http://www.openssl.org/docs/crypto/PKCS7_encrypt.html
 *
 */
class GDCM_EXPORT CryptographicMessageSyntax
{
public :
  CryptographicMessageSyntax();
  ~CryptographicMessageSyntax();

  // X.509
  bool ParseCertificateFile( const char *filename );
  bool ParseKeyFile( const char *filename );

  typedef enum {
    DES_CIPHER,    // DES
    DES3_CIPHER,   // Triple DES
    AES128_CIPHER, // CBC AES
    AES192_CIPHER, // '   '
    AES256_CIPHER  // '   '
  } CipherTypes;

  /// Set Cipher Type.
  /// Default is: AES256_CIPHER
  void SetCipherType(CipherTypes type);
  CipherTypes GetCipherType() const;

  /// create a PKCS#7 envelopedData structure
  bool Encrypt(char *output, size_t &outlen, const char *array, size_t len) const;

  /// decrypt content from a PKCS#7 envelopedData structure
  bool Decrypt(char *output, size_t &outlen, const char *array, size_t len) const;

private:
  CryptographicMessageSyntaxInternals *Internals;
private:
  CryptographicMessageSyntax(const CryptographicMessageSyntax&);  // Not implemented.
  void operator=(const CryptographicMessageSyntax&);  // Not implemented.
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMCRYPTOGRAPHICMESSAGESYNTAX_H
