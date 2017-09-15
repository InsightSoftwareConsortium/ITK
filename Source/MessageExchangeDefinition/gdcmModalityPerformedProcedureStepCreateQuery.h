/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMMODALITYPERFORMEDPROCEDURESTEPCREATEQUERY_H
#define GDCMMODALITYPERFORMEDPROCEDURESTEPCREATEQUERY_H

#include "gdcmBaseQuery.h"

namespace gdcm
{
/**
 * \brief ModalityPerformedProcedureStepCreateQuery
 * \details contains: the class which will produce a dataset for n-create for Modality Performed Procedure Step sop class
 */
class GDCM_EXPORT ModalityPerformedProcedureStepCreateQuery : public BaseQuery{
  friend class QueryFactory;
public:
  ModalityPerformedProcedureStepCreateQuery( const std::string & iSopInstanceUID );

  gdcm::DataSet GetRequiredDataSet() const;
  bool ValidateQuery(bool inStrict = true) const;
  UIDs::TSName GetAbstractSyntaxUID() const;
};

} // end namespace gdcm

#endif // GDCMMODALITYPERFORMEDPROCEDURESTEPCREATEQUERY_H
