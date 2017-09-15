/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMMODALITYPERFORMEDPROCEDURESTEPSETQUERY_H
#define GDCMMODALITYPERFORMEDPROCEDURESTEPSETQUERY_H

#include "gdcmBaseQuery.h"

namespace gdcm
{
/**
 * \brief ModalityPerformedProcedureStepSetQuery
 * \details contains: the class which will produce a dataset for n-set for Modality Performed Procedure Step sop class
 */
class GDCM_EXPORT ModalityPerformedProcedureStepSetQuery : public BaseQuery{
  friend class QueryFactory;
public:
  ModalityPerformedProcedureStepSetQuery( const std::string & iSopInstanceUID );

  gdcm::DataSet GetRequiredDataSet() const;
  bool ValidateQuery(bool inStrict = true) const;
  UIDs::TSName GetAbstractSyntaxUID() const;
};

} // end namespace gdcm

#endif // GDCMMODALITYPERFORMEDPROCEDURESTEPSETQUERY_H
