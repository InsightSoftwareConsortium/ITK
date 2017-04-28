
/**
 * \file label.c
 * \brief MINC 2.0 Label functions
 * \author Bert Vincent
 *
 * This small set of three functions are intended to allow for the
 * definition of labeled, or enumerated, volumes.
 * 
 * Labeled volumes must have been created with the class MI_CLASS_LABEL,
 * and with any integer subtype.
 *
 ************************************************************************/

#include <stdlib.h>
#include <hdf5.h>
#include "minc2.h"
#include "minc2_private.h"

#define MI_LABEL_MAX 128

static int miswap2(unsigned short tmp)
{
    unsigned char *x = (unsigned char *) &tmp;
    unsigned char t = x[0];
    x[0] = x[1];
    x[1] = t;
    return (tmp);
}

static int miswap4(unsigned int tmp)
{
    unsigned char *x = (unsigned char *) &tmp;
    unsigned char t = x[0];
    x[0] = x[3];
    x[3] = t;
    t = x[1];
    x[1] = x[2];
    x[2] = t;
    return (tmp);
}

/**
 * This function associates a label name with an integer value for the given
 * volume. Functions which read and write voxel values will read/write 
 * in integer values, and must call miget_label_name() to discover the 
 * descriptive text string which corresponds to the integer value.
*/
int  midefine_label(mihandle_t volume, int value, const char *name)
{
    int result;

    if (volume == NULL || name == NULL) {
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to use null volume or variable");
    }

    if (strlen(name) > MI_LABEL_MAX) {
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Label name is too long");
    }

    if (volume->volume_class != MI_CLASS_LABEL) {
      return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume class is not label");
    }

    if (volume->ftype_id <= 0 || volume->mtype_id <= 0) {
      return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume not initialized");
    }

    MI_CHECK_HDF_CALL_RET(result = H5Tenum_insert(volume->mtype_id, name, &value),"H5Tenum_insert");

    /* We might have to swap these values before adding them to
     * the file type.
     * 
     * COOL! the whole purpose of HDF5 being machine independent is defeated here!
     */
    if (H5Tget_order(volume->ftype_id) != H5Tget_order(volume->mtype_id)) {
        switch (H5Tget_size(volume->ftype_id)) {
        case 2:
            value = miswap2((unsigned short) value);
            break;
        case 4:
            value = miswap4((unsigned int) value);
            break;
        }
    }
    MI_CHECK_HDF_CALL_RET(result = H5Tenum_insert(volume->ftype_id, name, &value),"H5Tenum_insert");

    return (MI_NOERROR);
}

/**
 * For a labelled volume, this function retrieves the text name
 * associated with a given integer value.
 * 
 * The name pointer returned must be freed by calling mifree_name().
*/
int miget_label_name(mihandle_t volume, int value, char **name)
{
    int result;

    if (volume == NULL || name == NULL) {
       return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to use null volume or variable");
    }

    if (volume->volume_class != MI_CLASS_LABEL) {
         MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume class is not label");
    }
    if (volume->mtype_id <= 0) {
         MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume is not initialized");
    }
    *name = malloc(MI_LABEL_MAX);
    if (*name == NULL) {
        return MI_LOG_ERROR(MI2_MSG_OUTOFMEM,MI_LABEL_MAX);
    }

    H5E_BEGIN_TRY {
        result = H5Tenum_nameof(volume->mtype_id, &value, *name, MI_LABEL_MAX);
    } H5E_END_TRY;

    MI_CHECK_HDF_CALL_RET(result,"H5Tenum_nameof");
    return (MI_NOERROR);
}

/**
 * This function is the inverse of miget_label_name(). It is called to determine
 * what integer value, if any, corresponds to the given text string.
*/
int miget_label_value(mihandle_t volume, const char *name, int *value_ptr)
{
    int result;

    if (volume == NULL || name == NULL || value_ptr == NULL) {
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to use null volume or variable");
    }

    if (volume->volume_class != MI_CLASS_LABEL) {
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume class is not label");
    }

    if (volume->mtype_id <= 0) {
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume is not initialized");
    }

    H5E_BEGIN_TRY {
        result = H5Tenum_valueof(volume->mtype_id, name, value_ptr);
    } H5E_END_TRY;

    MI_CHECK_HDF_CALL_RET(result,"H5Tenum_valueof");
    return (MI_NOERROR);
}

/**
 * This function returns the number of defined labels, if any, or zero.
*/
int miget_number_of_defined_labels(mihandle_t volume, int *number_of_labels)
{
  int result;
 
  if (volume == NULL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to use null volume");
  }
  if (volume->volume_class != MI_CLASS_LABEL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume class is not label");
  }

  if (volume->mtype_id <= 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume is not initialized");
  }

  H5E_BEGIN_TRY {
    result = H5Tget_nmembers(volume->mtype_id);
  } H5E_END_TRY;

  MI_CHECK_HDF_CALL_RET(result,"H5Tget_nmembers");
  
  *number_of_labels = result;
    
  return (MI_NOERROR);
}

/**
 * This function returns the label value associated with an index (0,1,...)
*/
int miget_label_value_by_index(mihandle_t volume, int idx, int *value)
{
  int result;
  if (volume == NULL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to use null volume");
  }
  if (volume->volume_class != MI_CLASS_LABEL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume class is not label");
  }
  
  if (volume->mtype_id <= 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Volume is not initialized");
  }

  H5E_BEGIN_TRY {
    result = H5Tget_member_value(volume->mtype_id,idx,value);
  } H5E_END_TRY;

  MI_CHECK_HDF_CALL_RET(result,"H5Tget_member_value");

  return (MI_NOERROR);
}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
