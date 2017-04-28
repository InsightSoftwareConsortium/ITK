/** 
 * \file grpattr.c
 * \brief MINC 2.0 group/attribute functions
 * \author Bert Vincent and Leila Baghdadi
 *
 * Functions to manipulate attributes and groups.
 ************************************************************************/
#include <stdlib.h>
#include <hdf5.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "minc2.h"
#include "minc2_private.h"

#define MILIST_MAX_PATH 256
#define MILIST_RECURSE 0x0001

struct milistframe {
  struct milistframe *next;
  hid_t grp_id;
  unsigned int att_idx;
  int grp_idx;
  char relpath[MILIST_MAX_PATH];
};

struct milistdata {
  int flags;
  char *name_ptr;
  int name_len;
  struct milistframe *frame_ptr; /* recursive frame */
};

static void
full_path_for_attr(char *fullpath, int length, const char *path,
                   const char *name, mihandle_t vol)
{
  if (!strcmp(path, MIimage)) {
    snprintf(fullpath, length, MI_ROOT_PATH "/image/%d",
             vol->selected_resolution);
  }
  else {
    if ( (!strcmp(name, "history") ||
          !strcmp(name, "ident") ||
          !strcmp(name,"minc_version")) && ( *path==0 || !strcmp(path,"/")) ) {
      strncpy ( fullpath, MI_ROOT_PATH "/" , length );
    } else {
      strncpy ( fullpath, MI_ROOT_PATH "/" MI_INFO_NAME, length );
    }
  }

  if ( *path != '/' && *path != 0 ) {
    strncat(fullpath, "/", length - strlen(fullpath) - 1);
  }

  strncat(fullpath, path,  length - strlen(fullpath) - 1);
}

static void
full_path_for_group(char *fullpath, int length, const char *path)
{
  strncpy(fullpath, MI_ROOT_PATH "/" MI_INFO_NAME, length);

  if ( *path != '/' && *path != 0 ) {
    strncat(fullpath, "/", length - strlen(fullpath) - 1);
  }

  strncat(fullpath, path,  length - strlen(fullpath) - 1);
}

/** Start listing the objects in a group.
 */
int milist_start ( mihandle_t vol, const char *path, int flags,
               milisthandle_t *handle )
{
  hid_t grp_id;
  char fullpath[256];
  struct milistdata *data;
  struct milistframe *frame;

  full_path_for_group(fullpath, sizeof(fullpath), path);

  grp_id = midescend_path ( vol->hdf_id, fullpath );

  if ( grp_id < 0 ) {
    return ( MI_ERROR );
  }

  data = ( struct milistdata * ) malloc ( sizeof ( struct milistdata ) );

  if ( data == NULL ) {
    return ( MI_ERROR );
  }

  frame = ( struct milistframe * ) malloc ( sizeof ( struct milistframe ) );
  frame->next = NULL;
  frame->grp_id = grp_id;
  frame->att_idx = 0;
  frame->grp_idx = 0;
  strcpy ( frame->relpath, path );
  data->frame_ptr = frame;

  data->flags = flags;

  *handle = data;

  return ( MI_NOERROR );
}

static int
milist_recursion ( milisthandle_t handle, char *path )
{
  struct milistdata *data = ( struct milistdata * ) handle;
  herr_t r;
  struct milistframe *frame;

  for ( ;; ) {

    H5E_BEGIN_TRY {

      r = H5Gget_objtype_by_idx ( data->frame_ptr->grp_id,
      data->frame_ptr->grp_idx );
    } H5E_END_TRY;
    r--;

    if ( r < 0 ) {

      /* End of this group, need to pop the frame. */
      frame = data->frame_ptr->next;
      
      if ( H5Iget_type ( data->frame_ptr->grp_id ) == H5I_GROUP ) {
        H5Gclose ( data->frame_ptr->grp_id );
      } else {
        H5Dclose ( data->frame_ptr->grp_id );
      }

      free ( data->frame_ptr );
      data->frame_ptr = frame;

      if ( frame == NULL ) {
        return ( MI_ERROR );
      }
    } else {

      data->frame_ptr->grp_idx++;
      /* If the object is a group, we need to recurse into it.
       */
      r = 1;

      if ( r == H5G_GROUP ) {
        char tmp[256];
        size_t l;

        H5Gget_objname_by_idx ( data->frame_ptr->grp_id,
                                data->frame_ptr->grp_idx - 1,
                                tmp, sizeof ( tmp ) );
        frame = malloc ( sizeof ( struct milistframe ) );

        if ( frame == NULL ) {
          return ( MI_ERROR );
        }

        frame->grp_idx = 0;
        frame->att_idx = 0;
        frame->grp_id = H5Gopen1 ( data->frame_ptr->grp_id, tmp );

        strcpy ( frame->relpath, data->frame_ptr->relpath );
        l = strlen ( frame->relpath );

        if ( l > 0 && frame->relpath[l - 1] != '/' ) {
          strcat ( frame->relpath, "/" );
        }

        strcat ( frame->relpath, tmp );

        /* Start working on the next frame.
         */
        frame->next = data->frame_ptr;
        data->frame_ptr = frame;
        strncpy ( path, data->frame_ptr->relpath, MILIST_MAX_PATH );
        break;  /* Out of inner for(;;) */
      }
    }
  }

  return ( MI_NOERROR );
}


static herr_t
milist_attr_op ( hid_t loc_id, const char *attr_name, void *op_data )
{
  struct milistdata *data = ( struct milistdata * ) op_data;
  (void)loc_id;/*to remove unused variable warning*/

  strncpy ( data->name_ptr, attr_name, data->name_len );
  return ( 1 );
}

/** Iterate through attributes
 */
int milist_attr_next ( mihandle_t vol, milisthandle_t handle,
                   char *path, int maxpath,
                   char *name, int maxname )
{
  struct milistdata *data = ( struct milistdata * ) handle;
  herr_t r;

  data->name_ptr = name;
  data->name_len = maxname;

  for ( ;; ) {


    H5E_BEGIN_TRY {
      r = H5Aiterate1 ( data->frame_ptr->grp_id,
        &data->frame_ptr->att_idx, milist_attr_op, data );
    } H5E_END_TRY;

    if ( r > 0 ) {
      strncpy ( path, data->frame_ptr->relpath, maxpath );
      return ( MI_NOERROR );
    } else {
      
      if ( data->flags & MILIST_RECURSE ) {

        return milist_recursion ( handle, path );

      } else {
        return ( MI_ERROR );
      }
    }
  }

  return ( MI_NOERROR );
}

/** Finish listing attributes or groups
 */
int
milist_finish ( milisthandle_t handle )
{
  hid_t tmp_id;
  struct milistdata *data = ( struct milistdata * ) handle;
  struct milistframe *frame;

  if ( data == NULL ) {
    return ( MI_ERROR );
  }

  while ( ( frame = data->frame_ptr ) != NULL ) {
    data->frame_ptr = frame->next;
    //H5Gclose(frame->grp_id);
    H5E_BEGIN_TRY {
      tmp_id = H5Gclose ( frame->grp_id );

      if ( tmp_id < 0 ) {
        tmp_id = H5Dclose ( frame->grp_id );
      }
    } H5E_END_TRY;

    free ( frame );
  }

  free ( data );
  return ( MI_NOERROR );
}

static herr_t milist_grp_op ( hid_t loc_id, const char *name, void *op_data )
{
  struct milistdata *data = ( struct milistdata * ) op_data;
  H5G_stat_t statbuf;
  
  H5Gget_objinfo ( loc_id, name, FALSE, &statbuf );

  if ( statbuf.type == H5G_GROUP ) {
    size_t l;
    l = strlen ( data->frame_ptr->relpath );

    if ( l > 0 && data->frame_ptr->relpath[l - 1] != '/' ) {
      strcat ( data->frame_ptr->relpath, "/" );
    }

  }
  strcat ( data->frame_ptr->relpath, name );
  
  return ( 1 );
}

/** Get the group at given path
 */
int milist_grp_next ( milisthandle_t handle, char *path, int maxpath )
{
  struct milistdata *data = ( struct milistdata * ) handle;
  herr_t r;

  if ( ! ( data->flags & MILIST_RECURSE ) ) {
    char fullpath[256];
    char tmp[256];

    strncpy ( fullpath, MI_ROOT_PATH "/" MI_INFO_NAME, sizeof ( fullpath ) - 1 );
    strncat ( fullpath, data->frame_ptr->relpath, sizeof ( fullpath ) - strlen ( fullpath ) - 1 );
    strcpy ( tmp, data->frame_ptr->relpath );

    H5E_BEGIN_TRY {
      r = H5Giterate ( data->frame_ptr->grp_id, fullpath,
        &data->frame_ptr->grp_idx, milist_grp_op, data );

    } H5E_END_TRY;

    if ( r > 0 ) {
      strncpy ( path, data->frame_ptr->relpath, maxpath );
      strncpy ( data->frame_ptr->relpath, tmp, maxpath );
      return ( MI_NOERROR );
    } else {
      return ( MI_ERROR );
    }
  }

  else
    if ( data->flags & MILIST_RECURSE ) {

      r = milist_recursion ( handle, path );

      if ( r == MI_ERROR ) {
        return ( MI_ERROR );
      }
    } else {
      return ( MI_ERROR );
    }

  return ( MI_NOERROR );
}

/** Create a group at "path" using "name".
 */
int micreate_group ( mihandle_t vol, const char *path, const char *name )
{
  hid_t hdf_file;
  hid_t hdf_grp;
  hid_t hdf_new_grp;
  hid_t hdf_gpid;
  char fullpath[256];

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return ( MI_ERROR );
  }

  full_path_for_group(fullpath, sizeof(fullpath), path);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    return ( MI_ERROR );
  }

  H5E_BEGIN_TRY {
    /* See if the group already exists.  If so, just return.
     */
    hdf_new_grp = H5Gopen1 ( hdf_grp, name );

    if ( hdf_new_grp >= 0 ) {
      H5Gclose ( hdf_new_grp );
      return ( MI_NOERROR );
    }

    /* Actually create the requested group.
     */
    hdf_gpid = H5Pcreate (H5P_GROUP_CREATE);
    H5Pset_attr_phase_change (hdf_gpid, 0, 0);

    hdf_new_grp = H5Gcreate2 ( hdf_grp, name, H5P_DEFAULT, hdf_gpid, H5P_DEFAULT );

    if ( hdf_new_grp < 0 ) {
      return ( MI_ERROR );
    }
  } H5E_END_TRY;

  /* Close the handles we created.
   */
  H5Pclose ( hdf_gpid );
  H5Gclose ( hdf_new_grp );
  H5Gclose ( hdf_grp );

  return ( MI_NOERROR );
}

/** Delete the named attribute.
 */
int midelete_attr ( mihandle_t vol, const char *path, const char *name )
{
  hid_t tmp_id;
  hid_t hdf_file;
  hid_t hdf_grp;
  herr_t hdf_result;
  char fullpath[256];

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return ( MI_ERROR );
  }

  full_path_for_attr(fullpath, sizeof(fullpath), path, name, vol);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    return ( MI_ERROR );
  }

  /* Delete the attribute from the path.
   */
  hdf_result = H5Adelete ( hdf_grp, name );

  if ( hdf_result < 0 ) {
    return ( MI_ERROR );
  }

  /* Close the handles we created (PROPERLY!)
   */
  H5E_BEGIN_TRY {
    tmp_id = H5Gclose ( hdf_grp );

    if ( tmp_id < 0 ) {
      tmp_id = H5Dclose ( hdf_grp );
    }
  } H5E_END_TRY;

  return ( MI_NOERROR );
}

/** Delete the subgroup \a name from the group \a path
 */
int midelete_group ( mihandle_t vol, const char *path, const char *name )
{
  hid_t hdf_file;
  hid_t hdf_grp;
  herr_t hdf_result;
  char fullpath[256];

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return ( MI_ERROR );
  }

  full_path_for_group(fullpath, sizeof(fullpath), path);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    return ( MI_ERROR );
  }

  H5E_BEGIN_TRY {
    /* Delete the group (or any object, really) from the path.
     */
    hdf_result = H5Gunlink ( hdf_grp, name );

    if ( hdf_result < 0 ) {
      hdf_result = MI_ERROR;
    } else
    {
      hdf_result = MI_NOERROR;
    }
  } H5E_END_TRY;

  /* Close the handles we created.
   */
  H5Gclose ( hdf_grp );

  return ( hdf_result );
}

/** Get the length of a attribute
 */
int miget_attr_length ( mihandle_t vol, const char *path, const char *name,
                    size_t *length )
{
  hid_t hdf_file=-1;
  hid_t hdf_grp=-1;
  hid_t hdf_attr=-1;
  hsize_t hdf_dims[1];   /* TODO: symbolic constant for "1" here? */
  hid_t hdf_space=-1;
  hid_t hdf_type=-1;
  char fullpath[256];
  int status = MI_ERROR;      /* Guilty until proven innocent */

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"HDF file is not open");
  }

  full_path_for_attr(fullpath, sizeof(fullpath), path, name, vol);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 )
    goto cleanup;

  H5E_BEGIN_TRY {
    hdf_attr = H5Aopen_name ( hdf_grp, name );
  } H5E_END_TRY;
  
  if( hdf_attr<0 )
    goto cleanup;

  if((hdf_space = H5Aget_space ( hdf_attr ))<0)
    goto cleanup;

  if((hdf_type = H5Aget_type ( hdf_attr ))<0) 
    goto cleanup;

  switch ( H5Sget_simple_extent_ndims ( hdf_space ) ) {
  case 0:     /* Scalar */

    /* String types need to return the length of the string.
     */
    if ( H5Tget_class ( hdf_type ) == H5T_STRING ) {
      *length = H5Tget_size ( hdf_type );
    } else {
      *length = 1;
    }

    break;

  case 1:
    H5Sget_simple_extent_dims ( hdf_space, hdf_dims, NULL );
    *length = hdf_dims[0];
    break;

  default:
    /* For now, we allow only scalars and vectors.  No multidimensional
     * arrays for MINC 2.0 attributes.
     */
    MI_LOG_ERROR(MI2_MSG_GENERIC,"Only scalars and vectors are supported");
    goto cleanup;
  }
  
  status=MI_NOERROR;
  
cleanup:
  if(hdf_type >= 0 ) H5Tclose ( hdf_type );
  if(hdf_space >= 0) H5Sclose ( hdf_space );
  if(hdf_attr >= 0 ) H5Aclose ( hdf_attr );
  
  if ( hdf_grp >= 0 ) {
    /* The hdf_loc identifier could be a group or a dataset.
    */
    if ( H5Iget_type ( hdf_grp ) == H5I_GROUP ) {
      H5Gclose ( hdf_grp );
    } else {
      H5Dclose ( hdf_grp );
    }
  }
  
  return status;
}

/** Get the type of an attribute.
 */
int miget_attr_type ( mihandle_t vol, const char *path, const char *name,
                  mitype_t *data_type )
{
  hid_t hdf_file=-1;
  hid_t hdf_grp=-1;
  hid_t hdf_attr=-1;
  hid_t hdf_type=-1;
  char fullpath[256];
  int status = MI_ERROR;      /* Guilty until proven innocent */

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"HDF file is not open");
  }

  full_path_for_attr(fullpath, sizeof(fullpath), path, name, vol);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    goto cleanup;
  }

  H5E_BEGIN_TRY {
    hdf_attr = H5Aopen_name ( hdf_grp, name );
  } H5E_END_TRY;  
  
  if( hdf_attr<0 ) 
    goto cleanup;
  
  if( (hdf_type = H5Aget_type ( hdf_attr ))<0 ) 
    goto cleanup;

  switch ( H5Tget_class ( hdf_type ) ) {
  case H5T_FLOAT:

    if ( H5Tget_size ( hdf_type ) == sizeof ( float ) ) {
      *data_type = MI_TYPE_FLOAT;
    } else {
      *data_type = MI_TYPE_DOUBLE;
    }

    break;
  case H5T_STRING:
    *data_type = MI_TYPE_STRING;
    break;
  case H5T_INTEGER:
    *data_type = MI_TYPE_INT;
    break;
  default:
    goto cleanup;
  }
  status = MI_NOERROR;
  
cleanup:

  if( hdf_type >= 0 ) H5Tclose ( hdf_type );
  if( hdf_attr >= 0 ) H5Aclose ( hdf_attr );
  
  if ( hdf_grp >= 0 ) {
    /* The hdf_loc identifier could be a group or a dataset.
    */
    if ( H5Iget_type ( hdf_grp ) == H5I_GROUP ) {
      H5Gclose ( hdf_grp );
    } else {
      H5Dclose ( hdf_grp );
    }
  }
  return status;
}

/** Copy all attribute given a path
 */
int micopy_attr ( mihandle_t vol, const char *path, mihandle_t new_vol )
{
  milisthandle_t hlist = NULL;
  mitype_t data_type = MI_TYPE_UNKNOWN;
  char namebuf[256];
  char pathbuf[256];
  char valstr[128];
  int r;
  size_t length;

  /*TODO: make sure path size does not exceed 256 somehow*/
  r = milist_start ( vol, path, 1, &hlist );

  if ( r == MI_NOERROR ) {
    while ( milist_attr_next ( vol, hlist, pathbuf, sizeof ( pathbuf ),
                               namebuf, sizeof ( namebuf ) ) == MI_NOERROR ) {
      /*TODO: Add error checking here*/
      miget_attr_type ( vol, pathbuf, namebuf, &data_type );
      miget_attr_length ( vol, pathbuf, namebuf, &length );
    
      switch ( data_type ) {

      case MI_TYPE_STRING:
        if(length<sizeof(valstr))
        {
          miget_attr_values ( vol, MI_TYPE_STRING, pathbuf, namebuf, length, valstr );
          miset_attr_values ( new_vol, MI_TYPE_STRING, pathbuf, namebuf, length, valstr );
        } else {
          char *tmp_str=malloc(length+1); /*make sure we have space for terminating zero*/
          miget_attr_values ( vol, MI_TYPE_STRING, pathbuf, namebuf, length+1, tmp_str );
          miset_attr_values ( new_vol, MI_TYPE_STRING, pathbuf, namebuf, length+1, tmp_str );
          free(tmp_str);
        }
        break;

      case MI_TYPE_FLOAT:
        if(length==1)
        {
          float valflt;
          miget_attr_values ( vol, MI_TYPE_FLOAT, pathbuf, namebuf, 1, &valflt );
          miset_attr_values ( new_vol, MI_TYPE_FLOAT, pathbuf, namebuf, 1, &valflt );
        } else {
          float *tmp=malloc(length*sizeof(float));
          miget_attr_values ( vol, MI_TYPE_FLOAT, pathbuf, namebuf, length, tmp );
          miset_attr_values ( new_vol, MI_TYPE_FLOAT, pathbuf, namebuf, length, tmp );
          free(tmp);
        }
        break;
        
      case MI_TYPE_DOUBLE:
        if(length==1)
        {
          float valdbl;
          miget_attr_values ( vol, MI_TYPE_DOUBLE, pathbuf, namebuf, 1, &valdbl );
          miset_attr_values ( new_vol, MI_TYPE_DOUBLE, pathbuf, namebuf, 1, &valdbl );
        } else {
          double *tmp=malloc(length*sizeof(double));
          miget_attr_values ( vol, MI_TYPE_DOUBLE, pathbuf, namebuf, length, tmp );
          miset_attr_values ( new_vol, MI_TYPE_DOUBLE, pathbuf, namebuf, length, tmp );
          free(tmp);
        }
        break;

      case MI_TYPE_INT:
        if(length==1)
        {
          long  vallng;
          miget_attr_values ( vol, MI_TYPE_INT, pathbuf, namebuf, 1, &vallng );
          miset_attr_values ( new_vol, MI_TYPE_INT, pathbuf, namebuf, 1, &vallng );
        } else {
          long *tmp=malloc(length*sizeof(long));
          miget_attr_values ( vol, MI_TYPE_INT, pathbuf, namebuf, length, tmp );
          miset_attr_values ( new_vol, MI_TYPE_INT, pathbuf, namebuf, length, tmp );
          free(tmp);
        }
        break;
      default:
        return MI_LOG_ERROR(MI2_MSG_GENERIC,"Unsupported attribute type");
      }
    }
  } else {
    MI_LOG_ERROR(MI2_MSG_GENERIC,"milist_start failed");
  }

  milist_finish ( hlist );

  return ( MI_NOERROR );
}

/** Get the values of an attribute.
 */
int miget_attr_values ( mihandle_t vol, mitype_t data_type, const char *path,
                    const char *name, size_t length, void *values )
{
  hid_t hdf_file  = -1;
  hid_t hdf_grp   = -1;
  hid_t mtyp_id   = -1;
  hid_t hdf_space = -1;
  hid_t hdf_attr  = -1;
  hid_t hdf_type  = -1;
  char fullpath[256];
  int status = MI_ERROR;      /* Guilty until proven innocent */
  
  hsize_t hdf_attr_size = 0;

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"HDF file is not open");
  }

  full_path_for_attr(fullpath, sizeof(fullpath), path, name, vol);

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    goto cleanup;
  }

  H5E_BEGIN_TRY {
    hdf_attr = H5Aopen_name ( hdf_grp, name );
  } H5E_END_TRY;

  if( hdf_attr<0 ) 
    goto cleanup;

  switch ( data_type ) {
  case MI_TYPE_INT:
    mtyp_id = H5Tcopy ( H5T_NATIVE_INT );
    break;
  case MI_TYPE_FLOAT:
    mtyp_id = H5Tcopy ( H5T_NATIVE_FLOAT );
    break;
  case MI_TYPE_DOUBLE:
    mtyp_id = H5Tcopy ( H5T_NATIVE_DOUBLE );
    break;
  case MI_TYPE_STRING:
    mtyp_id = H5Tcopy ( H5T_C_S1 );
    H5Tset_size ( mtyp_id, length );
    break;
  default:
    goto cleanup;
  }

  if( (hdf_space = H5Aget_space ( hdf_attr )) < 0 ) goto cleanup;
  if( (hdf_type  = H5Aget_type ( hdf_attr )) <0 )   goto cleanup;
  /* If we're retrieving a vector, make certain the length passed into this
   * function is sufficient.
   */
  switch ( H5Sget_simple_extent_ndims ( hdf_space ) ) 
  {
    case 0:     /* Scalar */
   
    /* String types need to return the length of the string.
          */
       if ( H5Tget_class ( hdf_type ) == H5T_STRING ) {
        hdf_attr_size=H5Tget_size ( hdf_type );
       } else {
         hdf_attr_size = 1;
       }
                                     
       break;
                                        
     case 1:
       H5Sget_simple_extent_dims ( hdf_space, &hdf_attr_size, NULL );
       break;
                                         
     default:
       /* For now, we allow only scalars and vectors.  No multidimensional
        * arrays for MINC 2.0 attributes.
        */
       MI_LOG_ERROR(MI2_MSG_GENERIC,"Only scalars and vectors are supported");
       goto cleanup;
  }
 
  if ( length < hdf_attr_size ) {
    /* Not enough space in the output vector */
    fprintf(stderr,"Requested size:%d needed size:%d\n",(int)length,(int)hdf_attr_size);
    goto cleanup;
  }
  

  if ( H5Aread ( hdf_attr, mtyp_id, values ) <0 ) 
    goto cleanup;

  /* make sure string is zero terminated if there is enough space in the input buffer */
  if( data_type == MI_TYPE_STRING && length > hdf_attr_size )
    ( ( char * ) values ) [hdf_attr_size] = '\0';

  status = MI_NOERROR;

cleanup:

  if( hdf_space >= 0 ) H5Sclose ( hdf_space );
  if( hdf_type  >= 0 ) H5Tclose ( hdf_type );
  if( hdf_attr  >= 0 ) H5Aclose ( hdf_attr );
  if( mtyp_id   >= 0 ) H5Tclose ( mtyp_id );

  if ( hdf_grp  >= 0 ) {
    /* The hdf_loc identifier could be a group or a dataset.
    */
    if ( H5Iget_type ( hdf_grp ) == H5I_GROUP ) {
      H5Gclose ( hdf_grp );
    } else {
      H5Dclose ( hdf_grp );
    }
  }

  return status;
}

/** Set the values of an attribute.
 */
int miset_attr_values ( mihandle_t vol, mitype_t data_type, const char *path,
                    const char *name, size_t length, const void *values )
{
  hid_t hdf_file=-1;
  hid_t hdf_grp=-1;
  int result;
  char fullpath[256];
  hid_t tmp_id;
  char *std_name;
  char *pch;
  size_t i, slength;
  int status = MI_ERROR;      /* Guilty until proven innocent */

  /* Get a handle to the actual HDF file
   */
  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"HDF file is not open");
  }

  full_path_for_attr(fullpath, sizeof(fullpath), path, name, vol);

  /* find last occurance of '/' */
  pch = strrchr ( path, '/' );

  if ( pch != NULL ) {
    slength = strlen ( path ) - ( pch - path );
    std_name = malloc ( slength + 1 );

    for ( i = 0; i < slength; i++ )
      std_name[i] = path[pch - path + 1 + i];

    std_name[slength] = '\0';
  } else {
    std_name = malloc ( strlen ( path ) + 1 );
    strcpy ( std_name, path );
  }

  /* might need to create standard dataset first*/

  if ( !strcmp ( std_name, "acquisition" ) ||
       !strcmp ( std_name, "patient" ) ||
       !strcmp ( std_name, "study" ) ) {
    H5E_BEGIN_TRY {
      tmp_id = H5Dopen1 ( hdf_file, fullpath );

      if ( tmp_id < 0 ) {
        create_standard_dataset ( hdf_file, std_name );
      } else
      {
        H5Dclose ( tmp_id );
      }
    } H5E_END_TRY;
  } else {
    H5E_BEGIN_TRY {
      tmp_id = H5Dopen1 ( hdf_file, fullpath );

      if ( tmp_id < 0 ) {
        create_dataset ( hdf_file, std_name );
      } else
      {
        H5Dclose ( tmp_id );
      }
    } H5E_END_TRY;

  }

  free ( std_name );

  /* Search through the path, descending into each group encountered.
   */
  hdf_grp = midescend_path ( hdf_file, fullpath );

  if ( hdf_grp < 0 ) {
    goto cleanup;
  }

  result = miset_attr_at_loc ( hdf_grp, name, data_type, length, values );

  if ( result < 0 ) {
    goto cleanup;
  }
  
  status=MI_NOERROR;

cleanup:
  
  /* added the following instead H5Gclose(hdf_grp) */
  if ( hdf_grp >= 0 ) {
    /* The hdf_loc identifier could be a group or a dataset.
    */
    if ( H5Iget_type ( hdf_grp ) == H5I_GROUP ) {
      H5Gclose ( hdf_grp );
    } else {
      H5Dclose ( hdf_grp );
    }
  }
  return status;
}

/** Add global history attribute
 */
int miadd_history_attr ( mihandle_t vol, size_t length, const void *values )
{
  int result;
  hid_t hdf_file;
  hid_t hdf_grp;
  hid_t tmp_id;
  /* Get a handle to the actual HDF file
   */

  hdf_file = vol->hdf_id;

  if ( hdf_file < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"HDF file is not open");
  }

  hdf_grp = midescend_path ( hdf_file, MI_ROOT_PATH );

  if ( hdf_grp < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"midescend_path fail");
  }

  result = miset_attr_at_loc ( hdf_grp, "history", MI_TYPE_STRING, length, values );

  if ( result < 0 ) {
    return ( MI_ERROR );
  }

  /* added the following instead H5Gclose(hdf_grp) */
  H5E_BEGIN_TRY {
    tmp_id = H5Gclose ( hdf_grp );

    if ( tmp_id < 0 ) {
      tmp_id = H5Dclose ( hdf_grp );
    }
  } H5E_END_TRY;

  return ( MI_NOERROR );

}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
