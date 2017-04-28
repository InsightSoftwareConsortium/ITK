#if HAVE_CONFIG_H
#include "config.h"
#endif

#if MINC2                       /* Ignore if not MINC2 */
/* minc_compat.c
 * 
 * this code exists to provide a dispatch layer between the MI2* low-level
 * calls and their respective HDF5 and NetCDF implementations.
 *
 * Since each of these calls uses exactly one file descriptor, the logic is 
 * simple: we just apply the correct operation based on a quick determination
 * of whether this is an HDF5 handle or a NetCDF file descriptor.
 */
#define _MI2_FORCE_NETCDF_
#include "minc_private.h"
#include "hdf_convenience.h"

/* */
MNCAPI int
MI2varname(int fd, int varid, char *varnm)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varname(fd, varid, varnm));
    }
    else {
        return (nc_inq_varname(fd, varid, varnm));
    }
}

/* */
MNCAPI int
MI2varid(int fd, const char *varnm)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varid(fd, varnm));
    }
    else {
        return (ncvarid(fd, varnm));
    }
}

/* */
MNCAPI int
MI2attinq(int fd, int varid, const char *attnm, nc_type *type_ptr,
          int *length_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_attinq(fd, varid, attnm, type_ptr, length_ptr));
    }
    else {
        int status;
        int oldncopts =get_ncopts();
        set_ncopts(0);
        status = ncattinq(fd, varid, attnm, type_ptr, length_ptr);

        set_ncopts(oldncopts);
        if (status != 1 && oldncopts != 0) {
            fprintf(stderr,
                    _("ncattinq: ncid %d: varid: %d: Attribute '%s' not found"),
                    fd, varid, attnm);
	      
        }
        return (status);
    }
}

MNCAPI int
MI2attname(int fd, int varid, int attid, char *name)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_attname(fd, varid, attid, name));
    }
    else {
        return (ncattname(fd, varid, attid, name));
    }
}

/* */
MNCAPI int
MI2inquire(int fd, int *ndims_ptr, int *nvars_ptr, int *natts_ptr,
           int *unlimdim_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_inquire(fd, ndims_ptr, nvars_ptr, natts_ptr, unlimdim_ptr));
    }
    else {
        return (ncinquire(fd, ndims_ptr, nvars_ptr, natts_ptr, unlimdim_ptr));
    }
}

/* */
MNCAPI int
MI2varinq(int fd, int varid, char *varnm_ptr, nc_type *type_ptr,
          int *ndims_ptr, int *dims_ptr, int *natts_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varinq(fd, varid, varnm_ptr, type_ptr, ndims_ptr, 
                           dims_ptr, natts_ptr));
    }
    else {
        return (ncvarinq(fd, varid, varnm_ptr, type_ptr, ndims_ptr, 
                         dims_ptr, natts_ptr));
    }
}

/* */
MNCAPI int
MI2dimid(int fd, const char *dimnm)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_dimid(fd, dimnm));
    }
    else {
        return (ncdimid(fd, dimnm));
    }
}

/* */
MNCAPI int
MI2diminq(int fd, int dimid, char *dimnm_ptr, long *len_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_diminq(fd, dimid, dimnm_ptr, len_ptr));
    }
    else {
        return (ncdiminq(fd, dimid, dimnm_ptr, len_ptr));
    }
}

/* */
MNCAPI int
MI2dimdef(int fd, const char *dimnm, long length)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_dimdef(fd, dimnm, length));
    }
    else {
        return (ncdimdef(fd, dimnm, length));
    }
}

/* */
MNCAPI int
MI2attget(int fd, int varid, const char *attnm, void *value)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_attget(fd, varid, attnm, value));
    }
    else {
        return (ncattget(fd, varid, attnm, value));
    }
}

/* */
MNCAPI int
MI2attput(int fd, int varid, const char *attnm, nc_type val_typ, 
          int val_len, const void *val_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_attput(fd, varid, attnm, val_typ, val_len, val_ptr));
    }
    else {
        int old_ncopts =get_ncopts();
        int result;
        set_ncopts(0);
        result = ncattput(fd, varid, attnm, val_typ, val_len, val_ptr);
        set_ncopts(old_ncopts);
        return (result);
    }
}

/* */
MNCAPI int
MI2endef(int fd)
{
    if (MI2_ISH5OBJ(fd)) {
        return (MI_NOERROR);    /* Just a stub, HDF5 doesn't do this! */
    }
    else {
        return (ncendef(fd));
    }
}  

/* */
MNCAPI int
MI2vardef(int fd, const char *varnm, nc_type vartype, int ndims,
          const int *dimids)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_vardef(fd, varnm, vartype, ndims, dimids));
    }
    else {
        return (ncvardef(fd, varnm, vartype, ndims, dimids));
    }
}

/* */
MNCAPI int
MI2varget(int fd, int varid, const long *start_ptr, 
          const long *count_ptr, void *val_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varget(fd, varid, start_ptr, count_ptr, val_ptr));
    }
    else {
        return (ncvarget(fd, varid, start_ptr, count_ptr, val_ptr));
    }
}

/* */
MNCAPI int
MI2varput(int fd, int varid, const long *start_ptr,
          const long *count_ptr, const void *val_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varput(fd, varid, start_ptr, count_ptr, val_ptr));
    }
    else {
        return (ncvarput(fd, varid, start_ptr, count_ptr, val_ptr));
    }
}

/* */
MNCAPI int
MI2varput1(int fd, int varid, const long *mindex_ptr,
           const void *val_ptr)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varput1(fd, varid, mindex_ptr, val_ptr));
    }
    else {
        return (ncvarput1(fd, varid, mindex_ptr, val_ptr));
    }
}

MNCAPI int
MI2attdel(int fd, int varid, const char *attnm)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_attdel(fd, varid, attnm));
    }
    else {
        return (ncattdel(fd, varid, attnm));
    }
}


/* */
MNCAPI int
MI2dimrename(int fd, int dimid, const char *new_name)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_dimrename(fd, dimid, new_name));
    }
    else {
        return (ncdimrename(fd, dimid, new_name));
    }
}

MNCAPI int
MI2varputg(int fd, int varid, const long *startp, const long *countp,
           const long *stridep, const long *imapp, const void *valp)
{
    if (MI2_ISH5OBJ(fd)) {
        return (hdf_varputg(fd, varid, startp, countp, stridep, imapp, valp));
    }
    else {
        return (ncvarputg(fd, varid, startp, countp, stridep, imapp, valp));
    }
}

MNCAPI int
MI2attcopy(int infd, int invarid, const char *name, int outfd, 
           int outvarid)
{
    if (!MI2_ISH5OBJ(infd) && !MI2_ISH5OBJ(outfd)) {
        /* Trivial case. */
        return (ncattcopy(infd, invarid, name, outfd, outvarid));
    }
    else {
        /* Complex case.  Using our own compatibility layer functions lets us
         * handle all three other possible combinations of infd & outfd types.
         */
        nc_type att_type;
        int att_length;
        void *val_ptr;
        int status;

        status = MI2attinq(infd, invarid, name, &att_type, &att_length);
        if (status == MI_ERROR) {
            return (MI_ERROR);
        }

        /* Special case for att_type == NC_CHAR && att_length == 0 
         */
        if (att_type == NC_CHAR && att_length == 0) {
            val_ptr = malloc(1);
            if (val_ptr == NULL) {
                return (MI_ERROR);
            }
            *(char *)val_ptr = '\0';
            att_length = 1;
            status = MI_NOERROR;
        }
        else {
            val_ptr = malloc(MI2typelen(att_type) * att_length);
            if (val_ptr == NULL) {
                return (MI_ERROR);
            }

            status = MI2attget(infd, invarid, name, val_ptr);
        }

        if (status != MI_ERROR) {
            status = MI2attput(outfd, outvarid, name, att_type, att_length,
                               val_ptr);
        }
    
        free(val_ptr);
        return (status);
    }
}

MNCAPI int
MI2typelen(int type_id)
{
    switch (type_id) {
    case NC_BYTE:
    case NC_CHAR:
        return (1);
    case NC_SHORT:
        return (2);
    case NC_INT:
    case NC_FLOAT:
        return (4);
    case NC_DOUBLE:
        return (8);
    default:
        break;
    }
    fprintf(stderr, _("Unknown type %d"), type_id);
    return (-1);
}

MNCAPI int
MI2redef(int fd)
{
    if (MI2_ISH5OBJ(fd)) {
        /* Do nothing, since there is no equivalent in HDF5. */
        return (MI_NOERROR);
    }
    else {
        return (ncredef(fd));
    }
}

MNCAPI int
MI2sync(int fd)
{
    if (MI2_ISH5OBJ(fd) ) {
        /* Commit the (entire) file to disk. */
        return hdf_flush(fd);
    }
    else {
        return (ncsync(fd));
    }
}

MNCAPI int
MI2setfill(int fd, int fillmode)
{
    if (MI2_ISH5OBJ(fd)) {
        /* TODO: ??? */
        return (MI_NOERROR);
    }
    else {
        return (ncsetfill(fd, fillmode));
    }
}

#endif /* MINC2 defined */
