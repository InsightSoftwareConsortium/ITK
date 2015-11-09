/* ----------------------------------------------------------------------------
@COPYRIGHT  :
              Copyright 1993,1994,1995 David MacDonald,
              McConnell Brain Imaging Centre,
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/


#include  <internal_volume_io.h>

VIOAPI  void  convert_values_to_voxels(
    VIO_Volume   volume,
    int      n_voxels,
    VIO_Real     values[],
    VIO_Real     voxels[] )
{
    int    v;
    VIO_Real   scale, trans;

    if( !volume->real_range_set )
    {
        if( voxels != values )
        {
            for_less( v, 0, n_voxels )
                voxels[v] = values[v];
        }
        return;
    }

    scale = volume->real_value_scale;
    trans = volume->real_value_translation;

    for_less( v, 0, n_voxels )
        voxels[v] = (values[v] - trans) / scale;
}

VIOAPI  void  set_volume_value_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] )
{
    switch( get_volume_n_dimensions(volume) )
    {
    case 1:
        set_volume_value_hyperslab_1d( volume, v0, n0, values );
        break;
    case 2:
        set_volume_value_hyperslab_2d( volume, v0, v1, n0, n1, values );
        break;
    case 3:
        set_volume_value_hyperslab_3d( volume, v0, v1, v2, n0, n1, n2, values );
        break;
    case 4:
        set_volume_value_hyperslab_4d( volume, v0, v1, v2, v3,
                                       n0, n1, n2, n3, values );
        break;
    case 5:
        set_volume_value_hyperslab_5d( volume, v0, v1, v2, v3, v4,
                                       n0, n1, n2, n3, n4, values );
        break;
    }
}

VIOAPI  void  set_volume_value_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] )
{
    int  size;
    VIO_Real *voxels;

    size = n0 * n1 * n2 * n3 * n4;
    ALLOC( voxels, size );

    convert_values_to_voxels( volume, size, values, voxels );

    set_volume_voxel_hyperslab_5d( volume, v0, v1, v2, v3, v4,
                                   n0, n1, n2, n3, n4, voxels );

    FREE( voxels );
}

VIOAPI  void  set_volume_value_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] )
{
    int  size;
    VIO_Real *voxels;

    size = n0 * n1 * n2 * n3;
    ALLOC( voxels, size );

    convert_values_to_voxels( volume, size, values, voxels );

    set_volume_voxel_hyperslab_4d( volume, v0, v1, v2, v3,
                                   n0, n1, n2, n3, voxels );

    FREE( voxels );
}

VIOAPI  void  set_volume_value_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] )
{
    int  size;
    VIO_Real *voxels;

    size = n0 * n1 * n2;
    ALLOC( voxels, size );

    convert_values_to_voxels( volume, size, values, voxels );

    set_volume_voxel_hyperslab_3d( volume, v0, v1, v2, n0, n1, n2, voxels );

    FREE( voxels );
}

VIOAPI  void  set_volume_value_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] )
{
    int  size;
    VIO_Real *voxels;

    size = n0 * n1;
    ALLOC( voxels, size );

    convert_values_to_voxels( volume, size, values, voxels );

    set_volume_voxel_hyperslab_2d( volume, v0, v1, n0, n1, voxels );

    FREE( voxels );
}

VIOAPI  void  set_volume_value_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] )
{
    VIO_Real *voxels;

    ALLOC( voxels, n0 );

    convert_values_to_voxels( volume, n0, values, voxels );

    set_volume_voxel_hyperslab_1d( volume, v0, n0, voxels );

    FREE( voxels );
}

static  void  slow_set_volume_voxel_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] )
{
    int    i0, i1, i2, i3, i4, n_dims;

    n_dims = get_volume_n_dimensions( volume );

    if( n_dims < 5 )
        n4 = 1;
    if( n_dims < 4 )
        n3 = 1;
    if( n_dims < 3 )
        n2 = 1;
    if( n_dims < 2 )
        n1 = 1;
    if( n_dims < 1 )
        n0 = 1;

    for_less( i0, 0, n0 )
    for_less( i1, 0, n1 )
    for_less( i2, 0, n2 )
    for_less( i3, 0, n3 )
    for_less( i4, 0, n4 )
    {
        set_volume_voxel_value( volume, v0 + i0, v1 + i1, v2 + i2,
                                v3 + i3, v4 + i4, *values );
        ++values;
    }
}

static  void  set_voxel_values_5d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] )
{
    int              step0, step1, step2, step3, step4;
    int              i0, i1, i2, i3, i4;
    int              n0, n1, n2, n3, n4;
    unsigned  char   *VIO_UCHAR_ptr;
    signed  char     *signed_byte_ptr;
    unsigned  short  *unsigned_short_ptr;
    signed  short    *signed_short_ptr;
    unsigned  int    *unsigned_int_ptr;
    signed  int     *signed_int_ptr;
    float            *float_ptr;
    double           *double_ptr;

    n0 = counts[0];
    n1 = counts[1];
    n2 = counts[2];
    n3 = counts[3];
    n4 = counts[4];
    step0 = steps[0];
    step1 = steps[1];
    step2 = steps[2];
    step3 = steps[3];
    step4 = steps[4];
    step0 -= n1 * step1;
    step1 -= n2 * step2;
    step2 -= n3 * step3;
    step3 -= n4 * step4;

    switch( data_type )
    {
    case VIO_UNSIGNED_BYTE:
        ASSIGN_PTR(VIO_UCHAR_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *VIO_UCHAR_ptr = (unsigned char)
                                               floor( *values + 0.5 );
                            ++values;
                            VIO_UCHAR_ptr += step4;
                        }
                        VIO_UCHAR_ptr += step3;
                    }
                    VIO_UCHAR_ptr += step2;
                }
                VIO_UCHAR_ptr += step1;
            }
            VIO_UCHAR_ptr += step0;
        }
        break;

    case VIO_SIGNED_BYTE:
        ASSIGN_PTR(signed_byte_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *signed_byte_ptr = (signed char)
                                                   floor( *values + 0.5 );
                            ++values;
                            signed_byte_ptr += step4;
                        }
                        signed_byte_ptr += step3;
                    }
                    signed_byte_ptr += step2;
                }
                signed_byte_ptr += step1;
            }
            signed_byte_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_SHORT:
        ASSIGN_PTR(unsigned_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *unsigned_short_ptr = (unsigned short)
                                                   floor( *values + 0.5 );
                            ++values;
                            unsigned_short_ptr += step4;
                        }
                        unsigned_short_ptr += step3;
                    }
                    unsigned_short_ptr += step2;
                }
                unsigned_short_ptr += step1;
            }
            unsigned_short_ptr += step0;
        }
        break;

    case VIO_SIGNED_SHORT:
        ASSIGN_PTR(signed_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *signed_short_ptr = (signed short)
                                                   floor( *values + 0.5 );
                            ++values;
                            signed_short_ptr += step4;
                        }
                        signed_short_ptr += step3;
                    }
                    signed_short_ptr += step2;
                }
                signed_short_ptr += step1;
            }
            signed_short_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_INT:
        ASSIGN_PTR(unsigned_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *unsigned_int_ptr = (unsigned int)
                                                   floor( *values + 0.5 );
                            ++values;
                            unsigned_int_ptr += step4;
                        }
                        unsigned_int_ptr += step3;
                    }
                    unsigned_int_ptr += step2;
                }
                unsigned_int_ptr += step1;
            }
            unsigned_int_ptr += step0;
        }
        break;

    case VIO_SIGNED_INT:
        ASSIGN_PTR(signed_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *signed_int_ptr = (signed int)
                                                   floor( *values + 0.5 );
                            ++values;
                            signed_int_ptr += step4;
                        }
                        signed_int_ptr += step3;
                    }
                    signed_int_ptr += step2;
                }
                signed_int_ptr += step1;
            }
            signed_int_ptr += step0;
        }
        break;

    case VIO_FLOAT:
        ASSIGN_PTR(float_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *float_ptr = (float) *values;
                            ++values;
                            float_ptr += step4;
                        }
                        float_ptr += step3;
                    }
                    float_ptr += step2;
                }
                float_ptr += step1;
            }
            float_ptr += step0;
        }
        break;
    default:
    case VIO_DOUBLE:
        ASSIGN_PTR(double_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        for_less( i4, 0, n4 )
                        {
                            *double_ptr = (double) *values;
                            ++values;
                            double_ptr += step4;
                        }
                        double_ptr += step3;
                    }
                    double_ptr += step2;
                }
                double_ptr += step1;
            }
            double_ptr += step0;
        }
        break;
    }
}

static  void  set_voxel_values_4d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] )
{
    int              step0, step1, step2, step3;
    int              i0, i1, i2, i3;
    int              n0, n1, n2, n3;
    unsigned  char   *VIO_UCHAR_ptr;
    signed  char     *signed_byte_ptr;
    unsigned  short  *unsigned_short_ptr;
    signed  short    *signed_short_ptr;
    unsigned  int    *unsigned_int_ptr;
    signed  int      *signed_int_ptr;
    float            *float_ptr;
    double           *double_ptr;

    n0 = counts[0];
    n1 = counts[1];
    n2 = counts[2];
    n3 = counts[3];
    step0 = steps[0];
    step1 = steps[1];
    step2 = steps[2];
    step3 = steps[3];
    step0 -= n1 * step1;
    step1 -= n2 * step2;
    step2 -= n3 * step3;

    switch( data_type )
    {
    case VIO_UNSIGNED_BYTE:
        ASSIGN_PTR(VIO_UCHAR_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *VIO_UCHAR_ptr = (unsigned char)
                                           floor( *values + 0.5 );
                        ++values;
                        VIO_UCHAR_ptr += step3;
                    }
                    VIO_UCHAR_ptr += step2;
                }
                VIO_UCHAR_ptr += step1;
            }
            VIO_UCHAR_ptr += step0;
        }
        break;

    case VIO_SIGNED_BYTE:
        ASSIGN_PTR(signed_byte_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *signed_byte_ptr = (signed char) floor( *values + 0.5 );
                        ++values;
                        signed_byte_ptr += step3;
                    }
                    signed_byte_ptr += step2;
                }
                signed_byte_ptr += step1;
            }
            signed_byte_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_SHORT:
        ASSIGN_PTR(unsigned_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *unsigned_short_ptr = (unsigned short)
                                               floor( *values + 0.5 );
                        ++values;
                        unsigned_short_ptr += step3;
                    }
                    unsigned_short_ptr += step2;
                }
                unsigned_short_ptr += step1;
            }
            unsigned_short_ptr += step0;
        }
        break;

    case VIO_SIGNED_SHORT:
        ASSIGN_PTR(signed_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *signed_short_ptr = (signed short)
                                               floor( *values + 0.5 );
                        ++values;
                        signed_short_ptr += step3;
                    }
                    signed_short_ptr += step2;
                }
                signed_short_ptr += step1;
            }
            signed_short_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_INT:
        ASSIGN_PTR(unsigned_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *unsigned_int_ptr = (unsigned int)
                                               floor( *values + 0.5 );
                        ++values;
                        unsigned_int_ptr += step3;
                    }
                    unsigned_int_ptr += step2;
                }
                unsigned_int_ptr += step1;
            }
            unsigned_int_ptr += step0;
        }
        break;

    case VIO_SIGNED_INT:
        ASSIGN_PTR(signed_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *signed_int_ptr = (signed int) floor( *values + 0.5 );
                        ++values;
                        signed_int_ptr += step3;
                    }
                    signed_int_ptr += step2;
                }
                signed_int_ptr += step1;
            }
            signed_int_ptr += step0;
        }
        break;

    case VIO_FLOAT:
        ASSIGN_PTR(float_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *float_ptr = (float) *values;
                        ++values;
                        float_ptr += step3;
                    }
                    float_ptr += step2;
                }
                float_ptr += step1;
            }
            float_ptr += step0;
        }
        break;
    default:
    case VIO_DOUBLE:
        ASSIGN_PTR(double_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    for_less( i3, 0, n3 )
                    {
                        *double_ptr = (double) *values;
                        ++values;
                        double_ptr += step3;
                    }
                    double_ptr += step2;
                }
                double_ptr += step1;
            }
            double_ptr += step0;
        }
        break;
    }
}

static  void  set_voxel_values_3d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] )
{
    int              step0, step1, step2;
    int              i0, i1, i2;
    int              n0, n1, n2;
    unsigned  char   *VIO_UCHAR_ptr;
    signed  char     *signed_byte_ptr;
    unsigned  short  *unsigned_short_ptr;
    signed  short    *signed_short_ptr;
    unsigned  int    *unsigned_int_ptr;
    signed  int      *signed_int_ptr;
    float            *float_ptr;
    double           *double_ptr;

    n0 = counts[0];
    n1 = counts[1];
    n2 = counts[2];
    step0 = steps[0];
    step1 = steps[1];
    step2 = steps[2];
    step0 -= n1 * step1;
    step1 -= n2 * step2;

    switch( data_type )
    {
    case VIO_UNSIGNED_BYTE:
        ASSIGN_PTR(VIO_UCHAR_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *VIO_UCHAR_ptr = (unsigned char) floor( *values + 0.5 );
                    ++values;
                    VIO_UCHAR_ptr += step2;
                }
                VIO_UCHAR_ptr += step1;
            }
            VIO_UCHAR_ptr += step0;
        }
        break;

    case VIO_SIGNED_BYTE:
        ASSIGN_PTR(signed_byte_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *signed_byte_ptr = (signed char) floor( *values + 0.5 );
                    ++values;
                    signed_byte_ptr += step2;
                }
                signed_byte_ptr += step1;
            }
            signed_byte_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_SHORT:
        ASSIGN_PTR(unsigned_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *unsigned_short_ptr = (unsigned short)
                                           floor( *values + 0.5 );
                    ++values;
                    unsigned_short_ptr += step2;
                }
                unsigned_short_ptr += step1;
            }
            unsigned_short_ptr += step0;
        }
        break;

    case VIO_SIGNED_SHORT:
        ASSIGN_PTR(signed_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *signed_short_ptr = (signed short) floor( *values + 0.5 );
                    ++values;
                    signed_short_ptr += step2;
                }
                signed_short_ptr += step1;
            }
            signed_short_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_INT:
        ASSIGN_PTR(unsigned_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *unsigned_int_ptr = (unsigned int) floor( *values + 0.5 );
                    ++values;
                    unsigned_int_ptr += step2;
                }
                unsigned_int_ptr += step1;
            }
            unsigned_int_ptr += step0;
        }
        break;

    case VIO_SIGNED_INT:
        ASSIGN_PTR(signed_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *signed_int_ptr = (signed int) floor( *values + 0.5 );
                    ++values;
                    signed_int_ptr += step2;
                }
                signed_int_ptr += step1;
            }
            signed_int_ptr += step0;
        }
        break;

    case VIO_FLOAT:
        ASSIGN_PTR(float_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *float_ptr = (float) *values;
                    ++values;
                    float_ptr += step2;
                }
                float_ptr += step1;
            }
            float_ptr += step0;
        }
        break;
    default:
    case VIO_DOUBLE:
        ASSIGN_PTR(double_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                for_less( i2, 0, n2 )
                {
                    *double_ptr = (double) *values;
                    ++values;
                    double_ptr += step2;
                }
                double_ptr += step1;
            }
            double_ptr += step0;
        }
        break;
    }
}

static  void  set_voxel_values_2d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] )
{
    int              step0, step1;
    int              i0, i1;
    int              n0, n1;
    unsigned  char   *VIO_UCHAR_ptr;
    signed  char     *signed_byte_ptr;
    unsigned  short  *unsigned_short_ptr;
    signed  short    *signed_short_ptr;
    unsigned  int    *unsigned_int_ptr;
    signed  int      *signed_int_ptr;
    float            *float_ptr;
    double           *double_ptr;

    n0 = counts[0];
    n1 = counts[1];
    step0 = steps[0];
    step1 = steps[1];
    step0 -= n1 * step1;

    switch( data_type )
    {
    case VIO_UNSIGNED_BYTE:
        ASSIGN_PTR(VIO_UCHAR_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *VIO_UCHAR_ptr = (unsigned char) floor( *values + 0.5 );
                ++values;
                VIO_UCHAR_ptr += step1;
            }
            VIO_UCHAR_ptr += step0;
        }
        break;

    case VIO_SIGNED_BYTE:
        ASSIGN_PTR(signed_byte_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *signed_byte_ptr = (signed char) floor( *values + 0.5 );
                ++values;
                signed_byte_ptr += step1;
            }
            signed_byte_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_SHORT:
        ASSIGN_PTR(unsigned_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *unsigned_short_ptr = (unsigned short) floor( *values + 0.5 );
                ++values;
                unsigned_short_ptr += step1;
            }
            unsigned_short_ptr += step0;
        }
        break;

    case VIO_SIGNED_SHORT:
        ASSIGN_PTR(signed_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *signed_short_ptr = (signed short) floor( *values + 0.5 );
                ++values;
                signed_short_ptr += step1;
            }
            signed_short_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_INT:
        ASSIGN_PTR(unsigned_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *unsigned_int_ptr = (unsigned int) floor( *values + 0.5 );
                ++values;
                unsigned_int_ptr += step1;
            }
            unsigned_int_ptr += step0;
        }
        break;

    case VIO_SIGNED_INT:
        ASSIGN_PTR(signed_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *signed_int_ptr = (signed int) floor( *values + 0.5 );
                ++values;
                signed_int_ptr += step1;
            }
            signed_int_ptr += step0;
        }
        break;

    case VIO_FLOAT:
        ASSIGN_PTR(float_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *float_ptr = (float) *values;
                ++values;
                float_ptr += step1;
            }
            float_ptr += step0;
        }
        break;

    default:
    case VIO_DOUBLE:
        ASSIGN_PTR(double_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            for_less( i1, 0, n1 )
            {
                *double_ptr = (double) *values;
                ++values;
                double_ptr += step1;
            }
            double_ptr += step0;
        }
        break;
    }
}

static  void  set_voxel_values_1d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         step0,
    int         n0,
    VIO_Real        values[] )
{
    int              i0;
    unsigned  char   *VIO_UCHAR_ptr;
    signed  char     *signed_byte_ptr;
    unsigned  short  *unsigned_short_ptr;
    signed  short    *signed_short_ptr;
    unsigned  int    *unsigned_int_ptr;
    signed  int      *signed_int_ptr;
    float            *float_ptr;
    double           *double_ptr;

    switch( data_type )
    {
    case VIO_UNSIGNED_BYTE:
        ASSIGN_PTR(VIO_UCHAR_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *VIO_UCHAR_ptr = (unsigned char) floor( *values + 0.5 );
            ++values;
            VIO_UCHAR_ptr += step0;
        }
        break;

    case VIO_SIGNED_BYTE:
        ASSIGN_PTR(signed_byte_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *signed_byte_ptr = (signed char) floor( *values + 0.5 );
            ++values;
            signed_byte_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_SHORT:
        ASSIGN_PTR(unsigned_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *unsigned_short_ptr = (unsigned short) floor( *values + 0.5 );
            ++values;
            unsigned_short_ptr += step0;
        }
        break;

    case VIO_SIGNED_SHORT:
        ASSIGN_PTR(signed_short_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *signed_short_ptr = (signed short) floor( *values + 0.5 );
            ++values;
            signed_short_ptr += step0;
        }
        break;

    case VIO_UNSIGNED_INT:
        ASSIGN_PTR(unsigned_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *unsigned_int_ptr = (unsigned int) floor( *values + 0.5 );
            ++values;
            unsigned_int_ptr += step0;
        }
        break;

    case VIO_SIGNED_INT:
        ASSIGN_PTR(signed_int_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *signed_int_ptr = (signed int) floor( *values + 0.5 );
            ++values;
            signed_int_ptr += step0;
        }
        break;

    case VIO_FLOAT:
        ASSIGN_PTR(float_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *float_ptr = (float) *values;
            ++values;
            float_ptr += step0;
        }
        break;

    default:
    case VIO_DOUBLE:
        ASSIGN_PTR(double_ptr) = void_ptr;
        for_less( i0, 0, n0 )
        {
            *double_ptr = (double) *values;
            ++values;
            double_ptr += step0;
        }
        break;
    }
}

static  void  set_voxel_values(
    VIO_Volume   volume,
    void     *void_ptr,
    int      n_dims,
    int      steps[],
    int      counts[],
    VIO_Real     values[] )
{
    VIO_Data_types  data_type;

    data_type = get_volume_data_type( volume );
    switch( n_dims )
    {
    case 0:
        set_voxel_values_1d( data_type, void_ptr, 1, 1, values );
        break;
    case 1:
        set_voxel_values_1d( data_type, void_ptr, steps[0], counts[0], values );
        break;
    case 2:
        set_voxel_values_2d( data_type, void_ptr, steps, counts, values );
        break;
    case 3:
        set_voxel_values_3d( data_type, void_ptr, steps, counts, values );
        break;
    case 4:
        set_voxel_values_4d( data_type, void_ptr, steps, counts, values );
        break;
    case 5:
        set_voxel_values_5d( data_type, void_ptr, steps, counts, values );
        break;
    }
}

VIOAPI  void  set_volume_voxel_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] )
{
    int         steps[VIO_MAX_DIMENSIONS];
    int         counts[VIO_MAX_DIMENSIONS];
    int         sizes[VIO_MAX_DIMENSIONS];
    int         dim, stride;
    void        *void_ptr;

    if( volume->is_cached_volume )
    {
        slow_set_volume_voxel_hyperslab( volume, v0, v1, v2, v3, v4,
                                         n0, n1, n2, n3, n4, values );
        return;
    }

    get_volume_sizes( volume, sizes );

    GET_MULTIDIM_PTR_5D( void_ptr, volume->array, v0, v1, v2, v3, v4 )

    stride = 1;
    dim = 5;

    if( n4 > 1 )
    {
        --dim;
        counts[dim] = n4;
        steps[dim] = stride;
    }
    stride *= sizes[4];

    if( n3 > 1 )
    {
        --dim;
        counts[dim] = n3;
        steps[dim] = stride;
    }
    stride *= sizes[3];

    if( n2 > 1 )
    {
        --dim;
        counts[dim] = n2;
        steps[dim] = stride;
    }
    stride *= sizes[2];

    if( n1 > 1 )
    {
        --dim;
        counts[dim] = n1;
        steps[dim] = stride;
    }
    stride *= sizes[1];

    if( n0 > 1 )
    {
        --dim;
        counts[dim] = n0;
        steps[dim] = stride;
    }

    set_voxel_values( volume, void_ptr, 5 - dim, &steps[dim], &counts[dim],
                      values );
}

VIOAPI  void  set_volume_voxel_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] )
{
    int         steps[VIO_MAX_DIMENSIONS];
    int         counts[VIO_MAX_DIMENSIONS];
    int         sizes[VIO_MAX_DIMENSIONS];
    int         dim, stride;
    void        *void_ptr;

    if( volume->is_cached_volume )
    {
        slow_set_volume_voxel_hyperslab( volume, v0, v1, v2, v3, 0,
                                         n0, n1, n2, n3, 0, values );
        return;
    }

    get_volume_sizes( volume, sizes );

    GET_MULTIDIM_PTR_4D( void_ptr, volume->array, v0, v1, v2, v3 )

    stride = 1;
    dim = 4;

    if( n3 > 1 )
    {
        --dim;
        counts[dim] = n3;
        steps[dim] = stride;
    }
    stride *= sizes[3];

    if( n2 > 1 )
    {
        --dim;
        counts[dim] = n2;
        steps[dim] = stride;
    }
    stride *= sizes[2];

    if( n1 > 1 )
    {
        --dim;
        counts[dim] = n1;
        steps[dim] = stride;
    }
    stride *= sizes[1];

    if( n0 > 1 )
    {
        --dim;
        counts[dim] = n0;
        steps[dim] = stride;
    }

    set_voxel_values( volume, void_ptr, 4 - dim, &steps[dim], &counts[dim],
                      values );
}

VIOAPI  void  set_volume_voxel_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] )
{
    int         steps[VIO_MAX_DIMENSIONS];
    int         counts[VIO_MAX_DIMENSIONS];
    int         sizes[VIO_MAX_DIMENSIONS];
    int         dim, stride;
    void        *void_ptr;

    if( volume->is_cached_volume )
    {
        slow_set_volume_voxel_hyperslab( volume, v0, v1, v2, 0, 0,
                                         n0, n1, n2, 0, 0, values );
        return;
    }

    get_volume_sizes( volume, sizes );

    GET_MULTIDIM_PTR_3D( void_ptr, volume->array, v0, v1, v2 )

    stride = 1;
    dim = 3;

    if( n2 > 1 )
    {
        --dim;
        counts[dim] = n2;
        steps[dim] = stride;
    }
    stride *= sizes[2];

    if( n1 > 1 )
    {
        --dim;
        counts[dim] = n1;
        steps[dim] = stride;
    }
    stride *= sizes[1];

    if( n0 > 1 )
    {
        --dim;
        counts[dim] = n0;
        steps[dim] = stride;
    }

    set_voxel_values( volume, void_ptr, 3 - dim, &steps[dim], &counts[dim],
                      values );
}

VIOAPI  void  set_volume_voxel_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] )
{
    int         steps[VIO_MAX_DIMENSIONS];
    int         counts[VIO_MAX_DIMENSIONS];
    int         sizes[VIO_MAX_DIMENSIONS];
    int         dim, stride;
    void        *void_ptr;

    if( volume->is_cached_volume )
    {
        slow_set_volume_voxel_hyperslab( volume, v0, v1, 0, 0, 0,
                                         n0, n1, 0, 0, 0, values );
        return;
    }

    get_volume_sizes( volume, sizes );

    GET_MULTIDIM_PTR_2D( void_ptr, volume->array, v0, v1 )

    stride = 1;
    dim = 2;

    if( n1 > 1 )
    {
        --dim;
        counts[dim] = n1;
        steps[dim] = stride;
    }
    stride *= sizes[1];

    if( n0 > 1 )
    {
        --dim;
        counts[dim] = n0;
        steps[dim] = stride;
    }

    set_voxel_values( volume, void_ptr, 2 - dim, &steps[dim], &counts[dim],
                      values );
}

VIOAPI  void  set_volume_voxel_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] )
{
    int         steps[VIO_MAX_DIMENSIONS];
    int         counts[VIO_MAX_DIMENSIONS];
    int         sizes[VIO_MAX_DIMENSIONS];
    int         dim;
    void        *void_ptr;

    if( volume->is_cached_volume )
    {
        slow_set_volume_voxel_hyperslab( volume, v0, 0, 0, 0, 0,
                                         n0, 0, 0, 0, 0, values );
        return;
    }

    get_volume_sizes( volume, sizes );

    GET_MULTIDIM_PTR_1D( void_ptr, volume->array, v0 )

    dim = 1;

    if( n0 > 1 )
    {
        --dim;
        counts[dim] = n0;
        steps[dim] = 1;
    }

    set_voxel_values( volume, void_ptr, 1 - dim, &steps[dim], &counts[dim],
                      values );
}

VIOAPI  void  set_volume_voxel_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     voxels[] )
{
    switch( get_volume_n_dimensions(volume) )
    {
    case 1:
        set_volume_voxel_hyperslab_1d( volume, v0, n0, voxels );
        break;
    case 2:
        set_volume_voxel_hyperslab_2d( volume, v0, v1, n0, n1, voxels );
        break;
    case 3:
        set_volume_voxel_hyperslab_3d( volume, v0, v1, v2, n0, n1, n2, voxels );
        break;
    case 4:
        set_volume_voxel_hyperslab_4d( volume, v0, v1, v2, v3,
                                       n0, n1, n2, n3, voxels );
        break;
    case 5:
        set_volume_voxel_hyperslab_5d( volume, v0, v1, v2, v3, v4,
                                       n0, n1, n2, n3, n4, voxels );
        break;
    }
}

