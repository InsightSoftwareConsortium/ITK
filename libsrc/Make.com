$! --------------------------------------------------------------------------
$! For making minc.olb on vms
$! --------------------------------------------------------------------------
$!
$ ccc := cc /opt/nodebug/nolist/include=([],[--.netcdf.include])
$
$ on error then goto exit
$ on control_y then goto exit
$ set ver
$ ccc dim_conversion.c
$ ccc image_conversion.c
$ ccc minc_convenience.c
$ ccc minc_error.c
$ ccc minc_globdef.c
$ ccc netcdf_convenience.c
$ ccc value_conversion.c
$ ccc strdup.c
$
$ library/create minc.olb
$ library/replace minc dim_conversion, image_conversion, -
     minc_convenience, minc_error, minc_globdef, netcdf_convenience, -
     value_conversion, strdup
$exit:
$ set nover
