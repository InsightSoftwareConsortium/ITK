#ifndef __MINC_CONFIG_H__
#define __MINC_CONFIG_H__


enum MINC_CONFIG { 
  MICFG_FORCE_V2=0,
  MICFG_COMPRESS,
  MICFG_CHUNKING,
  MICFG_LOGFILE,
  MICFG_LOGLEVEL,
  MICFG_MAXBUF,
  MICFG_MAXMEM,
  MICFG_MINC_FILE_CACHE,
  MICFG_MINC_CHECKSUM,
  MICFG_COUNT
};

extern int          miget_cfg_present(int);
extern int          miget_cfg_bool(int);
extern int          miget_cfg_int(int);
extern const char * miget_cfg_str(int);
extern double miget_cfg_double(int);

#endif /* __MINC_CONFIG_H__ */
