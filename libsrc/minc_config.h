#ifndef MINC_CONFIG_H
#define MINC_CONFIG_H

#define MICFG_FORCE_V2 "MINC_FORCE_V2"
#define MICFG_COMPRESS "MINC_COMPRESS"
#define MICFG_CHUNKING "MINC_CHUNKING"
#define MICFG_LOGFILE  "MINC_LOGFILE"
#define MICFG_LOGLEVEL "MINC_LOGLEVEL"
#define MICFG_MAXBUF   "MINC_MAX_FILE_BUFFER_KB"
#define MICFG_MAXMEM   "MINC_MAX_MEMORY_KB"

extern int miget_cfg_bool(const char *);
extern int miget_cfg_int(const char *);
extern char * miget_cfg_str(const char *);

#endif /* MINC_CONFIG_H */
