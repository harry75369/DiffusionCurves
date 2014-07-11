#ifndef __LOGGER_HPP__
#define __LOGGER_HPP__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Utilities
///
/// STR(X) turns X into a string literature
/// XSTR(X) turns X into string of expansion of macro X
#define STR(X) #X
#define XSTR(X) STR(X)

/// Private macro for assertions
///
/// Use NDEBUG to disable assertions.
#ifndef NDEBUG
#define _ASSERT_(x,log,msg,...) \
  do{if(!(x)){ \
    fprintf((log),"[FAIL] " __FILE__ ":%d " STR(x) "\n",__LINE__); \
    if(strcmp((msg),"")){fprintf((log),"[FAIL] " msg "\n",##__VA_ARGS__);} \
    fflush((log)); \
    exit(EXIT_FAILURE); \
  }}while(0)
#else
#define _ASSERT_(x,log,msg,...) do{(x);}while(0)
#endif

/// Private macro for messages
#define _MSG_(type,log,msg,...) \
  do{ \
    fprintf((log),"[" type "] " msg "\n",##__VA_ARGS__); \
    fflush((log)); \
  }while(0)

/// Private function for log redirection
//#define LOG_REDIRECT "log.txt"
static inline FILE * _get_log_file_() {
#ifdef LOG_REDIRECT
  static FILE * fp = fopen(LOG_REDIRECT,"a+");
  if (fp) return fp; else return stderr;
#else
  return stderr;
#endif
}

/// Public macros
#define ASSERT(x) do{_ASSERT_(x,_get_log_file_(),"");}while(0)
#define ASSERT_MSG(x,msg,...) do{_ASSERT_(x,_get_log_file_(),msg,##__VA_ARGS__);}while(0)
#define WARN(msg,...) do{_MSG_("WARN",_get_log_file_(),msg,##__VA_ARGS__);}while(0)
#define INFO(msg,...) do{_MSG_("INFO",_get_log_file_(),msg,##__VA_ARGS__);}while(0)

#endif //__LOGGER_HPP__
