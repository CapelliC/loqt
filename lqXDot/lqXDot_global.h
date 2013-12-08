#ifndef LQXDOT_GLOBAL_H
#define LQXDOT_GLOBAL_H

#include <QtCore/qglobal.h>

#if defined(LQXDOT_LIBRARY)
#  define LQXDOTSHARED_EXPORT Q_DECL_EXPORT
#else
#  define LQXDOTSHARED_EXPORT Q_DECL_IMPORT
#endif

#endif // LQXDOT_GLOBAL_H
