/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef LQXDOT_GLOBAL_H
#define LQXDOT_GLOBAL_H

#include <QtCore/qglobal.h>

#if defined(LQXDOT_LIBRARY)
#  define LQXDOTSHARED_EXPORT Q_DECL_EXPORT
#else
#  define LQXDOTSHARED_EXPORT Q_DECL_IMPORT
#endif

#endif // LQXDOT_GLOBAL_H
