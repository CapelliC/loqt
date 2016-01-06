/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#ifndef PQTEXTATTRIBUTES_H
#define PQTEXTATTRIBUTES_H

#include "pqSource_global.h"

#include <QHash>
#include <QTextCharFormat>

#define PL_ARITY_AS_SIZE
#include <SWI-cpp.h>

/** decode syntax highlighting attributes
  * this is meant to be used as a static member cache, because those values are fixed on startup
  */
class PQSOURCESHARED_EXPORT pqTextAttributes
{
public:

    /** accumulate attributes from attr_list */
    QTextCharFormat operator[](const PlTerm &attr_list);

    /** apply naive color translation */
    QColor plColor2Qt(const PlTerm &colorname);

protected:

    /** map the actual term list (really, its string representation) to attributes */
    typedef QHash<QString, QTextCharFormat> attrs2format_t;
    attrs2format_t attrs2format;

    typedef QHash<QString, QColor> colorname2color_t;
    colorname2color_t colorname2color;
};

#endif // PQTEXTATTRIBUTES_H
