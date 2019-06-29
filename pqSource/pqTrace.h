/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

#ifndef PQTRACE_H
#define PQTRACE_H

#include "swi.h"
#include <QString>
#include <QObject>

namespace pqTrace
{
    QString newFileHeader(QString path);

    // see http://www.swi-prolog.org/pldoc/doc_for?object=prolog_trace_interception/4 for documentation
    typedef bool (*callback)(QObject *pThis,
         const PlTerm &Port,
         const PlTerm &Frame,
         const PlTerm &Choice,
         PlTerm &Action);

    void add_debug_callback(QObject* target, callback func);
}

#endif // PQTRACE_H
