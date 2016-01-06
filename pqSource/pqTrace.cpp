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
#include <QPair>
#include <QList>
#include <QHash>
#include <QTime>
#include <QDebug>
#include <QFileInfo>

#include "PREDICATE.h"
#include "pqTrace.h"
#include "SwiPrologEngine.h"

/*
  GUY debugger :)
*/

typedef QPair<QObject*, pqTrace::callback> t_callback;
static QList<t_callback> debug_callbacks;

PREDICATE(pq_trace_interception, 4) {
    //qDebug() << "pq_trace_interception";
    //return FALSE;
    const PlTerm
        &Port = PL_A1,
        &Frame = PL_A2,
        &Choice = PL_A3;
    qDebug() << "pq_trace_interception" << t2w(Port) << t2w(Frame) << t2w(Choice);
    PlTerm Action = PL_A4;
    foreach(t_callback c, debug_callbacks)
        if (c.second(c.first, Port, Frame, Choice, Action)) {
            qDebug() << "action:" << t2w(Action);
            return TRUE;
            //break;
        }
    //qDebug() << "false";
    return FALSE;
}

QString pqTrace::newFileHeader(QString path) {
    QString name = QFileInfo(path).baseName();
    return QString("/*\n\tFile: %1\n*/\n\n:- module(%2, [%2/0]).\n\n%2 :- true.\n").arg(path, name);
}

// assume debug_helper.pl already loaded
//
void pqTrace::add_debug_callback(QObject* target, callback func) {
    foreach (t_callback c, debug_callbacks)
        if (c.first == target && c.second == func)
            return;
    debug_callbacks.append(t_callback(target, func));
}

PREDICATE(pq_trace, 1) {
    qDebug() << QTime::currentTime() << t2w(PL_A1);
    return TRUE;
}
