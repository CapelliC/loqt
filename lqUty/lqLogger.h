/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#ifndef LQLOGGER_H
#define LQLOGGER_H

#include <QSignalMapper>

#include "lqUty_global.h"

//! log async messages - signal/slot/mapper
class LQUTYSHARED_EXPORT lqLogger : public QSignalMapper
{
    Q_OBJECT
public:

    //! define sink (or message target)
    explicit lqLogger(QObject *sink, const char* slot = SLOT(msg(QString)), QObject *parent = 0);

    //! route signal (or async source message) to target
    void print(QObject *source, const char* sig, QString msg);

signals:
    
public slots:
    
};

#endif // LQLOGGER_H
